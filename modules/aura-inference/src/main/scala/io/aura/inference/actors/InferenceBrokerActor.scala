// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.inference.actors

import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}

import io.aura.core.types.*
import io.aura.core.events.*
import io.aura.core.events.SimEventPayload.*
import io.aura.core.engine.TimeCoordinator
import io.aura.core.engine.TimeCoordinator.{EntityCommand, ProcessEvents, StepComplete}
import io.aura.inference.traces.AzureLlmTraceReader

/** Actor that generates and submits inference requests to an engine.
  *
  * Follows the ServerlessBrokerActor pattern:
  *   - Pre-generates events on the guardian thread before spawning
  *   - Tracks pending/completed request IDs
  *   - Reports SLO compliance statistics
  */
object InferenceBrokerActor:

  final case class Config(
      brokerId: String,
      engineRef: EntityRef,
      modelId: ModelId,
      coordinator: ActorRef[TimeCoordinator.Command],
      sloTtft: SimTime = SimTime(0.5),
      sloTpot: SimTime = SimTime(0.05)
  )

  /** Request batch configuration. */
  final case class RequestBatch(
      count: Int,
      promptTokens: Int,
      maxOutputTokens: Int,
      arrivalRatePerSecond: Double,
      startTime: SimTime = SimTime.Zero
  )

  /** Pre-generate all request submission events.
    *
    * Called on the guardian thread before spawning the actor, ensuring events are queued before simulation starts.
    *
    * @return
    *   (events to schedule, set of pending request IDs)
    */
  def generateEvents(
      config: Config,
      batches: Vector[RequestBatch],
      startRequestId: Long
  ): (Vector[SimEvent], Set[InferenceRequestId]) =
    val brokerRef = EntityRef(config.brokerId, EntityType.InferenceBroker)
    val rng       = new java.util.Random(startRequestId)

    case class BatchAcc(events: Vector[SimEvent], nextId: Long)

    val result = batches.foldLeft(BatchAcc(Vector.empty, startRequestId)) { (batchAcc, batch) =>
      val reqResult = (0 until batch.count).foldLeft((batchAcc, batch.startTime)) { case ((acc, currentTime), _) =>
        val reqId = InferenceRequestId(acc.nextId)
        val event = SimEvent(
          time = currentTime,
          source = brokerRef,
          destination = config.engineRef,
          payload = InferenceRequestSubmit(
            requestId = reqId,
            modelId = config.modelId,
            promptTokens = batch.promptTokens,
            maxOutputTokens = batch.maxOutputTokens,
            sloTtft = config.sloTtft,
            sloTpot = config.sloTpot
          ),
          serial = SerialNumber.Zero
        )

        // Poisson inter-arrival
        val interArrival =
          if batch.arrivalRatePerSecond > 0 then -math.log(1.0 - rng.nextDouble()) / batch.arrivalRatePerSecond
          else 1.0
        val nextTime = currentTime + SimTime(interArrival)

        (BatchAcc(acc.events :+ event, acc.nextId + 1), nextTime)
      }
      reqResult._1
    }

    val pendingIds = (startRequestId until result.nextId).map(InferenceRequestId(_)).toSet
    (result.events, pendingIds)

  /** Pre-generate events from trace entries. */
  def generateEventsFromTrace(
      config: Config,
      traceEntries: Vector[AzureLlmTraceReader.TraceEntry],
      startRequestId: Long
  ): (Vector[SimEvent], Set[InferenceRequestId]) =
    val brokerRef = EntityRef(config.brokerId, EntityType.InferenceBroker)

    val allEvents = traceEntries.zipWithIndex.map { (entry, idx) =>
      val reqId = InferenceRequestId(startRequestId + idx)
      SimEvent(
        time = entry.arrivalTime,
        source = brokerRef,
        destination = config.engineRef,
        payload = InferenceRequestSubmit(
          requestId = reqId,
          modelId = config.modelId,
          promptTokens = entry.promptTokens,
          maxOutputTokens = entry.completionTokens,
          sloTtft = config.sloTtft,
          sloTpot = config.sloTpot
        ),
        serial = SerialNumber.Zero
      )
    }
    val pendingIds = (startRequestId until startRequestId + traceEntries.size).map(InferenceRequestId(_)).toSet
    (allEvents, pendingIds)

  def apply(config: Config, pendingIds: Set[InferenceRequestId]): Behavior[EntityCommand] =
    Behaviors.setup { context =>
      val entityRef = EntityRef(config.brokerId, EntityType.InferenceBroker)
      config.coordinator ! TimeCoordinator.RegisterEntity(entityRef, context.self)

      active(config, entityRef, context, pendingIds, Set.empty)
    }

  private def active(
      config: Config,
      entityRef: EntityRef,
      context: ActorContext[EntityCommand],
      pendingIds: Set[InferenceRequestId],
      completedIds: Set[InferenceRequestId]
  ): Behavior[EntityCommand] =
    Behaviors.receiveMessage {
      case ProcessEvents(events, replyTo) =>
        val (newPending, newCompleted) = events.foldLeft((pendingIds, completedIds)) {
          case ((pending, completed), event) =>
            event.payload match
              case InferenceRequestComplete(reqId, _, _, _, _, _, _, _, _) =>
                (pending - reqId, completed + reqId)
              case InferenceRequestFailed(reqId, _) =>
                (pending - reqId, completed)
              case _ =>
                (pending, completed)
        }

        replyTo ! StepComplete(entityRef)
        active(config, entityRef, context, newPending, newCompleted)

      case TimeCoordinator.FinalSnapshot(_, replyTo) =>
        // No end-of-sim bookkeeping; reply immediately so the coordinator
        // can proceed with shutdown.
        replyTo ! TimeCoordinator.FinalSnapshotComplete(entityRef)
        Behaviors.same
    }
