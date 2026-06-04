// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.serverless.actors

import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}

import io.aura.core.types.*
import io.aura.core.events.*
import io.aura.core.events.SimEventPayload.*
import io.aura.core.engine.TimeCoordinator
import io.aura.core.engine.TimeCoordinator.{EntityCommand, ProcessEvents, StepComplete}
import io.aura.serverless.*

/** Actor representing a serverless workload generator (client).
  *
  * Follows the BrokerActor pattern:
  *   - Generates FunctionInvoke events from arrival patterns at registration time
  *   - Tracks invocation lifecycle (started, completed, timed out, throttled)
  *   - Coordinator terminates on quiescence (event-queue empty + FinalSnapshot handshake)
  */
object ServerlessBrokerActor:

  final case class InvocationBatch(
      functionId: FunctionId,
      count: Int,
      executionLength: MI,
      inputSize: MegaBytes,
      arrivalPattern: ArrivalPattern.ArrivalPattern
  )

  final case class Config(
      brokerId: String,
      platformRef: EntityRef,
      batches: Vector[InvocationBatch],
      coordinator: ActorRef[TimeCoordinator.Command],
      startTime: SimTime = SimTime.Zero
  )

  /** Pre-generate all FunctionInvoke events and pending IDs from batches. Call this on the spawning thread (before
    * actor setup) to avoid race conditions with the TimeCoordinator's event processing.
    */
  def generateEvents(
      config: Config,
      nextInvocationId: Long
  ): (Vector[SimEvent], Set[InvocationId]) =
    val entityRef         = EntityRef(config.brokerId, EntityType.ServerlessBroker)
    val eventsBuilder     = Vector.newBuilder[SimEvent]
    val pendingIdsBuilder = Set.newBuilder[InvocationId]
    val _ = config.batches.foldLeft(nextInvocationId) { case (currentId, batch) =>
      val arrivalTimes = batch.arrivalPattern(batch.count, config.startTime)
      arrivalTimes.foldLeft(currentId) { case (invIdCounter, arrivalTime) =>
        val invId = InvocationId(invIdCounter)
        eventsBuilder += SimEvent(
          time = arrivalTime,
          source = entityRef,
          destination = config.platformRef,
          payload = FunctionInvoke(invId, batch.functionId, batch.executionLength, batch.inputSize),
          serial = SerialNumber.Zero
        )
        pendingIdsBuilder += invId
        invIdCounter + 1
      }
    }
    (eventsBuilder.result(), pendingIdsBuilder.result())

  private final case class BrokerState(
      pendingInvocations: Set[InvocationId],
      completedInvocations: Vector[InvocationId],
      timedOutInvocations: Vector[InvocationId],
      throttledInvocations: Vector[InvocationId]
  ):
    def totalResolved: Int =
      completedInvocations.size + timedOutInvocations.size + throttledInvocations.size

  /** Immutable accumulator for broker event processing. */
  private case class BrokerAccumulator(
      state: BrokerState,
      emittedEvents: Vector[SimEvent]
  )

  /** Create broker with pre-generated pending IDs (events already scheduled by caller). */
  def apply(config: Config, pendingIds: Set[InvocationId]): Behavior[EntityCommand] =
    Behaviors.setup { context =>
      val entityRef = EntityRef(config.brokerId, EntityType.ServerlessBroker)
      config.coordinator ! TimeCoordinator.RegisterEntity(entityRef, context.self)

      val initialState = BrokerState(
        pendingInvocations = pendingIds,
        completedInvocations = Vector.empty,
        timedOutInvocations = Vector.empty,
        throttledInvocations = Vector.empty
      )

      // Empty-input case: no events ever scheduled, coordinator reaches
      // quiescence naturally — no SimulationEnd signal required.
      active(config, initialState, entityRef, context)
    }

  /** Legacy constructor: generates events in actor setup. Suitable for small workloads. */
  def apply(config: Config, nextInvocationId: Long): Behavior[EntityCommand] =
    val (allEvents, allPendingIds) = generateEvents(config, nextInvocationId)
    Behaviors.setup { context =>
      val entityRef = EntityRef(config.brokerId, EntityType.ServerlessBroker)
      config.coordinator ! TimeCoordinator.RegisterEntity(entityRef, context.self)

      if allEvents.nonEmpty then config.coordinator ! TimeCoordinator.ScheduleEvents(allEvents)

      val initialState = BrokerState(
        pendingInvocations = allPendingIds,
        completedInvocations = Vector.empty,
        timedOutInvocations = Vector.empty,
        throttledInvocations = Vector.empty
      )

      active(config, initialState, entityRef, context)
    }

  private def active(
      config: Config,
      state: BrokerState,
      entityRef: EntityRef,
      context: ActorContext[EntityCommand]
  ): Behavior[EntityCommand] =
    Behaviors.receiveMessage {
      case ProcessEvents(events, replyTo) =>
        val initial = BrokerAccumulator(state, Vector.empty)

        val acc = events.foldLeft(initial) { (acc, event) =>
          event.payload match
            case InvocationStarted(invocationId, _, _, _) =>
              // Informational — no state change needed beyond tracking
              acc

            case InvocationComplete(invocationId, _, _, _, _, _) =>
              acc.copy(state =
                acc.state.copy(
                  pendingInvocations = acc.state.pendingInvocations - invocationId,
                  completedInvocations = acc.state.completedInvocations :+ invocationId
                )
              )

            case InvocationTimedOut(invocationId, _, _) =>
              acc.copy(state =
                acc.state.copy(
                  pendingInvocations = acc.state.pendingInvocations - invocationId,
                  timedOutInvocations = acc.state.timedOutInvocations :+ invocationId
                )
              )

            case InvocationThrottled(invocationId, _, _) =>
              acc.copy(state =
                acc.state.copy(
                  pendingInvocations = acc.state.pendingInvocations - invocationId,
                  throttledInvocations = acc.state.throttledInvocations :+ invocationId
                )
              )

            case _ =>
              context.log.debug("Serverless broker {} ignoring event: {}", config.brokerId, event.payload)
              acc
        }

        if acc.emittedEvents.nonEmpty then config.coordinator ! TimeCoordinator.ScheduleEvents(acc.emittedEvents)

        replyTo ! StepComplete(entityRef)
        active(config, acc.state, entityRef, context)

      case TimeCoordinator.FinalSnapshot(_, replyTo) =>
        // No end-of-sim bookkeeping; invocation results are emitted by the
        // FaaS platform as they complete.
        replyTo ! TimeCoordinator.FinalSnapshotComplete(entityRef)
        Behaviors.same
    }
