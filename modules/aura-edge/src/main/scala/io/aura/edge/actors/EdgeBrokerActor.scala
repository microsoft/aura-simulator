// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.edge.actors

import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}

import io.aura.core.types.*
import io.aura.core.events.*
import io.aura.core.events.SimEventPayload.*
import io.aura.core.engine.TimeCoordinator
import io.aura.core.engine.TimeCoordinator.{EntityCommand, ProcessEvents, StepComplete}

/** Actor representing an edge task broker.
  *
  * Follows K8sBrokerActor pattern:
  *   - Tracks task lifecycle (started, completed, failed)
  *   - Coordinator terminates on quiescence (event-queue empty + FinalSnapshot handshake)
  *
  * Initial EdgeTaskSubmit events are scheduled by the guardian (spawnEntities) to avoid race conditions with
  * coordinator startup.
  */
object EdgeBrokerActor:

  final case class TaskBatch(
      sourceNodeName: String,
      count: Int,
      cpuRequired: MIPS,
      memRequired: MegaBytes,
      taskLength: MI,
      deadline: SimTime,
      interArrivalTime: SimTime
  )

  final case class Config(
      brokerId: String,
      environmentRef: EntityRef,
      coordinator: ActorRef[TimeCoordinator.Command]
  )

  private final case class BrokerState(
      totalTasks: Int,
      pendingTasks: Set[EdgeTaskId],
      runningTasks: Set[EdgeTaskId],
      completedTasks: Vector[EdgeTaskId],
      failedTasks: Vector[EdgeTaskId]
  ):
    def totalResolved: Int = completedTasks.size + failedTasks.size

  /** Immutable accumulator for broker event processing. */
  private case class BrokerAccumulator(
      state: BrokerState,
      emittedEvents: Vector[SimEvent]
  )

  /** Generate EdgeTaskSubmit events for all batches. Called from guardian. */
  def generateEvents(
      brokerRef: EntityRef,
      environmentRef: EntityRef,
      batches: Vector[TaskBatch],
      startTime: SimTime,
      nextTaskId: Long
  ): (Vector[SimEvent], Set[EdgeTaskId]) =
    val eventsBuilder     = Vector.newBuilder[SimEvent]
    val pendingIdsBuilder = Set.newBuilder[EdgeTaskId]
    val _ = batches.foldLeft(nextTaskId) { case (currentId, batch) =>
      (0 until batch.count).foldLeft(currentId) { case (taskIdCounter, i) =>
        val taskId      = EdgeTaskId(taskIdCounter)
        val arrivalTime = startTime + SimTime(batch.interArrivalTime.value * i)
        eventsBuilder += SimEvent(
          time = arrivalTime,
          source = brokerRef,
          destination = environmentRef,
          payload = EdgeTaskSubmit(
            taskId,
            batch.sourceNodeName,
            batch.cpuRequired,
            batch.memRequired,
            batch.taskLength,
            batch.deadline
          ),
          serial = SerialNumber.Zero
        )
        pendingIdsBuilder += taskId
        taskIdCounter + 1
      }
    }
    (eventsBuilder.result(), pendingIdsBuilder.result())

  def apply(config: Config, pendingTaskIds: Set[EdgeTaskId]): Behavior[EntityCommand] =
    Behaviors.setup { context =>
      val entityRef = EntityRef(config.brokerId, EntityType.EdgeEnvironment)
      config.coordinator ! TimeCoordinator.RegisterEntity(entityRef, context.self)

      val initialState = BrokerState(
        totalTasks = pendingTaskIds.size,
        pendingTasks = pendingTaskIds,
        runningTasks = Set.empty,
        completedTasks = Vector.empty,
        failedTasks = Vector.empty
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
            case EdgeTaskStarted(taskId, _, _, _, _, _) =>
              acc.copy(
                state = acc.state.copy(
                  pendingTasks = acc.state.pendingTasks - taskId,
                  runningTasks = acc.state.runningTasks + taskId
                )
              )

            case EdgeTaskCompleted(taskId, _, _, _, _, _, _, _) =>
              acc.copy(state =
                acc.state.copy(
                  runningTasks = acc.state.runningTasks - taskId,
                  completedTasks = acc.state.completedTasks :+ taskId
                )
              )

            case EdgeTaskFailed(taskId, _) =>
              acc.copy(state =
                acc.state.copy(
                  pendingTasks = acc.state.pendingTasks - taskId,
                  runningTasks = acc.state.runningTasks - taskId,
                  failedTasks = acc.state.failedTasks :+ taskId
                )
              )

            case _ =>
              context.log.debug("Edge broker {} ignoring event: {}", config.brokerId, event.payload)
              acc
        }

        if acc.emittedEvents.nonEmpty then config.coordinator ! TimeCoordinator.ScheduleEvents(acc.emittedEvents)

        replyTo ! StepComplete(entityRef)
        active(config, acc.state, entityRef, context)

      case TimeCoordinator.FinalSnapshot(_, replyTo) =>
        // Task results are emitted by the environment as tasks complete;
        // no end-of-sim bookkeeping required.
        replyTo ! TimeCoordinator.FinalSnapshotComplete(entityRef)
        Behaviors.same
    }
