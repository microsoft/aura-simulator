// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.actors

import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}

import io.aura.core.types.*
import io.aura.core.events.*
import io.aura.core.events.SimEventPayload.*
import io.aura.core.engine.TimeCoordinator
import io.aura.core.engine.TimeCoordinator.{EntityCommand, ProcessEvents, StepComplete}

/** Actor representing an HPC/Batch job scheduler.
  *
  * Manages a JobQueue with ComputeNodes, applies a scheduling algorithm (FCFS, SJF, priority-based, gang, backfill)
  * each tick, and tracks job completion based on estimated runtimes.
  */
object BatchBrokerActor:

  /** Scheduling algorithm type: transforms a JobQueue at a given time. */
  type BatchSchedulingAlgorithm = (JobQueue, SimTime) => JobQueue

  final case class Config(
      brokerId: String,
      nodes: Vector[ComputeNode],
      jobs: Vector[BatchJob],
      algorithm: BatchSchedulingAlgorithm,
      coordinator: ActorRef[TimeCoordinator.Command],
      tickInterval: SimTime = SimTime(10.0)
  )

  private final case class BatchState(
      queue: JobQueue,
      submittedJobs: Set[JobId],
      completedCount: Int,
      totalJobs: Int
  )

  /** Build this broker's initial events. Called by the actor itself during setup so RegisterEntity and ScheduleEvents
    * both originate from the broker actor — Pekko guarantees mailbox order between a single pair of actors, so the
    * coordinator will see RegisterEntity before any of these events get dequeued. This eliminates the race where events
    * were scheduled by the guardian thread before the broker had registered, causing them to be silently dropped on
    * dispatch.
    */
  private def buildInitialEvents(config: Config, entityRef: EntityRef): Vector[SimEvent] =
    val submitEvents = config.jobs.map { job =>
      SimEvent(
        time = job.submitTime,
        source = entityRef,
        destination = entityRef,
        payload = BatchJobSubmitted(job.id, job.name, job.priority.toString, job.requiredNodes, job.submitTime),
        serial = SerialNumber.Zero
      )
    }

    val tickEvent = SimEvent(
      time = SimTime.Zero,
      source = entityRef,
      destination = entityRef,
      payload = BatchScheduleTick(SimTime.Zero),
      serial = SerialNumber.Zero
    )

    submitEvents :+ tickEvent

  def apply(config: Config): Behavior[EntityCommand] =
    Behaviors.setup { context =>
      val entityRef = EntityRef(s"batch-${config.brokerId}", EntityType.BatchBroker)
      config.coordinator ! TimeCoordinator.RegisterEntity(entityRef, context.self)

      val initialEvents = buildInitialEvents(config, entityRef)
      if initialEvents.nonEmpty then config.coordinator ! TimeCoordinator.ScheduleEvents(initialEvents)

      val initialQueue = JobQueue(
        pending = config.jobs,
        running = Vector.empty,
        completed = Vector.empty,
        nodes = config.nodes,
        reservations = Vector.empty
      )

      active(
        config,
        BatchState(
          queue = initialQueue,
          submittedJobs = config.jobs.map(_.id).toSet,
          completedCount = 0,
          totalJobs = config.jobs.size
        ),
        entityRef,
        context
      )
    }

  private case class BatchAccumulator(
      state: BatchState,
      emittedEvents: Vector[SimEvent]
  )

  private def active(
      config: Config,
      state: BatchState,
      entityRef: EntityRef,
      context: ActorContext[EntityCommand]
  ): Behavior[EntityCommand] =
    Behaviors.receiveMessage {
      case ProcessEvents(events, replyTo) =>
        val initial = BatchAccumulator(state, Vector.empty)

        val acc = events.foldLeft(initial) { (acc, event) =>
          event.payload match
            case BatchScheduleTick(currentTime) =>
              // Step 1: Complete any running jobs whose estimated runtime has elapsed
              val (updatedQueue, completionEvents) = completeFinishedJobs(
                acc.state.queue,
                currentTime,
                entityRef
              )

              // Step 2: Run the scheduling algorithm to assign pending jobs
              val scheduledQueue = config.algorithm(updatedQueue, currentTime)

              // Step 3: Detect newly started jobs and emit events
              val previousRunningIds = updatedQueue.running.map(_.id).toSet
              val newlyStarted       = scheduledQueue.running.filterNot(j => previousRunningIds.contains(j.id))
              val startEvents = newlyStarted.map { job =>
                SimEvent(
                  time = currentTime,
                  source = entityRef,
                  destination = entityRef,
                  payload = BatchJobStarted(job.id, currentTime, job.requiredNodes),
                  serial = SerialNumber.Zero
                )
              }

              val newCompletedCount = acc.state.completedCount + completionEvents.size

              // Schedule the next tick only while there's work pending. When
              // all jobs are done, stop scheduling — the coordinator will
              // observe quiescence on the event queue and terminate naturally
              // (no SimulationEnd signal needed).
              val allDone = newCompletedCount >= acc.state.totalJobs &&
                scheduledQueue.pending.isEmpty && scheduledQueue.running.isEmpty

              val tickEvents = if allDone then
                context.log.info("BatchBroker {} - all {} jobs completed", config.brokerId, acc.state.totalJobs)
                Vector.empty
              else
                Vector(
                  SimEvent(
                    time = currentTime + config.tickInterval,
                    source = entityRef,
                    destination = entityRef,
                    payload = BatchScheduleTick(currentTime + config.tickInterval),
                    serial = SerialNumber.Zero
                  )
                )

              acc.copy(
                state = acc.state.copy(
                  queue = scheduledQueue,
                  completedCount = newCompletedCount
                ),
                emittedEvents = acc.emittedEvents ++ completionEvents ++ startEvents ++ tickEvents
              )

            case _ =>
              acc
        }

        if acc.emittedEvents.nonEmpty then config.coordinator ! TimeCoordinator.ScheduleEvents(acc.emittedEvents)

        replyTo ! StepComplete(entityRef)
        active(config, acc.state, entityRef, context)

      case TimeCoordinator.FinalSnapshot(_, replyTo) =>
        // Batch broker has no end-of-sim bookkeeping; completion events are
        // emitted continuously as jobs finish.
        replyTo ! TimeCoordinator.FinalSnapshotComplete(entityRef)
        Behaviors.same
    }

  /** Complete running jobs whose estimated runtime has elapsed. */
  private def completeFinishedJobs(
      queue: JobQueue,
      currentTime: SimTime,
      entityRef: EntityRef
  ): (JobQueue, Vector[SimEvent]) =
    val (finished, stillRunning) = queue.running.partition { job =>
      job.startTime match
        case Some(start) => currentTime.value >= start.value + job.estimatedRuntime.value
        case None        => false
    }

    val completionEvents = finished.map { job =>
      val startTime  = job.startTime.getOrElse(SimTime.Zero)
      val turnaround = SimTime(currentTime.value - job.submitTime.value)
      SimEvent(
        time = currentTime,
        source = entityRef,
        destination = EntityRef("sim-guardian", EntityType.SimGuardian),
        payload = BatchJobCompleted(job.id, startTime, currentTime, turnaround),
        serial = SerialNumber.Zero
      )
    }

    // Release resources for completed jobs
    val completedQueue = finished.foldLeft(queue) { (q, job) =>
      BatchScheduler.completeJob(q, job.id, currentTime)
    }

    (completedQueue, completionEvents)
