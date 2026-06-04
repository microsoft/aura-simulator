// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.engine

import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors, TimerScheduler}

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.concurrent.duration.*
import io.aura.core.types.*
import io.aura.core.events.*

/** The TimeCoordinator is the heart of Aura's parallel DES engine.
  *
  * Uses parallel dispatch instead of a sequential event loop:
  *   1. Holds an immutable event queue (TreeMap[SimTime, Vector[SimEvent]]) 2. Groups events at time `t` by destination
  *      actor and dispatches in parallel 3. Each actor processes events independently (actor isolation) 4. Actors
  *      report TimeStepComplete — barrier sync before advancing time 5. Determinism: events within an actor ordered by
  *      monotonic serial number
  */
object TimeCoordinator:

  // ─── Protocol ──────────────────────────────────────────────────────────
  sealed trait Command

  /** Schedule a new event into the future event queue. */
  case class ScheduleEvent(event: SimEvent) extends Command

  /** Schedule multiple events at once. */
  case class ScheduleEvents(events: Vector[SimEvent]) extends Command

  /** Entity reports it has finished processing the current time step. */
  case class StepComplete(entityRef: EntityRef) extends Command

  /** Start the simulation. */
  case class StartSimulation(replyTo: ActorRef[SimulationStatus]) extends Command

  /** Register an entity actor. */
  case class RegisterEntity(entityRef: EntityRef, actorRef: ActorRef[EntityCommand]) extends Command

  /** Announce the set of entities the simulation expects to register.
    *
    * Sent by the guardian after `spawnEntities()` returns but BEFORE `StartSimulation`. The coordinator uses this to
    * gate dispatch: if `StartSimulation` arrives while one or more `expectedEntities` have not yet registered, the
    * coordinator buffers the start signal and begins dispatching only once every expected entity has registered.
    *
    * Without this barrier, the asynchronous setup of spawned actors can race the synchronous `StartSimulation`, causing
    * the coordinator to start dispatching before slow entities have registered. Events whose destination is not yet in
    * `state.entities` would be silently dropped, and downstream events those entities would have scheduled (e.g.,
    * periodic energy samples) would never enter the queue. The result is a non-deterministic loss of simulation output
    * that manifests on slower runtimes (CI, smaller heaps, contended dispatchers).
    */
  case class ExpectEntities(expectedEntities: Set[EntityRef]) extends Command

  /** Query current simulation status. */
  case class GetStatus(replyTo: ActorRef[SimulationStatus]) extends Command

  /** Set the network topology for inter-DC latency. */
  case class SetNetworkTopology(topology: NetworkTopology) extends Command

  /** Register which datacenter an entity belongs to. */
  case class RegisterEntityDc(entityRef: EntityRef, datacenterId: DatacenterId) extends Command

  /** Request simulation stop (sent by guardian when broker completes). */
  case object StopSimulation extends Command

  /** Internal: process next time step. */
  private case object ProcessNextStep extends Command

  /** Sent by an entity to acknowledge its `FinalSnapshot`. The entity may (and typically should) emit any end-of-sim
    * bookkeeping events via `ScheduleEvents` BEFORE sending this acknowledgement so they're drained into metrics during
    * shutdown.
    */
  case class FinalSnapshotComplete(entityRef: EntityRef) extends Command

  // ─── Messages sent to entity actors ────────────────────────────────────
  sealed trait EntityCommand
  case class ProcessEvents(
      events: Vector[SimEvent],
      replyTo: ActorRef[Command]
  ) extends EntityCommand

  /** Sent to every registered entity exactly once when the simulation has reached quiescence (event queue is empty, or
    * time has passed `endTime`), but BEFORE the `Completed` status is produced. Lets entities emit any final
    * bookkeeping events (e.g., cost reports for VMs still active at end-of-sim) that should land in
    * `SimulationResults`.
    *
    * `simulationEndTime` is the coordinator's actual current time at quiescence — this is what entities should use as
    * "now" for end-of-sim calculations, NOT their configured `endTime`. If the simulation terminated early via
    * `StopSimulation` or natural queue exhaustion, this value can be well below `endTime`.
    *
    * Contract: an entity that handles `FinalSnapshot` MUST reply with `FinalSnapshotComplete(entityRef)` to the
    * coordinator's `replyTo` after scheduling whatever events it wants recorded. Entities that have nothing to flush
    * should still reply immediately. Events scheduled inside a `FinalSnapshot` handler are dispatched + recorded in
    * metrics before the coordinator emits `Completed`.
    */
  case class FinalSnapshot(simulationEndTime: SimTime, replyTo: ActorRef[Command]) extends EntityCommand

  // ─── Status responses ──────────────────────────────────────────────────
  sealed trait SimulationStatus
  case class Running(currentTime: SimTime, eventsProcessed: Long) extends SimulationStatus
  case class Completed(
      endTime: SimTime,
      totalEventsProcessed: Long,
      results: SimulationResults
  ) extends SimulationStatus
  case class Failed(reason: String) extends SimulationStatus

  // ─── State ─────────────────────────────────────────────────────────────
  private final case class CoordinatorState(
      currentTime: SimTime,
      eventQueue: TreeMap[SimTime, Vector[SimEvent]],
      entities: Map[EntityRef, ActorRef[EntityCommand]],
      pendingCompletions: Set[EntityRef],
      totalEventsProcessed: Long,
      nextSerial: SerialNumber,
      endTime: SimTime,
      metricsCollector: MetricsCollector,
      statusListener: Option[ActorRef[SimulationStatus]],
      networkTopology: NetworkTopology = NetworkTopology.empty,
      entityDatacenter: Map[EntityRef, DatacenterId] = Map.empty,
      simulationEnded: Boolean = false,
      pendingFinalSnapshots: Set[EntityRef] = Set.empty,
      // Registration barrier: simulation cannot begin dispatching until every
      // expected entity has registered. See ExpectEntities for rationale.
      expectedEntities: Set[EntityRef] = Set.empty,
      bufferedStart: Option[ActorRef[SimulationStatus]] = None
  ):
    def enqueue(event: SimEvent): CoordinatorState =
      // Apply inter-DC latency if source and destination are in different DCs
      val adjustedTime =
        val srcDc = entityDatacenter.get(event.source)
        val dstDc = entityDatacenter.get(event.destination)
        (srcDc, dstDc) match
          case (Some(s), Some(d)) if s != d =>
            event.time + networkTopology.delay(s, d)
          case _ => event.time

      val serialEvent = event.copy(time = adjustedTime, serial = nextSerial)
      val updated = eventQueue.updatedWith(serialEvent.time) {
        case Some(existing) => Some(existing :+ serialEvent)
        case None           => Some(Vector(serialEvent))
      }
      copy(eventQueue = updated, nextSerial = nextSerial.next)

    def enqueueAll(events: Vector[SimEvent]): CoordinatorState =
      events.foldLeft(this)(_.enqueue(_))

    def dequeueNextTimeStep: Option[(SimTime, Vector[SimEvent], CoordinatorState)] =
      eventQueue.headOption.map { case (time, events) =>
        (time, events, copy(eventQueue = eventQueue.removed(time), currentTime = time))
      }

  /** Internal: registration-barrier wall-clock timeout fired. */
  private case object RegistrationBarrierTimeout extends Command

  /** Default wall-clock budget for entities to register after `StartSimulation` arrives. Generous because slow CI
    * runners can take several seconds to spawn many actors; tight enough to surface authoring bugs (e.g., an entity
    * type added to `expectedEntities` but never spawned) within a single test run.
    */
  private val RegistrationBarrierBudget = 30.seconds

  // ─── Actor behavior ────────────────────────────────────────────────────
  def apply(endTime: SimTime): Behavior[Command] =
    Behaviors.setup { context =>
      Behaviors.withTimers { timers =>
        idle(
          CoordinatorState(
            currentTime = SimTime.Zero,
            eventQueue = TreeMap.empty(using SimTime.given_Ordering_SimTime),
            entities = Map.empty,
            pendingCompletions = Set.empty,
            totalEventsProcessed = 0L,
            nextSerial = SerialNumber.Zero,
            endTime = endTime,
            metricsCollector = MetricsCollector.empty,
            statusListener = None
          ),
          context,
          timers
        )
      }
    }

  private def idle(
      state: CoordinatorState,
      context: ActorContext[Command],
      timers: TimerScheduler[Command]
  ): Behavior[Command] =
    Behaviors.receiveMessage {
      case RegisterEntity(entityRef, actorRef) =>
        context.log.debug("Registered entity: {}", entityRef.name)
        val updated = state.copy(entities = state.entities + (entityRef -> actorRef))
        // Registration may have completed the barrier — if a Start has been
        // buffered and every expected entity is now registered, transition.
        maybeStart(updated, context, timers).getOrElse(idle(updated, context, timers))

      case ScheduleEvent(event) =>
        idle(state.enqueue(event), context, timers)

      case ScheduleEvents(events) =>
        idle(state.enqueueAll(events), context, timers)

      case SetNetworkTopology(topology) =>
        idle(state.copy(networkTopology = topology), context, timers)

      case RegisterEntityDc(entityRef, dcId) =>
        idle(state.copy(entityDatacenter = state.entityDatacenter + (entityRef -> dcId)), context, timers)

      case ExpectEntities(refs) =>
        idle(state.copy(expectedEntities = state.expectedEntities ++ refs), context, timers)

      case StartSimulation(replyTo) =>
        val withReplyTo = state.copy(
          statusListener = Some(replyTo),
          bufferedStart = Some(replyTo)
        )
        maybeStart(withReplyTo, context, timers).getOrElse {
          val missing = withReplyTo.expectedEntities -- withReplyTo.entities.keySet
          context.log.info(
            "StartSimulation received; waiting on {} entit{} to register before dispatch begins",
            missing.size,
            if missing.size == 1 then "y" else "ies"
          )
          // Arm the wall-clock barrier timeout. If entities don't all
          // register within the budget, we fail fast with a clear error
          // identifying the missing names — much friendlier than the
          // generic SimulationRunner timeout that would otherwise fire
          // 10 minutes later.
          timers.startSingleTimer(
            "registration-barrier",
            RegistrationBarrierTimeout,
            RegistrationBarrierBudget
          )
          idle(withReplyTo, context, timers)
        }

      case RegistrationBarrierTimeout =>
        val missing = state.expectedEntities -- state.entities.keySet
        val reason =
          s"Registration barrier timed out after ${RegistrationBarrierBudget.toSeconds}s. " +
            s"${missing.size} expected entit${if missing.size == 1 then "y" else "ies"} never registered: " +
            missing.map(_.name).toVector.sorted.mkString(", ") + ". " +
            "Likely cause: an entity name in DslSimulationConfig.expectedEntityRefs does not match the name the " +
            "actor uses in its RegisterEntity call, or the actor failed to spawn."
        context.log.error(reason)
        state.bufferedStart.foreach(_ ! Failed(reason))
        Behaviors.stopped

      case GetStatus(replyTo) =>
        replyTo ! Running(state.currentTime, state.totalEventsProcessed)
        Behaviors.same

      case _ => Behaviors.same
    }

  /** If a StartSimulation has been buffered and every expected entity is now registered, transition to `running`.
    * Cancels the registration-barrier timeout on success. Otherwise returns None and the coordinator stays idle.
    */
  private def maybeStart(
      state: CoordinatorState,
      context: ActorContext[Command],
      timers: TimerScheduler[Command]
  ): Option[Behavior[Command]] =
    state.bufferedStart match
      case Some(_) if state.expectedEntities.subsetOf(state.entities.keySet) =>
        timers.cancel("registration-barrier")
        context.log.info(
          "Starting simulation (endTime={}, entities={})",
          state.endTime.value,
          state.entities.size
        )
        context.self ! ProcessNextStep
        Some(running(state.copy(bufferedStart = None), context))
      case _ => None

  private def running(state: CoordinatorState, context: ActorContext[Command]): Behavior[Command] =
    Behaviors.receiveMessage {
      case ScheduleEvent(event) =>
        running(state.enqueue(event), context)

      case ScheduleEvents(events) =>
        running(state.enqueueAll(events), context)

      case StopSimulation =>
        // External early-stop. Treat as immediate quiescence — give entities
        // a chance to flush via FinalSnapshot, then complete.
        beginFinalSnapshot(state.copy(simulationEnded = true), context)

      case ProcessNextStep =>
        if state.simulationEnded then beginFinalSnapshot(state, context)
        else
          state.dequeueNextTimeStep match
            case None =>
              // Natural quiescence — event queue is empty.
              beginFinalSnapshot(state, context)

            case Some((time, events, newState)) =>
              if time > state.endTime then
                // endTime exceeded — treat as quiescence (do not dispatch the
                // overshoot events; they belong to a future the simulation
                // doesn't observe).
                beginFinalSnapshot(state, context)
              else
                // Record ALL events in the metrics collector
                val updatedMetrics = events.foldLeft(newState.metricsCollector) { (mc, event) =>
                  mc.recordEvent(event)
                }

                // Group events by destination and dispatch in parallel
                val eventsByDest = events.groupBy(_.destination)
                val destinations = eventsByDest.keySet

                val dispatched = destinations.flatMap { dest =>
                  state.entities.get(dest).map { actorRef =>
                    val destEvents = eventsByDest(dest).sortBy(_.serial)
                    actorRef ! ProcessEvents(destEvents, context.self)
                    dest
                  }
                }

                if dispatched.isEmpty then
                  // No actors to wait for — advance immediately
                  val finalState = newState.copy(
                    totalEventsProcessed = newState.totalEventsProcessed + events.size,
                    metricsCollector = updatedMetrics
                  )
                  context.self ! ProcessNextStep
                  running(finalState, context)
                else
                  // Barrier sync: wait for all dispatched actors to complete
                  val finalState = newState.copy(
                    pendingCompletions = dispatched,
                    totalEventsProcessed = newState.totalEventsProcessed + events.size,
                    metricsCollector = updatedMetrics
                  )
                  waitingForBarrier(finalState, context)

      case StepComplete(entityRef) =>
        // Stale completion from previous step — ignore
        running(state, context)

      case GetStatus(replyTo) =>
        replyTo ! Running(state.currentTime, state.totalEventsProcessed)
        Behaviors.same

      case RegisterEntity(entityRef, actorRef) =>
        running(state.copy(entities = state.entities + (entityRef -> actorRef)), context)

      case _ => Behaviors.same
    }

  /** Waiting for all actors to finish processing the current time step. */
  private def waitingForBarrier(
      state: CoordinatorState,
      context: ActorContext[Command]
  ): Behavior[Command] =
    Behaviors.receiveMessage {
      case StepComplete(entityRef) =>
        val remaining = state.pendingCompletions - entityRef
        if remaining.isEmpty then
          // All actors done — advance to next time step
          context.self ! ProcessNextStep
          running(state.copy(pendingCompletions = Set.empty), context)
        else waitingForBarrier(state.copy(pendingCompletions = remaining), context)

      case StopSimulation =>
        waitingForBarrier(state.copy(simulationEnded = true), context)

      case ScheduleEvent(event) =>
        waitingForBarrier(state.enqueue(event), context)

      case ScheduleEvents(events) =>
        waitingForBarrier(state.enqueueAll(events), context)

      case GetStatus(replyTo) =>
        replyTo ! Running(state.currentTime, state.totalEventsProcessed)
        Behaviors.same

      case RegisterEntity(entityRef, actorRef) =>
        waitingForBarrier(
          state.copy(entities = state.entities + (entityRef -> actorRef)),
          context
        )

      case _ => Behaviors.same
    }

  /** Drain all remaining events from the queue (up to endTime) into the metrics collector. Events are only recorded in
    * metrics — they are NOT dispatched to actors. Used during shutdown to ensure end-of-sim bookkeeping events (e.g.,
    * events emitted from `FinalSnapshot` handlers, or events queued between the last barrier and termination) are
    * reflected in `SimulationResults`.
    */
  private def drainRemainingEvents(state: CoordinatorState): CoordinatorState =
    @tailrec
    def drain(current: CoordinatorState): CoordinatorState =
      current.dequeueNextTimeStep match
        case Some((time, events, newState)) if time <= current.endTime =>
          val updatedMetrics = events.foldLeft(newState.metricsCollector) { (mc, event) =>
            mc.recordEvent(event)
          }
          drain(
            newState.copy(
              totalEventsProcessed = newState.totalEventsProcessed + events.size,
              metricsCollector = updatedMetrics
            )
          )
        case _ =>
          current
    drain(state)

  /** Initiate end-of-simulation handshake.
    *
    * Termination is unified through a single path: when the coordinator decides the simulation is over (queue
    * exhausted, `endTime` exceeded, or an external `StopSimulation` arrived), it sends `FinalSnapshot` to every
    * registered entity. Entities optionally emit final bookkeeping events (e.g., cost reports for VMs still active at
    * end-of-sim), then reply `FinalSnapshotComplete`. Once all replies are in, the coordinator drains any events those
    * handlers scheduled, records them in metrics, and emits `Completed`.
    *
    * If there are no registered entities (degenerate case), completion is immediate.
    */
  private def beginFinalSnapshot(
      state: CoordinatorState,
      context: ActorContext[Command]
  ): Behavior[Command] =
    val targets = state.entities.toVector
    if targets.isEmpty then completeSimulation(state, context)
    else
      context.log.info(
        "Simulation reached quiescence at time={}, requesting FinalSnapshot from {} entit{}",
        state.currentTime.value,
        targets.size,
        if targets.size == 1 then "y" else "ies"
      )
      targets.foreach { case (_, actorRef) =>
        actorRef ! FinalSnapshot(state.currentTime, context.self)
      }
      waitingForFinalSnapshots(
        state.copy(pendingFinalSnapshots = targets.map(_._1).toSet),
        context
      )

  /** Coordinator state while waiting for every entity to acknowledge its FinalSnapshot. Entities may schedule events
    * during this window via `ScheduleEvents`; those get drained and recorded after all acks land.
    */
  private def waitingForFinalSnapshots(
      state: CoordinatorState,
      context: ActorContext[Command]
  ): Behavior[Command] =
    Behaviors.receiveMessage {
      case FinalSnapshotComplete(entityRef) =>
        val remaining = state.pendingFinalSnapshots - entityRef
        if remaining.isEmpty then completeSimulation(state.copy(pendingFinalSnapshots = Set.empty), context)
        else waitingForFinalSnapshots(state.copy(pendingFinalSnapshots = remaining), context)

      case ScheduleEvent(event) =>
        waitingForFinalSnapshots(state.enqueue(event), context)

      case ScheduleEvents(events) =>
        waitingForFinalSnapshots(state.enqueueAll(events), context)

      case GetStatus(replyTo) =>
        replyTo ! Running(state.currentTime, state.totalEventsProcessed)
        Behaviors.same

      // Ignore late StepComplete / further StopSimulation / RegisterEntity in this terminal phase.
      case _ => Behaviors.same
    }

  /** Final step: drain remaining queued events into metrics and emit Completed. */
  private def completeSimulation(
      state: CoordinatorState,
      context: ActorContext[Command]
  ): Behavior[Command] =
    val drained = drainRemainingEvents(state)
    context.log.info(
      "Simulation complete at time={}, events={}",
      drained.currentTime.value,
      drained.totalEventsProcessed
    )
    drained.statusListener.foreach(
      _ ! Completed(
        drained.currentTime,
        drained.totalEventsProcessed,
        SimulationResults.fromMetrics(drained.metricsCollector, drained.currentTime)
      )
    )
    Behaviors.stopped
