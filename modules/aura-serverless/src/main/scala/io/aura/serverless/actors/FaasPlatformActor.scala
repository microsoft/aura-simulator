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
import io.aura.serverless.state.*

/** Actor representing a FaaS (Function-as-a-Service) platform.
  *
  * Follows the DatacenterActor pattern:
  *   - Registers with TimeCoordinator
  *   - Receives ProcessEvents, foldLeft over events
  *   - Emits events via ScheduleEvents, replies StepComplete
  *
  * Models cold starts, concurrency limits, container reuse, billing, and timeouts. Uses abstract executionMips for
  * compute speed (no IaaS host coupling).
  */
object FaasPlatformActor:

  final case class Config(
      platformName: String,
      coldStartModel: ColdStartModel.ColdStartModel,
      billingModel: BillingModel.BillingModel,
      scalingPolicy: ScalingPolicy.ScalingPolicy,
      containerTtl: SimTime,
      coordinator: ActorRef[TimeCoordinator.Command],
      executionMips: MIPS = MIPS(1000.0)
  )

  /** Immutable accumulator for platform event processing. */
  private case class PlatformAccumulator(
      state: PlatformState,
      emittedEvents: Vector[SimEvent]
  )

  def apply(config: Config): Behavior[EntityCommand] =
    Behaviors.setup { context =>
      val entityRef = EntityRef(config.platformName, EntityType.FaasPlatform)
      config.coordinator ! TimeCoordinator.RegisterEntity(entityRef, context.self)

      active(config, PlatformState.empty, entityRef, context)
    }

  private def active(
      config: Config,
      state: PlatformState,
      entityRef: EntityRef,
      context: ActorContext[EntityCommand]
  ): Behavior[EntityCommand] =
    Behaviors.receiveMessage {
      case ProcessEvents(events, replyTo) =>
        val initial = PlatformAccumulator(state, Vector.empty)

        val acc = events.foldLeft(initial) { (acc, event) =>
          event.payload match
            case FunctionDeploy(functionId, spec) =>
              val funcConfig = FunctionConfig(
                functionId = functionId,
                name = spec.name,
                memoryMB = spec.memoryMB,
                timeout = spec.timeout,
                runtime = Runtime.valueOf(spec.runtime),
                concurrencyLimit = spec.concurrencyLimit,
                reservedConcurrency = spec.reservedConcurrency
              )
              acc.copy(
                state = acc.state.copy(
                  functions = acc.state.functions + (functionId -> funcConfig)
                )
              )

            case FunctionInvoke(invocationId, functionId, executionLength, inputSize) =>
              acc.state.functions.get(functionId) match
                case None =>
                  // Unknown function — throttle with error
                  val throttleEvent = SimEvent(
                    time = event.time,
                    source = entityRef,
                    destination = event.source,
                    payload = InvocationThrottled(invocationId, functionId, "Unknown function"),
                    serial = SerialNumber.Zero
                  )
                  acc.copy(emittedEvents = acc.emittedEvents :+ throttleEvent)

                case Some(funcConfig) =>
                  val currentConc = acc.state.currentConcurrency(functionId)

                  // Check concurrency limit
                  if currentConc >= funcConfig.concurrencyLimit then
                    val throttleEvent = SimEvent(
                      time = event.time,
                      source = entityRef,
                      destination = event.source,
                      payload = InvocationThrottled(
                        invocationId,
                        functionId,
                        s"Concurrency limit reached (${funcConfig.concurrencyLimit})"
                      ),
                      serial = SerialNumber.Zero
                    )
                    acc.copy(emittedEvents = acc.emittedEvents :+ throttleEvent)
                  else
                    // Find warm container or create cold
                    val warmContainerOpt = acc.state.findWarmContainer(functionId, event.time, config.containerTtl)

                    val (containerId, coldStart, updatedContainers, nextContId) = warmContainerOpt match
                      case Some(cid) =>
                        // Reuse warm container
                        val updatedContainer = acc.state
                          .containers(cid)
                          .copy(
                            status = ContainerStatus.Executing,
                            lastUsedAt = event.time,
                            activeInvocations = acc.state.containers(cid).activeInvocations + 1
                          )
                        (cid, false, acc.state.containers + (cid -> updatedContainer), acc.state.nextContainerId)

                      case None =>
                        // Create new container (cold start)
                        val cid = ContainerId(acc.state.nextContainerId)
                        val newContainer = ContainerState(
                          containerId = cid,
                          functionId = functionId,
                          status = ContainerStatus.Executing,
                          createdAt = event.time,
                          lastUsedAt = event.time,
                          activeInvocations = 1
                        )
                        (cid, true, acc.state.containers + (cid -> newContainer), acc.state.nextContainerId + 1)

                    // Compute times
                    val coldStartDelay =
                      if coldStart then config.coldStartModel(funcConfig.memoryMB, funcConfig.runtime)
                      else SimTime.Zero
                    val executionTime = executionLength.executionTime(config.executionMips)
                    val startTime     = event.time + coldStartDelay
                    val finishTime    = startTime + executionTime

                    // Check timeout
                    if executionTime + coldStartDelay > funcConfig.timeout then
                      // Timed out — release container, don't increment concurrency
                      val timeoutEvent = SimEvent(
                        time = event.time + funcConfig.timeout,
                        source = entityRef,
                        destination = event.source,
                        payload = InvocationTimedOut(
                          invocationId,
                          functionId,
                          s"Exceeded timeout of ${funcConfig.timeout.value}s"
                        ),
                        serial = SerialNumber.Zero
                      )
                      // Release the container back to idle
                      val releasedContainer = updatedContainers(containerId).copy(
                        status = ContainerStatus.Idle,
                        activeInvocations = updatedContainers(containerId).activeInvocations - 1
                      )
                      acc.copy(
                        state = acc.state.copy(
                          containers = updatedContainers + (containerId -> releasedContainer),
                          nextContainerId = nextContId
                        ),
                        emittedEvents = acc.emittedEvents :+ timeoutEvent
                      )
                    else
                      // Success path: compute billing, schedule events
                      val billedGBs = config.billingModel(funcConfig.memoryMB, executionTime)

                      val invocation = ActiveInvocation(
                        invocationId = invocationId,
                        functionId = functionId,
                        containerId = containerId,
                        startTime = startTime,
                        expectedFinishTime = finishTime,
                        coldStart = coldStart
                      )

                      // InvocationStarted → broker
                      val startedEvent = SimEvent(
                        time = startTime,
                        source = entityRef,
                        destination = event.source,
                        payload = InvocationStarted(invocationId, functionId, coldStart, containerId),
                        serial = SerialNumber.Zero
                      )

                      // InvocationComplete → self (state bookkeeping)
                      val completeSelfEvent = SimEvent(
                        time = finishTime,
                        source = entityRef,
                        destination = entityRef,
                        payload =
                          InvocationComplete(invocationId, functionId, startTime, finishTime, billedGBs, coldStart),
                        serial = SerialNumber.Zero
                      )

                      // InvocationComplete → broker (result tracking)
                      val completeBrokerEvent = SimEvent(
                        time = finishTime,
                        source = entityRef,
                        destination = event.source,
                        payload =
                          InvocationComplete(invocationId, functionId, startTime, finishTime, billedGBs, coldStart),
                        serial = SerialNumber.Zero
                      )

                      val updatedConcurrency = acc.state.activeConcurrency +
                        (functionId -> (currentConc + 1))

                      acc.copy(
                        state = acc.state.copy(
                          containers = updatedContainers,
                          activeInvocations = acc.state.activeInvocations + (invocationId -> invocation),
                          nextContainerId = nextContId,
                          activeConcurrency = updatedConcurrency
                        ),
                        emittedEvents = acc.emittedEvents :+ startedEvent :+ completeSelfEvent :+ completeBrokerEvent
                      )

            case InvocationComplete(invocationId, functionId, _, _, _, _) =>
              // Self-delivered: release container and decrement concurrency
              acc.state.activeInvocations.get(invocationId) match
                case Some(inv) =>
                  val container = acc.state.containers.get(inv.containerId)
                  val updatedContainers = container match
                    case Some(cs) =>
                      val newActive = (cs.activeInvocations - 1).max(0)
                      val newStatus = if newActive == 0 then ContainerStatus.Idle else ContainerStatus.Executing
                      acc.state.containers + (inv.containerId -> cs.copy(
                        status = newStatus,
                        activeInvocations = newActive
                      ))
                    case None => acc.state.containers

                  val currentConc = acc.state.currentConcurrency(functionId)
                  val updatedConcurrency = acc.state.activeConcurrency +
                    (functionId -> (currentConc - 1).max(0))

                  acc.copy(
                    state = acc.state.copy(
                      containers = updatedContainers,
                      activeInvocations = acc.state.activeInvocations - invocationId,
                      activeConcurrency = updatedConcurrency
                    )
                  )

                case None =>
                  // Already processed or unknown — ignore
                  acc

            case _ =>
              context.log.debug("FaaS platform {} ignoring event: {}", config.platformName, event.payload)
              acc
        }

        if acc.emittedEvents.nonEmpty then config.coordinator ! TimeCoordinator.ScheduleEvents(acc.emittedEvents)

        replyTo ! StepComplete(entityRef)
        active(config, acc.state, entityRef, context)

      case TimeCoordinator.FinalSnapshot(_, replyTo) =>
        // No end-of-sim bookkeeping; reply immediately so the coordinator
        // can proceed with shutdown.
        replyTo ! TimeCoordinator.FinalSnapshotComplete(entityRef)
        Behaviors.same
    }
