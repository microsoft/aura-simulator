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
import io.aura.edge.*
import io.aura.edge.state.*

/** Actor representing an edge computing environment.
  *
  * Centralized actor (like K8sClusterActor) managing all edge nodes internally. Required because OffloadingPolicy needs
  * global utilization data from all candidate nodes to make decisions.
  */
object EdgeEnvironmentActor:

  final case class Config(
      environmentName: String,
      nodeSpecs: Vector[EdgeNodeSpec],
      latencyModel: LatencyModel,
      offloadingPolicy: OffloadingPolicy,
      coordinator: ActorRef[TimeCoordinator.Command]
  )

  /** Immutable accumulator for environment event processing. */
  private case class EnvironmentAccumulator(
      state: EdgeEnvironmentState,
      emittedEvents: Vector[SimEvent]
  )

  def apply(config: Config): Behavior[EntityCommand] =
    Behaviors.setup { context =>
      val entityRef = EntityRef(config.environmentName, EntityType.EdgeEnvironment)
      config.coordinator ! TimeCoordinator.RegisterEntity(entityRef, context.self)

      val initialState = EdgeEnvironmentState.empty.initializeNodes(config.nodeSpecs)

      active(config, initialState, entityRef, context)
    }

  private def active(
      config: Config,
      state: EdgeEnvironmentState,
      entityRef: EntityRef,
      context: ActorContext[EntityCommand]
  ): Behavior[EntityCommand] =
    Behaviors.receiveMessage {
      case ProcessEvents(events, replyTo) =>
        val initial = EnvironmentAccumulator(state, Vector.empty)

        val acc = events.foldLeft(initial) { (acc, event) =>
          event.payload match
            case EdgeTaskSubmit(taskId, sourceNodeName, cpuRequired, memRequired, taskLength, deadline) =>
              handleTaskSubmit(
                acc,
                event,
                entityRef,
                config,
                taskId,
                sourceNodeName,
                cpuRequired,
                memRequired,
                taskLength,
                deadline,
                context
              )

            case EdgeTaskCompleted(taskId, _, _, _, _, _, _, _) =>
              // Self-delivered: release resources
              handleSelfTaskCompleted(acc, taskId)

            case _ =>
              context.log.debug("Edge environment {} ignoring event: {}", config.environmentName, event.payload)
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

  private def handleTaskSubmit(
      acc: EnvironmentAccumulator,
      event: SimEvent,
      entityRef: EntityRef,
      config: Config,
      taskId: EdgeTaskId,
      sourceNodeName: String,
      cpuRequired: MIPS,
      memRequired: MegaBytes,
      taskLength: MI,
      deadline: SimTime,
      context: ActorContext[EntityCommand]
  ): EnvironmentAccumulator =
    // Check if source node exists
    acc.state.nodes.get(sourceNodeName) match
      case None =>
        val failEvent = SimEvent(
          time = event.time,
          source = entityRef,
          destination = event.source,
          payload = EdgeTaskFailed(taskId, s"Unknown source node: $sourceNodeName"),
          serial = SerialNumber.Zero
        )
        acc.copy(emittedEvents = acc.emittedEvents :+ failEvent)

      case Some(sourceState) =>
        val stateWithTask = acc.state.addTask(taskId, sourceNodeName)

        // Build offloading context with global utilization data
        val offloadingCtx = OffloadingContext(
          sourceNode = sourceState.spec,
          candidateNodes = config.nodeSpecs,
          taskCpuRequired = cpuRequired,
          taskMemRequired = memRequired,
          taskDeadline = deadline,
          currentNodeUtilizations = stateWithTask.utilizationMap,
          latencyModel = config.latencyModel
        )

        // Run offloading policy
        val decision       = config.offloadingPolicy(offloadingCtx)
        val targetNodeName = decision.targetNode
        val offloaded      = targetNodeName != sourceNodeName

        // Compute network latency
        val networkLatency = if offloaded then
          val targetSpec = config.nodeSpecs.find(_.name == targetNodeName)
          targetSpec
            .map(ts => config.latencyModel(sourceState.spec.location, ts.location, sourceState.spec.tier, ts.tier))
            .getOrElse(SimTime.Zero)
        else SimTime.Zero

        // Check if target node can accept the task
        val targetNode = stateWithTask.nodes.get(targetNodeName)
        targetNode match
          case None =>
            val failEvent = SimEvent(
              time = event.time,
              source = entityRef,
              destination = event.source,
              payload = EdgeTaskFailed(taskId, s"Target node not found: $targetNodeName"),
              serial = SerialNumber.Zero
            )
            acc.copy(state = stateWithTask, emittedEvents = acc.emittedEvents :+ failEvent)

          case Some(target) if !target.canAcceptTask(cpuRequired, memRequired) =>
            val failEvent = SimEvent(
              time = event.time,
              source = entityRef,
              destination = event.source,
              payload = EdgeTaskFailed(taskId, s"Node $targetNodeName at capacity"),
              serial = SerialNumber.Zero
            )
            acc.copy(
              state = stateWithTask.failTask(taskId, s"Node $targetNodeName at capacity"),
              emittedEvents = acc.emittedEvents :+ failEvent
            )

          case Some(target) =>
            val startTime = event.time + networkLatency
            // Analytical execution: completionTime = currentTime + networkLatency + taskLength/nodeMIPS
            val nodeMips      = target.spec.spec.mips.value
            val executionTime = if nodeMips > 0.0 then SimTime(taskLength.value / nodeMips) else SimTime(1.0)
            val finishTime    = startTime + executionTime

            val updatedState = stateWithTask.startTaskOnNode(
              taskId,
              targetNodeName,
              offloaded,
              networkLatency,
              startTime,
              cpuRequired,
              decision.reason
            )

            context.log.debug(
              "Edge task {} routed to {} (offloaded={}, latency={})",
              taskId.value,
              targetNodeName,
              offloaded,
              networkLatency.value
            )

            // Emit EdgeTaskStarted to broker
            val startedEvent = SimEvent(
              time = event.time,
              source = entityRef,
              destination = event.source,
              payload = EdgeTaskStarted(taskId, sourceNodeName, targetNodeName, offloaded, networkLatency, startTime),
              serial = SerialNumber.Zero
            )

            // Self-delivered EdgeTaskCompleted for resource release
            val selfCompletedEvent = SimEvent(
              time = finishTime,
              source = entityRef,
              destination = entityRef,
              payload = EdgeTaskCompleted(
                taskId,
                sourceNodeName,
                targetNodeName,
                offloaded,
                networkLatency,
                startTime,
                finishTime,
                decision.reason
              ),
              serial = SerialNumber.Zero
            )

            // Broker-bound EdgeTaskCompleted for lifecycle tracking
            val brokerCompletedEvent = SimEvent(
              time = finishTime,
              source = entityRef,
              destination = event.source,
              payload = EdgeTaskCompleted(
                taskId,
                sourceNodeName,
                targetNodeName,
                offloaded,
                networkLatency,
                startTime,
                finishTime,
                decision.reason
              ),
              serial = SerialNumber.Zero
            )

            acc.copy(
              state = updatedState,
              emittedEvents = acc.emittedEvents :+ startedEvent :+ selfCompletedEvent :+ brokerCompletedEvent
            )

  private def handleSelfTaskCompleted(
      acc: EnvironmentAccumulator,
      taskId: EdgeTaskId
  ): EnvironmentAccumulator =
    acc.copy(state = acc.state.completeTask(taskId, SimTime.Zero))
