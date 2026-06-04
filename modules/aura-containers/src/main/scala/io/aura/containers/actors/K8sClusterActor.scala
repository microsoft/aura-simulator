// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.containers.actors

import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}

import io.aura.core.types.*
import io.aura.core.events.*
import io.aura.core.events.SimEventPayload.*
import io.aura.core.engine.TimeCoordinator
import io.aura.core.engine.TimeCoordinator.{EntityCommand, ProcessEvents, StepComplete}
import io.aura.containers.*
import io.aura.containers.state.*

/** Actor representing a Kubernetes-style cluster.
  *
  * Manages pod scheduling on nodes (backed by IaaS hosts), tracks pod lifecycle, and supports restart policies. Each
  * pod gets a lightweight VM whose resource spec matches the pod's cpuRequest/memoryRequest, leveraging the existing
  * IaaS pipeline for actual workload execution.
  */
object K8sClusterActor:

  final case class HostNodeMapping(
      nodeName: String,
      hostId: HostId,
      totalCpu: MIPS,
      totalMemory: MegaBytes,
      labels: Map[String, String] = Map.empty,
      taints: Vector[Taint] = Vector.empty
  )

  final case class Config(
      clusterName: String,
      schedulingPolicy: K8sSchedulingPolicy,
      datacenterRef: EntityRef,
      hostConfigs: Vector[HostNodeMapping],
      coordinator: ActorRef[TimeCoordinator.Command],
      startupDelay: SimTime = SimTime.Zero,
      initialDeployments: Vector[DeploymentRecord] = Vector.empty
  )

  /** Immutable accumulator for cluster event processing. */
  private case class ClusterAccumulator(
      state: ClusterState,
      emittedEvents: Vector[SimEvent],
      nextVmId: Long
  )

  def apply(cfg: Config, nextVmId: Long): Behavior[EntityCommand] =
    Behaviors.setup { context =>
      val entityRef = EntityRef(cfg.clusterName, EntityType.K8sCluster)
      cfg.coordinator ! TimeCoordinator.RegisterEntity(entityRef, context.self)

      val baseState = ClusterState.empty.initializeNodes(
        cfg.hostConfigs.map(h => (h.nodeName, h.hostId, h.totalCpu, h.totalMemory, h.labels, h.taints))
      )
      val initialState = cfg.initialDeployments.foldLeft(baseState) { (st, dr) =>
        st.addDeployment(dr)
      }

      active(cfg, initialState, entityRef, context, nextVmId)
    }

  private def brokerRef(clusterName: String): EntityRef =
    EntityRef(s"k8s-broker-$clusterName", EntityType.K8sCluster)

  private def active(
      config: Config,
      state: ClusterState,
      entityRef: EntityRef,
      context: ActorContext[EntityCommand],
      nextVmId: Long
  ): Behavior[EntityCommand] =
    Behaviors.receiveMessage {
      case ProcessEvents(events, replyTo) =>
        val initial = ClusterAccumulator(state, Vector.empty, nextVmId)

        val acc = events.foldLeft(initial) { (acc, event) =>
          event.payload match
            case PodScheduleRequest(podId, deploymentId, spec) =>
              handlePodScheduleRequest(acc, event, entityRef, config, podId, deploymentId, spec, context)

            case WorkloadFinished(_, vmId, hostId, finishTime, _) =>
              handleWorkloadFinished(acc, event, entityRef, config, vmId, hostId, finishTime, context)

            case VmCreated(vmId, hostId, _) =>
              handleVmCreated(acc, event, entityRef, config, vmId, hostId, context)

            case VmCreateFailed(vmId, reason) =>
              handleVmCreateFailed(acc, event, entityRef, config, vmId, reason, context)

            case PodCompleted(podId, hostId, finishTime) =>
              // Self-delivered: for pods without workloads
              handleSelfPodCompleted(acc, event, entityRef, config, podId, hostId, finishTime, context)

            case _ =>
              context.log.debug("K8s cluster {} ignoring event: {}", config.clusterName, event.payload)
              acc
        }

        if acc.emittedEvents.nonEmpty then config.coordinator ! TimeCoordinator.ScheduleEvents(acc.emittedEvents)

        replyTo ! StepComplete(entityRef)
        active(config, acc.state, entityRef, context, acc.nextVmId)

      case TimeCoordinator.FinalSnapshot(_, replyTo) =>
        // No end-of-sim bookkeeping; reply immediately so the coordinator
        // can proceed with shutdown.
        replyTo ! TimeCoordinator.FinalSnapshotComplete(entityRef)
        Behaviors.same
    }

  private def handlePodScheduleRequest(
      acc: ClusterAccumulator,
      event: SimEvent,
      entityRef: EntityRef,
      config: Config,
      podId: PodId,
      deploymentId: DeploymentId,
      spec: PodDeploySpec,
      context: ActorContext[EntityCommand]
  ): ClusterAccumulator =
    val containers =
      spec.containers.map(c => ContainerSpec(c.name, c.image, c.cpuRequest, c.cpuLimit, c.memoryRequest, c.memoryLimit))
    val restartPolicy = RestartPolicy.valueOf(spec.restartPolicy)
    val podSpec = PodSpec(
      name = spec.name,
      namespace = spec.namespace,
      containers = containers,
      restartPolicy = restartPolicy,
      priority = spec.priority,
      nodeSelector = spec.nodeSelector
    )

    val hasWorkload = acc.state.deployments
      .get(deploymentId)
      .flatMap(_.workloadSpec)
      .isDefined
    val stateWithRequester = acc.state.setRequester(podId, event.source)
    val updatedState = if stateWithRequester.pods.contains(podId) then
      // Re-schedule: update spec but keep existing record fields
      val existing = stateWithRequester.pods(podId)
      stateWithRequester.copy(pods =
        stateWithRequester.pods + (podId -> existing.copy(
          spec = podSpec,
          phase = PodPhase.Pending,
          nodeName = None,
          startTime = None,
          vmId = None
        ))
      )
    else
      stateWithRequester.addPod(
        PodRecord(
          podId = podId,
          deploymentId = deploymentId,
          spec = podSpec,
          phase = PodPhase.Pending,
          nodeName = None,
          startTime = None,
          restartCount = 0,
          hasWorkload = hasWorkload
        )
      )

    val nodeStates = updatedState.nodes.values.toVector
    config.schedulingPolicy(nodeStates, podSpec) match
      case Some(nodeName) =>
        val node           = updatedState.nodes(nodeName)
        val scheduledState = updatedState.scheduleOnNode(podId, nodeName, event.time)

        context.log.debug("K8s cluster {} scheduled pod {} on node {}", config.clusterName, podId.value, nodeName)

        val scheduledEvent = SimEvent(
          time = event.time,
          source = entityRef,
          destination = updatedState.requesterFor(podId, brokerRef(config.clusterName)),
          payload = PodScheduled(podId, node.hostId, nodeName),
          serial = SerialNumber.Zero
        )

        val vmId = VmId(acc.nextVmId)
        val vmSpec = ResourceSpec(
          pes = PEs(1),
          mips = podSpec.totalCpuRequest,
          ram = podSpec.totalMemoryRequest,
          bw = Mbps(10.0),
          storage = MegaBytes(100.0)
        )
        val vmCreateEvent = SimEvent(
          time = event.time,
          source = entityRef,
          destination = config.datacenterRef,
          payload = VmCreateRequest(vmId, BrokerId(0L), vmSpec),
          serial = SerialNumber.Zero
        )

        val podWithVm = scheduledState.pods(podId).copy(vmId = Some(vmId))
        val finalState = scheduledState.copy(
          pods = scheduledState.pods + (podId -> podWithVm)
        )

        acc.copy(
          state = finalState,
          emittedEvents = acc.emittedEvents :+ scheduledEvent :+ vmCreateEvent,
          nextVmId = acc.nextVmId + 1
        )

      case None =>
        context.log.warn("K8s cluster {} cannot schedule pod {} - no suitable node", config.clusterName, podId.value)
        val unschedulableEvent = SimEvent(
          time = event.time,
          source = entityRef,
          destination = updatedState.requesterFor(podId, brokerRef(config.clusterName)),
          payload = PodUnschedulable(podId, "No suitable node found"),
          serial = SerialNumber.Zero
        )
        val failedState = updatedState.markPodFailed(podId, "Unschedulable")
        acc.copy(
          state = failedState,
          emittedEvents = acc.emittedEvents :+ unschedulableEvent
        )

  private def handleVmCreated(
      acc: ClusterAccumulator,
      event: SimEvent,
      entityRef: EntityRef,
      config: Config,
      vmId: VmId,
      hostId: HostId,
      @annotation.unused context: ActorContext[EntityCommand]
  ): ClusterAccumulator =
    val podOpt = acc.state.pods.collectFirst {
      case (podId, record) if record.vmId.contains(vmId) => (podId, record)
    }

    podOpt match
      case Some((podId, record)) =>
        val startedEvent = SimEvent(
          time = event.time,
          source = entityRef,
          destination = acc.state.requesterFor(podId, brokerRef(config.clusterName)),
          payload = PodStarted(podId, hostId, event.time),
          serial = SerialNumber.Zero
        )

        val updatedState = acc.state.markPodRunning(podId, event.time)

        val deployment = updatedState.deployments.get(record.deploymentId)
        // Compute completion analytically rather than submitting to IaaS pipeline,
        // since HostActor routes WorkloadFinished to broker entities (not K8sCluster).
        // VMs are still created for host resource accounting.
        val completionEvents = deployment.flatMap(_.workloadSpec) match
          case Some(wc) =>
            val cpuMips = record.spec.totalCpuRequest.value
            val completionTime =
              if cpuMips > 0.0 then event.time + SimTime(wc.length.value / cpuMips)
              else event.time + SimTime(1.0)
            Vector(
              SimEvent(
                time = completionTime,
                source = entityRef,
                destination = entityRef,
                payload = PodCompleted(podId, hostId, completionTime),
                serial = SerialNumber.Zero
              )
            )
          case None =>
            // No workload — complete after short delay (service-style pod)
            Vector(
              SimEvent(
                time = event.time + SimTime(1.0),
                source = entityRef,
                destination = entityRef,
                payload = PodCompleted(podId, hostId, event.time + SimTime(1.0)),
                serial = SerialNumber.Zero
              )
            )

        acc.copy(
          state = updatedState,
          emittedEvents = acc.emittedEvents :+ startedEvent :++ completionEvents
        )

      case None => acc

  private def handleVmCreateFailed(
      acc: ClusterAccumulator,
      event: SimEvent,
      entityRef: EntityRef,
      config: Config,
      vmId: VmId,
      reason: String,
      context: ActorContext[EntityCommand]
  ): ClusterAccumulator =
    val podOpt = acc.state.pods.collectFirst {
      case (podId, record) if record.vmId.contains(vmId) => (podId, record)
    }
    podOpt match
      case Some((podId, record)) =>
        context.log.warn("Pod {} VM creation failed: {}", podId.value, reason)
        val hostId = record.nodeName
          .flatMap(n => acc.state.nodes.get(n).map(_.hostId))
          .getOrElse(HostId(0L))
        val failedEvent = SimEvent(
          time = event.time,
          source = entityRef,
          destination = acc.state.requesterFor(podId, brokerRef(config.clusterName)),
          payload = PodFailed(podId, hostId, reason),
          serial = SerialNumber.Zero
        )
        val updatedState = acc.state
          .removePodFromNode(podId)
          .markPodFailed(podId, reason)
        acc.copy(
          state = updatedState,
          emittedEvents = acc.emittedEvents :+ failedEvent
        )
      case None => acc

  private def handleSelfPodCompleted(
      acc: ClusterAccumulator,
      @annotation.unused event: SimEvent,
      entityRef: EntityRef,
      config: Config,
      podId: PodId,
      hostId: HostId,
      finishTime: SimTime,
      @annotation.unused context: ActorContext[EntityCommand]
  ): ClusterAccumulator =
    acc.state.pods.get(podId) match
      case Some(record) if record.phase == PodPhase.Running =>
        val releasedState = acc.state.removePodFromNode(podId)

        record.spec.restartPolicy match
          case RestartPolicy.Always =>
            val restartedState = releasedState.incrementRestarts(podId)
            val rescheduleEvent = SimEvent(
              time = finishTime,
              source = entityRef,
              destination = entityRef,
              payload = PodScheduleRequest(
                podId,
                record.deploymentId,
                toPodDeploySpec(record.spec)
              ),
              serial = SerialNumber.Zero
            )
            acc.copy(
              state = restartedState,
              emittedEvents = acc.emittedEvents :+ rescheduleEvent
            )

          case _ =>
            val completedState = releasedState.markPodCompleted(podId, finishTime)
            val completedEvent = SimEvent(
              time = finishTime,
              source = entityRef,
              destination = acc.state.requesterFor(podId, brokerRef(config.clusterName)),
              payload = PodCompleted(podId, hostId, finishTime),
              serial = SerialNumber.Zero
            )
            acc.copy(
              state = completedState,
              emittedEvents = acc.emittedEvents :+ completedEvent
            )
      case _ => acc

  private def handleWorkloadFinished(
      acc: ClusterAccumulator,
      @annotation.unused event: SimEvent,
      entityRef: EntityRef,
      config: Config,
      vmId: VmId,
      hostId: HostId,
      finishTime: SimTime,
      context: ActorContext[EntityCommand]
  ): ClusterAccumulator =
    val podOpt = acc.state.pods.collectFirst {
      case (podId, record) if record.vmId.contains(vmId) && record.phase == PodPhase.Running =>
        (podId, record)
    }

    podOpt match
      case Some((podId, record)) =>
        context.log.debug("Pod {} workload completed", podId.value)
        val releasedState = acc.state.removePodFromNode(podId)

        record.spec.restartPolicy match
          case RestartPolicy.Always =>
            val restartedState = releasedState.incrementRestarts(podId)
            val rescheduleEvent = SimEvent(
              time = finishTime,
              source = entityRef,
              destination = entityRef,
              payload = PodScheduleRequest(
                podId,
                record.deploymentId,
                toPodDeploySpec(record.spec)
              ),
              serial = SerialNumber.Zero
            )
            acc.copy(
              state = restartedState,
              emittedEvents = acc.emittedEvents :+ rescheduleEvent
            )

          case _ =>
            val completedState = releasedState.markPodCompleted(podId, finishTime)
            val completedEvent = SimEvent(
              time = finishTime,
              source = entityRef,
              destination = acc.state.requesterFor(podId, brokerRef(config.clusterName)),
              payload = PodCompleted(podId, hostId, finishTime),
              serial = SerialNumber.Zero
            )
            acc.copy(
              state = completedState,
              emittedEvents = acc.emittedEvents :+ completedEvent
            )

      case None => acc

  private def toPodDeploySpec(podSpec: PodSpec): PodDeploySpec =
    PodDeploySpec(
      name = podSpec.name,
      namespace = podSpec.namespace,
      containers = podSpec.containers.map(c =>
        ContainerResourceSpec(c.name, c.image, c.cpuRequest, c.cpuLimit, c.memoryRequest, c.memoryLimit)
      ),
      restartPolicy = podSpec.restartPolicy.toString,
      priority = podSpec.priority,
      nodeSelector = podSpec.nodeSelector
    )
