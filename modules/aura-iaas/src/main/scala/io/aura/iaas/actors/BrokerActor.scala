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
import io.aura.iaas.policies.{
  CostRates,
  FaultRecoveryAction,
  FaultRecoveryPolicy,
  HorizontalScalingContext,
  HorizontalScalingPolicy,
  PredictiveScalingConfig,
  PredictiveScalingState,
  RecoveryContext,
  SpotInstanceConfig,
  SpotInterruptionScheduler,
  VmCostCalculator
}
import io.aura.core.types.VmBootConfig

/** Actor representing a cloud service Broker.
  *
  * Submits VM creation requests and workloads to datacenters. Tracks VM lifecycle and workload completion.
  */
object BrokerActor:

  private val DynamicVmIdBase           = 100_000L
  private val RecoveredWorkloadIdOffset = 200_000L

  final case class VmRequest(
      vmId: VmId,
      spec: ResourceSpec,
      submissionTime: SimTime
  )

  final case class Config(
      brokerId: BrokerId,
      vmRequests: Vector[VmRequest],
      workloads: Vector[WorkloadSpec],
      datacenterRef: EntityRef,
      coordinator: ActorRef[TimeCoordinator.Command],
      horizontalScaling: Option[HorizontalScalingPolicy] = None,
      maxVms: Int = Int.MaxValue,
      scalingCheckInterval: SimTime = SimTime(50.0),
      costRates: Option[CostRates] = None,
      spotConfig: Option[SpotInstanceConfig] = None,
      faultRecovery: FaultRecoveryPolicy = FaultRecoveryPolicy.none,
      vmBootConfig: VmBootConfig = VmBootConfig.instant,
      predictiveScalingConfig: Option[(PredictiveScalingConfig, ResourceSpec)] = None
  )

  private final case class BrokerState(
      pendingVms: Set[VmId],
      activeVms: Map[VmId, (HostId, DatacenterId)],
      pendingWorkloads: Vector[WorkloadSpec],
      outstandingWorkloads: Set[WorkloadId],     // submitted but not yet started/failed
      submittedWorkloads: Map[WorkloadId, VmId], // started, awaiting completion
      completedWorkloads: Vector[WorkloadId],
      failedWorkloads: Vector[WorkloadId],
      failedVms: Set[VmId],
      nextDynamicVmId: Long = DynamicVmIdBase,
      totalVmsCreated: Int = 0,
      vmCreationTimes: Map[VmId, (SimTime, ResourceSpec)] = Map.empty,
      spotVms: Set[VmId] = Set.empty,
      interruptedVms: Set[VmId] = Set.empty,
      workloadSpecs: Map[WorkloadId, WorkloadSpec] = Map.empty,
      workloadRetries: Map[WorkloadId, Int] = Map.empty,
      preemptedProgress: Map[WorkloadId, MI] = Map.empty,
      predictiveScalingState: PredictiveScalingState = PredictiveScalingState()
  )

  /** Pre-generate all initial events on the caller thread (avoids race with StartSimulation). */
  def generateEvents(config: Config): Vector[SimEvent] =
    val entityRef = EntityRef(s"broker-${config.brokerId.value}", EntityType.Broker)

    val vmEvents = config.vmRequests.map { req =>
      SimEvent(
        time = req.submissionTime,
        source = entityRef,
        destination = config.datacenterRef,
        payload = VmCreateRequest(req.vmId, config.brokerId, req.spec),
        serial = SerialNumber.Zero
      )
    }

    val scalingEvents = config.horizontalScaling match
      case Some(_) =>
        Vector(
          SimEvent(
            time = config.scalingCheckInterval,
            source = entityRef,
            destination = entityRef,
            payload = ScalingCheck(config.brokerId),
            serial = SerialNumber.Zero
          )
        )
      case None => Vector.empty

    val spotEvents = config.spotConfig match
      case Some(sc) =>
        SpotInterruptionScheduler.generateEvents(entityRef, config.vmRequests.map(_.vmId), sc)
      case None => Vector.empty

    vmEvents ++ scalingEvents ++ spotEvents

  def apply(config: Config): Behavior[EntityCommand] =
    Behaviors.setup { context =>
      val entityRef = EntityRef(s"broker-${config.brokerId.value}", EntityType.Broker)
      config.coordinator ! TimeCoordinator.RegisterEntity(entityRef, context.self)

      active(
        config,
        BrokerState(
          pendingVms = config.vmRequests.map(_.vmId).toSet,
          activeVms = Map.empty,
          pendingWorkloads = config.workloads,
          submittedWorkloads = Map.empty,
          outstandingWorkloads = Set.empty,
          completedWorkloads = Vector.empty,
          failedWorkloads = Vector.empty,
          failedVms = Set.empty,
          totalVmsCreated = config.vmRequests.size,
          spotVms = if config.spotConfig.isDefined then config.vmRequests.map(_.vmId).toSet else Set.empty,
          workloadSpecs = config.workloads.map(w => w.id -> w).toMap
        ),
        entityRef,
        context
      )
    }

  /** Immutable accumulator for broker event processing. */
  private case class BrokerAccumulator(
      state: BrokerState,
      emittedEvents: Vector[SimEvent]
  )

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
            case VmCreated(vmId, hostId, dcId) =>
              context.log.debug(
                "Broker {} received VM {} created on host {}",
                config.brokerId.value,
                vmId.value,
                hostId.value
              )
              // Look up VM spec for cost tracking
              val vmSpec = config.vmRequests.find(_.vmId == vmId).map(_.spec)
              val updatedCreationTimes = vmSpec match
                case Some(spec) => acc.state.vmCreationTimes + (vmId -> (event.time, spec))
                case None       => acc.state.vmCreationTimes
              val updatedState = acc.state.copy(
                pendingVms = acc.state.pendingVms - vmId,
                activeVms = acc.state.activeVms + (vmId -> (hostId, dcId)),
                vmCreationTimes = updatedCreationTimes
              )

              // Submit pending workloads to datacenter once all VMs are resolved
              if updatedState.pendingWorkloads.nonEmpty && updatedState.pendingVms.isEmpty then
                val completedSet = updatedState.completedWorkloads.toSet
                val (ready, waiting) = updatedState.pendingWorkloads.partition { wl =>
                  wl.predecessors.isEmpty || wl.predecessors.subsetOf(completedSet)
                }
                val bootDelay = config.vmBootConfig.startupDelay
                val submitEvents = ready.map { workload =>
                  val baseTime =
                    if workload.submissionDelay.isZero then event.time
                    else event.time + workload.submissionDelay
                  val submitTime = baseTime + bootDelay
                  SimEvent(
                    time = submitTime,
                    source = entityRef,
                    destination = config.datacenterRef,
                    payload = WorkloadSubmit(workload),
                    serial = SerialNumber.Zero
                  )
                }
                val outstandingIds = ready.map(_.id).toSet
                acc.copy(
                  state = updatedState.copy(
                    pendingWorkloads = waiting,
                    outstandingWorkloads = updatedState.outstandingWorkloads ++ outstandingIds
                  ),
                  emittedEvents = acc.emittedEvents ++ submitEvents
                )
              else acc.copy(state = updatedState)

            case VmCreateFailed(vmId, reason) =>
              context.log.warn("VM {} creation failed: {}", vmId.value, reason)
              acc.copy(
                state = acc.state.copy(
                  pendingVms = acc.state.pendingVms - vmId,
                  failedVms = acc.state.failedVms + vmId
                )
              )

            case WorkloadStarted(workloadId, vmId, _, _) =>
              acc.copy(
                state = acc.state.copy(
                  outstandingWorkloads = acc.state.outstandingWorkloads - workloadId,
                  submittedWorkloads = acc.state.submittedWorkloads + (workloadId -> vmId)
                )
              )

            case WorkloadFinished(workloadId, _, _, _, _) =>
              context.log.debug("Workload {} completed", workloadId.value)
              val stateWithCompletion = acc.state.copy(
                submittedWorkloads = acc.state.submittedWorkloads - workloadId,
                completedWorkloads = acc.state.completedWorkloads :+ workloadId
              )

              // Check if any DAG-pending workloads are now ready
              val completedSet = stateWithCompletion.completedWorkloads.toSet
              val (ready, waiting) = stateWithCompletion.pendingWorkloads.partition { wl =>
                wl.predecessors.nonEmpty && wl.predecessors.subsetOf(completedSet)
              }
              val dagSubmitEvents = ready.map { workload =>
                SimEvent(
                  time = event.time,
                  source = entityRef,
                  destination = config.datacenterRef,
                  payload = WorkloadSubmit(workload),
                  serial = SerialNumber.Zero
                )
              }
              val newOutstandingIds = ready.map(_.id).toSet
              val updatedState = stateWithCompletion.copy(
                pendingWorkloads = waiting,
                outstandingWorkloads = stateWithCompletion.outstandingWorkloads ++ newOutstandingIds,
                workloadSpecs = stateWithCompletion.workloadSpecs ++ ready.map(w => w.id -> w)
              )

              acc.copy(
                state = updatedState,
                emittedEvents = acc.emittedEvents ++ dagSubmitEvents
              )

            case WorkloadFailed(workloadId, reason) =>
              context.log.warn("Workload {} failed: {}", workloadId.value, reason)
              val updatedState = acc.state.copy(
                outstandingWorkloads = acc.state.outstandingWorkloads - workloadId,
                failedWorkloads = acc.state.failedWorkloads :+ workloadId
              )

              acc.copy(state = updatedState)

            case WorkloadPreempted(workloadId, vmId, executedMI) =>
              context.log.debug(
                "Broker {} workload {} preempted on VM {} (executed: {} MI)",
                config.brokerId.value,
                workloadId.value,
                vmId.value,
                executedMI.value
              )
              acc.copy(
                state = acc.state.copy(
                  preemptedProgress = acc.state.preemptedProgress + (workloadId -> executedMI)
                )
              )

            case VmDestroyed(vmId, hostId) =>
              context.log.info("Broker {} VM {} destroyed on host {}", config.brokerId.value, vmId.value, hostId.value)
              // Compute cost for this VM if costRates configured
              val costEvents = config.costRates match
                case Some(rates) =>
                  acc.state.vmCreationTimes.get(vmId) match
                    case Some((createTime, spec)) =>
                      val duration  = event.time - createTime
                      val breakdown = VmCostCalculator.fromRates(rates)(spec, duration)
                      Vector(
                        SimEvent(
                          time = event.time,
                          source = entityRef,
                          destination = EntityRef("sim-guardian", EntityType.SimGuardian),
                          payload = VmCostReport(
                            vmId,
                            breakdown.cpuCost,
                            breakdown.ramCost,
                            breakdown.bwCost,
                            breakdown.storageCost,
                            breakdown.total
                          ),
                          serial = SerialNumber.Zero
                        )
                      )
                    case None => Vector.empty
                case None => Vector.empty

              // Remove from active VMs
              val workloadsOnVm = acc.state.submittedWorkloads.filter(_._2 == vmId).keys.toVector

              // Apply fault recovery policy for each affected workload
              val recoveryResults = workloadsOnVm.flatMap { wlId =>
                val spec       = acc.state.workloadSpecs.get(wlId)
                val progress   = acc.state.preemptedProgress.getOrElse(wlId, MI.Zero)
                val retryCount = acc.state.workloadRetries.getOrElse(wlId, 0)
                spec.map(s => (wlId, config.faultRecovery(RecoveryContext(s, progress, retryCount))))
              }

              // Accumulate recovery results using foldLeft
              case class RecoveryAcc(
                  nextWlId: Long,
                  resubmitEvents: Vector[SimEvent],
                  newWorkloadSpecs: Map[WorkloadId, WorkloadSpec],
                  updatedRetries: Map[WorkloadId, Int],
                  recoveredIds: Set[WorkloadId]
              )

              val recoveryAcc = recoveryResults.foldLeft(
                RecoveryAcc(
                  nextWlId = acc.state.nextDynamicVmId + RecoveredWorkloadIdOffset,
                  resubmitEvents = Vector.empty,
                  newWorkloadSpecs = Map.empty,
                  updatedRetries = acc.state.workloadRetries,
                  recoveredIds = Set.empty
                )
              ) { case (ra, (originalId, action)) =>
                action match
                  case FaultRecoveryAction.Resubmit(spec) =>
                    val newId      = WorkloadId(ra.nextWlId)
                    val newSpec    = spec.copy(id = newId)
                    val retryCount = acc.state.workloadRetries.getOrElse(originalId, 0) + 1
                    context.log.info(
                      "Broker {} recovering workload {} as {} (retry {})",
                      config.brokerId.value,
                      originalId.value,
                      newId.value,
                      retryCount
                    )
                    RecoveryAcc(
                      nextWlId = ra.nextWlId + 1,
                      resubmitEvents = ra.resubmitEvents :+ SimEvent(
                        time = event.time,
                        source = entityRef,
                        destination = config.datacenterRef,
                        payload = WorkloadSubmit(newSpec),
                        serial = SerialNumber.Zero
                      ),
                      newWorkloadSpecs = ra.newWorkloadSpecs + (newId -> newSpec),
                      updatedRetries = ra.updatedRetries + (newId     -> retryCount),
                      recoveredIds = ra.recoveredIds + originalId
                    )
                  case FaultRecoveryAction.Drop =>
                    ra
              }

              val failedIds = workloadsOnVm.filterNot(recoveryAcc.recoveredIds.contains)
              val newOutstanding = recoveryResults.collect { case (_, FaultRecoveryAction.Resubmit(spec)) =>
                spec.id
              }.toSet

              val updatedState = acc.state.copy(
                activeVms = acc.state.activeVms - vmId,
                submittedWorkloads = acc.state.submittedWorkloads -- workloadsOnVm,
                outstandingWorkloads = acc.state.outstandingWorkloads ++ newOutstanding,
                failedWorkloads = acc.state.failedWorkloads ++ failedIds.map(_.value).map(WorkloadId(_)),
                vmCreationTimes = acc.state.vmCreationTimes - vmId,
                workloadSpecs = acc.state.workloadSpecs ++ recoveryAcc.newWorkloadSpecs,
                workloadRetries = recoveryAcc.updatedRetries,
                preemptedProgress = acc.state.preemptedProgress -- workloadsOnVm
              )

              acc.copy(
                state = updatedState,
                emittedEvents = acc.emittedEvents ++ costEvents ++ recoveryAcc.resubmitEvents
              )

            case ScalingCheck(brokerId) =>
              // Evaluate horizontal scaling policy
              val (scalingResult, newPredictiveState) = config.predictiveScalingConfig match
                case Some((psConfig, vmTemplate)) =>
                  val ctx = HorizontalScalingContext(
                    activeVmCount = acc.state.activeVms.size,
                    pendingWorkloads = acc.state.pendingWorkloads.size + acc.state.outstandingWorkloads.size,
                    runningWorkloads = acc.state.submittedWorkloads.size,
                    avgCpuUtilization = Utilization(0.5),
                    currentTime = event.time
                  )
                  val (result, newState) =
                    HorizontalScalingPolicy.predictiveEval(psConfig, vmTemplate, acc.state.predictiveScalingState, ctx)
                  (result, Some(newState))
                case None =>
                  config.horizontalScaling match
                    case Some(policy) =>
                      val ctx = HorizontalScalingContext(
                        activeVmCount = acc.state.activeVms.size,
                        pendingWorkloads = acc.state.pendingWorkloads.size + acc.state.outstandingWorkloads.size,
                        runningWorkloads = acc.state.submittedWorkloads.size,
                        avgCpuUtilization = Utilization(0.5),
                        currentTime = event.time
                      )
                      (policy(ctx), None)
                    case None =>
                      (None, None)

              val (scaleEvent, updatedState) = scalingResult match
                case Some(vmSpec) if acc.state.totalVmsCreated < config.maxVms =>
                  val newVmId = VmId(acc.state.nextDynamicVmId)
                  context.log.info("Broker {} scaling out: creating VM {}", config.brokerId.value, newVmId.value)
                  val evt = SimEvent(
                    time = event.time,
                    source = entityRef,
                    destination = config.datacenterRef,
                    payload = VmCreateRequest(newVmId, config.brokerId, vmSpec),
                    serial = SerialNumber.Zero
                  )
                  val st = acc.state.copy(
                    pendingVms = acc.state.pendingVms + newVmId,
                    nextDynamicVmId = acc.state.nextDynamicVmId + 1,
                    totalVmsCreated = acc.state.totalVmsCreated + 1,
                    predictiveScalingState = newPredictiveState.getOrElse(acc.state.predictiveScalingState)
                  )
                  (Some(evt), st)
                case _ =>
                  (
                    None,
                    acc.state.copy(
                      predictiveScalingState = newPredictiveState.getOrElse(acc.state.predictiveScalingState)
                    )
                  )

              // Schedule next scaling check
              val nextCheck = SimEvent(
                time = event.time + config.scalingCheckInterval,
                source = entityRef,
                destination = entityRef,
                payload = ScalingCheck(config.brokerId),
                serial = SerialNumber.Zero
              )

              acc.copy(
                state = updatedState,
                emittedEvents = acc.emittedEvents ++ scaleEvent.toVector :+ nextCheck
              )

            case SpotInterruption(vmId, noticePeriod) =>
              if acc.state.activeVms.contains(vmId) && !acc.state.interruptedVms.contains(vmId) then
                context.log.warn("Broker {} spot interruption for VM {}", config.brokerId.value, vmId.value)
                val destroyEvents = Vector.newBuilder[SimEvent]

                // Destroy the spot VM
                val (hostId, _) = acc.state.activeVms(vmId)
                destroyEvents += SimEvent(
                  time = event.time + noticePeriod,
                  source = entityRef,
                  destination = config.datacenterRef,
                  payload = VmDestroyRequest(vmId),
                  serial = SerialNumber.Zero
                )

                // Optionally create on-demand replacement
                val (fallbackEvents, updatedState) = config.spotConfig match
                  case Some(sc) if sc.fallbackToOnDemand =>
                    val newVmId = VmId(acc.state.nextDynamicVmId)
                    val vmSpec = acc.state.vmCreationTimes
                      .get(vmId)
                      .map(_._2)
                      .orElse(config.vmRequests.find(_.vmId == vmId).map(_.spec))
                    vmSpec match
                      case Some(spec) =>
                        val createEvent = SimEvent(
                          time = event.time + noticePeriod,
                          source = entityRef,
                          destination = config.datacenterRef,
                          payload = VmCreateRequest(newVmId, config.brokerId, spec),
                          serial = SerialNumber.Zero
                        )
                        context.log.info(
                          "Broker {} creating on-demand fallback VM {}",
                          config.brokerId.value,
                          newVmId.value
                        )
                        (
                          Vector(createEvent),
                          acc.state.copy(
                            pendingVms = acc.state.pendingVms + newVmId,
                            nextDynamicVmId = acc.state.nextDynamicVmId + 1,
                            totalVmsCreated = acc.state.totalVmsCreated + 1,
                            interruptedVms = acc.state.interruptedVms + vmId
                          )
                        )
                      case None =>
                        (Vector.empty, acc.state.copy(interruptedVms = acc.state.interruptedVms + vmId))
                  case _ =>
                    (Vector.empty, acc.state.copy(interruptedVms = acc.state.interruptedVms + vmId))

                acc.copy(
                  state = updatedState,
                  emittedEvents = acc.emittedEvents ++ destroyEvents.result() ++ fallbackEvents
                )
              else acc

            case _ =>
              context.log.debug("Broker {} ignoring event: {}", config.brokerId.value, event.payload)
              acc
        }

        if acc.emittedEvents.nonEmpty then config.coordinator ! TimeCoordinator.ScheduleEvents(acc.emittedEvents)

        replyTo ! StepComplete(entityRef)
        active(config, acc.state, entityRef, context)

      case TimeCoordinator.FinalSnapshot(simulationEndTime, replyTo) =>
        // Flush per-VM cost reports for any VM still active at end-of-sim.
        // VMs already destroyed have their costs reported by the VmDestroyed
        // handler; this fills in the still-active ones. Use the coordinator's
        // actual simulation end time (which may be < config.endTime if the
        // simulation terminated early via natural quiescence) so we don't
        // over-bill long-running VMs.
        val finalEvents = computeRemainingCosts(
          config,
          state,
          simulationEndTime,
          entityRef
        )
        if finalEvents.nonEmpty then config.coordinator ! TimeCoordinator.ScheduleEvents(finalEvents)
        replyTo ! TimeCoordinator.FinalSnapshotComplete(entityRef)
        Behaviors.same
    }

  /** Compute cost reports for all remaining active VMs at simulation end. */
  private def computeRemainingCosts(
      config: Config,
      state: BrokerState,
      currentTime: SimTime,
      entityRef: EntityRef
  ): Vector[SimEvent] =
    config.costRates match
      case Some(rates) =>
        state.vmCreationTimes.toVector.map { case (vmId, (createTime, spec)) =>
          val duration  = currentTime - createTime
          val breakdown = VmCostCalculator.fromRates(rates)(spec, duration)
          SimEvent(
            time = currentTime,
            source = entityRef,
            destination = EntityRef("sim-guardian", EntityType.SimGuardian),
            payload = VmCostReport(
              vmId,
              breakdown.cpuCost,
              breakdown.ramCost,
              breakdown.bwCost,
              breakdown.storageCost,
              breakdown.total
            ),
            serial = SerialNumber.Zero
          )
        }
      case None => Vector.empty
