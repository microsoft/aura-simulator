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
import io.aura.iaas.state.{HostState, VmState}
import io.aura.iaas.policies.{ConsolidationEngine, MigrationModel, MigrationParams, VmAllocationPolicy}
import io.aura.iaas.policies.{
  OverloadDetector as ConsolidationOverloadDetector,
  UnderloadDetector as ConsolidationUnderloadDetector,
  VmSelectionPolicy as ConsolidationVmSelection
}
import io.aura.power.PowerModel

/** Actor representing a Datacenter containing multiple Hosts.
  *
  * Routes VM creation requests to appropriate Hosts using the configured VmAllocationPolicy. Manages host actor
  * spawning and lifecycle. Handles VM migration orchestration and overload detection.
  */
object DatacenterActor:

  final case class HostConfig(
      hostId: HostId,
      spec: ResourceSpec,
      powerModel: PowerModel
  )

  type OverloadDetector  = (HostState, Vector[(SimTime, Utilization)]) => Boolean
  type VmSelectionPolicy = HostState => Option[VmId]

  /** Consolidation configuration using Beloglazov-style detectors. */
  final case class ConsolidationConfig(
      overloadDetector: ConsolidationOverloadDetector,
      underloadDetector: ConsolidationUnderloadDetector,
      vmSelector: ConsolidationVmSelection,
      interval: SimTime = SimTime(100.0)
  )

  final case class Config(
      datacenterId: DatacenterId,
      hosts: Vector[HostConfig],
      allocationPolicy: VmAllocationPolicy,
      scheduler: io.aura.iaas.policies.WorkloadScheduler,
      coordinator: ActorRef[TimeCoordinator.Command],
      migrationModel: Option[MigrationModel] = None,
      migrationBandwidth: Mbps = Mbps(10000.0),
      overloadDetector: Option[OverloadDetector] = None,
      vmSelector: Option[VmSelectionPolicy] = None,
      networkTopology: Option[NetworkTopology] = None,
      consolidation: Option[ConsolidationConfig] = None
  )

  def apply(config: Config): Behavior[EntityCommand] =
    Behaviors.setup { context =>
      val entityRef = EntityRef(s"dc-${config.datacenterId.value}", EntityType.Datacenter)
      config.coordinator ! TimeCoordinator.RegisterEntity(entityRef, context.self)

      // Spawn HostActors
      val hostStates = config.hosts.map { hc =>
        context.spawn(
          HostActor(
            HostActor.Config(
              hostId = hc.hostId,
              datacenterId = config.datacenterId,
              spec = hc.spec,
              scheduler = config.scheduler,
              powerModel = hc.powerModel,
              coordinator = config.coordinator,
              datacenterRef = if config.overloadDetector.isDefined then Some(entityRef) else None
            )
          ),
          s"host-${hc.hostId.value}"
        )
        val hostEntityRef = EntityRef(s"host-${hc.hostId.value}", EntityType.Host)
        hc.hostId -> (HostState.create(hc.hostId, config.datacenterId, hc.spec), hostEntityRef)
      }.toMap

      // Schedule periodic consolidation if configured
      config.consolidation.foreach { consConfig =>
        val firstCheck = SimEvent(
          time = consConfig.interval,
          source = entityRef,
          destination = entityRef,
          payload = ConsolidationCheck(config.datacenterId, consConfig.interval),
          serial = SerialNumber.Zero
        )
        config.coordinator ! TimeCoordinator.ScheduleEvents(Vector(firstCheck))
      }

      active(config, hostStates, entityRef, context, lastConsolidationTime = SimTime.Zero)
    }

  /** Immutable accumulator for datacenter event processing. */
  private case class DcAccumulator(
      hosts: Map[HostId, (HostState, EntityRef)],
      emittedEvents: Vector[SimEvent],
      flowManager: NetworkFlowManager = NetworkFlowManager.empty,
      migrationFlowIds: Map[VmId, Long] = Map.empty
  )

  private def active(
      config: Config,
      hostStates: Map[HostId, (HostState, EntityRef)],
      entityRef: EntityRef,
      context: ActorContext[EntityCommand],
      flowManager: NetworkFlowManager = NetworkFlowManager.empty,
      migrationFlowIds: Map[VmId, Long] = Map.empty,
      lastConsolidationTime: SimTime = SimTime.Zero
  ): Behavior[EntityCommand] =
    Behaviors.receiveMessage {
      case ProcessEvents(events, replyTo) =>
        val initial = DcAccumulator(hostStates, Vector.empty, flowManager, migrationFlowIds)

        val acc = events.foldLeft(initial) { (acc, event) =>
          event.payload match
            case VmCreateRequest(vmId, brokerId, vmSpec) =>
              val hostList = acc.hosts.values.map(_._1).toIndexedSeq
              config.allocationPolicy(hostList, vmSpec) match
                case Some(hostId) =>
                  acc.hosts.get(hostId) match
                    case Some((hostState, hostRef)) =>
                      // Add VM to mirror state for workload routing
                      val vm          = VmState.create(vmId, brokerId, vmSpec, event.time)
                      val updatedHost = hostState.placeVm(vm)
                      acc.copy(
                        hosts = acc.hosts + (hostId -> (updatedHost, hostRef)),
                        emittedEvents = acc.emittedEvents :+ SimEvent(
                          time = event.time,
                          source = event.source,
                          destination = hostRef,
                          payload = VmCreateRequest(vmId, brokerId, vmSpec),
                          serial = SerialNumber.Zero
                        )
                      )
                    case None =>
                      acc.copy(
                        emittedEvents = acc.emittedEvents :+ SimEvent(
                          time = event.time,
                          source = entityRef,
                          destination = event.source,
                          payload = VmCreateFailed(vmId, "Host not found in datacenter"),
                          serial = SerialNumber.Zero
                        )
                      )

                case None =>
                  context.log.warn(
                    "DC {} cannot place VM {} - no suitable host",
                    config.datacenterId.value,
                    vmId.value
                  )
                  acc.copy(
                    emittedEvents = acc.emittedEvents :+ SimEvent(
                      time = event.time,
                      source = entityRef,
                      destination = event.source,
                      payload = VmCreateFailed(vmId, "No suitable host found"),
                      serial = SerialNumber.Zero
                    )
                  )

            case WorkloadSubmit(workload) =>
              val targetHostOpt = acc.hosts.collectFirst {
                case (hostId, (hostState, hostRef)) if hostState.findVmForWorkload(workload).isDefined =>
                  (hostId, hostState, hostRef, hostState.findVmForWorkload(workload).get)
              }
              targetHostOpt match
                case Some((hostId, hostState, hostRef, vmId)) =>
                  // Update mirror: track workload allocation on VM for accurate routing
                  val vm = hostState.vms(vmId)
                  val updatedVm =
                    vm.startWorkload(workload.id, workload.requiredMips, event.time, workload.length, workload.weight)
                  val updatedHost = hostState.updateVm(updatedVm)
                  acc.copy(
                    hosts = acc.hosts + (hostId -> (updatedHost, hostRef)),
                    emittedEvents = acc.emittedEvents :+ SimEvent(
                      time = event.time,
                      source = event.source,
                      destination = hostRef,
                      payload = WorkloadSubmit(workload),
                      serial = SerialNumber.Zero
                    )
                  )
                case None =>
                  context.log.warn(
                    "DC {} has no host with VM for workload {}",
                    config.datacenterId.value,
                    workload.id.value
                  )
                  acc.copy(
                    emittedEvents = acc.emittedEvents :+ SimEvent(
                      time = event.time,
                      source = entityRef,
                      destination = event.source,
                      payload =
                        WorkloadFailed(workload.id, s"No VM available in datacenter ${config.datacenterId.value}"),
                      serial = SerialNumber.Zero
                    )
                  )

            case HostUtilizationUpdate(hostId, utilization, time) =>
              // Update host mirror's utilization history
              val updatedAcc = acc.hosts.get(hostId) match
                case Some((hs, ref)) =>
                  val updatedHs = hs.recordUtilization(time)
                  acc.copy(hosts = acc.hosts + (hostId -> (updatedHs, ref)))
                case None => acc

              // Check for overload and trigger migration if needed
              val migrationEvents = (config.overloadDetector, config.vmSelector, config.migrationModel) match
                case (Some(detector), Some(selector), Some(_)) =>
                  updatedAcc.hosts.get(hostId) match
                    case Some((hs, _)) if detector(hs, hs.utilizationHistory) =>
                      selector(hs) match
                        case Some(vmId) =>
                          // Find a target host that can fit the VM
                          val vmSpec = hs.vms.get(vmId).map(_.spec)
                          vmSpec match
                            case Some(spec) =>
                              val targetOpt = updatedAcc.hosts.collectFirst {
                                case (tid, (ts, _)) if tid != hostId && ts.canPlaceVm(spec) => tid
                              }
                              targetOpt match
                                case Some(targetHostId) =>
                                  Vector(
                                    SimEvent(
                                      time = time,
                                      source = entityRef,
                                      destination = entityRef,
                                      payload = VmMigrationRequest(vmId, hostId, targetHostId),
                                      serial = SerialNumber.Zero
                                    )
                                  )
                                case None => Vector.empty
                            case None => Vector.empty
                        case None => Vector.empty
                    case _ => Vector.empty
                case _ => Vector.empty

              updatedAcc.copy(emittedEvents = updatedAcc.emittedEvents ++ migrationEvents)

            case ConsolidationCheck(dcId, currentTime) =>
              config.consolidation match
                case Some(consConfig) =>
                  val allHosts = acc.hosts.values.map(_._1).toVector
                  val plan = ConsolidationEngine.planConsolidation(
                    allHosts,
                    consConfig.overloadDetector,
                    consConfig.underloadDetector,
                    consConfig.vmSelector,
                    config.allocationPolicy
                  )
                  val migEvents = plan.migrations.flatMap { m =>
                    config.migrationModel.map { _ =>
                      SimEvent(
                        time = currentTime,
                        source = entityRef,
                        destination = entityRef,
                        payload = VmMigrationRequest(m.vmId, m.sourceHostId, m.targetHostId),
                        serial = SerialNumber.Zero
                      )
                    }
                  }
                  // Schedule next consolidation check
                  val nextCheck = SimEvent(
                    time = currentTime + consConfig.interval,
                    source = entityRef,
                    destination = entityRef,
                    payload = ConsolidationCheck(dcId, currentTime + consConfig.interval),
                    serial = SerialNumber.Zero
                  )
                  if plan.migrations.nonEmpty then
                    context.log.info(
                      "DC {} consolidation: {} migrations, {} hosts to deactivate",
                      config.datacenterId.value,
                      plan.migrationCount,
                      plan.hostsSavedCount
                    )
                  acc.copy(emittedEvents = acc.emittedEvents ++ migEvents :+ nextCheck)
                case None => acc

            case VmMigrationRequest(vmId, sourceHostId, targetHostId) =>
              config.migrationModel match
                case Some(model) =>
                  // Compute migration plan with network-aware bandwidth
                  val vmRam = acc.hosts
                    .get(sourceHostId)
                    .flatMap(_._1.vms.get(vmId))
                    .map(_.spec.ram)
                    .getOrElse(MegaBytes(1024.0))

                  // Register flow for bandwidth contention tracking
                  val (updatedFlowMgr, flowId) = acc.flowManager.addFlow(
                    config.datacenterId,
                    config.datacenterId,
                    vmId,
                    event.time
                  )

                  // Use topology bandwidth if available, else fall back to config
                  val bandwidth = config.networkTopology match
                    case Some(topo) =>
                      val effectiveBw = updatedFlowMgr.effectiveBandwidth(
                        config.datacenterId,
                        config.datacenterId,
                        topo
                      )
                      if effectiveBw.value > 0 then effectiveBw else config.migrationBandwidth
                    case None => config.migrationBandwidth

                  val plan      = model(MigrationParams(vmRam, bandwidth))
                  val sourceRef = acc.hosts.get(sourceHostId).map(_._2)
                  val targetRef = acc.hosts.get(targetHostId).map(_._2)

                  (sourceRef, targetRef) match
                    case (Some(sRef), Some(tRef)) =>
                      val startEvent = SimEvent(
                        time = event.time,
                        source = entityRef,
                        destination = sRef,
                        payload =
                          VmMigrationStart(vmId, sourceHostId, targetHostId, plan.totalTime, plan.totalDataTransferred),
                        serial = SerialNumber.Zero
                      )
                      val completeEvent = SimEvent(
                        time = event.time + plan.totalTime,
                        source = entityRef,
                        destination = entityRef,
                        payload = VmMigrationComplete(vmId, sourceHostId, targetHostId),
                        serial = SerialNumber.Zero
                      )
                      acc.copy(
                        emittedEvents = acc.emittedEvents :+ startEvent :+ completeEvent,
                        flowManager = updatedFlowMgr,
                        migrationFlowIds = acc.migrationFlowIds + (vmId -> flowId)
                      )
                    case _ =>
                      acc.copy(emittedEvents =
                        acc.emittedEvents :+ SimEvent(
                          time = event.time,
                          source = entityRef,
                          destination = entityRef,
                          payload = VmMigrationFailed(vmId, "Source or target host not found"),
                          serial = SerialNumber.Zero
                        )
                      )
                case None =>
                  acc.copy(emittedEvents =
                    acc.emittedEvents :+ SimEvent(
                      time = event.time,
                      source = entityRef,
                      destination = entityRef,
                      payload = VmMigrationFailed(vmId, "No migration model configured"),
                      serial = SerialNumber.Zero
                    )
                  )

            case VmMigrationComplete(vmId, sourceHostId, targetHostId) =>
              // Remove network flow for this migration
              val updatedFlowMgr = acc.migrationFlowIds.get(vmId) match
                case Some(flowId) => acc.flowManager.removeFlow(flowId)
                case None         => acc.flowManager
              val updatedFlowIds = acc.migrationFlowIds - vmId

              // Move VM from source to target in our mirror state
              val result = for
                (sourceHs, sourceRef) <- acc.hosts.get(sourceHostId)
                vm                    <- sourceHs.vms.get(vmId)
                (targetHs, targetRef) <- acc.hosts.get(targetHostId)
              yield
                val updatedSource = sourceHs.removeVm(vmId)
                val runningVm     = vm.copy(status = io.aura.iaas.state.VmStatus.Running)
                val updatedTarget = targetHs.placeVm(runningVm)
                acc.copy(
                  hosts = acc.hosts
                    + (sourceHostId -> (updatedSource, sourceRef))
                    + (targetHostId -> (updatedTarget, targetRef)),
                  flowManager = updatedFlowMgr,
                  migrationFlowIds = updatedFlowIds
                )

              result.getOrElse(acc.copy(flowManager = updatedFlowMgr, migrationFlowIds = updatedFlowIds))

            case _ =>
              context.log.debug("DC {} ignoring event: {}", config.datacenterId.value, event.payload)
              acc
        }

        if acc.emittedEvents.nonEmpty then config.coordinator ! TimeCoordinator.ScheduleEvents(acc.emittedEvents)

        replyTo ! StepComplete(entityRef)
        active(config, acc.hosts, entityRef, context, acc.flowManager, acc.migrationFlowIds, lastConsolidationTime)

      case TimeCoordinator.FinalSnapshot(_, replyTo) =>
        // No end-of-sim bookkeeping; reply immediately so the coordinator
        // can proceed with shutdown.
        replyTo ! TimeCoordinator.FinalSnapshotComplete(entityRef)
        Behaviors.same
    }
