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
import io.aura.iaas.state.*
import io.aura.iaas.policies.WorkloadScheduler
import io.aura.power.PowerModel

/** Actor representing a physical Host in a datacenter.
  *
  * Manages immutable HostState, handles VM placement, workload execution, power tracking, and energy reporting. Reports
  * TimeStepComplete for barrier synchronization.
  */
object HostActor:

  final case class Config(
      hostId: HostId,
      datacenterId: DatacenterId,
      spec: ResourceSpec,
      scheduler: WorkloadScheduler,
      powerModel: PowerModel,
      coordinator: ActorRef[TimeCoordinator.Command],
      datacenterRef: Option[EntityRef] = None,
      energySamplingInterval: SimTime = SimTime(10.0)
  )

  def apply(config: Config): Behavior[EntityCommand] =
    Behaviors.setup { context =>
      val entityRef = EntityRef(s"host-${config.hostId.value}", EntityType.Host)
      config.coordinator ! TimeCoordinator.RegisterEntity(entityRef, context.self)

      // Schedule periodic energy sampling
      val firstSample = SimEvent(
        time = config.energySamplingInterval,
        source = entityRef,
        destination = entityRef,
        payload = EnergySample(config.energySamplingInterval),
        serial = SerialNumber.Zero
      )
      config.coordinator ! TimeCoordinator.ScheduleEvents(Vector(firstSample))

      active(
        config,
        HostState.create(config.hostId, config.datacenterId, config.spec),
        SimTime.Zero,
        SimTime.Zero, // lastEnergyTime
        None,         // no pending WorkloadUpdate scheduled yet
        entityRef,
        context
      )
    }

  /** Immutable accumulator for event processing within a time step. */
  private case class StepAccumulator(
      hostState: HostState,
      lastUpdateTime: SimTime,
      lastEnergyTime: SimTime,
      emittedEvents: Vector[SimEvent],
      workloadsChanged: Boolean,
      scheduledUpdateTime: Option[SimTime]
  )

  /** @param scheduledUpdateTime
    *   tracks the time of the already-scheduled WorkloadUpdate to avoid scheduling duplicates
    */
  private def active(
      config: Config,
      state: HostState,
      lastUpdateTime: SimTime,
      lastEnergyTime: SimTime,
      scheduledUpdateTime: Option[SimTime],
      entityRef: EntityRef,
      context: ActorContext[EntityCommand]
  ): Behavior[EntityCommand] =
    Behaviors.receiveMessage {
      case ProcessEvents(events, replyTo) =>
        val initial = StepAccumulator(state, lastUpdateTime, lastEnergyTime, Vector.empty, false, scheduledUpdateTime)

        val acc = events.foldLeft(initial) { (acc, event) =>
          event.payload match
            case VmCreateRequest(vmId, brokerId, vmSpec) =>
              if acc.hostState.canPlaceVm(vmSpec) then
                val vm = VmState.create(vmId, brokerId, vmSpec, event.time)
                context.log.debug("Host {} placed VM {}", config.hostId.value, vmId.value)
                acc.copy(
                  hostState = acc.hostState.placeVm(vm),
                  emittedEvents = acc.emittedEvents :+ SimEvent(
                    time = event.time,
                    source = entityRef,
                    destination = event.source,
                    payload = VmCreated(vmId, config.hostId, config.datacenterId),
                    serial = SerialNumber.Zero
                  )
                )
              else
                acc.copy(
                  emittedEvents = acc.emittedEvents :+ SimEvent(
                    time = event.time,
                    source = entityRef,
                    destination = event.source,
                    payload = VmCreateFailed(vmId, s"Insufficient resources on host ${config.hostId.value}"),
                    serial = SerialNumber.Zero
                  )
                )

            case VmDestroyRequest(vmId) =>
              acc.copy(
                hostState = acc.hostState.removeVm(vmId),
                emittedEvents = acc.emittedEvents :+ SimEvent(
                  time = event.time,
                  source = entityRef,
                  destination = event.source,
                  payload = VmDestroyed(vmId, config.hostId),
                  serial = SerialNumber.Zero
                )
              )

            case WorkloadSubmit(workload) =>
              acc.hostState.findVmForWorkload(workload) match
                case Some(vmId) =>
                  val vm = acc.hostState.vms(vmId)
                  val updatedVm = vm.startWorkload(
                    workload.id,
                    workload.requiredMips,
                    event.time,
                    workload.length,
                    workload.weight
                  )
                  acc.copy(
                    hostState = acc.hostState.updateVm(updatedVm),
                    workloadsChanged = true,
                    emittedEvents = acc.emittedEvents :+ SimEvent(
                      time = event.time,
                      source = entityRef,
                      destination = event.source,
                      payload = WorkloadStarted(workload.id, vmId, config.hostId, event.time),
                      serial = SerialNumber.Zero
                    )
                  )

                case None =>
                  context.log.warn(
                    "Host {} has no VM available for workload {}",
                    config.hostId.value,
                    workload.id.value
                  )
                  acc.copy(
                    emittedEvents = acc.emittedEvents :+ SimEvent(
                      time = event.time,
                      source = entityRef,
                      destination = event.source,
                      payload = WorkloadFailed(workload.id, s"No VM available on host ${config.hostId.value}"),
                      serial = SerialNumber.Zero
                    )
                  )

            case WorkloadUpdate(currentTime) =>
              // Process all VMs' workload progress via fold
              val (updatedHostState, completionEvents) =
                acc.hostState.vms.foldLeft((acc.hostState, Vector.empty[SimEvent])) { case ((hs, events), (vmId, vm)) =>
                  if vm.runningWorkloads.isEmpty then (hs, events)
                  else
                    val result    = config.scheduler(vm, currentTime, acc.lastUpdateTime)
                    val newHs     = hs.updateVm(result.updatedVm)
                    val brokerRef = EntityRef(s"broker-${vm.brokerId.value}", EntityType.Broker)
                    // Capture totalMI from pre-scheduling state (before finishWorkload removes it)
                    val newEvents = result.completedWorkloads.map { completedId =>
                      SimEvent(
                        time = currentTime,
                        source = entityRef,
                        destination = brokerRef,
                        payload = WorkloadFinished(
                          completedId,
                          vmId,
                          config.hostId,
                          currentTime,
                          vm.runningWorkloads.get(completedId).map(_.totalMI).getOrElse(MI.Zero)
                        ),
                        serial = SerialNumber.Zero
                      )
                    }
                    (newHs, events ++ newEvents)
                }

              // Compute energy since last energy report (avoids double-counting with EnergySample)
              val energyDelta = currentTime - acc.lastEnergyTime
              val utilization = updatedHostState.cpuUtilization
              val watts       = config.powerModel(utilization)
              val energyWh =
                if energyDelta.value > 0 then WattHours(watts.value * energyDelta.value / 3600.0) else WattHours.Zero
              val metricsRef = EntityRef("metrics", EntityType.TimeCoord)

              val energyEvent = SimEvent(
                time = currentTime,
                source = entityRef,
                destination = metricsRef,
                payload = EnergyReport(config.hostId, watts, acc.lastEnergyTime, currentTime, energyWh),
                serial = SerialNumber.Zero
              )

              val utilEvent = SimEvent(
                time = currentTime,
                source = entityRef,
                destination = metricsRef,
                payload = HostUtilizationUpdate(config.hostId, utilization, currentTime),
                serial = SerialNumber.Zero
              )

              // Also send utilization to datacenter for overload detection
              val dcUtilEvents = config.datacenterRef match
                case Some(dcRef) =>
                  Vector(
                    SimEvent(
                      time = currentTime,
                      source = entityRef,
                      destination = dcRef,
                      payload = HostUtilizationUpdate(config.hostId, utilization, currentTime),
                      serial = SerialNumber.Zero
                    )
                  )
                case None => Vector.empty

              acc.copy(
                hostState = updatedHostState,
                lastUpdateTime = currentTime,
                lastEnergyTime = currentTime,
                emittedEvents = (acc.emittedEvents ++ completionEvents :+ energyEvent :+ utilEvent) ++ dcUtilEvents,
                workloadsChanged = true,
                scheduledUpdateTime = None // consumed — clear tracking
              )

            case VmMigrationStart(vmId, _, _, _, _) =>
              acc.hostState.vms.get(vmId) match
                case Some(vm) =>
                  val migratingVm = vm.copy(status = VmStatus.Migrating)
                  acc.copy(hostState = acc.hostState.updateVm(migratingVm))
                case None => acc

            case VmScaleRequest(vmId, resource, newAmount, direction) =>
              acc.hostState.vms.get(vmId) match
                case Some(vm) =>
                  resource match
                    case ScalableResource.CPU =>
                      val oldMips = vm.currentSpec.mips.value
                      val delta   = newAmount - oldMips
                      if direction == ScalingDirection.Up && delta > 0 && acc.hostState.available.mips.value >= MIPS(
                          delta
                        ).value
                      then
                        val scaledVm = vm.scaleMips(MIPS(newAmount))
                        val newAvail =
                          acc.hostState.available.copy(mips = MIPS(acc.hostState.available.mips.value - delta))
                        acc.copy(
                          hostState = acc.hostState.copy(available = newAvail).updateVm(scaledVm),
                          emittedEvents = acc.emittedEvents :+ SimEvent(
                            time = event.time,
                            source = entityRef,
                            destination = event.source,
                            payload = VmScaled(vmId, ScalableResource.CPU, oldMips, newAmount),
                            serial = SerialNumber.Zero
                          )
                        )
                      else if direction == ScalingDirection.Down && delta < 0 then
                        val scaledVm = vm.scaleMips(MIPS(newAmount))
                        val newAvail =
                          acc.hostState.available.copy(mips = MIPS(acc.hostState.available.mips.value - delta))
                        acc.copy(
                          hostState = acc.hostState.copy(available = newAvail).updateVm(scaledVm),
                          emittedEvents = acc.emittedEvents :+ SimEvent(
                            time = event.time,
                            source = entityRef,
                            destination = event.source,
                            payload = VmScaled(vmId, ScalableResource.CPU, oldMips, newAmount),
                            serial = SerialNumber.Zero
                          )
                        )
                      else acc
                    case _ => acc // RAM, BW scaling can be added later
                case None => acc

            case HostFaultEvent(hostId, failedPEs) =>
              // Reduce available PEs and destroy VMs that can't run
              val currentAvail  = acc.hostState.available
              val newPEs        = PEs(math.max(0, currentAvail.pes.value - failedPEs.value))
              val updatedAvail  = currentAvail.copy(pes = newPEs)
              val baseHostState = acc.hostState.copy(available = updatedAvail)

              // Check each VM: if allocated PEs > remaining total PEs, destroy it
              val totalRemainingPEs = config.spec.pes.value - failedPEs.value
              val (finalHostState, destroyEvents) =
                baseHostState.vms.foldLeft((baseHostState, Vector.empty[SimEvent])) { case ((hs, events), (vmId, vm)) =>
                  if vm.currentSpec.pes.value > totalRemainingPEs then
                    val brokerRef = EntityRef(s"broker-${vm.brokerId.value}", EntityType.Broker)
                    val preemptEvents = vm.runningWorkloads.map { case (wlId, exec) =>
                      SimEvent(
                        time = event.time,
                        source = entityRef,
                        destination = brokerRef,
                        payload = WorkloadPreempted(wlId, vmId, exec.executedMI),
                        serial = SerialNumber.Zero
                      )
                    }.toVector
                    val destroyEvent = SimEvent(
                      time = event.time,
                      source = entityRef,
                      destination = brokerRef,
                      payload = VmDestroyed(vmId, config.hostId),
                      serial = SerialNumber.Zero
                    )
                    (hs.removeVm(vmId), events ++ preemptEvents :+ destroyEvent)
                  else (hs, events)
                }

              // Report fault to metrics
              val metricsRef = EntityRef("metrics", EntityType.TimeCoord)
              val faultEvent = SimEvent(
                time = event.time,
                source = entityRef,
                destination = metricsRef,
                payload = HostFaultEvent(config.hostId, failedPEs),
                serial = SerialNumber.Zero
              )

              acc.copy(
                hostState = finalHostState,
                emittedEvents = acc.emittedEvents ++ destroyEvents :+ faultEvent
              )

            case EnergySample(currentTime) =>
              val energyDelta = currentTime - acc.lastEnergyTime
              if energyDelta.value > 0 then
                val utilization = acc.hostState.cpuUtilization
                val watts       = config.powerModel(utilization)
                val energyWh    = WattHours(watts.value * energyDelta.value / 3600.0)
                val metricsRef  = EntityRef("metrics", EntityType.TimeCoord)

                val energyEvent = SimEvent(
                  time = currentTime,
                  source = entityRef,
                  destination = metricsRef,
                  payload = EnergyReport(config.hostId, watts, acc.lastEnergyTime, currentTime, energyWh),
                  serial = SerialNumber.Zero
                )
                val utilEvent = SimEvent(
                  time = currentTime,
                  source = entityRef,
                  destination = metricsRef,
                  payload = HostUtilizationUpdate(config.hostId, utilization, currentTime),
                  serial = SerialNumber.Zero
                )
                // Schedule next energy sample
                val nextSampleTime = currentTime + config.energySamplingInterval
                val nextSample = SimEvent(
                  time = nextSampleTime,
                  source = entityRef,
                  destination = entityRef,
                  payload = EnergySample(nextSampleTime),
                  serial = SerialNumber.Zero
                )
                acc.copy(
                  lastEnergyTime = currentTime,
                  emittedEvents = acc.emittedEvents :+ energyEvent :+ utilEvent :+ nextSample
                )
              else
                // Schedule next sample without reporting
                val nextSampleTime = currentTime + config.energySamplingInterval
                val nextSample = SimEvent(
                  time = nextSampleTime,
                  source = entityRef,
                  destination = entityRef,
                  payload = EnergySample(nextSampleTime),
                  serial = SerialNumber.Zero
                )
                acc.copy(emittedEvents = acc.emittedEvents :+ nextSample)

            case _ =>
              context.log.debug("Host {} ignoring event: {}", config.hostId.value, event.payload)
              acc
        }

        // After processing all events, run the scheduler with zero delta to adjust
        // MIPS allocations (needed after WorkloadSubmit so CFS/timeShared proportional
        // MIPS are set before computing the next WorkloadUpdate time).
        val (finalEvents, finalScheduledUpdate, finalHostState) =
          if !acc.workloadsChanged then (acc.emittedEvents, acc.scheduledUpdateTime, acc.hostState)
          else
            // Run scheduler on all VMs with zero delta to ensure MIPS are properly allocated
            val adjustedHostState = acc.hostState.vms.foldLeft(acc.hostState) { case (hs, (_, vm)) =>
              if vm.runningWorkloads.isEmpty then hs
              else
                val result = config.scheduler(vm, acc.lastUpdateTime, acc.lastUpdateTime)
                hs.updateVm(result.updatedVm)
            }

            val earliestCompletion = adjustedHostState.vms.values
              .flatMap { vm =>
                if vm.runningWorkloads.isEmpty then None
                else
                  val remaining = vm.runningWorkloads.values.map { exec =>
                    if exec.allocatedMips.value <= 0.0 then SimTime.MaxValue
                    else SimTime(acc.lastUpdateTime.value + exec.remainingMI.value / exec.allocatedMips.value)
                  }
                  Some(remaining.minBy(_.value))
              }
              .minByOption(_.value)

            earliestCompletion match
              case Some(nextTime) =>
                val shouldSchedule = acc.scheduledUpdateTime match
                  case Some(existingTime) => nextTime < existingTime
                  case None               => true

                if shouldSchedule then
                  val updateEvent = SimEvent(
                    time = nextTime,
                    source = entityRef,
                    destination = entityRef,
                    payload = WorkloadUpdate(nextTime),
                    serial = SerialNumber.Zero
                  )
                  (acc.emittedEvents :+ updateEvent, Some(nextTime), adjustedHostState)
                else (acc.emittedEvents, acc.scheduledUpdateTime, adjustedHostState)

              case None =>
                // No running workloads — clear scheduled update
                (acc.emittedEvents, None, adjustedHostState)

        // Schedule any generated events
        if finalEvents.nonEmpty then config.coordinator ! TimeCoordinator.ScheduleEvents(finalEvents)

        // Signal barrier completion
        replyTo ! StepComplete(entityRef)

        active(config, finalHostState, acc.lastUpdateTime, acc.lastEnergyTime, finalScheduledUpdate, entityRef, context)

      case TimeCoordinator.FinalSnapshot(_, replyTo) =>
        // No end-of-sim bookkeeping; reply immediately so the coordinator
        // can proceed with shutdown.
        replyTo ! TimeCoordinator.FinalSnapshotComplete(entityRef)
        Behaviors.same
    }
