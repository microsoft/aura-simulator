// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.state

import io.aura.core.types.*
import io.aura.core.events.WorkloadSpec

/** Immutable Host state.
  *
  * HostState is a pure immutable case class — every transition returns a new instance.
  */
final case class HostState(
    id: HostId,
    datacenterId: DatacenterId,
    spec: ResourceSpec,
    available: AvailableResources,
    vms: Map[VmId, VmState],
    active: Boolean,
    utilizationHistory: Vector[(SimTime, Utilization)] = Vector.empty
):
  def cpuUtilization: Utilization =
    if spec.mips.value <= 0 then Utilization.Zero
    else
      val used = spec.mips.value - available.mips.value
      Utilization(used / spec.mips.value)

  def canPlaceVm(vmSpec: ResourceSpec): Boolean =
    active && available.canFit(vmSpec)

  def placeVm(vm: VmState): HostState =
    copy(
      available = available.allocate(vm.spec),
      vms = vms + (vm.id -> vm)
    )

  def removeVm(vmId: VmId): HostState =
    vms.get(vmId) match
      case Some(vm) =>
        copy(
          available = available.release(vm.spec),
          vms = vms - vmId
        )
      case None => this

  def updateVm(vm: VmState): HostState =
    copy(vms = vms + (vm.id -> vm))

  def totalRunningWorkloads: Int =
    vms.values.map(_.runningWorkloads.size).sum

  /** Find a VM that can run the given workload. */
  def findVmForWorkload(workload: WorkloadSpec): Option[VmId] =
    vms.collectFirst {
      case (vmId, vm) if vm.canRunWorkload(workload) => vmId
    }

  def recordUtilization(time: SimTime): HostState =
    copy(utilizationHistory = utilizationHistory :+ (time, cpuUtilization))

object HostState:
  def create(
      id: HostId,
      datacenterId: DatacenterId,
      spec: ResourceSpec
  ): HostState =
    HostState(
      id = id,
      datacenterId = datacenterId,
      spec = spec,
      available = AvailableResources.fromSpec(spec),
      vms = Map.empty,
      active = true
    )
