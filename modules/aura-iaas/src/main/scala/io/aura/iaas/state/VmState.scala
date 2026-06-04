// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.state

import io.aura.core.types.*
import io.aura.core.events.WorkloadSpec

/** Immutable VM state managed by the HostActor.
  *
  * All state transitions produce new immutable instances.
  */
final case class VmState(
    id: VmId,
    brokerId: BrokerId,
    spec: ResourceSpec,
    currentSpec: ResourceSpec,
    status: VmStatus,
    runningWorkloads: Map[WorkloadId, WorkloadExecution],
    finishedWorkloads: Vector[WorkloadId],
    creationTime: SimTime
):
  def totalAllocatedMips: MIPS =
    MIPS(runningWorkloads.values.map(_.allocatedMips.value).sum)

  def availableMips: MIPS =
    MIPS(currentSpec.mips.value - totalAllocatedMips.value)

  def cpuUtilization: Utilization =
    if currentSpec.mips.value <= 0 then Utilization.Zero
    else Utilization(totalAllocatedMips.value / currentSpec.mips.value)

  def canRunWorkload(workload: WorkloadSpec): Boolean =
    status == VmStatus.Running &&
      workload.pes <= currentSpec.pes &&
      availableMips >= workload.requiredMips

  def startWorkload(
      workloadId: WorkloadId,
      allocatedMips: MIPS,
      startTime: SimTime,
      length: MI,
      weight: Int = 1024
  ): VmState =
    copy(
      runningWorkloads = runningWorkloads + (workloadId -> WorkloadExecution(
        workloadId = workloadId,
        allocatedMips = allocatedMips,
        startTime = startTime,
        executedMI = MI.Zero,
        totalMI = length,
        weight = weight
      ))
    )

  def finishWorkload(workloadId: WorkloadId): VmState =
    copy(
      runningWorkloads = runningWorkloads - workloadId,
      finishedWorkloads = finishedWorkloads :+ workloadId
    )

  def updateWorkloadProgress(workloadId: WorkloadId, additionalMI: MI): VmState =
    runningWorkloads.get(workloadId) match
      case Some(exec) =>
        val updated = exec.copy(executedMI = exec.executedMI + additionalMI)
        copy(runningWorkloads = runningWorkloads + (workloadId -> updated))
      case None => this

  def scaleMips(newMips: MIPS): VmState =
    copy(currentSpec = currentSpec.copy(mips = newMips))

enum VmStatus:
  case Created, Running, Paused, Destroyed, Migrating

/** Tracks the execution progress of a workload on a VM. */
final case class WorkloadExecution(
    workloadId: WorkloadId,
    allocatedMips: MIPS,
    startTime: SimTime,
    executedMI: MI,
    totalMI: MI,
    weight: Int = 1024
):
  def remainingMI: MI = MI(totalMI.value - executedMI.value)

  def isComplete: Boolean = executedMI.value >= totalMI.value

  def estimatedCompletionTime: SimTime =
    if allocatedMips.value <= 0.0 then SimTime.MaxValue
    else SimTime(startTime.value + totalMI.value / allocatedMips.value)

object VmState:
  def create(
      id: VmId,
      brokerId: BrokerId,
      spec: ResourceSpec,
      creationTime: SimTime
  ): VmState =
    VmState(
      id = id,
      brokerId = brokerId,
      spec = spec,
      currentSpec = spec,
      status = VmStatus.Running,
      runningWorkloads = Map.empty,
      finishedWorkloads = Vector.empty,
      creationTime = creationTime
    )
