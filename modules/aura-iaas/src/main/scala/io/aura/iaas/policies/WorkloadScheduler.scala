// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import io.aura.core.types.*
import io.aura.iaas.state.VmState

/** Workload scheduling as pure functions.
  *
  * Implemented as pure functions: (vmState, currentTime, lastUpdateTime) => SchedulingResult.
  */

/** Result of scheduling: updated VM state + list of completed workload IDs. */
final case class SchedulingResult(
    updatedVm: VmState,
    completedWorkloads: Vector[WorkloadId],
    nextEventTime: Option[SimTime]
)

type WorkloadScheduler = (VmState, SimTime, SimTime) => SchedulingResult

object WorkloadScheduler:

  /** Space-shared scheduler: each workload gets dedicated PEs. Only one workload per PE — no sharing. Workloads queue
    * if all PEs are busy.
    */
  val spaceShared: WorkloadScheduler = (vm, currentTime, lastUpdateTime) =>
    val deltaTime = currentTime - lastUpdateTime
    if deltaTime.isZero || deltaTime.value < 0 then
      SchedulingResult(vm, Vector.empty, nextCompletionTime(vm, currentTime))
    else
      val (updatedVm, completed) = processWorkloads(vm, deltaTime)
      SchedulingResult(updatedVm, completed, nextCompletionTime(updatedVm, currentTime))

  /** Time-shared scheduler: all workloads share the VM's CPU equally. MIPS are divided evenly among all running
    * workloads.
    */
  val timeShared: WorkloadScheduler = (vm, currentTime, lastUpdateTime) =>
    val deltaTime = currentTime - lastUpdateTime
    val n         = vm.runningWorkloads.size
    if n == 0 then SchedulingResult(vm, Vector.empty, None)
    else
      // Divide total MIPS equally among all running workloads
      val mipsPerWorkload = MIPS(vm.currentSpec.mips.value / n.toDouble)
      val adjustedVm = vm.copy(
        runningWorkloads = vm.runningWorkloads.map { case (wId, exec) =>
          wId -> exec.copy(allocatedMips = mipsPerWorkload)
        }
      )
      if deltaTime.isZero || deltaTime.value < 0 then
        SchedulingResult(adjustedVm, Vector.empty, nextCompletionTime(adjustedVm, currentTime))
      else
        val (updatedVm, completed) = processWorkloads(adjustedVm, deltaTime)
        SchedulingResult(updatedVm, completed, nextCompletionTime(updatedVm, currentTime))

  /** CFS (Completely Fair Scheduler): allocates MIPS proportional to workload weight.
    *
    * Formula: mipsForWorkload = totalVmMips * (workloadWeight / sumOfAllWeights) Default weight 1024 = Linux nice-0.
    * Weight 3072 = 3x more CPU. Degrades to timeShared when all weights are equal.
    */
  val cfs: WorkloadScheduler = (vm, currentTime, lastUpdateTime) =>
    val deltaTime = currentTime - lastUpdateTime
    val n         = vm.runningWorkloads.size
    if n == 0 then SchedulingResult(vm, Vector.empty, None)
    else
      val totalWeight = vm.runningWorkloads.values.map(_.weight.toDouble).sum
      val adjustedVm = vm.copy(
        runningWorkloads = vm.runningWorkloads.map { case (wId, exec) =>
          val proportion = if totalWeight > 0 then exec.weight.toDouble / totalWeight else 1.0 / n
          wId -> exec.copy(allocatedMips = MIPS(vm.currentSpec.mips.value * proportion))
        }
      )
      if deltaTime.isZero || deltaTime.value < 0 then
        SchedulingResult(adjustedVm, Vector.empty, nextCompletionTime(adjustedVm, currentTime))
      else
        val (updatedVm, completed) = processWorkloads(adjustedVm, deltaTime)
        SchedulingResult(updatedVm, completed, nextCompletionTime(updatedVm, currentTime))

  /** Process all running workloads for the given time delta. Pure function. */
  private def processWorkloads(vm: VmState, deltaTime: SimTime): (VmState, Vector[WorkloadId]) =
    vm.runningWorkloads.foldLeft((vm, Vector.empty[WorkloadId])) { case ((accVm, accCompleted), (wId, exec)) =>
      val executedMI  = MI(exec.allocatedMips.value * deltaTime.value)
      val newExecuted = MI(exec.executedMI.value + executedMI.value)

      if newExecuted.value >= exec.totalMI.value then (accVm.finishWorkload(wId), accCompleted :+ wId)
      else (accVm.updateWorkloadProgress(wId, executedMI), accCompleted)
    }

  /** Calculate the earliest completion time among all running workloads.
    *
    * Uses: currentTime + remainingMI / allocatedMips This correctly handles partial progress and changing MIPS
    * allocations.
    */
  private def nextCompletionTime(vm: VmState, currentTime: SimTime): Option[SimTime] =
    if vm.runningWorkloads.isEmpty then None
    else
      val times = vm.runningWorkloads.values.map { exec =>
        val remaining = exec.remainingMI
        if exec.allocatedMips.value <= 0.0 then SimTime.MaxValue
        else SimTime(currentTime.value + remaining.value / exec.allocatedMips.value)
      }
      Some(times.minBy(_.value))
