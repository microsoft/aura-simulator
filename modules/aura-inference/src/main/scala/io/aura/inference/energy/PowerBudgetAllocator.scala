// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.inference.energy

import io.aura.core.types.*

/** Power budget allocation across GPUs in a cluster.
  *
  * Distributes a cluster-level power budget across individual GPUs to maximize throughput while staying within energy
  * constraints.
  */
object PowerBudgetAllocator:

  /** Per-GPU power allocation. */
  final case class GpuPowerAllocation(
      deviceId: GpuDeviceId,
      allocatedWatts: Watts,
      targetFreqMHz: Int
  )

  /** Equal allocation: distribute budget evenly across all active GPUs. */
  def equalAllocation(
      totalBudget: Watts,
      activeGpuCount: Int,
      spec: GpuDeviceSpec,
      deviceIds: Vector[GpuDeviceId]
  ): Vector[GpuPowerAllocation] =
    if activeGpuCount <= 0 || deviceIds.isEmpty then Vector.empty
    else
      val perGpuBudget = Watts(totalBudget.value / activeGpuCount)
      val cappedBudget = Watts(perGpuBudget.value.min(spec.tdpWatts.value))
      // Estimate frequency from power budget
      val freqRatio =
        ((cappedBudget.value - spec.idleWatts.value) / (spec.tdpWatts.value - spec.idleWatts.value)).max(0.0).min(1.0)
      val targetFreq =
        spec.frequencyMinMHz + ((spec.frequencyMaxMHz - spec.frequencyMinMHz) * math.sqrt(freqRatio)).toInt
      deviceIds.map { devId =>
        GpuPowerAllocation(devId, cappedBudget, targetFreq.max(spec.frequencyMinMHz).min(spec.frequencyMaxMHz))
      }

  /** Proportional allocation: allocate based on workload demand per GPU. */
  def proportionalAllocation(
      totalBudget: Watts,
      demands: Vector[(GpuDeviceId, Double)],
      spec: GpuDeviceSpec
  ): Vector[GpuPowerAllocation] =
    if demands.isEmpty then Vector.empty
    else
      val totalDemand = demands.map(_._2).sum.max(0.01)
      demands.map { case (devId, demand) =>
        val fraction  = demand / totalDemand
        val allocated = Watts((totalBudget.value * fraction).min(spec.tdpWatts.value).max(spec.idleWatts.value))
        val freqRatio =
          ((allocated.value - spec.idleWatts.value) / (spec.tdpWatts.value - spec.idleWatts.value)).max(0.0).min(1.0)
        val targetFreq =
          spec.frequencyMinMHz + ((spec.frequencyMaxMHz - spec.frequencyMinMHz) * math.sqrt(freqRatio)).toInt
        GpuPowerAllocation(devId, allocated, targetFreq.max(spec.frequencyMinMHz).min(spec.frequencyMaxMHz))
      }
