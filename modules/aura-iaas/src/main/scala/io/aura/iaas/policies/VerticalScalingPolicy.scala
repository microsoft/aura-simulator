// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import io.aura.core.types.*
import io.aura.core.events.{ScalableResource, ScalingDirection}
import io.aura.iaas.state.{HostState, VmState}

/** Scaling action recommended by a vertical scaling policy. */
final case class ScalingAction(
    vmId: VmId,
    resource: ScalableResource,
    newAmount: Double,
    direction: ScalingDirection
)

/** Vertical scaling policy as a pure function: (VmState, HostState) => Option[ScalingAction] */
type VerticalScalingPolicy = (VmState, HostState) => Option[ScalingAction]

object VerticalScalingPolicy:

  /** Gradual scaling: scale up by factor when utilization > upper threshold, scale down when utilization < lower
    * threshold.
    *
    * @param upperThreshold
    *   CPU utilization above which to scale up (e.g., 0.8)
    * @param lowerThreshold
    *   CPU utilization below which to scale down (e.g., 0.2)
    * @param scalingFactor
    *   Multiply/divide MIPS by this factor (e.g., 1.5 = 50% increase)
    */
  def gradual(
      upperThreshold: Double = 0.8,
      lowerThreshold: Double = 0.2,
      scalingFactor: Double = 1.5
  ): VerticalScalingPolicy =
    (vm: VmState, host: HostState) =>
      val util = vm.cpuUtilization.value
      if util > upperThreshold then
        val newMips = vm.currentSpec.mips.value * scalingFactor
        // Check if host has capacity
        val needed = newMips - vm.currentSpec.mips.value
        if needed > 0 && host.available.mips.value >= MIPS(needed).value then
          Some(ScalingAction(vm.id, ScalableResource.CPU, newMips, ScalingDirection.Up))
        else None
      else if util < lowerThreshold && vm.runningWorkloads.nonEmpty then
        val newMips = math.max(vm.currentSpec.mips.value / scalingFactor, vm.spec.mips.value * 0.25)
        if newMips < vm.currentSpec.mips.value then
          Some(ScalingAction(vm.id, ScalableResource.CPU, newMips, ScalingDirection.Down))
        else None
      else None
