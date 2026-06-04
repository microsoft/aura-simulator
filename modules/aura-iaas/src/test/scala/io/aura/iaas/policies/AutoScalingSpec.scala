// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.types.*

class AutoScalingSpec extends AnyFlatSpec with Matchers:

  val vmTemplate: ResourceSpec = ResourceSpec(PEs(2), MIPS(1000.0), MegaBytes(2048.0), Mbps(500.0), MegaBytes(10000.0))

  // ─── targetUtilization ──────────────────────────────────────────────

  "HorizontalScalingPolicy.targetUtilization" should "trigger when CPU exceeds target + tolerance" in {
    val policy = HorizontalScalingPolicy.targetUtilization(target = 0.7, tolerance = 0.1, vmTemplate = vmTemplate)
    val ctx = HorizontalScalingContext(
      activeVmCount = 3,
      pendingWorkloads = 0,
      runningWorkloads = 5,
      avgCpuUtilization = Utilization(0.85),
      currentTime = SimTime(100.0)
    )
    policy(ctx) shouldBe Some(vmTemplate)
  }

  it should "not trigger when CPU is within tolerance band" in {
    val policy = HorizontalScalingPolicy.targetUtilization(target = 0.7, tolerance = 0.1, vmTemplate = vmTemplate)
    val ctx = HorizontalScalingContext(
      activeVmCount = 3,
      pendingWorkloads = 0,
      runningWorkloads = 5,
      avgCpuUtilization = Utilization(0.75),
      currentTime = SimTime(100.0)
    )
    policy(ctx) shouldBe None
  }

  it should "not trigger with no active VMs" in {
    val policy = HorizontalScalingPolicy.targetUtilization(target = 0.7, tolerance = 0.1, vmTemplate = vmTemplate)
    val ctx = HorizontalScalingContext(
      activeVmCount = 0,
      pendingWorkloads = 5,
      runningWorkloads = 0,
      avgCpuUtilization = Utilization(0.95),
      currentTime = SimTime(100.0)
    )
    policy(ctx) shouldBe None
  }

  // ─── workloadRatio ──────────────────────────────────────────────────

  "HorizontalScalingPolicy.workloadRatio" should "trigger when ratio exceeds threshold" in {
    val policy = HorizontalScalingPolicy.workloadRatio(maxWorkloadsPerVm = 3.0, vmTemplate = vmTemplate)
    val ctx = HorizontalScalingContext(
      activeVmCount = 2,
      pendingWorkloads = 5,
      runningWorkloads = 4,
      avgCpuUtilization = Utilization(0.5),
      currentTime = SimTime(100.0)
    )
    // ratio = (5+4)/2 = 4.5 > 3.0
    policy(ctx) shouldBe Some(vmTemplate)
  }

  it should "not trigger when ratio is within threshold" in {
    val policy = HorizontalScalingPolicy.workloadRatio(maxWorkloadsPerVm = 5.0, vmTemplate = vmTemplate)
    val ctx = HorizontalScalingContext(
      activeVmCount = 2,
      pendingWorkloads = 3,
      runningWorkloads = 4,
      avgCpuUtilization = Utilization(0.5),
      currentTime = SimTime(100.0)
    )
    // ratio = (3+4)/2 = 3.5 < 5.0
    policy(ctx) shouldBe None
  }

  it should "trigger with zero VMs but pending workloads" in {
    val policy = HorizontalScalingPolicy.workloadRatio(maxWorkloadsPerVm = 5.0, vmTemplate = vmTemplate)
    val ctx = HorizontalScalingContext(
      activeVmCount = 0,
      pendingWorkloads = 3,
      runningWorkloads = 0,
      avgCpuUtilization = Utilization(0.0),
      currentTime = SimTime(100.0)
    )
    policy(ctx) shouldBe Some(vmTemplate)
  }

  // ─── combined ───────────────────────────────────────────────────────

  "HorizontalScalingPolicy.combined" should "return first matching policy result" in {
    val neverTrigger  = HorizontalScalingPolicy.cpuThreshold(upperThreshold = 0.99, vmTemplate = vmTemplate)
    val alwaysTrigger = HorizontalScalingPolicy.queueLength(maxPending = 0, vmTemplate = vmTemplate)
    val combined      = HorizontalScalingPolicy.combined(neverTrigger, alwaysTrigger)

    val ctx = HorizontalScalingContext(
      activeVmCount = 1,
      pendingWorkloads = 5,
      runningWorkloads = 0,
      avgCpuUtilization = Utilization(0.5),
      currentTime = SimTime(100.0)
    )
    // First policy doesn't trigger (CPU 0.5 < 0.99), second triggers (pending 5 > 0)
    combined(ctx) shouldBe Some(vmTemplate)
  }

  it should "return None when no policy triggers" in {
    val policy1  = HorizontalScalingPolicy.cpuThreshold(upperThreshold = 0.99, vmTemplate = vmTemplate)
    val policy2  = HorizontalScalingPolicy.queueLength(maxPending = 100, vmTemplate = vmTemplate)
    val combined = HorizontalScalingPolicy.combined(policy1, policy2)

    val ctx = HorizontalScalingContext(
      activeVmCount = 1,
      pendingWorkloads = 2,
      runningWorkloads = 0,
      avgCpuUtilization = Utilization(0.5),
      currentTime = SimTime(100.0)
    )
    combined(ctx) shouldBe None
  }
