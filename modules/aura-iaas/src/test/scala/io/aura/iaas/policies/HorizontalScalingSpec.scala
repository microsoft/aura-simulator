// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.types.*

class HorizontalScalingSpec extends AnyFlatSpec with Matchers:

  val vmTemplate: ResourceSpec = ResourceSpec(PEs(2), MIPS(1000.0), MegaBytes(2048.0), Mbps(500.0), MegaBytes(10000.0))

  "HorizontalScalingPolicy.cpuThreshold" should "trigger above threshold" in {
    val policy = HorizontalScalingPolicy.cpuThreshold(0.8, vmTemplate)
    val ctx = HorizontalScalingContext(
      activeVmCount = 3,
      pendingWorkloads = 5,
      runningWorkloads = 10,
      avgCpuUtilization = Utilization(0.9),
      currentTime = SimTime(100.0)
    )
    policy(ctx) shouldBe Some(vmTemplate)
  }

  it should "not trigger below threshold" in {
    val policy = HorizontalScalingPolicy.cpuThreshold(0.8, vmTemplate)
    val ctx = HorizontalScalingContext(
      activeVmCount = 3,
      pendingWorkloads = 5,
      runningWorkloads = 10,
      avgCpuUtilization = Utilization(0.6),
      currentTime = SimTime(100.0)
    )
    policy(ctx) shouldBe None
  }

  it should "not trigger with no active VMs" in {
    val policy = HorizontalScalingPolicy.cpuThreshold(0.8, vmTemplate)
    val ctx = HorizontalScalingContext(
      activeVmCount = 0,
      pendingWorkloads = 5,
      runningWorkloads = 0,
      avgCpuUtilization = Utilization(0.9),
      currentTime = SimTime(100.0)
    )
    policy(ctx) shouldBe None
  }

  "HorizontalScalingPolicy.queueLength" should "trigger when pending exceeds threshold" in {
    val policy = HorizontalScalingPolicy.queueLength(5, vmTemplate)
    val ctx = HorizontalScalingContext(
      activeVmCount = 2,
      pendingWorkloads = 10,
      runningWorkloads = 5,
      avgCpuUtilization = Utilization(0.5),
      currentTime = SimTime(100.0)
    )
    policy(ctx) shouldBe Some(vmTemplate)
  }

  it should "not trigger when pending is within threshold" in {
    val policy = HorizontalScalingPolicy.queueLength(5, vmTemplate)
    val ctx = HorizontalScalingContext(
      activeVmCount = 2,
      pendingWorkloads = 3,
      runningWorkloads = 5,
      avgCpuUtilization = Utilization(0.5),
      currentTime = SimTime(100.0)
    )
    policy(ctx) shouldBe None
  }

  "HorizontalScalingPolicy.none" should "never trigger" in {
    val ctx = HorizontalScalingContext(
      activeVmCount = 100,
      pendingWorkloads = 1000,
      runningWorkloads = 500,
      avgCpuUtilization = Utilization(1.0),
      currentTime = SimTime(100.0)
    )
    HorizontalScalingPolicy.none(ctx) shouldBe None
  }
