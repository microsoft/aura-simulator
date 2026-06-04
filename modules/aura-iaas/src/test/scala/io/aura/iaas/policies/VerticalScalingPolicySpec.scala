// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.core.events.{ScalableResource, ScalingDirection}
import io.aura.iaas.state.{HostState, VmState}

class VerticalScalingPolicySpec extends AnyFlatSpec with Matchers:

  private val hostSpec = ResourceSpec(
    PEs(8),
    MIPS(20000.0),
    MegaBytes(32768.0),
    Mbps(10000.0),
    MegaBytes(1000000.0)
  )

  private def makeHost(id: Long): HostState =
    HostState.create(HostId(id), DatacenterId(0L), hostSpec)

  private def makeVm(id: Long, mips: Double): VmState =
    VmState.create(
      VmId(id),
      BrokerId(0L),
      ResourceSpec(PEs(2), MIPS(mips), MegaBytes(4096.0), Mbps(1000.0), MegaBytes(10000.0)),
      SimTime.Zero
    )

  "gradual scaling policy" should "recommend scale up when utilization exceeds upper threshold" in {
    val vm = makeVm(0L, 1000.0)
      .startWorkload(WorkloadId(0L), MIPS(900.0), SimTime.Zero, MI(10000.0))
    // utilization = 900/1000 = 0.9 > 0.8
    val host   = makeHost(0L).placeVm(vm)
    val policy = VerticalScalingPolicy.gradual(0.8, 0.2, 1.5)

    val action = policy(vm, host)
    action shouldBe defined
    action.get.direction shouldBe ScalingDirection.Up
    action.get.newAmount shouldBe 1500.0 +- 0.01 // 1000 * 1.5
    action.get.resource shouldBe ScalableResource.CPU
  }

  it should "recommend scale down when utilization is below lower threshold" in {
    val vm = makeVm(0L, 10000.0)
      .startWorkload(WorkloadId(0L), MIPS(500.0), SimTime.Zero, MI(10000.0))
    // utilization = 500/10000 = 0.05 < 0.2
    val host   = makeHost(0L).placeVm(vm)
    val policy = VerticalScalingPolicy.gradual(0.8, 0.2, 1.5)

    val action = policy(vm, host)
    action shouldBe defined
    action.get.direction shouldBe ScalingDirection.Down
    action.get.newAmount shouldBe (10000.0 / 1.5) +- 1.0
  }

  it should "not scale when utilization is within bounds" in {
    val vm = makeVm(0L, 1000.0)
      .startWorkload(WorkloadId(0L), MIPS(500.0), SimTime.Zero, MI(10000.0))
    // utilization = 500/1000 = 0.5, within [0.2, 0.8]
    val host   = makeHost(0L).placeVm(vm)
    val policy = VerticalScalingPolicy.gradual(0.8, 0.2, 1.5)

    policy(vm, host) shouldBe None
  }

  it should "not scale up if host lacks capacity" in {
    val vm = makeVm(0L, 19000.0)
      .startWorkload(WorkloadId(0L), MIPS(18000.0), SimTime.Zero, MI(10000.0))
    // utilization = 18000/19000 ≈ 0.95 > 0.8, but host has only 1000 free MIPS
    // needs 19000*1.5 - 19000 = 9500 additional → not available
    val host   = makeHost(0L).placeVm(vm)
    val policy = VerticalScalingPolicy.gradual(0.8, 0.2, 1.5)

    policy(vm, host) shouldBe None
  }

  it should "not scale down when no workloads are running" in {
    val vm = makeVm(0L, 1000.0)
    // utilization = 0.0 < 0.2, but no running workloads
    val host   = makeHost(0L).placeVm(vm)
    val policy = VerticalScalingPolicy.gradual(0.8, 0.2, 1.5)

    policy(vm, host) shouldBe None
  }
