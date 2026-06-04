// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.iaas.state.VmState

class WorkloadSchedulerSpec extends AnyFlatSpec with Matchers:

  private def makeVm(mips: Double, pes: Int): VmState =
    VmState.create(
      VmId(0L),
      BrokerId(0L),
      ResourceSpec(PEs(pes), MIPS(mips), MegaBytes(4096.0), Mbps(1000.0), MegaBytes(10000.0)),
      SimTime.Zero
    )

  "SpaceShared scheduler" should "complete a single workload correctly" in {
    val vm = makeVm(1000.0, 4)
      .startWorkload(WorkloadId(0L), MIPS(1000.0), SimTime.Zero, MI(10000.0))

    val result = WorkloadScheduler.spaceShared(vm, SimTime(10.0), SimTime.Zero)

    result.completedWorkloads should contain(WorkloadId(0L))
    result.updatedVm.runningWorkloads shouldBe empty
  }

  it should "track partial progress" in {
    val vm = makeVm(1000.0, 4)
      .startWorkload(WorkloadId(0L), MIPS(1000.0), SimTime.Zero, MI(10000.0))

    val result = WorkloadScheduler.spaceShared(vm, SimTime(5.0), SimTime.Zero)

    result.completedWorkloads shouldBe empty
    val exec = result.updatedVm.runningWorkloads(WorkloadId(0L))
    exec.executedMI.value shouldBe 5000.0 +- 0.001
  }

  it should "handle multiple workloads independently" in {
    val vm = makeVm(1000.0, 4)
      .startWorkload(WorkloadId(0L), MIPS(1000.0), SimTime.Zero, MI(5000.0))
      .startWorkload(WorkloadId(1L), MIPS(1000.0), SimTime.Zero, MI(10000.0))

    val result = WorkloadScheduler.spaceShared(vm, SimTime(5.0), SimTime.Zero)

    result.completedWorkloads should contain(WorkloadId(0L))
    result.completedWorkloads should not contain WorkloadId(1L)
    result.updatedVm.runningWorkloads should contain key WorkloadId(1L)
  }

  "TimeShared scheduler" should "divide MIPS equally among workloads" in {
    val vm = makeVm(1000.0, 4)
      .startWorkload(WorkloadId(0L), MIPS(1000.0), SimTime.Zero, MI(10000.0))
      .startWorkload(WorkloadId(1L), MIPS(1000.0), SimTime.Zero, MI(10000.0))

    // With time-sharing, each workload gets 500 MIPS (1000 / 2)
    val result = WorkloadScheduler.timeShared(vm, SimTime(10.0), SimTime.Zero)

    // After 10 seconds at 500 MIPS each: 5000 MI done out of 10000
    result.completedWorkloads shouldBe empty
    for (_, exec) <- result.updatedVm.runningWorkloads do exec.executedMI.value shouldBe 5000.0 +- 1.0
  }

  it should "complete workloads when enough time has passed" in {
    val vm = makeVm(1000.0, 4)
      .startWorkload(WorkloadId(0L), MIPS(1000.0), SimTime.Zero, MI(10000.0))
      .startWorkload(WorkloadId(1L), MIPS(1000.0), SimTime.Zero, MI(10000.0))

    // 20 seconds at 500 MIPS each = 10000 MI each
    val result = WorkloadScheduler.timeShared(vm, SimTime(20.0), SimTime.Zero)

    result.completedWorkloads should have size 2
  }

  "Both schedulers" should "handle no running workloads" in {
    val vm = makeVm(1000.0, 4)

    val spaceResult = WorkloadScheduler.spaceShared(vm, SimTime(10.0), SimTime.Zero)
    spaceResult.completedWorkloads shouldBe empty

    val timeResult = WorkloadScheduler.timeShared(vm, SimTime(10.0), SimTime.Zero)
    timeResult.completedWorkloads shouldBe empty
  }

  it should "handle zero time delta" in {
    val vm = makeVm(1000.0, 4)
      .startWorkload(WorkloadId(0L), MIPS(1000.0), SimTime.Zero, MI(10000.0))

    val spaceResult = WorkloadScheduler.spaceShared(vm, SimTime.Zero, SimTime.Zero)
    spaceResult.completedWorkloads shouldBe empty

    val timeResult = WorkloadScheduler.timeShared(vm, SimTime.Zero, SimTime.Zero)
    timeResult.completedWorkloads shouldBe empty
  }
