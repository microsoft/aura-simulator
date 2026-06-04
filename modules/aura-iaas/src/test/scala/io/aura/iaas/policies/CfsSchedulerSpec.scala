// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.iaas.state.VmState

class CfsSchedulerSpec extends AnyFlatSpec with Matchers:

  private def makeVm(mips: Double, pes: Int): VmState =
    VmState.create(
      VmId(0L),
      BrokerId(0L),
      ResourceSpec(PEs(pes), MIPS(mips), MegaBytes(4096.0), Mbps(1000.0), MegaBytes(10000.0)),
      SimTime.Zero
    )

  "CFS scheduler" should "allocate MIPS proportional to weight" in {
    // Two workloads: weight 1024 and 3072 (1:3 ratio)
    val vm = makeVm(4000.0, 4)
      .startWorkload(WorkloadId(0L), MIPS(4000.0), SimTime.Zero, MI(40000.0), weight = 1024)
      .startWorkload(WorkloadId(1L), MIPS(4000.0), SimTime.Zero, MI(40000.0), weight = 3072)

    // After 10 seconds: wl0 gets 1/4 * 4000 = 1000 MIPS → 10000 MI
    //                    wl1 gets 3/4 * 4000 = 3000 MIPS → 30000 MI
    val result = WorkloadScheduler.cfs(vm, SimTime(10.0), SimTime.Zero)

    result.completedWorkloads shouldBe empty
    val exec0 = result.updatedVm.runningWorkloads(WorkloadId(0L))
    val exec1 = result.updatedVm.runningWorkloads(WorkloadId(1L))
    exec0.executedMI.value shouldBe 10000.0 +- 1.0
    exec1.executedMI.value shouldBe 30000.0 +- 1.0
  }

  it should "degrade to equal sharing when all weights are equal" in {
    val vm = makeVm(2000.0, 4)
      .startWorkload(WorkloadId(0L), MIPS(2000.0), SimTime.Zero, MI(20000.0), weight = 1024)
      .startWorkload(WorkloadId(1L), MIPS(2000.0), SimTime.Zero, MI(20000.0), weight = 1024)

    // Equal weights → each gets 1000 MIPS, same as timeShared
    val result = WorkloadScheduler.cfs(vm, SimTime(10.0), SimTime.Zero)

    result.completedWorkloads shouldBe empty
    for (_, exec) <- result.updatedVm.runningWorkloads do exec.executedMI.value shouldBe 10000.0 +- 1.0
  }

  it should "handle a single workload getting all MIPS" in {
    val vm = makeVm(1000.0, 4)
      .startWorkload(WorkloadId(0L), MIPS(1000.0), SimTime.Zero, MI(10000.0), weight = 2048)

    val result = WorkloadScheduler.cfs(vm, SimTime(10.0), SimTime.Zero)

    result.completedWorkloads should contain(WorkloadId(0L))
    result.updatedVm.runningWorkloads shouldBe empty
  }

  it should "complete higher-weight workloads faster" in {
    // wl0 (weight 512) and wl1 (weight 2048), same length
    val vm = makeVm(1000.0, 4)
      .startWorkload(WorkloadId(0L), MIPS(1000.0), SimTime.Zero, MI(10000.0), weight = 512)
      .startWorkload(WorkloadId(1L), MIPS(1000.0), SimTime.Zero, MI(10000.0), weight = 2048)

    // wl1 gets 2048/2560 = 0.8 of 1000 = 800 MIPS → completes in 12.5s
    // wl0 gets 512/2560 = 0.2 of 1000 = 200 MIPS → completes in 50s
    val result = WorkloadScheduler.cfs(vm, SimTime(12.5), SimTime.Zero)

    result.completedWorkloads should contain(WorkloadId(1L))
    result.completedWorkloads should not contain WorkloadId(0L)
  }

  it should "handle zero time delta" in {
    val vm = makeVm(1000.0, 4)
      .startWorkload(WorkloadId(0L), MIPS(1000.0), SimTime.Zero, MI(10000.0), weight = 1024)

    val result = WorkloadScheduler.cfs(vm, SimTime.Zero, SimTime.Zero)
    result.completedWorkloads shouldBe empty
  }

  it should "handle no running workloads" in {
    val vm = makeVm(1000.0, 4)

    val result = WorkloadScheduler.cfs(vm, SimTime(10.0), SimTime.Zero)
    result.completedWorkloads shouldBe empty
    result.nextEventTime shouldBe None
  }
