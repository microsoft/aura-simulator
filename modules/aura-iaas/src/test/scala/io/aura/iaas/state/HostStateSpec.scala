// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.state

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*

class HostStateSpec extends AnyFlatSpec with Matchers:

  val hostSpec = ResourceSpec(
    PEs(8),
    MIPS(20000.0),
    MegaBytes(32768.0),
    Mbps(10000.0),
    MegaBytes(1000000.0)
  )

  val vmSpec = ResourceSpec(
    PEs(2),
    MIPS(5000.0),
    MegaBytes(4096.0),
    Mbps(1000.0),
    MegaBytes(100000.0)
  )

  "HostState" should "be created with full available resources" in {
    val host = HostState.create(HostId(0L), DatacenterId(0L), hostSpec)
    host.available.pes.value shouldBe 8
    host.available.mips.value shouldBe 20000.0
    host.available.ram.value shouldBe 32768.0
    host.vms shouldBe empty
    host.active shouldBe true
  }

  it should "place a VM and reduce available resources" in {
    val host    = HostState.create(HostId(0L), DatacenterId(0L), hostSpec)
    val vm      = VmState.create(VmId(0L), BrokerId(0L), vmSpec, SimTime.Zero)
    val updated = host.placeVm(vm)

    updated.available.pes.value shouldBe 6
    updated.available.mips.value shouldBe 15000.0
    updated.available.ram.value shouldBe 28672.0
    updated.vms should have size 1
  }

  it should "remove a VM and restore resources" in {
    val host    = HostState.create(HostId(0L), DatacenterId(0L), hostSpec)
    val vm      = VmState.create(VmId(0L), BrokerId(0L), vmSpec, SimTime.Zero)
    val withVm  = host.placeVm(vm)
    val removed = withVm.removeVm(VmId(0L))

    removed.available.pes.value shouldBe 8
    removed.available.mips.value shouldBe 20000.0 +- 0.001
    removed.vms shouldBe empty
  }

  it should "report whether it can place a VM" in {
    val host = HostState.create(HostId(0L), DatacenterId(0L), hostSpec)
    host.canPlaceVm(vmSpec) shouldBe true

    val largeVmSpec = ResourceSpec(PEs(16), MIPS(50000.0), MegaBytes(65536.0), Mbps(20000.0), MegaBytes(2000000.0))
    host.canPlaceVm(largeVmSpec) shouldBe false
  }

  it should "track CPU utilization" in {
    val host = HostState.create(HostId(0L), DatacenterId(0L), hostSpec)
    host.cpuUtilization.value shouldBe 0.0

    val vm     = VmState.create(VmId(0L), BrokerId(0L), vmSpec, SimTime.Zero)
    val withVm = host.placeVm(vm)
    withVm.cpuUtilization.value shouldBe 0.25 +- 0.001 // 5000 / 20000
  }

  it should "handle multiple VMs" in {
    val host = HostState.create(HostId(0L), DatacenterId(0L), hostSpec)
    val vm1  = VmState.create(VmId(0L), BrokerId(0L), vmSpec, SimTime.Zero)
    val vm2  = VmState.create(VmId(1L), BrokerId(0L), vmSpec, SimTime.Zero)

    val updated = host.placeVm(vm1).placeVm(vm2)
    updated.vms should have size 2
    updated.available.pes.value shouldBe 4
    updated.cpuUtilization.value shouldBe 0.5 +- 0.001
  }

  it should "not crash when removing a non-existent VM" in {
    val host   = HostState.create(HostId(0L), DatacenterId(0L), hostSpec)
    val result = host.removeVm(VmId(999L))
    result shouldBe host
  }

  "VmState" should "track workload execution" in {
    val vm = VmState.create(VmId(0L), BrokerId(0L), vmSpec, SimTime.Zero)
    vm.runningWorkloads shouldBe empty

    val withWorkload = vm.startWorkload(WorkloadId(0L), MIPS(1000.0), SimTime.Zero, MI(10000.0))
    withWorkload.runningWorkloads should have size 1
    withWorkload.totalAllocatedMips.value shouldBe 1000.0

    val finished = withWorkload.finishWorkload(WorkloadId(0L))
    finished.runningWorkloads shouldBe empty
    finished.finishedWorkloads should contain(WorkloadId(0L))
  }

  it should "report CPU utilization based on workloads" in {
    val vm = VmState.create(VmId(0L), BrokerId(0L), vmSpec, SimTime.Zero)
    vm.cpuUtilization.value shouldBe 0.0

    val withWorkload = vm.startWorkload(WorkloadId(0L), MIPS(2500.0), SimTime.Zero, MI(10000.0))
    withWorkload.cpuUtilization.value shouldBe 0.5 +- 0.001 // 2500 / 5000
  }
