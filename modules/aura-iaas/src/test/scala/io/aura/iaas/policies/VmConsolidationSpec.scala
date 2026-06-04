// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.types.*
import io.aura.iaas.state.{HostState, VmState}

class VmConsolidationSpec extends AnyFlatSpec with Matchers:

  private def mkVm(vmId: Long, mips: Double, ram: Double = 1024.0): VmState =
    val spec = ResourceSpec(PEs(1), MIPS(mips), MegaBytes(ram), Mbps(100), MegaBytes(5000))
    VmState.create(VmId(vmId), BrokerId(0), spec, SimTime.Zero)

  private def makeHost(
      id: Long,
      totalMips: Double,
      usedMips: Double,
      vmCount: Int = 1,
      historySize: Int = 0,
      historyUtil: Double = 0.5
  ): HostState =
    val spec      = ResourceSpec(PEs(4), MIPS(totalMips), MegaBytes(8192), Mbps(1000), MegaBytes(50000))
    val perVmMips = if vmCount > 0 then usedMips / vmCount else 0.0
    val available = AvailableResources(
      PEs(4),
      MIPS(totalMips - usedMips),
      MegaBytes(8192 - vmCount * 1024),
      Mbps(1000),
      MegaBytes(50000)
    )
    val vms = (0 until vmCount).map { i =>
      val vm = mkVm(id * 100 + i, perVmMips)
      vm.id -> vm
    }.toMap
    val history = (0 until historySize).map(t => (SimTime(t.toDouble), Utilization(historyUtil))).toVector
    HostState(HostId(id), DatacenterId(0), spec, available, vms, active = true, history)

  // ── OverloadDetector ──────────────────────────────────────────────

  "OverloadDetector.staticThreshold" should "detect overloaded hosts" in {
    val detector   = OverloadDetector.staticThreshold(0.8)
    val overloaded = makeHost(1, 1000, 900)
    val normal     = makeHost(2, 1000, 500)
    detector(overloaded) shouldBe true
    detector(normal) shouldBe false
  }

  it should "not flag inactive hosts" in {
    val detector = OverloadDetector.staticThreshold(0.8)
    val inactive = makeHost(1, 1000, 900).copy(active = false)
    detector(inactive) shouldBe false
  }

  "OverloadDetector.mad" should "adapt threshold from history" in {
    val detector = OverloadDetector.mad(safety = 2.5, minHistory = 5)
    // Stable high utilization → MAD = 0 → threshold = 1.0 → not overloaded at 95%
    val stableHigh = makeHost(1, 1000, 950, historySize = 20, historyUtil = 0.95)
    detector(stableHigh) shouldBe false
  }

  it should "require minimum history" in {
    val detector = OverloadDetector.mad(minHistory = 12)
    val tooFew   = makeHost(1, 1000, 950, historySize = 5, historyUtil = 0.9)
    detector(tooFew) shouldBe false
  }

  "OverloadDetector.iqr" should "detect variable utilization" in {
    val detector  = OverloadDetector.iqr(safety = 1.5, minHistory = 5)
    val spec      = ResourceSpec(PEs(4), MIPS(1000), MegaBytes(8192), Mbps(1000), MegaBytes(50000))
    val available = AvailableResources(PEs(4), MIPS(50), MegaBytes(7168), Mbps(1000), MegaBytes(50000))
    val vm        = mkVm(0, 950)
    val history = Vector(0.3, 0.4, 0.5, 0.6, 0.7, 0.3, 0.8, 0.4, 0.9, 0.5, 0.3, 0.7, 0.95).zipWithIndex.map {
      case (u, i) => (SimTime(i.toDouble), Utilization(u))
    }
    val host = HostState(HostId(1), DatacenterId(0), spec, available, Map(vm.id -> vm), active = true, history)
    detector(host) shouldBe true
  }

  "OverloadDetector.localRegression" should "predict overload from trend" in {
    val detector  = OverloadDetector.localRegression(threshold = 0.9, minHistory = 6)
    val spec      = ResourceSpec(PEs(4), MIPS(1000), MegaBytes(8192), Mbps(1000), MegaBytes(50000))
    val available = AvailableResources(PEs(4), MIPS(100), MegaBytes(7168), Mbps(1000), MegaBytes(50000))
    val vm        = mkVm(0, 900)
    // Rising trend: 0.5, 0.6, 0.7, 0.8, 0.9, 1.0 → next predicted ~1.1
    val history = (0 to 5).map(i => (SimTime(i.toDouble), Utilization(math.min(1.0, 0.5 + i * 0.1)))).toVector
    val host    = HostState(HostId(1), DatacenterId(0), spec, available, Map(vm.id -> vm), active = true, history)
    detector(host) shouldBe true
  }

  // ── UnderloadDetector ─────────────────────────────────────────────

  "UnderloadDetector.staticThreshold" should "detect underloaded hosts" in {
    val detector    = UnderloadDetector.staticThreshold(0.2)
    val underloaded = makeHost(1, 1000, 100)
    val normal      = makeHost(2, 1000, 500)
    detector(underloaded) shouldBe true
    detector(normal) shouldBe false
  }

  it should "not flag hosts with no VMs" in {
    val detector  = UnderloadDetector.staticThreshold(0.2)
    val emptyHost = makeHost(1, 1000, 100, vmCount = 0)
    detector(emptyHost) shouldBe false
  }

  "UnderloadDetector.idle" should "detect near-zero utilization" in {
    val host = makeHost(1, 1000, 5)
    UnderloadDetector.idle(host) shouldBe true
    val active = makeHost(2, 1000, 500)
    UnderloadDetector.idle(active) shouldBe false
  }

  // ── VmSelectionPolicy ─────────────────────────────────────────────

  "VmSelectionPolicy.minimumMigrationTime" should "select smallest RAM VM" in {
    val spec      = ResourceSpec(PEs(4), MIPS(1000), MegaBytes(8192), Mbps(1000), MegaBytes(50000))
    val available = AvailableResources(PEs(2), MIPS(100), MegaBytes(4096), Mbps(800), MegaBytes(40000))
    val smallVm   = mkVm(0, 200, ram = 512)
    val largeVm   = mkVm(1, 700, ram = 2048)
    val host = HostState(
      HostId(1),
      DatacenterId(0),
      spec,
      available,
      Map(smallVm.id -> smallVm, largeVm.id -> largeVm),
      active = true
    )
    val selected = VmSelectionPolicy.minimumMigrationTime(host)
    selected should have size 1
    selected.head shouldBe VmId(0)
  }

  "VmSelectionPolicy.highestUtilization" should "select most CPU-intensive VM" in {
    val spec      = ResourceSpec(PEs(4), MIPS(1000), MegaBytes(8192), Mbps(1000), MegaBytes(50000))
    val available = AvailableResources(PEs(2), MIPS(100), MegaBytes(4096), Mbps(800), MegaBytes(40000))
    val lowVm     = mkVm(0, 200)
    val highVm    = mkVm(1, 700)
    val host =
      HostState(HostId(1), DatacenterId(0), spec, available, Map(lowVm.id -> lowVm, highVm.id -> highVm), active = true)
    val selected = VmSelectionPolicy.highestUtilization(host)
    selected.head shouldBe VmId(1)
  }

  "VmSelectionPolicy.randomVm" should "select a VM deterministically" in {
    val host      = makeHost(1, 1000, 500, vmCount = 3)
    val selected1 = VmSelectionPolicy.randomVm(42L)(host)
    val selected2 = VmSelectionPolicy.randomVm(42L)(host)
    selected1 should have size 1
    selected1 shouldBe selected2
  }

  "VmSelectionPolicy.minimumUtilization" should "select least CPU-intensive VM" in {
    val spec      = ResourceSpec(PEs(4), MIPS(1000), MegaBytes(8192), Mbps(1000), MegaBytes(50000))
    val available = AvailableResources(PEs(2), MIPS(100), MegaBytes(4096), Mbps(800), MegaBytes(40000))
    val lowVm     = mkVm(0, 200)
    val highVm    = mkVm(1, 700)
    val host =
      HostState(HostId(1), DatacenterId(0), spec, available, Map(lowVm.id -> lowVm, highVm.id -> highVm), active = true)
    val selected = VmSelectionPolicy.minimumUtilization(host)
    selected.head shouldBe VmId(0)
  }

  // ── ConsolidationEngine ───────────────────────────────────────────

  "ConsolidationEngine.planConsolidation" should "migrate VMs from overloaded hosts" in {
    val overloaded = makeHost(1, 1000, 950, vmCount = 2)
    val target     = makeHost(2, 1000, 300, vmCount = 1)
    val hosts      = Vector(overloaded, target)

    val plan = ConsolidationEngine.planConsolidation(
      hosts,
      OverloadDetector.staticThreshold(0.8),
      UnderloadDetector.staticThreshold(0.2),
      VmSelectionPolicy.minimumMigrationTime,
      VmAllocationPolicy.firstFit
    )

    plan.overloadedHosts should contain(HostId(1))
    plan.migrations should not be empty
    plan.migrations.head.sourceHostId shouldBe HostId(1)
    plan.migrations.head.targetHostId shouldBe HostId(2)
    plan.migrations.head.reason shouldBe ConsolidationReason.OverloadRelief
  }

  it should "evacuate underloaded hosts and mark for deactivation" in {
    val underloaded = makeHost(1, 1000, 50, vmCount = 1)
    val target      = makeHost(2, 1000, 400, vmCount = 2)
    val hosts       = Vector(underloaded, target)

    val plan = ConsolidationEngine.planConsolidation(
      hosts,
      OverloadDetector.staticThreshold(0.8),
      UnderloadDetector.staticThreshold(0.2),
      VmSelectionPolicy.minimumMigrationTime,
      VmAllocationPolicy.firstFit
    )

    plan.underloadedHosts should contain(HostId(1))
    plan.hostsToDeactivate should contain(HostId(1))
    plan.migrations should not be empty
    plan.migrations.exists(_.reason == ConsolidationReason.UnderloadEvacuation) shouldBe true
  }

  it should "produce empty plan when all hosts are balanced" in {
    val h1 = makeHost(1, 1000, 500, vmCount = 2)
    val h2 = makeHost(2, 1000, 600, vmCount = 2)

    val plan = ConsolidationEngine.planConsolidation(
      Vector(h1, h2),
      OverloadDetector.staticThreshold(0.8),
      UnderloadDetector.staticThreshold(0.2),
      VmSelectionPolicy.minimumMigrationTime,
      VmAllocationPolicy.firstFit
    )

    plan.migrations shouldBe empty
    plan.hostsToDeactivate shouldBe empty
  }

  it should "not evacuate if target hosts are full" in {
    val h1 = makeHost(1, 100, 15, vmCount = 1)
    val h2 = makeHost(2, 100, 15, vmCount = 1)

    val plan = ConsolidationEngine.planConsolidation(
      Vector(h1, h2),
      OverloadDetector.staticThreshold(0.8),
      UnderloadDetector.staticThreshold(0.2),
      VmSelectionPolicy.minimumMigrationTime,
      VmAllocationPolicy.firstFit
    )

    plan.hostsToDeactivate.size should be <= 1
  }

  // ── Utility Functions ─────────────────────────────────────────────

  "ConsolidationEngine.slaViolationRate" should "compute fraction of overloaded hosts" in {
    val h1 = makeHost(1, 1000, 950)
    val h2 = makeHost(2, 1000, 500)
    val h3 = makeHost(3, 1000, 900)
    val rate = ConsolidationEngine.slaViolationRate(
      Vector(h1, h2, h3),
      OverloadDetector.staticThreshold(0.8)
    )
    rate shouldBe (2.0 / 3.0) +- 0.01
  }

  "ConsolidationEngine.dataCenterUtilization" should "average CPU across active hosts" in {
    val h1 = makeHost(1, 1000, 600)
    val h2 = makeHost(2, 1000, 400)
    ConsolidationEngine.dataCenterUtilization(Vector(h1, h2)) shouldBe 0.5 +- 0.01
  }

  "ConsolidationEngine.idleHostCount" should "count hosts with no VMs" in {
    val busy  = makeHost(1, 1000, 500, vmCount = 2)
    val empty = makeHost(2, 1000, 0, vmCount = 0)
    ConsolidationEngine.idleHostCount(Vector(busy, empty)) shouldBe 1
  }

  "ConsolidationEngine.estimatedPowerSaved" should "scale with host count" in {
    val saved = ConsolidationEngine.estimatedPowerSaved(Set(HostId(1), HostId(2)), Vector.empty, 150.0)
    saved.value shouldBe 300.0
  }
