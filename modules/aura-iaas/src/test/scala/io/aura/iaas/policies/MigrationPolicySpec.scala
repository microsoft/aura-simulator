// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.iaas.state.{HostState, VmState}

class MigrationPolicySpec extends AnyFlatSpec with Matchers:

  private val hostSpec = ResourceSpec(
    PEs(8),
    MIPS(20000.0),
    MegaBytes(32768.0),
    Mbps(10000.0),
    MegaBytes(1000000.0)
  )

  private def makeHost(id: Long): HostState =
    HostState.create(HostId(id), DatacenterId(0L), hostSpec)

  private def makeVm(id: Long, mips: Double, ram: Double): VmState =
    VmState.create(
      VmId(id),
      BrokerId(0L),
      ResourceSpec(PEs(2), MIPS(mips), MegaBytes(ram), Mbps(1000.0), MegaBytes(10000.0)),
      SimTime.Zero
    )

  // ─── OverloadDetector tests ──────────────────────────────────────

  "staticThreshold" should "detect overload above threshold" in {
    val host = makeHost(0L).placeVm(makeVm(0L, 18000.0, 4096.0))
    // utilization = 18000/20000 = 0.9
    val detector = MigrationOverloadDetector.staticThreshold(0.8)
    detector(host, Vector.empty) shouldBe true
  }

  it should "not trigger below threshold" in {
    val host = makeHost(0L).placeVm(makeVm(0L, 5000.0, 4096.0))
    // utilization = 5000/20000 = 0.25
    val detector = MigrationOverloadDetector.staticThreshold(0.8)
    detector(host, Vector.empty) shouldBe false
  }

  "MAD detector" should "not trigger with insufficient history" in {
    val host     = makeHost(0L).placeVm(makeVm(0L, 18000.0, 4096.0))
    val detector = MigrationOverloadDetector.mad(2.5)
    detector(host, Vector((SimTime(1.0), Utilization(0.9)))) shouldBe false
  }

  it should "detect overload with varied utilization history" in {
    val host = makeHost(0L).placeVm(makeVm(0L, 19000.0, 4096.0))
    // utilization = 0.95
    // Varied history: MAD will be non-zero, producing a lower threshold
    val history = Vector(
      (SimTime(1.0), Utilization(0.5)),
      (SimTime(2.0), Utilization(0.6)),
      (SimTime(3.0), Utilization(0.7)),
      (SimTime(4.0), Utilization(0.8)),
      (SimTime(5.0), Utilization(0.9)),
      (SimTime(6.0), Utilization(0.85)),
      (SimTime(7.0), Utilization(0.75)),
      (SimTime(8.0), Utilization(0.65)),
      (SimTime(9.0), Utilization(0.55)),
      (SimTime(10.0), Utilization(0.70))
    )
    val detector = MigrationOverloadDetector.mad(2.5)
    // With varied history, MAD > 0, threshold < 1.0
    // Host util = 0.95 should exceed the dynamic threshold
    detector(host, history) shouldBe true
  }

  "IQR detector" should "not trigger with insufficient history" in {
    val host     = makeHost(0L).placeVm(makeVm(0L, 18000.0, 4096.0))
    val detector = MigrationOverloadDetector.iqr(1.5)
    detector(host, Vector.empty) shouldBe false
  }

  // ─── VmSelectionPolicy tests ──────────────────────────────────────

  "minimumMigrationTime" should "select VM with smallest RAM" in {
    val vm1  = makeVm(0L, 5000.0, 2048.0)
    val vm2  = makeVm(1L, 5000.0, 8192.0)
    val host = makeHost(0L).placeVm(vm1).placeVm(vm2)

    MigrationVmSelector.minimumMigrationTime(host) shouldBe Some(VmId(0L))
  }

  it should "return None for empty host" in {
    val host = makeHost(0L)
    MigrationVmSelector.minimumMigrationTime(host) shouldBe None
  }

  "maximumUtilization" should "select VM with highest CPU usage" in {
    val vm1 = makeVm(0L, 5000.0, 4096.0)
      .startWorkload(WorkloadId(0L), MIPS(1000.0), SimTime.Zero, MI(10000.0))
    val vm2 = makeVm(1L, 5000.0, 4096.0)
      .startWorkload(WorkloadId(1L), MIPS(4000.0), SimTime.Zero, MI(10000.0))
    val host = makeHost(0L).placeVm(vm1).placeVm(vm2)

    MigrationVmSelector.maximumUtilization(host) shouldBe Some(VmId(1L))
  }

  "random (deterministic)" should "select the middle VM" in {
    val vm1  = makeVm(0L, 5000.0, 4096.0)
    val vm2  = makeVm(1L, 5000.0, 4096.0)
    val vm3  = makeVm(2L, 5000.0, 4096.0)
    val host = makeHost(0L).placeVm(vm1).placeVm(vm2).placeVm(vm3)

    val selected = MigrationVmSelector.random(host)
    selected shouldBe defined
  }
