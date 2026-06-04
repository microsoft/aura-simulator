// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UnitsSpec extends AnyFlatSpec with Matchers:

  // ─── SimTime ──────────────────────────────────────────────────────────

  "SimTime" should "support arithmetic operations" in {
    val t1 = SimTime(10.0)
    val t2 = SimTime(3.0)

    (t1 + t2).value shouldBe 13.0
    (t1 - t2).value shouldBe 7.0
    (t1 * 2.0).value shouldBe 20.0
    (t1 / 2.0).value shouldBe 5.0
  }

  it should "support comparison operations" in {
    val t1 = SimTime(10.0)
    val t2 = SimTime(20.0)

    (t1 < t2) shouldBe true
    (t2 > t1) shouldBe true
    (t1 <= t1) shouldBe true
    (t1 >= t1) shouldBe true
  }

  it should "provide Zero and MaxValue constants" in {
    SimTime.Zero.value shouldBe 0.0
    SimTime.MaxValue.value shouldBe Double.MaxValue
  }

  it should "support max and min" in {
    val t1 = SimTime(5.0)
    val t2 = SimTime(10.0)

    t1.max(t2).value shouldBe 10.0
    t1.min(t2).value shouldBe 5.0
  }

  it should "be orderable" in {
    val times = Vector(SimTime(5.0), SimTime(1.0), SimTime(3.0))
    times.sorted.map(_.value) shouldBe Vector(1.0, 3.0, 5.0)
  }

  // ─── MIPS ─────────────────────────────────────────────────────────────

  "MIPS" should "support arithmetic" in {
    val m1 = MIPS(1000.0)
    val m2 = MIPS(500.0)

    (m1 + m2).value shouldBe 1500.0
    (m1 - m2).value shouldBe 500.0
    (m1 * 2.0).value shouldBe 2000.0
  }

  it should "compare correctly" in {
    MIPS(1000.0) > MIPS(500.0) shouldBe true
    MIPS(500.0) < MIPS(1000.0) shouldBe true
  }

  // ─── MI ───────────────────────────────────────────────────────────────

  "MI" should "calculate execution time given MIPS" in {
    val mi   = MI(10000.0)
    val mips = MIPS(1000.0)

    mi.executionTime(mips).value shouldBe 10.0 +- 0.001
  }

  it should "return MaxValue for zero MIPS" in {
    MI(100.0).executionTime(MIPS.Zero).value shouldBe SimTime.MaxValue.value
  }

  // ─── MegaBytes ────────────────────────────────────────────────────────

  "MegaBytes" should "support arithmetic" in {
    val mb1 = MegaBytes(1024.0)
    val mb2 = MegaBytes(512.0)

    (mb1 + mb2).value shouldBe 1536.0
    (mb1 - mb2).value shouldBe 512.0
  }

  // ─── Watts ────────────────────────────────────────────────────────────

  "Watts" should "compute energy in watt-seconds" in {
    val w       = Watts(100.0)
    val seconds = 3600.0

    (w * seconds) shouldBe 360000.0
  }

  // ─── PEs ──────────────────────────────────────────────────────────────

  "PEs" should "support integer arithmetic" in {
    val p1 = PEs(4)
    val p2 = PEs(2)

    (p1 + p2).value shouldBe 6
    (p1 - p2).value shouldBe 2
    (p1 > p2) shouldBe true
  }

  // ─── Negative value rejection ─────────────────────────────────────────

  "SimTime" should "reject negative values" in {
    an[IllegalArgumentException] should be thrownBy SimTime(-1.0)
  }

  "MIPS" should "reject negative values" in {
    an[IllegalArgumentException] should be thrownBy MIPS(-1.0)
  }

  "MI" should "reject negative values" in {
    an[IllegalArgumentException] should be thrownBy MI(-1.0)
  }

  "MegaBytes" should "reject negative values" in {
    an[IllegalArgumentException] should be thrownBy MegaBytes(-1.0)
  }

  "Mbps" should "reject negative values" in {
    an[IllegalArgumentException] should be thrownBy Mbps(-1.0)
  }

  "Watts" should "reject negative values" in {
    an[IllegalArgumentException] should be thrownBy Watts(-1.0)
  }

  "WattHours" should "reject negative values" in {
    an[IllegalArgumentException] should be thrownBy WattHours(-1.0)
  }

  "GBSeconds" should "reject negative values" in {
    an[IllegalArgumentException] should be thrownBy GBSeconds(-1.0)
  }

  "Cost" should "reject negative values" in {
    an[IllegalArgumentException] should be thrownBy Cost(-1.0)
  }

  "PEs" should "reject negative values" in {
    an[IllegalArgumentException] should be thrownBy PEs(-1)
  }

  // ─── Utilization ──────────────────────────────────────────────────────

  "Utilization" should "clamp values to [0, 1]" in {
    Utilization(1.5).value shouldBe 1.0
    Utilization(-0.5).value shouldBe 0.0
    Utilization(0.5).value shouldBe 0.5
  }

  it should "support arithmetic" in {
    val u1 = Utilization(0.3)
    val u2 = Utilization(0.4)

    (u1 + u2).value shouldBe 0.7 +- 0.001
    (u1 * 2.0).value shouldBe 0.6 +- 0.001
  }

  // ─── WattHours ────────────────────────────────────────────────────────

  "WattHours" should "convert to kWh" in {
    WattHours(1500.0).toKWh shouldBe 1.5 +- 0.001
  }
