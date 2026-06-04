// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CarbonAwareSchedulingSpec extends AnyFlatSpec with Matchers:

  private val solarRegion  = CarbonTimeSeries.solarDominant("us-west-solar")
  private val fossilRegion = CarbonTimeSeries.fossilDominant("us-east-fossil")
  private val windRegion   = CarbonTimeSeries.windDominant("eu-north-wind")

  // ── CarbonIntensity ─────────────────────────────────────────────────────

  "CarbonIntensity" should "support arithmetic" in {
    val a = CarbonIntensity(100.0)
    val b = CarbonIntensity(50.0)
    (a + b).value shouldBe 150.0
    (a * 0.5).value shouldBe 50.0
    (b < a) shouldBe true
  }

  // ── CarbonTimeSeries ────────────────────────────────────────────────────

  "CarbonTimeSeries.solarDominant" should "have lowest intensity at midday" in {
    val midday   = solarRegion.intensityAt(SimTime(13 * 3600.0))
    val midnight = solarRegion.intensityAt(SimTime(0.0))
    midday.value should be < midnight.value
  }

  "CarbonTimeSeries.fossilDominant" should "have consistently high intensity" in {
    val avg = fossilRegion.averageIntensity
    avg.value should be > 300.0
  }

  "CarbonTimeSeries.windDominant" should "have lower intensity at night" in {
    val night     = windRegion.intensityAt(SimTime(2 * 3600.0))
    val afternoon = windRegion.intensityAt(SimTime(14 * 3600.0))
    night.value should be < afternoon.value
  }

  "CarbonTimeSeries.intensityAt" should "interpolate between data points" in {
    val ts = CarbonTimeSeries(
      "test",
      Vector(
        (SimTime(0.0), CarbonIntensity(100.0)),
        (SimTime(100.0), CarbonIntensity(200.0))
      )
    )
    ts.intensityAt(SimTime(50.0)).value shouldBe 150.0 +- 0.1
  }

  it should "return zero for empty series" in {
    CarbonTimeSeries("empty", Vector.empty).intensityAt(SimTime(10.0)).value shouldBe 0.0
  }

  "CarbonTimeSeries.greenestWindow" should "find lowest-carbon window" in {
    val result = solarRegion.greenestWindow(SimTime(3 * 3600.0))
    result shouldBe defined
    // Greenest window should be around midday for solar
    val startHour = result.get._1.value / 3600.0
    startHour should be >= 9.0
    startHour should be <= 15.0
  }

  "CarbonTimeSeries.averageIntensity" should "compute mean" in {
    val ts = CarbonTimeSeries(
      "test",
      Vector(
        (SimTime(0.0), CarbonIntensity(100.0)),
        (SimTime(1.0), CarbonIntensity(200.0)),
        (SimTime(2.0), CarbonIntensity(300.0))
      )
    )
    ts.averageIntensity.value shouldBe 200.0
  }

  // ── CarbonAwarePolicy ──────────────────────────────────────────────────

  "CarbonAwarePolicy.lowestCarbonRegion" should "pick greenest region" in {
    val regions = Vector(solarRegion, fossilRegion, windRegion)
    val midday  = SimTime(13 * 3600.0)
    val best    = CarbonAwarePolicy.lowestCarbonRegion(regions, midday)
    best shouldBe defined
    // Solar region should be greenest at midday
    best.get.regionName shouldBe "us-west-solar"
  }

  it should "return None for empty list" in {
    CarbonAwarePolicy.lowestCarbonRegion(Vector.empty, SimTime(0.0)) shouldBe None
  }

  "CarbonAwarePolicy.carbonCost" should "compute gCO2 from power and intensity" in {
    // 100W for 1 hour at 500 gCO2/kWh = 0.1 kWh * 500 = 50 gCO2
    val grams = CarbonAwarePolicy.carbonCost(Watts(100.0), SimTime(3600.0), CarbonIntensity(500.0))
    grams shouldBe 50.0 +- 0.1
  }

  "CarbonAwarePolicy.deferToGreen" should "defer when intensity is above threshold" in {
    CarbonAwarePolicy.deferToGreen(CarbonIntensity(400.0), CarbonIntensity(200.0)) shouldBe true
    CarbonAwarePolicy.deferToGreen(CarbonIntensity(100.0), CarbonIntensity(200.0)) shouldBe false
  }

  "CarbonAwarePolicy.carbonSavings" should "compute percentage reduction" in {
    CarbonAwarePolicy.carbonSavings(100.0, 70.0) shouldBe 30.0 +- 0.1
    CarbonAwarePolicy.carbonSavings(100.0, 100.0) shouldBe 0.0
    CarbonAwarePolicy.carbonSavings(0.0, 50.0) shouldBe 0.0
  }

  // ── CarbonFootprint ────────────────────────────────────────────────────

  "CarbonFootprint" should "sum operational and embodied" in {
    val fp = CarbonFootprint(operationalGrams = 100.0, embodiedGrams = 50.0)
    fp.totalGrams shouldBe 150.0
    fp.operationalPct shouldBe 66.67 +- 0.1
  }

  "CarbonFootprint.estimateEmbodied" should "amortize over lifetime" in {
    // 1000 kg manufacturing, 4 year lifetime, 10% usage for 1 hour
    val grams = CarbonFootprint.estimateEmbodied(1000.0, 4.0, 0.1, 1.0)
    grams should be > 0.0
    grams should be < 50.0 // Should be small for 1 hour
  }

  // ── bestStartTime ──────────────────────────────────────────────────────

  "CarbonAwarePolicy.bestStartTime" should "pick low-carbon start time" in {
    val best = CarbonAwarePolicy.bestStartTime(
      solarRegion,
      windowStart = SimTime(0.0),
      windowEnd = SimTime(24 * 3600.0),
      workloadDuration = SimTime(3600.0)
    )
    val bestHour = best.value / 3600.0
    // Solar region is greenest around midday
    bestHour should be >= 10.0
    bestHour should be <= 16.0
  }
