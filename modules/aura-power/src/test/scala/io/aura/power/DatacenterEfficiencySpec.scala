// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.power

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.types.{WattHours, Watts}

class DatacenterEfficiencySpec extends AnyFlatSpec with Matchers:

  // ─── PUE Model ───────────────────────────────────────────────────────

  "PueModel.constant" should "multiply IT power by PUE factor" in {
    val pue = PueModel.constant(1.5)
    pue(Watts(100.0)).value shouldBe 150.0
  }

  it should "return IT power unchanged for PUE 1.0" in {
    val pue = PueModel.constant(1.0)
    pue(Watts(200.0)).value shouldBe 200.0
  }

  it should "reject PUE less than 1.0" in {
    an[IllegalArgumentException] should be thrownBy PueModel.constant(0.9)
  }

  "PueModel.temperatureBased" should "increase PUE with temperature" in {
    val model   = PueModel.temperatureBased(basePue = 1.2, referenceTemp = 20.0, tempCoefficient = 0.01)
    val coolDay = model(15.0) // 5 degrees below ref
    val hotDay  = model(35.0) // 15 degrees above ref
    coolDay(Watts(100.0)).value should be < hotDay(Watts(100.0)).value
  }

  it should "not go below minimum PUE" in {
    val model = PueModel.temperatureBased(basePue = 1.2, referenceTemp = 20.0, tempCoefficient = 0.01, minPue = 1.1)
    val veryColDay = model(-20.0)
    // basePue + (-40)*0.01 = 0.8, clamped to minPue 1.1
    veryColDay(Watts(100.0)).value shouldBe 110.0 +- 0.001
  }

  "PueModel presets" should "have expected ordering" in {
    val itPower = Watts(100.0)
    PueModel.ideal(itPower).value shouldBe 100.0
    PueModel.hyperscale(itPower).value shouldBe 110.0 +- 0.001
    PueModel.colocation(itPower).value shouldBe 140.0
    PueModel.enterprise(itPower).value shouldBe 160.0
    PueModel.legacy(itPower).value shouldBe 200.0
  }

  // ─── Cooling Model ──────────────────────────────────────────────────

  "CoolingModel.linear" should "increase cooling with temperature" in {
    val model = CoolingModel.linear(baseFraction = 0.1, referenceTemp = 20.0, tempFactor = 0.005)
    val cool  = model(Watts(100.0), 10.0)
    val hot   = model(Watts(100.0), 40.0)
    cool.value should be < hot.value
  }

  it should "not produce negative cooling power" in {
    val model   = CoolingModel.linear(baseFraction = 0.1, referenceTemp = 20.0, tempFactor = 0.005)
    val veryLow = model(Watts(100.0), -100.0)
    veryLow.value shouldBe 0.0
  }

  "CoolingModel.freeCooling" should "use low fraction below threshold" in {
    val model =
      CoolingModel.freeCooling(freeCoolingThreshold = 18.0, freeCoolingFraction = 0.02, mechanicalFraction = 0.3)
    val coolWeather = model(Watts(100.0), 10.0)
    val hotWeather  = model(Watts(100.0), 30.0)
    coolWeather.value shouldBe 2.0 // 2% of 100W
    hotWeather.value shouldBe 30.0 // 30% of 100W
  }

  "CoolingModel.none" should "return zero" in {
    CoolingModel.none(Watts(500.0), 40.0).value shouldBe 0.0
  }

  // ─── Carbon Intensity ────────────────────────────────────────────────

  "CarbonIntensity" should "compute emissions from energy" in {
    val ci     = CarbonIntensity(400.0) // 400 gCO2/kWh
    val energy = WattHours(1000.0)      // 1 kWh
    ci.emissionsGrams(energy) shouldBe 400.0
    ci.emissionsKg(energy) shouldBe 0.4
  }

  it should "return zero emissions for zero intensity" in {
    CarbonIntensity.zero.emissionsGrams(WattHours(1000.0)) shouldBe 0.0
  }

  it should "reject negative intensity" in {
    an[IllegalArgumentException] should be thrownBy CarbonIntensity(-10.0)
  }

  "CarbonIntensity presets" should "have Nordic lower than India" in {
    CarbonIntensity.euNorth.gCO2PerKWh should be < CarbonIntensity.india.gCO2PerKWh
  }

  // ─── EfficiencyMetrics ───────────────────────────────────────────────

  "EfficiencyMetrics.compute" should "calculate correct facility totals" in {
    val metrics = EfficiencyMetrics.compute(
      itEnergy = WattHours(1000.0),
      pueModel = PueModel.constant(1.5),
      carbonIntensity = CarbonIntensity(400.0)
    )
    metrics.pue shouldBe 1.5
    metrics.totalFacilityEnergyWh.value shouldBe 1500.0
    metrics.coolingEnergyWh.value shouldBe 500.0
    // 1.5 kWh * 400 gCO2/kWh = 600g = 0.6 kg
    metrics.carbonEmissionsKg shouldBe 0.6 +- 0.001
  }

  it should "produce zero overhead with ideal PUE" in {
    val metrics = EfficiencyMetrics.compute(
      itEnergy = WattHours(1000.0),
      pueModel = PueModel.ideal,
      carbonIntensity = CarbonIntensity.zero
    )
    metrics.pue shouldBe 1.0
    metrics.coolingEnergyWh.value shouldBe 0.0
    metrics.carbonEmissionsKg shouldBe 0.0
  }

  "EfficiencyMetrics.cue" should "compute carbon usage effectiveness" in {
    val metrics = EfficiencyMetrics.compute(
      itEnergy = WattHours(1000.0), // 1 kWh IT
      pueModel = PueModel.constant(1.5),
      carbonIntensity = CarbonIntensity(400.0)
    )
    // CUE = total CO2 grams / IT kWh = 600 / 1.0 = 600
    metrics.cue shouldBe 600.0 +- 1.0
  }

  "EfficiencyMetrics.formatReport" should "produce readable output" in {
    val metrics = EfficiencyMetrics.compute(
      itEnergy = WattHours(500.0),
      pueModel = PueModel.hyperscale,
      carbonIntensity = CarbonIntensity.usWest
    )
    val report = EfficiencyMetrics.formatReport(metrics)
    report should include("PUE")
    report should include("Carbon")
    report should include("CUE")
  }
