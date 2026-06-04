// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.power

import io.aura.core.types.{WattHours, Watts}

/** Power Usage Effectiveness (PUE) model.
  *
  * PUE = Total Facility Power / IT Equipment Power. A PUE of 1.0 means all power goes to IT equipment (theoretical
  * ideal). Typical values range from 1.1 (hyperscale) to 2.0+ (legacy).
  */
type PueModel = Watts => Watts

object PueModel:

  /** Constant PUE factor. Most common model for data centers with stable cooling infrastructure.
    *
    * @param pue
    *   PUE value (>= 1.0). E.g. 1.2 means 20% overhead.
    */
  def constant(pue: Double): PueModel =
    require(pue >= 1.0, s"PUE must be >= 1.0, got $pue")
    (itPower: Watts) => Watts(itPower.value * pue)

  /** Temperature-dependent PUE model. Cooling overhead increases with outside temperature. Models free cooling at low
    * temperatures and mechanical cooling at high.
    *
    * @param basePue
    *   PUE at reference temperature (e.g. 1.1 at 20C)
    * @param referenceTemp
    *   Reference outside temperature in Celsius
    * @param tempCoefficient
    *   PUE increase per degree above reference (e.g. 0.01)
    * @param minPue
    *   Minimum PUE (free cooling floor)
    */
  def temperatureBased(
      basePue: Double = 1.1,
      referenceTemp: Double = 20.0,
      tempCoefficient: Double = 0.01,
      minPue: Double = 1.05
  ): Double => PueModel =
    (outsideTemp: Double) =>
      val deltaPue     = (outsideTemp - referenceTemp) * tempCoefficient
      val effectivePue = math.max(minPue, basePue + deltaPue)
      constant(effectivePue)

  /** Ideal PUE = 1.0 (all power goes to IT equipment). */
  val ideal: PueModel = (itPower: Watts) => itPower

  /** Typical PUE values for different data center types. */
  val hyperscale: PueModel = constant(1.1)
  val colocation: PueModel = constant(1.4)
  val enterprise: PueModel = constant(1.6)
  val legacy: PueModel     = constant(2.0)

/** Cooling model as a pure function. Given IT power load and outside temperature, returns cooling power required.
  */
type CoolingModel = (Watts, Double) => Watts

object CoolingModel:

  /** Linear cooling model: cooling power is a fraction of IT power, scaling with temperature.
    *
    * @param baseFraction
    *   Cooling power fraction at reference temp (e.g. 0.1 = 10%)
    * @param referenceTemp
    *   Reference temperature in Celsius
    * @param tempFactor
    *   Additional fraction per degree above reference
    */
  def linear(
      baseFraction: Double = 0.1,
      referenceTemp: Double = 20.0,
      tempFactor: Double = 0.005
  ): CoolingModel =
    (itPower: Watts, outsideTemp: Double) =>
      val fraction = math.max(0.0, baseFraction + (outsideTemp - referenceTemp) * tempFactor)
      Watts(itPower.value * fraction)

  /** Free cooling model: below threshold temperature, minimal power needed. Above threshold, mechanical cooling kicks
    * in.
    *
    * @param freeCoolingThreshold
    *   Temperature below which free cooling works
    * @param freeCoolingFraction
    *   Power fraction when free cooling (very low)
    * @param mechanicalFraction
    *   Power fraction for mechanical cooling
    */
  def freeCooling(
      freeCoolingThreshold: Double = 18.0,
      freeCoolingFraction: Double = 0.02,
      mechanicalFraction: Double = 0.3
  ): CoolingModel =
    (itPower: Watts, outsideTemp: Double) =>
      val fraction =
        if outsideTemp <= freeCoolingThreshold then freeCoolingFraction
        else mechanicalFraction
      Watts(itPower.value * fraction)

  /** No cooling overhead (theoretical). */
  val none: CoolingModel = (_: Watts, _: Double) => Watts.Zero

/** Carbon intensity per region (gCO2 per kWh).
  *
  * Carbon intensity measures how much CO2 is emitted per unit of electricity consumed. It depends on the energy mix of
  * the region (renewables vs fossil fuels).
  */
final case class CarbonIntensity(gCO2PerKWh: Double):
  require(gCO2PerKWh >= 0.0, s"Carbon intensity must be >= 0, got $gCO2PerKWh")

  /** Calculate CO2 emissions in grams for given energy consumption. */
  def emissionsGrams(energy: WattHours): Double =
    gCO2PerKWh * energy.toKWh

  /** Calculate CO2 emissions in kilograms. */
  def emissionsKg(energy: WattHours): Double =
    emissionsGrams(energy) / 1000.0

object CarbonIntensity:
  /** Regional carbon intensity presets (approximate 2024 values). */
  val usWest: CarbonIntensity      = CarbonIntensity(200.0) // California, lots of solar/wind
  val usEast: CarbonIntensity      = CarbonIntensity(380.0) // Virginia, mix
  val euWest: CarbonIntensity      = CarbonIntensity(250.0) // Ireland/Netherlands, wind heavy
  val euNorth: CarbonIntensity     = CarbonIntensity(30.0)  // Sweden/Norway, hydro/nuclear
  val asiaPacific: CarbonIntensity = CarbonIntensity(500.0) // Singapore, natural gas heavy
  val india: CarbonIntensity       = CarbonIntensity(700.0) // Coal heavy
  val brazil: CarbonIntensity      = CarbonIntensity(80.0)  // Hydro heavy
  val zero: CarbonIntensity        = CarbonIntensity(0.0)   // 100% renewable

/** Aggregated data center efficiency metrics. */
final case class EfficiencyMetrics(
    itEnergyWh: WattHours,
    totalFacilityEnergyWh: WattHours,
    coolingEnergyWh: WattHours,
    pue: Double,
    carbonEmissionsKg: Double,
    carbonIntensity: CarbonIntensity
):
  /** Carbon Usage Effectiveness (CUE) = total CO2 / IT energy in kWh. */
  def cue: Double =
    if itEnergyWh.value > 0 then carbonEmissionsKg * 1000.0 / itEnergyWh.toKWh
    else 0.0

/** Compute full efficiency metrics from IT energy, PUE model, and carbon intensity. */
object EfficiencyMetrics:
  def compute(
      itEnergy: WattHours,
      pueModel: PueModel,
      carbonIntensity: CarbonIntensity
  ): EfficiencyMetrics =
    // Approximate: use average IT power of 1W to compute PUE ratio
    // then apply to total energy
    val sampleIt    = Watts(1.0)
    val sampleTotal = pueModel(sampleIt)
    val pueValue    = if sampleIt.value > 0 then sampleTotal.value / sampleIt.value else 1.0

    val totalEnergy   = WattHours(itEnergy.value * pueValue)
    val coolingEnergy = WattHours(totalEnergy.value - itEnergy.value)
    val emissions     = carbonIntensity.emissionsKg(totalEnergy)

    EfficiencyMetrics(
      itEnergyWh = itEnergy,
      totalFacilityEnergyWh = totalEnergy,
      coolingEnergyWh = coolingEnergy,
      pue = pueValue,
      carbonEmissionsKg = emissions,
      carbonIntensity = carbonIntensity
    )

  /** Format a human-readable efficiency report. */
  def formatReport(metrics: EfficiencyMetrics): String =
    val lines = Vector(
      f"IT Energy:            ${metrics.itEnergyWh.value}%.4f Wh (${metrics.itEnergyWh.toKWh}%.6f kWh)",
      f"Total Facility Energy: ${metrics.totalFacilityEnergyWh.value}%.4f Wh (${metrics.totalFacilityEnergyWh.toKWh}%.6f kWh)",
      f"Cooling Overhead:     ${metrics.coolingEnergyWh.value}%.4f Wh",
      f"PUE:                  ${metrics.pue}%.3f",
      f"Carbon Emissions:     ${metrics.carbonEmissionsKg}%.6f kg CO2",
      f"Carbon Intensity:     ${metrics.carbonIntensity.gCO2PerKWh}%.1f gCO2/kWh",
      f"CUE:                  ${metrics.cue}%.3f gCO2/kWh"
    )
    lines.mkString("\n")
