// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

/** Carbon-aware scheduling: carbon intensity tracking, emission computation, green-region selection, and deferral
  * policies — as pure functions.
  */

/** Carbon intensity in gCO2eq per kWh. */
opaque type CarbonIntensity = Double
object CarbonIntensity:
  def apply(value: Double): CarbonIntensity = value
  val Zero: CarbonIntensity                 = 0.0
  extension (ci: CarbonIntensity)
    def value: Double                              = ci
    def +(other: CarbonIntensity): CarbonIntensity = ci + other
    def *(factor: Double): CarbonIntensity         = ci * factor
    def <(other: CarbonIntensity): Boolean         = ci < other
    def >(other: CarbonIntensity): Boolean         = ci > other
  given Ordering[CarbonIntensity] with
    def compare(x: CarbonIntensity, y: CarbonIntensity): Int = java.lang.Double.compare(x, y)

/** Hourly carbon intensity time series for a region. */
final case class CarbonTimeSeries(
    regionName: String,
    /** (hour offset in seconds, intensity) pairs, sorted by time. */
    data: Vector[(SimTime, CarbonIntensity)]
):
  /** Get carbon intensity at a specific time (interpolated). */
  def intensityAt(time: SimTime): CarbonIntensity =
    if data.isEmpty then CarbonIntensity.Zero
    else if data.size == 1 then data.head._2
    else
      val t = time.value
      // Find bracketing entries
      val before = data.lastOption
        .filter(_._1.value <= t)
        .orElse(data.findLast(_._1.value <= t))
        .getOrElse(data.head)
      val after = data.find(_._1.value > t).getOrElse(data.last)
      if before._1.value == after._1.value then before._2
      else
        // Linear interpolation
        val fraction = (t - before._1.value) / (after._1.value - before._1.value)
        CarbonIntensity(before._2.value + fraction * (after._2.value - before._2.value))

  /** Average intensity over the entire series. */
  def averageIntensity: CarbonIntensity =
    if data.isEmpty then CarbonIntensity.Zero
    else CarbonIntensity(data.map(_._2.value).sum / data.size)

  /** Find the time window with lowest average intensity. */
  def greenestWindow(windowDuration: SimTime): Option[(SimTime, CarbonIntensity)] =
    if data.size < 2 then data.headOption.map(d => (d._1, d._2))
    else
      val windows = data.indices.flatMap { i =>
        val start        = data(i)._1
        val endTime      = start.value + windowDuration.value
        val windowPoints = data.filter(d => d._1.value >= start.value && d._1.value < endTime)
        if windowPoints.nonEmpty then
          val avg = CarbonIntensity(windowPoints.map(_._2.value).sum / windowPoints.size)
          Some((start, avg))
        else None
      }
      if windows.isEmpty then None
      else Some(windows.minBy(_._2.value))

object CarbonTimeSeries:
  /** Create a typical renewable-heavy region (low at midday, high at night). */
  def solarDominant(regionName: String): CarbonTimeSeries =
    val hourly = (0 until 24).map { h =>
      val intensity =
        if h >= 10 && h <= 16 then 50.0 + (h - 13).abs * 10.0
        else 300.0 - (h - 3).abs * 5.0
      (SimTime(h * 3600.0), CarbonIntensity(math.max(30.0, intensity)))
    }.toVector
    CarbonTimeSeries(regionName, hourly)

  /** Create a fossil-heavy region (consistently high). */
  def fossilDominant(regionName: String): CarbonTimeSeries =
    val hourly = (0 until 24).map { h =>
      (SimTime(h * 3600.0), CarbonIntensity(400.0 + (h % 6) * 20.0))
    }.toVector
    CarbonTimeSeries(regionName, hourly)

  /** Create a wind-dominant region (lower at night when wind is stronger). */
  def windDominant(regionName: String): CarbonTimeSeries =
    val hourly = (0 until 24).map { h =>
      val intensity =
        if h >= 20 || h <= 6 then 80.0 + (h % 4) * 10.0
        else 200.0 + (h - 13).abs * 15.0
      (SimTime(h * 3600.0), CarbonIntensity(intensity))
    }.toVector
    CarbonTimeSeries(regionName, hourly)

/** Carbon footprint breakdown for a workload or VM. */
final case class CarbonFootprint(
    operationalGrams: Double,
    embodiedGrams: Double
):
  def totalGrams: Double = operationalGrams + embodiedGrams
  def operationalPct: Double =
    if totalGrams <= 0 then 0.0
    else operationalGrams / totalGrams * 100.0

object CarbonFootprint:
  val zero: CarbonFootprint = CarbonFootprint(0.0, 0.0)

  /** Estimate embodied carbon for a server over its lifetime.
    * @param manufacturingKgCO2
    *   total manufacturing emissions (typ. 500-1500 kg)
    * @param lifetimeYears
    *   expected server lifetime (typ. 3-5 years)
    * @param usageFraction
    *   fraction of server used by this workload
    * @param durationHours
    *   how long the workload runs
    */
  def estimateEmbodied(
      manufacturingKgCO2: Double,
      lifetimeYears: Double,
      usageFraction: Double,
      durationHours: Double
  ): Double =
    val lifetimeHours = lifetimeYears * 365.25 * 24.0
    manufacturingKgCO2 * 1000.0 * usageFraction * durationHours / lifetimeHours

/** Pure-function carbon-aware scheduling policies. */
object CarbonAwarePolicy:

  /** Select the region with lowest carbon intensity at the given time. */
  def lowestCarbonRegion(
      regions: Vector[CarbonTimeSeries],
      time: SimTime
  ): Option[CarbonTimeSeries] =
    if regions.isEmpty then None
    else Some(regions.minBy(_.intensityAt(time).value))

  /** Compute operational carbon emissions (gCO2).
    * @param watts
    *   power consumption
    * @param duration
    *   in seconds
    * @param intensity
    *   gCO2eq/kWh
    */
  def carbonCost(watts: Watts, duration: SimTime, intensity: CarbonIntensity): Double =
    val kWh = watts.value * duration.value / 3600000.0 // W * s / (1000 * 3600)
    kWh * intensity.value

  /** Should a deferrable workload wait for greener grid conditions? */
  def deferToGreen(
      currentIntensity: CarbonIntensity,
      threshold: CarbonIntensity
  ): Boolean =
    currentIntensity > threshold

  /** Integrate total emissions over a power + carbon profile. Both vectors should be time-aligned samples.
    */
  def totalEmissions(
      powerProfile: Vector[(SimTime, Watts)],
      carbonProfile: CarbonTimeSeries
  ): Double =
    if powerProfile.size < 2 then 0.0
    else
      powerProfile
        .sliding(2)
        .map { window =>
          val t1        = window(0)._1
          val t2        = window(1)._1
          val avgWatts  = (window(0)._2.value + window(1)._2.value) / 2.0
          val duration  = t2.value - t1.value
          val midTime   = SimTime((t1.value + t2.value) / 2.0)
          val intensity = carbonProfile.intensityAt(midTime)
          val kWh       = avgWatts * duration / 3600000.0
          kWh * intensity.value
        }
        .sum

  /** Compute percentage carbon savings between baseline and optimized. */
  def carbonSavings(baselineGrams: Double, optimizedGrams: Double): Double =
    if baselineGrams <= 0 then 0.0
    else (1.0 - optimizedGrams / baselineGrams) * 100.0

  /** Find the best time to run a workload within a window to minimize carbon. */
  def bestStartTime(
      carbonProfile: CarbonTimeSeries,
      windowStart: SimTime,
      windowEnd: SimTime,
      workloadDuration: SimTime
  ): SimTime =
    val latestStart = windowEnd.value - workloadDuration.value
    if latestStart <= windowStart.value then windowStart
    else
      val candidates = carbonProfile.data.filter { case (t, _) =>
        t.value >= windowStart.value && t.value <= latestStart
      }
      if candidates.isEmpty then windowStart
      else candidates.minBy(_._2.value)._1
