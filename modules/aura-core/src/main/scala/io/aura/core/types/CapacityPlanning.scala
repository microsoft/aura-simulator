// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

/** Capacity planning, right-sizing recommendations, and utilization forecasting — modeled as pure functions over
  * immutable data.
  */

/** Aggregated utilization statistics for a resource over a time window. */
final case class ResourceUtilizationStats(
    avg: Double,
    p50: Double,
    p95: Double,
    p99: Double,
    max: Double,
    min: Double,
    sampleCount: Int
)

object ResourceUtilizationStats:
  def fromSamples(samples: Vector[Double]): ResourceUtilizationStats =
    if samples.isEmpty then ResourceUtilizationStats(0, 0, 0, 0, 0, 0, 0)
    else
      val sorted = samples.sorted
      val n      = sorted.size
      ResourceUtilizationStats(
        avg = sorted.sum / n,
        p50 = percentile(sorted, 0.50),
        p95 = percentile(sorted, 0.95),
        p99 = percentile(sorted, 0.99),
        max = sorted.last,
        min = sorted.head,
        sampleCount = n
      )

  private def percentile(sorted: Vector[Double], p: Double): Double =
    val idx = math.ceil(p * sorted.size).toInt - 1
    sorted(math.max(0, math.min(idx, sorted.size - 1)))

/** Right-sizing recommendation for a VM. */
enum RightSizingRecommendation:
  /** Reduce resources — VM is over-provisioned. */
  case Downsize(currentSpec: ResourceSpec, recommendedSpec: ResourceSpec, estimatedSavingsPct: Double)

  /** Increase resources — VM is under-provisioned. */
  case Upsize(currentSpec: ResourceSpec, recommendedSpec: ResourceSpec)

  /** VM is idle and can be terminated. */
  case Terminate(currentSpec: ResourceSpec)

  /** Current sizing is appropriate. */
  case NoChange(currentSpec: ResourceSpec)

/** Linear forecast of future utilization. */
final case class CapacityForecast(
    slope: Double,
    intercept: Double,
    currentUtilization: Double,
    forecastHorizon: SimTime,
    predictedUtilization: Double,
    timeToExhaustion: Option[SimTime]
):
  /** Predict utilization at a given future time offset. */
  def predictAt(timeOffset: SimTime): Double =
    math.max(0.0, math.min(1.0, intercept + slope * timeOffset.value))

/** Pure-function capacity planning engine. */
object CapacityPlanningEngine:

  /** Analyze utilization from a time-series of (time, utilization) samples. */
  def analyzeUtilization(history: Vector[(SimTime, Utilization)]): ResourceUtilizationStats =
    ResourceUtilizationStats.fromSamples(history.map(_._2.value))

  /** Right-size a VM based on its utilization stats.
    * @param headroom
    *   fraction of headroom to keep above peak (e.g. 0.2 = 20%)
    * @param idleThreshold
    *   utilization below which VM is considered idle
    */
  def rightSize(
      currentSpec: ResourceSpec,
      stats: ResourceUtilizationStats,
      headroom: Double = 0.2,
      idleThreshold: Double = 0.05
  ): RightSizingRecommendation =
    if stats.sampleCount == 0 || stats.max < idleThreshold then RightSizingRecommendation.Terminate(currentSpec)
    else
      val targetUtilization = stats.p95 * (1.0 + headroom)
      if targetUtilization >= 0.9 then
        // Under-provisioned: p95 usage is near capacity
        val scaleFactor = 1.0 / stats.p95 * (1.0 + headroom)
        RightSizingRecommendation.Upsize(currentSpec, scaleSpec(currentSpec, scaleFactor))
      else if targetUtilization < 0.4 then
        // Over-provisioned: can downsize
        val scaleFactor = targetUtilization
        val savings     = (1.0 - scaleFactor) * 100.0
        RightSizingRecommendation.Downsize(currentSpec, scaleSpec(currentSpec, scaleFactor), savings)
      else RightSizingRecommendation.NoChange(currentSpec)

  private def scaleSpec(spec: ResourceSpec, factor: Double): ResourceSpec =
    ResourceSpec(
      pes = PEs(math.max(1, (spec.pes.value * factor).toInt)),
      mips = MIPS(spec.mips.value * factor),
      ram = MegaBytes(spec.ram.value * factor),
      bw = Mbps(spec.bw.value * factor),
      storage = spec.storage // Storage doesn't scale with compute
    )

  /** Forecast future utilization using linear regression on historical data. */
  def forecastUtilization(
      history: Vector[(SimTime, Utilization)],
      horizon: SimTime
  ): CapacityForecast =
    if history.size < 2 then
      val current = history.headOption.map(_._2.value).getOrElse(0.0)
      CapacityForecast(0.0, current, current, horizon, current, None)
    else
      val n     = history.size.toDouble
      val xs    = history.map(_._1.value)
      val ys    = history.map(_._2.value)
      val xMean = xs.sum / n
      val yMean = ys.sum / n

      val numerator   = xs.zip(ys).map((x, y) => (x - xMean) * (y - yMean)).sum
      val denominator = xs.map(x => (x - xMean) * (x - xMean)).sum

      val slope     = if denominator == 0.0 then 0.0 else numerator / denominator
      val intercept = yMean - slope * xMean

      val lastTime    = xs.last
      val currentUtil = intercept + slope * lastTime
      val futureTime  = lastTime + horizon.value
      val predicted   = math.max(0.0, math.min(1.0, intercept + slope * futureTime))

      val exhaustionTime =
        if slope <= 0.0 then None
        else
          val timeToOne = (1.0 - intercept) / slope
          if timeToOne > lastTime then Some(SimTime(timeToOne - lastTime))
          else None

      CapacityForecast(slope, intercept, currentUtil, horizon, predicted, exhaustionTime)

  /** Bin-pack VMs onto hosts using first-fit decreasing by MIPS. Returns the minimum number of hosts needed.
    */
  def binPackHosts(vms: Vector[ResourceSpec], hostSpec: ResourceSpec): Int =
    if vms.isEmpty then 0
    else
      val sorted       = vms.sortBy(-_.mips.value)
      val hostCapacity = hostSpec.mips.value
      val bins = sorted.foldLeft(Vector.empty[Double]) { (bins, vm) =>
        val vmMips = vm.mips.value
        val fitIdx = bins.indexWhere(remaining => remaining >= vmMips)
        if fitIdx >= 0 then bins.updated(fitIdx, bins(fitIdx) - vmMips)
        else bins :+ (hostCapacity - vmMips)
      }
      bins.size

  /** Calculate total wasted (idle) resources across a fleet of hosts. */
  def wastedResources(
      hosts: Vector[(ResourceSpec, ResourceUtilizationStats)]
  ): ResourceSpec =
    hosts.foldLeft(ResourceSpec(PEs(0), MIPS(0), MegaBytes.Zero, Mbps.Zero, MegaBytes.Zero)) {
      case (acc, (spec, stats)) =>
        val idleFraction = 1.0 - stats.avg
        ResourceSpec(
          pes = PEs(acc.pes.value + (spec.pes.value * idleFraction).toInt),
          mips = MIPS(acc.mips.value + spec.mips.value * idleFraction),
          ram = MegaBytes(acc.ram.value + spec.ram.value * idleFraction),
          bw = Mbps(acc.bw.value + spec.bw.value * idleFraction),
          storage = MegaBytes(acc.storage.value + spec.storage.value * idleFraction)
        )
    }

  /** Recommend host count given current VMs and forecasted growth. */
  def recommendHostCount(
      currentVms: Vector[ResourceSpec],
      hostSpec: ResourceSpec,
      growthFactor: Double = 1.0,
      headroom: Double = 0.2
  ): Int =
    val scaledVms = if growthFactor > 1.0 then
      val extraCount = ((currentVms.size * growthFactor) - currentVms.size).toInt
      currentVms ++ currentVms.take(extraCount)
    else currentVms
    val packed = binPackHosts(scaledVms, hostSpec)
    math.ceil(packed * (1.0 + headroom)).toInt
