// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.engine

/** A single metric collected across experiment runs. */
final case class ExperimentMetric(
    name: String,
    values: Vector[Double]
):
  def mean: Double = if values.isEmpty then 0.0 else values.sum / values.size

  def stdDev: Double =
    if values.size <= 1 then 0.0
    else
      val m = mean
      math.sqrt(values.map(v => (v - m) * (v - m)).sum / (values.size - 1))

  def confidenceInterval(confidence: Double = 0.95): (Double, Double) =
    if values.size <= 1 then (mean, mean)
    else
      val tCritical = Experiment.tDistribution(values.size - 1, confidence)
      val margin    = tCritical * stdDev / math.sqrt(values.size.toDouble)
      (mean - margin, mean + margin)

/** Experiment configuration. */
final case class ExperimentConfig(
    name: String,
    runs: Int = 30,
    baseSeed: Long = 42L,
    confidenceLevel: Double = 0.95
)

object Experiment:
  /** Run an experiment: execute the simulation factory N times, collect metrics.
    * @param config
    *   experiment config
    * @param simulationFactory
    *   takes a seed and returns simulation results
    * @param metricExtractors
    *   named functions extracting a metric value from results
    */
  def run(
      config: ExperimentConfig,
      simulationFactory: Long => Either[String, SimulationResults],
      metricExtractors: Vector[(String, SimulationResults => Double)]
  ): ExperimentReport =
    val metricValues = metricExtractors.map((name, _) => name -> Vector.newBuilder[Double]).toMap
    val (successfulRuns, failedRuns) = (0 until config.runs).foldLeft((0, 0)) { case ((success, failed), i) =>
      val seed = config.baseSeed + i
      simulationFactory(seed) match
        case Right(results) =>
          metricExtractors.foreach { (name, extractor) =>
            metricValues(name) += extractor(results)
          }
          (success + 1, failed)
        case Left(_) => (success, failed + 1)
    }
    ExperimentReport(
      name = config.name,
      totalRuns = config.runs,
      successfulRuns = successfulRuns,
      failedRuns = failedRuns,
      metrics = metricExtractors.map { (name, _) =>
        ExperimentMetric(name, metricValues(name).result())
      },
      confidenceLevel = config.confidenceLevel
    )

  /** Approximate t-distribution critical value for two-tailed test. Uses a lookup table for common degrees of freedom
    * and 95% confidence.
    */
  def tDistribution(degreesOfFreedom: Int, confidence: Double): Double =
    // Common t-values for 95% confidence (two-tailed)
    val t95 = Map(
      1   -> 12.706,
      2   -> 4.303,
      3   -> 3.182,
      4   -> 2.776,
      5   -> 2.571,
      6   -> 2.447,
      7   -> 2.365,
      8   -> 2.306,
      9   -> 2.262,
      10  -> 2.228,
      11  -> 2.201,
      12  -> 2.179,
      13  -> 2.160,
      14  -> 2.145,
      15  -> 2.131,
      16  -> 2.120,
      17  -> 2.110,
      18  -> 2.101,
      19  -> 2.093,
      20  -> 2.086,
      25  -> 2.060,
      29  -> 2.045,
      30  -> 2.042,
      40  -> 2.021,
      50  -> 2.009,
      60  -> 2.000,
      80  -> 1.990,
      100 -> 1.984,
      120 -> 1.980
    )
    if confidence != 0.95 then 1.96 // fallback to z-score for non-95%
    else
      t95.getOrElse(
        degreesOfFreedom,
        if degreesOfFreedom > 120 then 1.96 // approximate with z
        else
          // Find nearest keys
          val keys  = t95.keys.toVector.sorted
          val lower = keys.filter(_ <= degreesOfFreedom).lastOption.getOrElse(1)
          val upper = keys.filter(_ >= degreesOfFreedom).headOption.getOrElse(120)
          if lower == upper then t95(lower)
          else
            val lv = t95(lower)
            val uv = t95(upper)
            lv + (uv - lv) * (degreesOfFreedom - lower).toDouble / (upper - lower)
      )

final case class ExperimentReport(
    name: String,
    totalRuns: Int,
    successfulRuns: Int,
    failedRuns: Int,
    metrics: Vector[ExperimentMetric],
    confidenceLevel: Double
):
  def formatReport: String =
    val header    = s"Experiment: $name ($successfulRuns/$totalRuns runs succeeded)"
    val separator = "=" * header.length
    val metricLines = metrics.map { m =>
      val (lo, hi) = m.confidenceInterval(confidenceLevel)
      f"  ${m.name}%-30s mean=${m.mean}%10.4f  stdDev=${m.stdDev}%10.4f  CI=[${lo}%.4f, ${hi}%.4f]"
    }
    (header +: separator +: metricLines).mkString("\n")
