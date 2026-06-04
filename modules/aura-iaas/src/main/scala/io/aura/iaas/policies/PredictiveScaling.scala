// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import io.aura.core.types.*

/** ML-based predictive auto-scaling: time-series forecasting, trend detection, and proactive scaling decisions — as
  * immutable data and pure functions.
  */

// ── Time Series ─────────────────────────────────────────────────────

/** A single data point in a time series. */
final case class TimeSeriesPoint(time: SimTime, value: Double)

/** A time series of observed values. */
final case class TimeSeries(points: Vector[TimeSeriesPoint]):
  def size: Int                   = points.size
  def isEmpty: Boolean            = points.isEmpty
  def values: Vector[Double]      = points.map(_.value)
  def latest: Option[Double]      = points.lastOption.map(_.value)
  def latestTime: Option[SimTime] = points.lastOption.map(_.time)

  def append(time: SimTime, value: Double): TimeSeries =
    copy(points = points :+ TimeSeriesPoint(time, value))

  def window(n: Int): TimeSeries =
    copy(points = points.takeRight(n))

object TimeSeries:
  val empty: TimeSeries = TimeSeries(Vector.empty)

  def fromValues(startTime: Double, interval: Double, values: Vector[Double]): TimeSeries =
    TimeSeries(values.zipWithIndex.map { case (v, i) =>
      TimeSeriesPoint(SimTime(startTime + i * interval), v)
    })

// ── Forecasting Models ──────────────────────────────────────────────

/** Result of a forecast. */
final case class ForecastResult(
    predictedValue: Double,
    confidenceLow: Double,
    confidenceHigh: Double,
    horizon: SimTime
)

object Forecaster:

  /** Simple Moving Average (SMA). */
  def simpleMovingAverage(series: TimeSeries, windowSize: Int): Double =
    if series.isEmpty then 0.0
    else
      val window = series.values.takeRight(windowSize)
      window.sum / window.size

  /** Exponential Moving Average (EMA). More weight on recent observations. alpha ∈ (0,1), higher = more weight on
    * recent.
    */
  def exponentialMovingAverage(series: TimeSeries, alpha: Double): Double =
    if series.isEmpty then 0.0
    else
      series.values.foldLeft(series.values.head) { (ema, v) =>
        alpha * v + (1.0 - alpha) * ema
      }

  /** Weighted Moving Average (WMA): linearly increasing weights. */
  def weightedMovingAverage(series: TimeSeries, windowSize: Int): Double =
    if series.isEmpty then 0.0
    else
      val window      = series.values.takeRight(windowSize)
      val n           = window.size
      val totalWeight = n * (n + 1) / 2.0
      window.zipWithIndex.map { case (v, i) => v * (i + 1) }.sum / totalWeight

  /** Double Exponential Smoothing (Holt's method). Captures both level and trend.
    * @param alpha
    *   smoothing factor for level (0,1)
    * @param beta
    *   smoothing factor for trend (0,1)
    * @param stepsAhead
    *   number of steps to forecast ahead
    */
  def doubleExponentialSmoothing(
      series: TimeSeries,
      alpha: Double = 0.3,
      beta: Double = 0.1,
      stepsAhead: Int = 1
  ): ForecastResult =
    if series.size < 2 then ForecastResult(series.latest.getOrElse(0.0), 0.0, 0.0, SimTime.Zero)
    else
      val values = series.values
      val (level, trend) = values.tail.foldLeft((values.head, values(1) - values(0))) {
        case ((prevLevel, prevTrend), v) =>
          val newLevel = alpha * v + (1.0 - alpha) * (prevLevel + prevTrend)
          val newTrend = beta * (newLevel - prevLevel) + (1.0 - beta) * prevTrend
          (newLevel, newTrend)
      }

      val predicted = level + stepsAhead * trend
      // Rough confidence interval based on residuals
      val residuals   = values.sliding(2).toVector.map(w => math.abs(w(1) - w(0)))
      val avgResidual = if residuals.nonEmpty then residuals.sum / residuals.size else 0.0
      val confidence  = avgResidual * stepsAhead * 1.96

      val interval =
        if series.size >= 2 then SimTime(series.points.last.time.value - series.points(series.size - 2).time.value)
        else SimTime.Zero

      ForecastResult(
        predicted,
        predicted - confidence,
        predicted + confidence,
        SimTime(interval.value * stepsAhead)
      )

  /** Linear regression forecast. */
  def linearRegression(series: TimeSeries, stepsAhead: Int = 1): ForecastResult =
    if series.size < 2 then ForecastResult(series.latest.getOrElse(0.0), 0.0, 0.0, SimTime.Zero)
    else
      val n     = series.size.toDouble
      val xs    = (0 until series.size).map(_.toDouble).toVector
      val ys    = series.values
      val xMean = xs.sum / n
      val yMean = ys.sum / n
      val num   = xs.zip(ys).map((x, y) => (x - xMean) * (y - yMean)).sum
      val den   = xs.map(x => (x - xMean) * (x - xMean)).sum

      val slope     = if den != 0.0 then num / den else 0.0
      val intercept = yMean - slope * xMean
      val predicted = slope * (n - 1 + stepsAhead) + intercept

      val residuals = xs.zip(ys).map { (x, y) =>
        val pred = slope * x + intercept
        (y - pred) * (y - pred)
      }
      val mse        = residuals.sum / n
      val confidence = math.sqrt(mse) * 1.96

      val interval =
        if series.size >= 2 then SimTime(series.points.last.time.value - series.points(series.size - 2).time.value)
        else SimTime.Zero

      ForecastResult(predicted, predicted - confidence, predicted + confidence, SimTime(interval.value * stepsAhead))

// ── Trend Detection ─────────────────────────────────────────────────

enum TrendDirection:
  case Rising, Falling, Stable

object TrendDetector:

  /** Detect trend direction from recent observations. */
  def detect(series: TimeSeries, windowSize: Int = 10, threshold: Double = 0.01): TrendDirection =
    if series.size < 2 then TrendDirection.Stable
    else
      val window = series.values.takeRight(windowSize)
      if window.size < 2 then TrendDirection.Stable
      else
        val first     = window.take(window.size / 2)
        val second    = window.drop(window.size / 2)
        val firstAvg  = first.sum / first.size
        val secondAvg = second.sum / second.size
        val change    = if firstAvg != 0.0 then (secondAvg - firstAvg) / firstAvg else 0.0
        if change > threshold then TrendDirection.Rising
        else if change < -threshold then TrendDirection.Falling
        else TrendDirection.Stable

  /** Detect if a metric is approaching a threshold (will breach within N steps). */
  def willBreachThreshold(
      series: TimeSeries,
      threshold: Double,
      withinSteps: Int,
      alpha: Double = 0.3,
      beta: Double = 0.1
  ): Boolean =
    val forecast = Forecaster.doubleExponentialSmoothing(series, alpha, beta, withinSteps)
    forecast.predictedValue >= threshold

// ── Predictive Scaling Policy ───────────────────────────────────────

/** Scaling decision from predictive analysis. */
enum ScalingDecision:
  case ScaleUp(additionalInstances: Int, reason: String)
  case ScaleDown(removeInstances: Int, reason: String)
  case NoChange(reason: String)

/** Configuration for predictive scaling. */
final case class PredictiveScalingConfig(
    targetUtilization: Double = 0.7,
    scaleUpThreshold: Double = 0.8,
    scaleDownThreshold: Double = 0.3,
    forecastHorizon: Int = 5,
    cooldownPeriod: SimTime = SimTime(300.0),
    minInstances: Int = 1,
    maxInstances: Int = 100,
    alpha: Double = 0.3, // EMA alpha for level
    beta: Double = 0.1   // EMA beta for trend
)

object PredictiveScalingPolicy:

  /** Make a scaling decision based on forecasted utilization. */
  def decide(
      utilizationHistory: TimeSeries,
      currentInstances: Int,
      config: PredictiveScalingConfig,
      lastScaleTime: SimTime,
      currentTime: SimTime
  ): ScalingDecision =
    // Check cooldown
    if (currentTime.value - lastScaleTime.value) < config.cooldownPeriod.value then
      ScalingDecision.NoChange("In cooldown period")
    else if utilizationHistory.size < 3 then ScalingDecision.NoChange("Insufficient data")
    else
      val forecast = Forecaster.doubleExponentialSmoothing(
        utilizationHistory,
        config.alpha,
        config.beta,
        config.forecastHorizon
      )
      val predicted = forecast.predictedValue
      val trend     = TrendDetector.detect(utilizationHistory)

      if predicted >= config.scaleUpThreshold then
        // Scale up: how many instances to reach target utilization?
        val currentLoad     = predicted * currentInstances
        val neededInstances = math.ceil(currentLoad / config.targetUtilization).toInt
        val additional = math.min(
          neededInstances - currentInstances,
          config.maxInstances - currentInstances
        )
        if additional > 0 then
          ScalingDecision.ScaleUp(
            additional,
            f"Predicted utilization ${predicted}%.2f exceeds ${config.scaleUpThreshold}%.2f"
          )
        else ScalingDecision.NoChange("At max instances")
      else if predicted <= config.scaleDownThreshold && trend == TrendDirection.Falling then
        // Scale down: reduce to meet target utilization
        val currentLoad     = predicted * currentInstances
        val neededInstances = math.max(config.minInstances, math.ceil(currentLoad / config.targetUtilization).toInt)
        val remove          = currentInstances - neededInstances
        if remove > 0 then
          ScalingDecision.ScaleDown(
            remove,
            f"Predicted utilization ${predicted}%.2f below ${config.scaleDownThreshold}%.2f with falling trend"
          )
        else ScalingDecision.NoChange("Already at minimum")
      else ScalingDecision.NoChange(f"Predicted utilization ${predicted}%.2f within thresholds")

  /** Evaluate forecast accuracy against actual observed values. */
  def forecastAccuracy(
      series: TimeSeries,
      forecastHorizon: Int,
      alpha: Double = 0.3,
      beta: Double = 0.1
  ): Double =
    if series.size < forecastHorizon + 5 then 1.0 // not enough data
    else
      val trainSize = series.size - forecastHorizon
      val train     = TimeSeries(series.points.take(trainSize))
      val actual    = series.values.drop(trainSize)
      val forecast  = Forecaster.doubleExponentialSmoothing(train, alpha, beta, forecastHorizon)

      // MAPE: Mean Absolute Percentage Error
      val errors = actual.map { a =>
        if a != 0.0 then math.abs(forecast.predictedValue - a) / math.abs(a) else 0.0
      }
      1.0 - (errors.sum / errors.size) // accuracy = 1 - MAPE

  /** Estimate cost savings from predictive vs reactive scaling. */
  def estimatedSavings(
      utilizationHistory: TimeSeries,
      reactiveOverprovisionPct: Double = 0.3,  // Reactive typically overprovisions 30%
      predictiveOverprovisionPct: Double = 0.1 // Predictive overprovisions ~10%
  ): Double =
    if utilizationHistory.isEmpty then 0.0
    else
      val avgUtil             = utilizationHistory.values.sum / utilizationHistory.size
      val reactiveInstances   = avgUtil * (1.0 + reactiveOverprovisionPct)
      val predictiveInstances = avgUtil * (1.0 + predictiveOverprovisionPct)
      if reactiveInstances > 0 then (reactiveInstances - predictiveInstances) / reactiveInstances
      else 0.0
