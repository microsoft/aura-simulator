// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.types.*

class PredictiveScalingSpec extends AnyFlatSpec with Matchers:

  private def seriesFromValues(values: Vector[Double], start: Double = 0.0, interval: Double = 60.0) =
    TimeSeries.fromValues(start, interval, values)

  // Stable utilization around 0.5
  private val stableSeries = seriesFromValues(
    Vector(0.48, 0.52, 0.49, 0.51, 0.50, 0.48, 0.52, 0.49, 0.51, 0.50)
  )

  // Rising utilization approaching overload
  private val risingSeries = seriesFromValues(
    Vector(0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85)
  )

  // Falling utilization
  private val fallingSeries = seriesFromValues(
    Vector(0.8, 0.75, 0.7, 0.65, 0.6, 0.55, 0.5, 0.45, 0.4, 0.35, 0.3, 0.25)
  )

  // ── TimeSeries ────────────────────────────────────────────────────

  "TimeSeries" should "track values and provide windowing" in {
    val ts = TimeSeries.empty.append(SimTime(0), 1.0).append(SimTime(1), 2.0)
    ts.size shouldBe 2
    ts.latest shouldBe Some(2.0)
    ts.window(1).size shouldBe 1
  }

  "TimeSeries.fromValues" should "create series with proper times" in {
    val ts = TimeSeries.fromValues(100.0, 60.0, Vector(0.5, 0.6, 0.7))
    ts.size shouldBe 3
    ts.points.head.time.value shouldBe 100.0
    ts.points.last.time.value shouldBe 220.0
  }

  // ── Simple Moving Average ─────────────────────────────────────────

  "Forecaster.simpleMovingAverage" should "compute average of last N values" in {
    val sma = Forecaster.simpleMovingAverage(stableSeries, 5)
    sma shouldBe 0.5 +- 0.05
  }

  it should "handle window larger than series" in {
    val sma = Forecaster.simpleMovingAverage(stableSeries, 100)
    sma shouldBe 0.5 +- 0.05
  }

  // ── Exponential Moving Average ────────────────────────────────────

  "Forecaster.exponentialMovingAverage" should "weight recent values more" in {
    val highAlpha = Forecaster.exponentialMovingAverage(risingSeries, 0.9) // very responsive
    val lowAlpha  = Forecaster.exponentialMovingAverage(risingSeries, 0.1) // very smooth
    highAlpha should be > lowAlpha // High alpha tracks recent values more
  }

  // ── Weighted Moving Average ───────────────────────────────────────

  "Forecaster.weightedMovingAverage" should "give more weight to recent values" in {
    val wma = Forecaster.weightedMovingAverage(risingSeries, 5)
    val sma = Forecaster.simpleMovingAverage(risingSeries, 5)
    wma should be > sma // WMA biased toward recent (higher) values
  }

  // ── Double Exponential Smoothing ──────────────────────────────────

  "Forecaster.doubleExponentialSmoothing" should "forecast rising trend" in {
    val forecast = Forecaster.doubleExponentialSmoothing(risingSeries, stepsAhead = 3)
    forecast.predictedValue should be > risingSeries.latest.get
  }

  it should "forecast falling trend" in {
    val forecast = Forecaster.doubleExponentialSmoothing(fallingSeries, stepsAhead = 3)
    forecast.predictedValue should be < fallingSeries.latest.get
  }

  it should "provide confidence intervals" in {
    val forecast = Forecaster.doubleExponentialSmoothing(risingSeries, stepsAhead = 5)
    forecast.confidenceLow should be < forecast.predictedValue
    forecast.confidenceHigh should be > forecast.predictedValue
  }

  // ── Linear Regression ─────────────────────────────────────────────

  "Forecaster.linearRegression" should "extrapolate trend" in {
    val forecast = Forecaster.linearRegression(risingSeries, stepsAhead = 5)
    forecast.predictedValue should be > risingSeries.latest.get
  }

  it should "predict decline for falling series" in {
    val forecast = Forecaster.linearRegression(fallingSeries, stepsAhead = 5)
    forecast.predictedValue should be < fallingSeries.latest.get
  }

  // ── Trend Detection ───────────────────────────────────────────────

  "TrendDetector.detect" should "identify rising trend" in {
    TrendDetector.detect(risingSeries) shouldBe TrendDirection.Rising
  }

  it should "identify falling trend" in {
    TrendDetector.detect(fallingSeries) shouldBe TrendDirection.Falling
  }

  it should "identify stable series" in {
    TrendDetector.detect(stableSeries) shouldBe TrendDirection.Stable
  }

  "TrendDetector.willBreachThreshold" should "predict breach for rising series" in {
    TrendDetector.willBreachThreshold(risingSeries, 0.9, withinSteps = 5) shouldBe true
  }

  it should "not predict breach for stable series" in {
    TrendDetector.willBreachThreshold(stableSeries, 0.9, withinSteps = 5) shouldBe false
  }

  // ── PredictiveScalingPolicy ───────────────────────────────────────

  private val config = PredictiveScalingConfig(
    targetUtilization = 0.7,
    scaleUpThreshold = 0.8,
    scaleDownThreshold = 0.3,
    forecastHorizon = 3,
    cooldownPeriod = SimTime(60.0)
  )

  "PredictiveScalingPolicy.decide" should "scale up when predicted utilization is high" in {
    val decision = PredictiveScalingPolicy.decide(
      risingSeries,
      currentInstances = 5,
      config,
      lastScaleTime = SimTime(0.0),
      currentTime = SimTime(1000.0)
    )
    decision shouldBe a[ScalingDecision.ScaleUp]
  }

  it should "scale down when predicted utilization is low and falling" in {
    val decision = PredictiveScalingPolicy.decide(
      fallingSeries,
      currentInstances = 10,
      config,
      lastScaleTime = SimTime(0.0),
      currentTime = SimTime(1000.0)
    )
    decision shouldBe a[ScalingDecision.ScaleDown]
  }

  it should "not change when utilization is stable within thresholds" in {
    val decision = PredictiveScalingPolicy.decide(
      stableSeries,
      currentInstances = 5,
      config,
      lastScaleTime = SimTime(0.0),
      currentTime = SimTime(1000.0)
    )
    decision shouldBe a[ScalingDecision.NoChange]
  }

  it should "respect cooldown period" in {
    val decision = PredictiveScalingPolicy.decide(
      risingSeries,
      currentInstances = 5,
      config,
      lastScaleTime = SimTime(990.0),
      currentTime = SimTime(1000.0) // within 60s cooldown
    )
    decision shouldBe a[ScalingDecision.NoChange]
  }

  it should "respect max instances" in {
    val smallConfig = config.copy(maxInstances = 6)
    val decision = PredictiveScalingPolicy.decide(
      risingSeries,
      currentInstances = 6,
      smallConfig,
      lastScaleTime = SimTime(0.0),
      currentTime = SimTime(1000.0)
    )
    decision shouldBe a[ScalingDecision.NoChange]
  }

  // ── Forecast Accuracy ─────────────────────────────────────────────

  "PredictiveScalingPolicy.forecastAccuracy" should "be reasonable for trending data" in {
    val longSeries = seriesFromValues(
      (0 to 20).map(i => 0.3 + i * 0.03).toVector
    )
    val accuracy = PredictiveScalingPolicy.forecastAccuracy(longSeries, forecastHorizon = 3)
    accuracy should be > 0.5 // At least 50% accurate
  }

  // ── Estimated Savings ─────────────────────────────────────────────

  "PredictiveScalingPolicy.estimatedSavings" should "show positive savings" in {
    val savings = PredictiveScalingPolicy.estimatedSavings(stableSeries)
    savings should be > 0.0
    savings should be < 1.0
  }

  it should "return 0 for empty series" in {
    PredictiveScalingPolicy.estimatedSavings(TimeSeries.empty) shouldBe 0.0
  }
