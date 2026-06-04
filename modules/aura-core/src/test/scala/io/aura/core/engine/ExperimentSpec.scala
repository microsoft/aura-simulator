// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.types.*

class ExperimentSpec extends AnyFlatSpec with Matchers:

  "Experiment.run" should "execute N runs and collect metrics" in {
    val config = ExperimentConfig("test", runs = 10, baseSeed = 42L)

    val report = Experiment.run(
      config,
      seed =>
        Right(
          SimulationResults.empty.copy(
            simulationEndTime = SimTime(seed.toDouble),
            workloadResults = Vector(
              WorkloadResult(WorkloadId(1), VmId(1), HostId(1), SimTime(seed.toDouble * 0.1), MI(1000.0))
            )
          )
        ),
      Vector(
        "endTime"       -> (r => r.simulationEndTime.value),
        "avgCompletion" -> (r => r.avgCompletionTime.value)
      )
    )

    report.totalRuns shouldBe 10
    report.successfulRuns shouldBe 10
    report.failedRuns shouldBe 0
    report.metrics should have size 2
    report.metrics(0).values should have size 10
    report.metrics(1).values should have size 10
  }

  it should "count failed runs" in {
    val config = ExperimentConfig("test-failures", runs = 10)

    val report = Experiment.run(
      config,
      seed => if seed % 3 == 0 then Left("failure") else Right(SimulationResults.empty),
      Vector("endTime" -> (r => r.simulationEndTime.value))
    )

    report.failedRuns should be > 0
    report.successfulRuns + report.failedRuns shouldBe 10
  }

  "ExperimentMetric" should "compute mean correctly" in {
    val metric = ExperimentMetric("test", Vector(1.0, 2.0, 3.0, 4.0, 5.0))
    metric.mean shouldBe 3.0 +- 0.001
  }

  it should "compute stdDev correctly" in {
    val metric = ExperimentMetric("test", Vector(2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0))
    // Known stdDev for this dataset is ~2.0
    metric.stdDev shouldBe 2.0 +- 0.2
  }

  it should "return zero mean and stdDev for empty values" in {
    val metric = ExperimentMetric("empty", Vector.empty)
    metric.mean shouldBe 0.0
    metric.stdDev shouldBe 0.0
  }

  it should "compute confidence intervals" in {
    val metric   = ExperimentMetric("test", Vector(1.0, 2.0, 3.0, 4.0, 5.0))
    val (lo, hi) = metric.confidenceInterval(0.95)

    lo should be < metric.mean
    hi should be > metric.mean
    lo should be > 0.0
    hi should be < 6.0
  }

  it should "return mean for single value" in {
    val metric   = ExperimentMetric("test", Vector(42.0))
    val (lo, hi) = metric.confidenceInterval(0.95)
    lo shouldBe 42.0
    hi shouldBe 42.0
  }

  "Experiment.tDistribution" should "return known t-values for 95% confidence" in {
    Experiment.tDistribution(1, 0.95) shouldBe 12.706 +- 0.01
    Experiment.tDistribution(10, 0.95) shouldBe 2.228 +- 0.01
    Experiment.tDistribution(29, 0.95) shouldBe 2.045 +- 0.01
  }

  it should "approximate z-score for large degrees of freedom" in {
    Experiment.tDistribution(1000, 0.95) shouldBe 1.96 +- 0.01
  }

  "ExperimentReport.formatReport" should "produce readable output" in {
    val report = ExperimentReport(
      name = "test-experiment",
      totalRuns = 30,
      successfulRuns = 30,
      failedRuns = 0,
      metrics = Vector(
        ExperimentMetric("avgTime", (1 to 30).map(_.toDouble).toVector)
      ),
      confidenceLevel = 0.95
    )
    val output = report.formatReport
    output should include("test-experiment")
    output should include("30/30")
    output should include("avgTime")
    output should include("mean=")
    output should include("CI=")
  }
