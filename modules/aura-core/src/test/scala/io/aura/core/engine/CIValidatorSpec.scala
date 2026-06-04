// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.types.*

class CIValidatorSpec extends AnyFlatSpec with Matchers:

  private def resultsWithWorkloads(n: Int, avgTime: Double = 50.0): SimulationResults =
    SimulationResults.empty.copy(
      simulationEndTime = SimTime(100.0),
      workloadResults = (0 until n)
        .map(i => WorkloadResult(WorkloadId(i.toLong), VmId(0), HostId(0), SimTime(avgTime), MI(1000.0)))
        .toVector,
      totalEventsProcessed = n.toLong * 3
    )

  // ─── validate ──────────────────────────────────────────────────────

  "CIValidator.validate" should "pass basic checks for valid results" in {
    val report = CIValidator.validate(resultsWithWorkloads(10))
    report.allPassed shouldBe true
    report.passedCount shouldBe report.totalCount
  }

  it should "fail for empty simulation" in {
    val report = CIValidator.validate(SimulationResults.empty)
    report.allPassed shouldBe false
    report.failedChecks.map(_.name) should contain("simulation-completed")
  }

  it should "check avg completion time threshold" in {
    val report = CIValidator.validate(
      resultsWithWorkloads(10, avgTime = 100.0),
      maxAvgCompletionTime = Some(SimTime(50.0))
    )
    report.allPassed shouldBe false
    report.failedChecks.map(_.name) should contain("avg-completion-time")
  }

  it should "pass when avg time is within threshold" in {
    val report = CIValidator.validate(
      resultsWithWorkloads(10, avgTime = 30.0),
      maxAvgCompletionTime = Some(SimTime(50.0))
    )
    report.checks.find(_.name == "avg-completion-time").get.passed shouldBe true
  }

  it should "check failure rate" in {
    val results = resultsWithWorkloads(8).copy(
      failedWorkloads = Vector(
        FailedWorkload(WorkloadId(100), "timeout", SimTime(50.0)),
        FailedWorkload(WorkloadId(101), "timeout", SimTime(60.0))
      )
    )
    // 2 out of 10 = 20% failure rate
    val report = CIValidator.validate(results, maxFailureRate = Some(0.1))
    report.failedChecks.map(_.name) should contain("failure-rate")
  }

  it should "check energy budget" in {
    val results = resultsWithWorkloads(5).copy(
      energyRecords = Vector(
        EnergyRecord(HostId(0), Watts(200.0), SimTime(0.0), SimTime(100.0), WattHours(20.0))
      )
    )
    val report = CIValidator.validate(results, maxEnergyWh = Some(WattHours(10.0)))
    report.failedChecks.map(_.name) should contain("energy-budget")
  }

  it should "check minimum workloads completed" in {
    val report = CIValidator.validate(
      resultsWithWorkloads(3),
      minWorkloadsCompleted = Some(5)
    )
    report.failedChecks.map(_.name) should contain("min-workloads")
  }

  // ─── formatReport ──────────────────────────────────────────────────

  "ValidationReport.formatReport" should "show PASSED status" in {
    val report    = CIValidator.validate(resultsWithWorkloads(10), simulationId = "test-sim")
    val formatted = report.formatReport
    formatted should include("PASSED")
    formatted should include("test-sim")
    formatted should include("[PASS]")
  }

  it should "show FAILED status with failed checks" in {
    val report    = CIValidator.validate(SimulationResults.empty, simulationId = "failing-sim")
    val formatted = report.formatReport
    formatted should include("FAILED")
    formatted should include("[FAIL]")
  }

  // ─── quickCheck ────────────────────────────────────────────────────

  "CIValidator.quickCheck" should "return true for valid results" in {
    CIValidator.quickCheck(resultsWithWorkloads(5)) shouldBe true
  }

  it should "return false for empty results" in {
    CIValidator.quickCheck(SimulationResults.empty) shouldBe false
  }

  // ─── regressionCheck ───────────────────────────────────────────────

  "CIValidator.regressionCheck" should "pass when current matches baseline" in {
    val baseline = resultsWithWorkloads(10, avgTime = 50.0)
    val current  = resultsWithWorkloads(10, avgTime = 50.0)
    val report   = CIValidator.regressionCheck(baseline, current)
    report.allPassed shouldBe true
  }

  it should "detect workload count regression" in {
    val baseline = resultsWithWorkloads(10)
    val current  = resultsWithWorkloads(5)
    val report   = CIValidator.regressionCheck(baseline, current)
    report.failedChecks.map(_.name) should contain("workload-count-regression")
  }

  it should "detect avg time regression beyond tolerance" in {
    val baseline = resultsWithWorkloads(10, avgTime = 50.0)
    val current  = resultsWithWorkloads(10, avgTime = 70.0) // 40% worse
    val report   = CIValidator.regressionCheck(baseline, current, tolerancePct = 10.0)
    report.failedChecks.map(_.name) should contain("avg-time-regression")
  }

  it should "allow regression within tolerance" in {
    val baseline = resultsWithWorkloads(10, avgTime = 50.0)
    val current  = resultsWithWorkloads(10, avgTime = 54.0) // 8% worse
    val report   = CIValidator.regressionCheck(baseline, current, tolerancePct = 10.0)
    report.checks.find(_.name == "avg-time-regression").get.passed shouldBe true
  }

  it should "detect failure count regression" in {
    val baseline = resultsWithWorkloads(10).copy(
      failedWorkloads = Vector(FailedWorkload(WorkloadId(100), "err", SimTime(50.0)))
    )
    val current = resultsWithWorkloads(10).copy(
      failedWorkloads =
        (0 until 5).map(i => FailedWorkload(WorkloadId((100 + i).toLong), "err", SimTime(50.0))).toVector
    )
    val report = CIValidator.regressionCheck(baseline, current, tolerancePct = 10.0)
    report.failedChecks.map(_.name) should contain("failure-regression")
  }
