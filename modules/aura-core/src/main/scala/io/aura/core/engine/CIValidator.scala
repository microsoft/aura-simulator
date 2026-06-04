// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.engine

import io.aura.core.types.*

/** CI/CD validation utilities for simulation health checks.
  *
  * Provides programmatic checks that can be used in CI pipelines to validate simulation outputs and catch regressions.
  */
object CIValidator:

  /** Validation result for a single check. */
  final case class CheckResult(name: String, passed: Boolean, message: String)

  /** Validation report for a complete simulation run. */
  final case class ValidationReport(
      checks: Vector[CheckResult],
      simulationId: String
  ):
    def allPassed: Boolean                = checks.forall(_.passed)
    def failedChecks: Vector[CheckResult] = checks.filter(!_.passed)
    def passedCount: Int                  = checks.count(_.passed)
    def totalCount: Int                   = checks.size

    def formatReport: String =
      val status = if allPassed then "PASSED" else "FAILED"
      val lines = checks.map { c =>
        val icon = if c.passed then "[PASS]" else "[FAIL]"
        s"  $icon ${c.name}: ${c.message}"
      }
      s"""CI Validation Report ($simulationId): $status
  Passed: $passedCount / $totalCount
${lines.mkString("\n")}"""

  /** Run all standard validation checks on simulation results. */
  def validate(
      results: SimulationResults,
      simulationId: String = "default",
      maxAvgCompletionTime: Option[SimTime] = None,
      maxFailureRate: Option[Double] = None,
      maxEnergyWh: Option[WattHours] = None,
      minWorkloadsCompleted: Option[Int] = None
  ): ValidationReport =
    val basicChecks = Vector(
      CheckResult(
        "simulation-completed",
        results.simulationEndTime.value > 0,
        f"End time: ${results.simulationEndTime.value}%.2f"
      ),
      CheckResult(
        "no-negative-times",
        results.workloadResults.forall(_.finishTime.value >= 0),
        s"${results.workloadResults.size} workloads checked"
      ),
      CheckResult(
        "events-processed",
        results.totalEventsProcessed > 0,
        s"${results.totalEventsProcessed} events"
      )
    )

    val thresholdChecks = Vector(
      maxAvgCompletionTime.map { maxTime =>
        val avg = results.avgCompletionTime
        CheckResult(
          "avg-completion-time",
          avg.value <= maxTime.value,
          f"${avg.value}%.2f <= ${maxTime.value}%.2f"
        )
      },
      maxFailureRate.map { maxRate =>
        val total = results.workloadResults.size + results.failedWorkloads.size
        val rate  = if total > 0 then results.failedWorkloads.size.toDouble / total else 0.0
        CheckResult(
          "failure-rate",
          rate <= maxRate,
          f"${rate * 100}%.1f%% <= ${maxRate * 100}%.1f%%"
        )
      },
      maxEnergyWh.map { maxEnergy =>
        CheckResult(
          "energy-budget",
          results.totalEnergyWh.value <= maxEnergy.value,
          f"${results.totalEnergyWh.value}%.2f Wh <= ${maxEnergy.value}%.2f Wh"
        )
      },
      minWorkloadsCompleted.map { minCount =>
        CheckResult(
          "min-workloads",
          results.workloadResults.size >= minCount,
          s"${results.workloadResults.size} >= $minCount"
        )
      }
    ).flatten

    val checks = basicChecks ++ thresholdChecks

    ValidationReport(checks, simulationId)

  /** Quick pass/fail check for CI exit codes. */
  def quickCheck(results: SimulationResults): Boolean =
    results.simulationEndTime.value > 0 &&
      results.workloadResults.forall(_.finishTime.value >= 0) &&
      results.totalEventsProcessed > 0

  /** Compare two simulation results for regression detection. */
  def regressionCheck(
      baseline: SimulationResults,
      current: SimulationResults,
      tolerancePct: Double = 10.0
  ): ValidationReport =
    val tolerance = tolerancePct / 100.0

    // Workload count regression
    val baselineCount = baseline.workloadResults.size
    val currentCount  = current.workloadResults.size

    val baseChecks = Vector(
      CheckResult(
        "workload-count-regression",
        currentCount >= baselineCount,
        s"$currentCount >= $baselineCount (baseline)"
      )
    )

    // Avg completion time regression
    val baselineAvg = baseline.avgCompletionTime.value
    val currentAvg  = current.avgCompletionTime.value
    val maxAllowed  = baselineAvg * (1 + tolerance)

    val avgTimeCheck =
      if baselineAvg > 0 then
        Vector(
          CheckResult(
            "avg-time-regression",
            currentAvg <= maxAllowed,
            f"$currentAvg%.2f <= $maxAllowed%.2f (baseline: $baselineAvg%.2f + ${tolerancePct}%.0f%%)"
          )
        )
      else Vector.empty

    // Failure rate regression
    val baselineFailures = baseline.failedWorkloads.size
    val currentFailures  = current.failedWorkloads.size
    val maxFailures      = math.ceil(baselineFailures * (1 + tolerance)).toInt

    val failureCheck = Vector(
      CheckResult(
        "failure-regression",
        currentFailures <= maxFailures,
        s"$currentFailures <= $maxFailures (baseline: $baselineFailures)"
      )
    )

    // Energy regression
    val baselineEnergy = baseline.totalEnergyWh.value
    val currentEnergy  = current.totalEnergyWh.value

    val energyCheck = if baselineEnergy > 0 then
      val maxEnergy = baselineEnergy * (1 + tolerance)
      Vector(
        CheckResult(
          "energy-regression",
          currentEnergy <= maxEnergy,
          f"$currentEnergy%.2f Wh <= $maxEnergy%.2f Wh (baseline: $baselineEnergy%.2f)"
        )
      )
    else Vector.empty

    val checks = baseChecks ++ avgTimeCheck ++ failureCheck ++ energyCheck

    ValidationReport(checks, "regression-check")
