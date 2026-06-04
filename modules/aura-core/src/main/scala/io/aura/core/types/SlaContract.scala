// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import io.aura.core.engine.SimulationResults

enum SlaMetricName:
  case Availability, CpuUtilization, TaskCompletionTime, Price, WaitTime, FaultTolerance

final case class SlaConstraint(
    metric: SlaMetricName,
    minValue: Option[Double] = None,
    maxValue: Option[Double] = None
)

final case class SlaContract(constraints: Vector[SlaConstraint]):
  def maxPrice: Option[Double]        = constraints.find(_.metric == SlaMetricName.Price).flatMap(_.maxValue)
  def minAvailability: Option[Double] = constraints.find(_.metric == SlaMetricName.Availability).flatMap(_.minValue)
  def maxCompletionTime: Option[Double] =
    constraints.find(_.metric == SlaMetricName.TaskCompletionTime).flatMap(_.maxValue)

object SlaContract:
  val empty: SlaContract = SlaContract(Vector.empty)

  def apply(constraints: SlaConstraint*): SlaContract =
    SlaContract(constraints.toVector)

/** SLA violation detected during simulation. */
final case class SlaViolation(
    metric: SlaMetricName,
    constraintValue: Double,
    actualValue: Double,
    time: SimTime,
    entityId: String
)

/** SLA evaluator: checks results against contract. */
object SlaEvaluator:
  def evaluate(results: SimulationResults, contract: SlaContract): Vector[SlaViolation] =
    val violations = Vector.newBuilder[SlaViolation]

    // Check task completion time
    contract.maxCompletionTime.foreach { maxTime =>
      results.workloadResults.foreach { r =>
        if r.finishTime.value > maxTime then
          violations += SlaViolation(
            SlaMetricName.TaskCompletionTime,
            maxTime,
            r.finishTime.value,
            r.finishTime,
            s"workload-${r.workloadId.value}"
          )
      }
    }

    // Check availability (% of successful vs total workloads)
    contract.minAvailability.foreach { minAvail =>
      val total = results.workloadResults.size + results.failedWorkloads.size
      if total > 0 then
        val actual = results.workloadResults.size.toDouble / total * 100.0
        if actual < minAvail then
          violations += SlaViolation(
            SlaMetricName.Availability,
            minAvail,
            actual,
            results.simulationEndTime,
            "system"
          )
    }

    // Check price
    contract.maxPrice.foreach { maxP =>
      results.costRecords.foreach { cr =>
        if cr.totalCost.value > maxP then
          violations += SlaViolation(
            SlaMetricName.Price,
            maxP,
            cr.totalCost.value,
            cr.time,
            s"vm-${cr.vmId.value}"
          )
      }
    }

    violations.result()
