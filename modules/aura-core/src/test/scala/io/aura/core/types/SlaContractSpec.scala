// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.engine.*

class SlaContractSpec extends AnyFlatSpec with Matchers:

  "SlaContract" should "construct from varargs" in {
    val contract = SlaContract(
      SlaConstraint(SlaMetricName.Availability, minValue = Some(99.5)),
      SlaConstraint(SlaMetricName.TaskCompletionTime, maxValue = Some(100.0)),
      SlaConstraint(SlaMetricName.Price, maxValue = Some(50.0))
    )
    contract.constraints should have size 3
    contract.minAvailability shouldBe Some(99.5)
    contract.maxCompletionTime shouldBe Some(100.0)
    contract.maxPrice shouldBe Some(50.0)
  }

  "SlaContract.empty" should "have no constraints" in {
    SlaContract.empty.constraints shouldBe empty
    SlaContract.empty.maxPrice shouldBe None
    SlaContract.empty.minAvailability shouldBe None
    SlaContract.empty.maxCompletionTime shouldBe None
  }

  "SlaEvaluator" should "detect completion time violations" in {
    val contract = SlaContract(
      SlaConstraint(SlaMetricName.TaskCompletionTime, maxValue = Some(50.0))
    )
    val results = SimulationResults.empty.copy(
      workloadResults = Vector(
        WorkloadResult(WorkloadId(1), VmId(1), HostId(1), SimTime(30.0), MI(1000.0)),
        WorkloadResult(WorkloadId(2), VmId(1), HostId(1), SimTime(60.0), MI(1000.0)),
        WorkloadResult(WorkloadId(3), VmId(1), HostId(1), SimTime(40.0), MI(1000.0))
      )
    )
    val violations = SlaEvaluator.evaluate(results, contract)
    violations should have size 1
    violations.head.metric shouldBe SlaMetricName.TaskCompletionTime
    violations.head.actualValue shouldBe 60.0
    violations.head.entityId shouldBe "workload-2"
  }

  it should "detect availability violations" in {
    val contract = SlaContract(
      SlaConstraint(SlaMetricName.Availability, minValue = Some(90.0))
    )
    val results = SimulationResults.empty.copy(
      simulationEndTime = SimTime(100.0),
      workloadResults = Vector(
        WorkloadResult(WorkloadId(1), VmId(1), HostId(1), SimTime(30.0), MI(1000.0))
      ),
      failedWorkloads = Vector(
        FailedWorkload(WorkloadId(2), "timeout", SimTime(50.0)),
        FailedWorkload(WorkloadId(3), "timeout", SimTime(60.0))
      )
    )
    val violations = SlaEvaluator.evaluate(results, contract)
    violations should have size 1
    violations.head.metric shouldBe SlaMetricName.Availability
    // 1/3 = 33.3% which is below 90%
    violations.head.actualValue shouldBe 33.33 +- 0.1
  }

  it should "report no violations when all constraints are satisfied" in {
    val contract = SlaContract(
      SlaConstraint(SlaMetricName.TaskCompletionTime, maxValue = Some(100.0)),
      SlaConstraint(SlaMetricName.Availability, minValue = Some(50.0))
    )
    val results = SimulationResults.empty.copy(
      simulationEndTime = SimTime(100.0),
      workloadResults = Vector(
        WorkloadResult(WorkloadId(1), VmId(1), HostId(1), SimTime(50.0), MI(1000.0)),
        WorkloadResult(WorkloadId(2), VmId(1), HostId(1), SimTime(60.0), MI(1000.0))
      )
    )
    val violations = SlaEvaluator.evaluate(results, contract)
    violations shouldBe empty
  }
