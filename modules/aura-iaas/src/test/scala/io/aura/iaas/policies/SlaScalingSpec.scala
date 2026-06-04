// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.types.*

class SlaScalingSpec extends AnyFlatSpec with Matchers:

  private val vmTemplate = ResourceSpec(PEs(4), MIPS(10000.0), MegaBytes(8192.0), Mbps(5000.0), MegaBytes(200000.0))
  private val baseCtx = HorizontalScalingContext(
    activeVmCount = 3,
    pendingWorkloads = 5,
    runningWorkloads = 10,
    avgCpuUtilization = Utilization(0.6),
    currentTime = SimTime(100.0)
  )

  // ─── completionTimeBased ──────────────────────────────────────────────

  "SlaScalingPolicy.completionTimeBased" should "scale when avg completion approaches SLA deadline" in {
    val policy = SlaScalingPolicy.completionTimeBased(
      maxCompletionTime = SimTime(100.0),
      triggerFraction = 0.8,
      vmTemplate = vmTemplate
    )
    val ctx = SlaScalingContext(
      baseCtx,
      completedWorkloads = 10,
      failedWorkloads = 0,
      avgCompletionTime = SimTime(85.0),
      maxCompletionTime = SimTime(90.0),
      slaViolationCount = 0
    )
    policy(ctx) shouldBe defined
  }

  it should "not scale when avg completion is well below deadline" in {
    val policy = SlaScalingPolicy.completionTimeBased(
      maxCompletionTime = SimTime(100.0),
      triggerFraction = 0.8,
      vmTemplate = vmTemplate
    )
    val ctx = SlaScalingContext(
      baseCtx,
      completedWorkloads = 10,
      failedWorkloads = 0,
      avgCompletionTime = SimTime(50.0),
      maxCompletionTime = SimTime(60.0),
      slaViolationCount = 0
    )
    policy(ctx) shouldBe None
  }

  it should "not scale with zero completed workloads" in {
    val policy = SlaScalingPolicy.completionTimeBased(
      maxCompletionTime = SimTime(100.0),
      triggerFraction = 0.8,
      vmTemplate = vmTemplate
    )
    val ctx = SlaScalingContext(
      baseCtx,
      completedWorkloads = 0,
      failedWorkloads = 0,
      avgCompletionTime = SimTime.Zero,
      maxCompletionTime = SimTime.Zero,
      slaViolationCount = 0
    )
    policy(ctx) shouldBe None
  }

  // ─── availabilityBased ───────────────────────────────────────────────

  "SlaScalingPolicy.availabilityBased" should "scale when availability is near SLA threshold" in {
    val policy = SlaScalingPolicy.availabilityBased(
      minAvailability = 95.0,
      triggerBuffer = 2.0,
      vmTemplate = vmTemplate
    )
    // 90/100 = 90% < 95+2 = 97%
    val ctx = SlaScalingContext(
      baseCtx,
      completedWorkloads = 90,
      failedWorkloads = 10,
      avgCompletionTime = SimTime(50.0),
      maxCompletionTime = SimTime(80.0),
      slaViolationCount = 0
    )
    policy(ctx) shouldBe defined
  }

  it should "not scale when availability is well above threshold" in {
    val policy = SlaScalingPolicy.availabilityBased(
      minAvailability = 95.0,
      triggerBuffer = 2.0,
      vmTemplate = vmTemplate
    )
    // 99/100 = 99% > 97%
    val ctx = SlaScalingContext(
      baseCtx,
      completedWorkloads = 99,
      failedWorkloads = 1,
      avgCompletionTime = SimTime(50.0),
      maxCompletionTime = SimTime(80.0),
      slaViolationCount = 0
    )
    policy(ctx) shouldBe None
  }

  // ─── violationBased ──────────────────────────────────────────────────

  "SlaScalingPolicy.violationBased" should "scale when violations exceed threshold" in {
    val policy = SlaScalingPolicy.violationBased(maxViolationsBeforeScale = 3, vmTemplate = vmTemplate)
    val ctx = SlaScalingContext(
      baseCtx,
      completedWorkloads = 10,
      failedWorkloads = 2,
      avgCompletionTime = SimTime(50.0),
      maxCompletionTime = SimTime(80.0),
      slaViolationCount = 5
    )
    policy(ctx) shouldBe defined
  }

  it should "not scale when violations are within tolerance" in {
    val policy = SlaScalingPolicy.violationBased(maxViolationsBeforeScale = 3, vmTemplate = vmTemplate)
    val ctx = SlaScalingContext(
      baseCtx,
      completedWorkloads = 10,
      failedWorkloads = 0,
      avgCompletionTime = SimTime(50.0),
      maxCompletionTime = SimTime(80.0),
      slaViolationCount = 2
    )
    policy(ctx) shouldBe None
  }

  // ─── composite ───────────────────────────────────────────────────────

  "SlaScalingPolicy.composite" should "trigger on first matching policy" in {
    val contract = SlaContract(
      SlaConstraint(SlaMetricName.TaskCompletionTime, maxValue = Some(100.0)),
      SlaConstraint(SlaMetricName.Availability, minValue = Some(95.0))
    )
    val policy = SlaScalingPolicy.composite(contract, vmTemplate, completionTimeTrigger = 0.8)
    // Completion time 85 > 80 (100*0.8) => triggers
    val ctx = SlaScalingContext(
      baseCtx,
      completedWorkloads = 50,
      failedWorkloads = 0,
      avgCompletionTime = SimTime(85.0),
      maxCompletionTime = SimTime(90.0),
      slaViolationCount = 0
    )
    policy(ctx) shouldBe defined
  }

  it should "not trigger when all metrics are healthy" in {
    val contract = SlaContract(
      SlaConstraint(SlaMetricName.TaskCompletionTime, maxValue = Some(100.0)),
      SlaConstraint(SlaMetricName.Availability, minValue = Some(95.0))
    )
    val policy = SlaScalingPolicy.composite(contract, vmTemplate)
    // 50/50 = 100% availability, avgTime=30 < 80
    val ctx = SlaScalingContext(
      baseCtx,
      completedWorkloads = 50,
      failedWorkloads = 0,
      avgCompletionTime = SimTime(30.0),
      maxCompletionTime = SimTime(40.0),
      slaViolationCount = 0
    )
    policy(ctx) shouldBe None
  }

  // ─── toHorizontalPolicy ─────────────────────────────────────────────

  "SlaScalingPolicy.toHorizontalPolicy" should "adapt SLA policy for use in HorizontalScalingContext" in {
    val slaPolicy = SlaScalingPolicy.violationBased(maxViolationsBeforeScale = 0, vmTemplate = vmTemplate)
    val adapted   = SlaScalingPolicy.toHorizontalPolicy(slaPolicy, slaViolationCount = () => 5)
    adapted(baseCtx) shouldBe defined
  }

  "SlaScalingPolicy.none" should "never trigger" in {
    val ctx = SlaScalingContext(
      baseCtx,
      completedWorkloads = 100,
      failedWorkloads = 50,
      avgCompletionTime = SimTime(999.0),
      maxCompletionTime = SimTime(999.0),
      slaViolationCount = 100
    )
    SlaScalingPolicy.none(ctx) shouldBe None
  }
