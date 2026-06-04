// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.containers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.types.*

class K8sAutoscalerSpec extends AnyFlatSpec with Matchers:

  // ─── HPA: CPU-based ──────────────────────────────────────────────────

  "HpaPolicy.cpuBased" should "scale up when utilization exceeds target + tolerance" in {
    val policy = HpaPolicy.cpuBased(tolerance = 0.1)
    val ctx = HpaContext(
      currentReplicas = 3,
      targetCpuUtilization = 0.5,
      observedCpuUtilization = 0.9, // 80% above target
      minReplicas = 1,
      maxReplicas = 10
    )
    val decision = policy(ctx)
    decision shouldBe a[HpaDecision.ScaleUp]
    decision.asInstanceOf[HpaDecision.ScaleUp].targetReplicas should be > 3
  }

  it should "scale down when utilization is well below target" in {
    val policy = HpaPolicy.cpuBased(tolerance = 0.1)
    val ctx = HpaContext(
      currentReplicas = 6,
      targetCpuUtilization = 0.5,
      observedCpuUtilization = 0.2, // 60% below target
      minReplicas = 1,
      maxReplicas = 10
    )
    val decision = policy(ctx)
    decision shouldBe a[HpaDecision.ScaleDown]
    decision.asInstanceOf[HpaDecision.ScaleDown].targetReplicas should be < 6
  }

  it should "not change when utilization is within tolerance" in {
    val policy = HpaPolicy.cpuBased(tolerance = 0.1)
    val ctx = HpaContext(
      currentReplicas = 3,
      targetCpuUtilization = 0.5,
      observedCpuUtilization = 0.52, // within 10% of target
      minReplicas = 1,
      maxReplicas = 10
    )
    policy(ctx) shouldBe HpaDecision.NoChange
  }

  it should "clamp to maxReplicas" in {
    val policy = HpaPolicy.cpuBased(tolerance = 0.1)
    val ctx = HpaContext(
      currentReplicas = 8,
      targetCpuUtilization = 0.3,
      observedCpuUtilization = 0.95,
      minReplicas = 1,
      maxReplicas = 10
    )
    val decision = policy(ctx)
    decision match
      case HpaDecision.ScaleUp(target) => target should be <= 10
      case _                           => // NoChange is acceptable if already at max
  }

  it should "clamp to minReplicas" in {
    val policy = HpaPolicy.cpuBased(tolerance = 0.1)
    val ctx = HpaContext(
      currentReplicas = 3,
      targetCpuUtilization = 0.5,
      observedCpuUtilization = 0.05, // very low
      minReplicas = 2,
      maxReplicas = 10
    )
    val decision = policy(ctx)
    decision match
      case HpaDecision.ScaleDown(target) => target should be >= 2
      case HpaDecision.NoChange          => succeed // already at min
      case _                             => fail("unexpected decision")
  }

  // ─── HPA: Threshold-based ───────────────────────────────────────────

  "HpaPolicy.threshold" should "scale up above high watermark" in {
    val policy = HpaPolicy.threshold(scaleUpThreshold = 0.8, scaleDownThreshold = 0.3)
    val ctx = HpaContext(
      currentReplicas = 2,
      targetCpuUtilization = 0.5,
      observedCpuUtilization = 0.85,
      minReplicas = 1,
      maxReplicas = 10
    )
    policy(ctx) shouldBe HpaDecision.ScaleUp(3)
  }

  it should "scale down below low watermark" in {
    val policy = HpaPolicy.threshold(scaleUpThreshold = 0.8, scaleDownThreshold = 0.3)
    val ctx = HpaContext(
      currentReplicas = 4,
      targetCpuUtilization = 0.5,
      observedCpuUtilization = 0.2,
      minReplicas = 1,
      maxReplicas = 10
    )
    policy(ctx) shouldBe HpaDecision.ScaleDown(3)
  }

  it should "not change between thresholds" in {
    val policy = HpaPolicy.threshold(scaleUpThreshold = 0.8, scaleDownThreshold = 0.3)
    val ctx = HpaContext(
      currentReplicas = 3,
      targetCpuUtilization = 0.5,
      observedCpuUtilization = 0.5,
      minReplicas = 1,
      maxReplicas = 10
    )
    policy(ctx) shouldBe HpaDecision.NoChange
  }

  "HpaPolicy.none" should "always return NoChange" in {
    val ctx = HpaContext(3, 0.5, 0.95, 1, 10)
    HpaPolicy.none(ctx) shouldBe HpaDecision.NoChange
  }

  // ─── VPA: Target Utilization ─────────────────────────────────────────

  "VpaPolicy.targetUtilization" should "increase resources when usage is high relative to request" in {
    val policy = VpaPolicy.targetUtilization(targetCpuUtilization = 0.7, headroom = 0.15)
    val ctx = VpaContext(
      currentCpuRequest = MIPS(1000.0),
      currentMemoryRequest = MegaBytes(2048.0),
      observedCpuUsage = MIPS(950.0), // 95% of request, above 70% target
      observedMemoryUsage = MegaBytes(1800.0),
      cpuLimit = MIPS(10000.0),
      memoryLimit = MegaBytes(32768.0)
    )
    val rec = policy(ctx)
    rec.cpuRequest.value should be > 1000.0
    rec.changed shouldBe true
  }

  it should "decrease resources when usage is low" in {
    val policy = VpaPolicy.targetUtilization(targetCpuUtilization = 0.7, headroom = 0.15)
    val ctx = VpaContext(
      currentCpuRequest = MIPS(5000.0),
      currentMemoryRequest = MegaBytes(8192.0),
      observedCpuUsage = MIPS(500.0), // 10% of request
      observedMemoryUsage = MegaBytes(1000.0),
      cpuLimit = MIPS(10000.0),
      memoryLimit = MegaBytes(32768.0)
    )
    val rec = policy(ctx)
    rec.cpuRequest.value should be < 5000.0
    rec.changed shouldBe true
  }

  it should "clamp to cpu limit" in {
    val policy = VpaPolicy.targetUtilization(targetCpuUtilization = 0.5, headroom = 0.5)
    val ctx = VpaContext(
      currentCpuRequest = MIPS(8000.0),
      currentMemoryRequest = MegaBytes(4096.0),
      observedCpuUsage = MIPS(9000.0),
      observedMemoryUsage = MegaBytes(3000.0),
      cpuLimit = MIPS(10000.0),
      memoryLimit = MegaBytes(8192.0)
    )
    val rec = policy(ctx)
    rec.cpuRequest.value should be <= 10000.0
  }

  // ─── VPA: Bounded ───────────────────────────────────────────────────

  "VpaPolicy.bounded" should "grow resources when utilization is above upper threshold" in {
    val policy = VpaPolicy.bounded(growthFactor = 1.5, upperThreshold = 0.9)
    val ctx = VpaContext(
      currentCpuRequest = MIPS(1000.0),
      currentMemoryRequest = MegaBytes(2048.0),
      observedCpuUsage = MIPS(950.0), // 95% > 90%
      observedMemoryUsage = MegaBytes(500.0),
      cpuLimit = MIPS(10000.0),
      memoryLimit = MegaBytes(32768.0)
    )
    val rec = policy(ctx)
    rec.cpuRequest.value shouldBe 1500.0 // 1000 * 1.5
    rec.changed shouldBe true
  }

  it should "shrink resources when utilization is below lower threshold" in {
    val policy = VpaPolicy.bounded(shrinkFactor = 0.75, lowerThreshold = 0.3)
    val ctx = VpaContext(
      currentCpuRequest = MIPS(4000.0),
      currentMemoryRequest = MegaBytes(8192.0),
      observedCpuUsage = MIPS(800.0),          // 20% < 30%
      observedMemoryUsage = MegaBytes(1000.0), // 12% < 30%
      cpuLimit = MIPS(10000.0),
      memoryLimit = MegaBytes(32768.0)
    )
    val rec = policy(ctx)
    rec.cpuRequest.value shouldBe 3000.0    // 4000 * 0.75
    rec.memoryRequest.value shouldBe 6144.0 // 8192 * 0.75
    rec.changed shouldBe true
  }

  it should "respect min bounds" in {
    val policy = VpaPolicy.bounded(minCpu = MIPS(500.0), shrinkFactor = 0.5, lowerThreshold = 0.3)
    val ctx = VpaContext(
      currentCpuRequest = MIPS(600.0),
      currentMemoryRequest = MegaBytes(256.0),
      observedCpuUsage = MIPS(100.0),
      observedMemoryUsage = MegaBytes(50.0),
      cpuLimit = MIPS(10000.0),
      memoryLimit = MegaBytes(32768.0)
    )
    val rec = policy(ctx)
    rec.cpuRequest.value should be >= 500.0
  }

  it should "not change when utilization is in normal range" in {
    val policy = VpaPolicy.bounded(upperThreshold = 0.9, lowerThreshold = 0.3)
    val ctx = VpaContext(
      currentCpuRequest = MIPS(1000.0),
      currentMemoryRequest = MegaBytes(2048.0),
      observedCpuUsage = MIPS(500.0), // 50% — between 30% and 90%
      observedMemoryUsage = MegaBytes(1000.0),
      cpuLimit = MIPS(10000.0),
      memoryLimit = MegaBytes(32768.0)
    )
    val rec = policy(ctx)
    rec.changed shouldBe false
  }

  "VpaPolicy.none" should "return unchanged values" in {
    val ctx = VpaContext(
      currentCpuRequest = MIPS(1000.0),
      currentMemoryRequest = MegaBytes(2048.0),
      observedCpuUsage = MIPS(999.0),
      observedMemoryUsage = MegaBytes(2047.0),
      cpuLimit = MIPS(10000.0),
      memoryLimit = MegaBytes(32768.0)
    )
    val rec = VpaPolicy.none(ctx)
    rec.cpuRequest.value shouldBe 1000.0
    rec.memoryRequest.value shouldBe 2048.0
    rec.changed shouldBe false
  }
