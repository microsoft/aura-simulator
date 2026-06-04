// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CapacityPlanningSpec extends AnyFlatSpec with Matchers:

  private val vmSpec   = ResourceSpec(PEs(4), MIPS(2000.0), MegaBytes(8192.0), Mbps(1000.0), MegaBytes(50000.0))
  private val hostSpec = ResourceSpec(PEs(32), MIPS(20000.0), MegaBytes(65536.0), Mbps(10000.0), MegaBytes(500000.0))

  // ── ResourceUtilizationStats ────────────────────────────────────────────

  "ResourceUtilizationStats.fromSamples" should "compute correct statistics" in {
    val samples = (1 to 100).map(_.toDouble / 100.0).toVector
    val stats   = ResourceUtilizationStats.fromSamples(samples)
    stats.avg shouldBe 0.505 +- 0.01
    stats.min shouldBe 0.01
    stats.max shouldBe 1.0
    stats.p50 shouldBe 0.5 +- 0.02
    stats.p95 shouldBe 0.95 +- 0.02
    stats.sampleCount shouldBe 100
  }

  it should "handle single sample" in {
    val stats = ResourceUtilizationStats.fromSamples(Vector(0.75))
    stats.avg shouldBe 0.75
    stats.p50 shouldBe 0.75
    stats.max shouldBe 0.75
    stats.sampleCount shouldBe 1
  }

  it should "handle empty samples" in {
    val stats = ResourceUtilizationStats.fromSamples(Vector.empty)
    stats.sampleCount shouldBe 0
    stats.avg shouldBe 0.0
  }

  // ── Right-sizing ────────────────────────────────────────────────────────

  "CapacityPlanningEngine.rightSize" should "recommend terminate for idle VMs" in {
    val stats = ResourceUtilizationStats.fromSamples(Vector(0.01, 0.02, 0.01, 0.03))
    val rec   = CapacityPlanningEngine.rightSize(vmSpec, stats)
    rec shouldBe a[RightSizingRecommendation.Terminate]
  }

  it should "recommend downsize for over-provisioned VMs" in {
    val stats = ResourceUtilizationStats.fromSamples(Vector.fill(100)(0.2))
    val rec   = CapacityPlanningEngine.rightSize(vmSpec, stats)
    rec shouldBe a[RightSizingRecommendation.Downsize]
    rec match
      case RightSizingRecommendation.Downsize(_, recommended, savings) =>
        recommended.mips.value should be < vmSpec.mips.value
        savings should be > 0.0
      case _ => fail("Expected Downsize")
  }

  it should "recommend upsize for under-provisioned VMs" in {
    val stats = ResourceUtilizationStats.fromSamples(Vector.fill(100)(0.92))
    val rec   = CapacityPlanningEngine.rightSize(vmSpec, stats)
    rec shouldBe a[RightSizingRecommendation.Upsize]
    rec match
      case RightSizingRecommendation.Upsize(_, recommended) =>
        recommended.mips.value should be > vmSpec.mips.value
      case _ => fail("Expected Upsize")
  }

  it should "recommend no change for well-sized VMs" in {
    val stats = ResourceUtilizationStats.fromSamples(Vector.fill(100)(0.55))
    val rec   = CapacityPlanningEngine.rightSize(vmSpec, stats)
    rec shouldBe a[RightSizingRecommendation.NoChange]
  }

  it should "recommend terminate for empty stats" in {
    val stats = ResourceUtilizationStats.fromSamples(Vector.empty)
    val rec   = CapacityPlanningEngine.rightSize(vmSpec, stats)
    rec shouldBe a[RightSizingRecommendation.Terminate]
  }

  // ── Forecasting ─────────────────────────────────────────────────────────

  "CapacityPlanningEngine.forecastUtilization" should "predict increasing trend" in {
    val history  = (0 until 10).map(i => (SimTime(i.toDouble * 10), Utilization(0.1 + i * 0.05))).toVector
    val forecast = CapacityPlanningEngine.forecastUtilization(history, SimTime(100.0))
    forecast.slope should be > 0.0
    forecast.predictedUtilization should be > forecast.currentUtilization
  }

  it should "predict exhaustion for growing workload" in {
    val history  = (0 until 10).map(i => (SimTime(i.toDouble), Utilization(0.1 + i * 0.08))).toVector
    val forecast = CapacityPlanningEngine.forecastUtilization(history, SimTime(20.0))
    forecast.timeToExhaustion shouldBe defined
  }

  it should "predict no exhaustion for stable workload" in {
    val history  = (0 until 10).map(i => (SimTime(i.toDouble), Utilization(0.5))).toVector
    val forecast = CapacityPlanningEngine.forecastUtilization(history, SimTime(100.0))
    forecast.slope shouldBe 0.0 +- 0.001
    forecast.timeToExhaustion shouldBe None
  }

  it should "handle single data point" in {
    val history  = Vector((SimTime(0.0), Utilization(0.6)))
    val forecast = CapacityPlanningEngine.forecastUtilization(history, SimTime(100.0))
    forecast.currentUtilization shouldBe 0.6
    forecast.predictedUtilization shouldBe 0.6
    forecast.slope shouldBe 0.0
  }

  it should "clamp predicted utilization to [0, 1]" in {
    val history  = (0 until 5).map(i => (SimTime(i.toDouble), Utilization(0.8 + i * 0.05))).toVector
    val forecast = CapacityPlanningEngine.forecastUtilization(history, SimTime(100.0))
    forecast.predictedUtilization should be <= 1.0
  }

  // ── Bin packing ─────────────────────────────────────────────────────────

  "CapacityPlanningEngine.binPackHosts" should "pack VMs into minimum hosts" in {
    // 10 VMs @ 2000 MIPS each = 20000 total, hostSpec has 20000 MIPS => 1 host
    val vms = Vector.fill(10)(vmSpec)
    CapacityPlanningEngine.binPackHosts(vms, hostSpec) shouldBe 1
  }

  it should "require more hosts when VMs exceed capacity" in {
    val vms = Vector.fill(11)(vmSpec) // 22000 > 20000, need 2 hosts
    CapacityPlanningEngine.binPackHosts(vms, hostSpec) shouldBe 2
  }

  it should "return 0 for empty VM list" in {
    CapacityPlanningEngine.binPackHosts(Vector.empty, hostSpec) shouldBe 0
  }

  // ── Wasted resources ───────────────────────────────────────────────────

  "CapacityPlanningEngine.wastedResources" should "compute idle resources" in {
    val stats50 = ResourceUtilizationStats.fromSamples(Vector.fill(10)(0.5))
    val hosts   = Vector((hostSpec, stats50), (hostSpec, stats50))
    val wasted  = CapacityPlanningEngine.wastedResources(hosts)
    wasted.mips.value shouldBe 20000.0 +- 1.0 // 50% of 2 * 20000
  }

  // ── recommendHostCount ─────────────────────────────────────────────────

  "CapacityPlanningEngine.recommendHostCount" should "include headroom" in {
    val vms   = Vector.fill(10)(vmSpec)
    val count = CapacityPlanningEngine.recommendHostCount(vms, hostSpec, headroom = 0.5)
    count should be > 1 // 1 host + 50% headroom = 2
  }
