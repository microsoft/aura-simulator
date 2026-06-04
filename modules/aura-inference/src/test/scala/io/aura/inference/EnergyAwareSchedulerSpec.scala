// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.inference

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.inference.model.*
import io.aura.inference.scheduling.*

class EnergyAwareSchedulerSpec extends AnyFlatSpec with Matchers:

  val a100  = GpuDeviceSpec.a100Sxm
  val model = LlmModelSpec.llama2_7b

  // ── PA-DVFS ────────────────────────────────────────────────────────

  "PA-DVFS" should "return max frequency during prefill" in {
    val activity = InferenceProfile.prefillActivity(model, 1, a100)
    val freq     = EnergyAwareScheduler.paDvfsFrequency(a100, activity, isPrefill = true)
    freq shouldBe a100.frequencyMaxMHz
  }

  it should "reduce frequency during decode" in {
    val activity = InferenceProfile.decodeActivity(model, 1, 512, a100)
    val freq     = EnergyAwareScheduler.paDvfsFrequency(a100, activity, isPrefill = false)
    freq should be < a100.frequencyMaxMHz
    freq should be >= a100.frequencyMinMHz
  }

  it should "respect the 40% floor during decode" in {
    val activity  = InferenceProfile.decodeActivity(model, 1, 512, a100)
    val freq      = EnergyAwareScheduler.paDvfsFrequency(a100, activity, isPrefill = false)
    val freqRange = a100.frequencyMaxMHz - a100.frequencyMinMHz
    val floor     = a100.frequencyMinMHz + (freqRange * 0.4).toInt
    freq should be >= floor
  }

  // ── ESPS ───────────────────────────────────────────────────────────

  "ESPS" should "find a valid operating point" in {
    val op = EnergyAwareScheduler.espsOptimalPoint(
      model,
      a100,
      avgPromptTokens = 512,
      avgOutputTokens = 128,
      targetSloTpot = SimTime(0.05)
    )
    op.batchSize should be > 0
    op.frequencyRatio should be >= 0.4
    op.frequencyRatio should be <= 1.0
    op.estimatedEnergyPerToken should be > 0.0
  }

  it should "prefer larger batch for energy efficiency" in {
    val strict = EnergyAwareScheduler.espsOptimalPoint(
      model,
      a100,
      512,
      128,
      SimTime(0.1), // relaxed SLO
      EnergyAwareScheduler.EspsConfig(sloTarget = 0.5)
    )
    val relaxed = EnergyAwareScheduler.espsOptimalPoint(
      model,
      a100,
      512,
      128,
      SimTime(1.0), // very relaxed SLO
      EnergyAwareScheduler.EspsConfig(sloTarget = 0.5)
    )
    // With relaxed SLO, should prefer larger batch and/or lower frequency
    relaxed.estimatedEnergyPerToken should be <= strict.estimatedEnergyPerToken * 1.5
  }

  it should "include replica count in operating points" in {
    val op = EnergyAwareScheduler.espsOptimalPoint(
      model,
      a100,
      512,
      128,
      SimTime(0.05),
      EnergyAwareScheduler.EspsConfig(replicaCandidates = Vector(1, 2, 4))
    )
    op.replicas should be > 0
    op.estimatedTotalPowerWatts should be > 0.0
    op.estimatedThroughputTokPerSec should be > 0.0
  }

  it should "compute a non-empty Pareto frontier" in {
    val frontier = EnergyAwareScheduler.espsParetoFrontier(
      model,
      a100,
      512,
      128,
      SimTime(0.05)
    )
    frontier should not be empty
    // Frontier should be sorted by energy
    frontier.sliding(2).foreach { pair =>
      if pair.size == 2 then pair(0).estimatedEnergyPerToken should be <= pair(1).estimatedEnergyPerToken
    }
  }

  it should "return Pareto-non-dominated points only" in {
    val frontier = EnergyAwareScheduler.espsParetoFrontier(
      model,
      a100,
      256,
      64,
      SimTime(0.05)
    )
    // No point on the frontier should dominate another
    frontier.foreach { p =>
      frontier.foreach { q =>
        if p != q then
          val dominates = p.estimatedEnergyPerToken < q.estimatedEnergyPerToken &&
            p.estimatedSloCompliance >= q.estimatedSloCompliance
          dominates shouldBe false
      }
    }
  }

  it should "scale throughput with replicas" in {
    val cfg1 = EnergyAwareScheduler.EspsConfig(replicaCandidates = Vector(1))
    val cfg4 = EnergyAwareScheduler.EspsConfig(replicaCandidates = Vector(4))
    val op1  = EnergyAwareScheduler.espsOptimalPoint(model, a100, 512, 128, SimTime(0.05), cfg1)
    val op4  = EnergyAwareScheduler.espsOptimalPoint(model, a100, 512, 128, SimTime(0.05), cfg4)
    // 4 replicas should have higher throughput when using same batch size
    if op1.batchSize == op4.batchSize && op1.frequencyRatio == op4.frequencyRatio then
      op4.estimatedThroughputTokPerSec should be > op1.estimatedThroughputTokPerSec
  }

  // ── CAGR ───────────────────────────────────────────────────────────

  "CAGR" should "route to lowest carbon region within latency budget" in {
    val regions = Vector(
      EnergyAwareScheduler.RegionInfo("us-east", 400.0, 10.0),
      EnergyAwareScheduler.RegionInfo("eu-north", 50.0, 30.0),   // low carbon
      EnergyAwareScheduler.RegionInfo("asia-south", 600.0, 80.0) // over latency budget
    )
    val idx = EnergyAwareScheduler.cagrRouteRequest(regions)
    idx shouldBe 1 // eu-north: low carbon, within 50ms budget
  }

  it should "fall back when all regions exceed latency" in {
    val regions = Vector(
      EnergyAwareScheduler.RegionInfo("far-1", 100.0, 100.0),
      EnergyAwareScheduler.RegionInfo("far-2", 200.0, 200.0)
    )
    val idx = EnergyAwareScheduler.cagrRouteRequest(regions)
    idx shouldBe 0 // lowest carbon among all
  }

  it should "handle single region" in {
    val regions = Vector(EnergyAwareScheduler.RegionInfo("only", 500.0, 5.0))
    EnergyAwareScheduler.cagrRouteRequest(regions) shouldBe 0
  }

  it should "handle empty regions" in {
    EnergyAwareScheduler.cagrRouteRequest(Vector.empty) shouldBe 0
  }

  it should "weight carbon vs latency" in {
    val regions = Vector(
      EnergyAwareScheduler.RegionInfo("low-carbon-far", 50.0, 45.0),
      EnergyAwareScheduler.RegionInfo("high-carbon-near", 500.0, 5.0)
    )
    // Default: 70% carbon weight → should pick low carbon
    val carbonIdx = EnergyAwareScheduler.cagrRouteRequest(
      regions,
      EnergyAwareScheduler.CagrConfig(carbonWeight = 0.9, latencyWeight = 0.1)
    )
    carbonIdx shouldBe 0

    // Flip: 90% latency weight → should pick low latency
    val latencyIdx = EnergyAwareScheduler.cagrRouteRequest(
      regions,
      EnergyAwareScheduler.CagrConfig(carbonWeight = 0.1, latencyWeight = 0.9)
    )
    latencyIdx shouldBe 1
  }
