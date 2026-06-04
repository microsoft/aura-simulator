// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.gpu

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*

class GpuDvfsSpec extends AnyFlatSpec with Matchers:

  val a100   = GpuDeviceSpec.a100Sxm
  val budget = Watts(400.0)

  val computeHeavy = GpuActivityProfile(
    smUtilization = Utilization(0.9),
    hbmReadBWGBps = 200.0,
    hbmWriteBWGBps = 50.0,
    nvlinkUtilization = Utilization(0.1),
    currentFreqMHz = a100.frequencyMaxMHz
  )

  val memoryHeavy = GpuActivityProfile(
    smUtilization = Utilization(0.05),
    hbmReadBWGBps = a100.hbmBandwidthGBps * 0.7,
    hbmWriteBWGBps = a100.hbmBandwidthGBps * 0.05,
    nvlinkUtilization = Utilization(0.1),
    currentFreqMHz = a100.frequencyMaxMHz
  )

  "maxPerformance" should "always return max frequency" in {
    DvfsPolicy.maxPerformance(a100, computeHeavy, budget) shouldBe a100.frequencyMaxMHz
    DvfsPolicy.maxPerformance(a100, memoryHeavy, budget) shouldBe a100.frequencyMaxMHz
    DvfsPolicy.maxPerformance(a100, GpuActivityProfile.idle, budget) shouldBe a100.frequencyMaxMHz
  }

  "powerCapped" should "return max frequency when under budget" in {
    val lowBudget = Watts(500.0) // well above idle power
    DvfsPolicy.powerCapped(
      a100,
      GpuActivityProfile.idle.copy(currentFreqMHz = a100.frequencyMaxMHz),
      lowBudget
    ) shouldBe a100.frequencyMaxMHz
  }

  it should "reduce frequency when over budget" in {
    val tightBudget = Watts(100.0)
    val freq        = DvfsPolicy.powerCapped(a100, computeHeavy, tightBudget)
    freq should be < a100.frequencyMaxMHz
    freq should be >= a100.frequencyMinMHz
  }

  "energyProportional" should "give lower frequency for memory-bound workloads" in {
    val computeFreq = DvfsPolicy.energyProportional(a100, computeHeavy, budget)
    val memoryFreq  = DvfsPolicy.energyProportional(a100, memoryHeavy, budget)
    computeFreq should be > memoryFreq
  }

  it should "stay within valid frequency range" in {
    val freq = DvfsPolicy.energyProportional(a100, computeHeavy, budget)
    freq should be >= a100.frequencyMinMHz
    freq should be <= a100.frequencyMaxMHz
  }

  "phaseAware" should "use max frequency during prefill" in {
    val freq = DvfsPolicy.phaseAware(isPrefillPhase = true)(a100, computeHeavy, budget)
    freq shouldBe a100.frequencyMaxMHz
  }

  it should "reduce frequency during decode" in {
    val freq = DvfsPolicy.phaseAware(isPrefillPhase = false)(a100, memoryHeavy, budget)
    freq should be < a100.frequencyMaxMHz
  }

  it should "not go below 40% of max during decode" in {
    val freq  = DvfsPolicy.phaseAware(isPrefillPhase = false)(a100, memoryHeavy, budget)
    val floor = a100.frequencyMinMHz + ((a100.frequencyMaxMHz - a100.frequencyMinMHz) * 0.4).toInt
    freq should be >= floor
  }

  "fixed" should "always return the specified frequency" in {
    val freq = 1000
    DvfsPolicy.fixed(freq)(a100, computeHeavy, budget) shouldBe freq
    DvfsPolicy.fixed(freq)(a100, memoryHeavy, budget) shouldBe freq
  }

  it should "clamp to valid range" in {
    DvfsPolicy.fixed(9999)(a100, computeHeavy, budget) shouldBe a100.frequencyMaxMHz
    DvfsPolicy.fixed(1)(a100, computeHeavy, budget) shouldBe a100.frequencyMinMHz
  }
