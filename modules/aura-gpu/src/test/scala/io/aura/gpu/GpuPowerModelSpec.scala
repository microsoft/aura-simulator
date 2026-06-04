// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.gpu

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*

class GpuPowerModelSpec extends AnyFlatSpec with Matchers:

  val a100 = GpuDeviceSpec.a100Sxm

  val idleActivity = GpuActivityProfile.idle.copy(currentFreqMHz = a100.frequencyMaxMHz)

  val fullActivity = GpuActivityProfile(
    smUtilization = Utilization.Full,
    hbmReadBWGBps = a100.hbmBandwidthGBps * 0.7,
    hbmWriteBWGBps = a100.hbmBandwidthGBps * 0.1,
    nvlinkUtilization = Utilization(0.5),
    currentFreqMHz = a100.frequencyMaxMHz
  )

  val prefillActivity = GpuActivityProfile(
    smUtilization = Utilization(0.85),
    hbmReadBWGBps = a100.hbmBandwidthGBps * 0.3,
    hbmWriteBWGBps = a100.hbmBandwidthGBps * 0.05,
    nvlinkUtilization = Utilization(0.2),
    currentFreqMHz = a100.frequencyMaxMHz
  )

  val decodeActivity = GpuActivityProfile(
    smUtilization = Utilization(0.05),
    hbmReadBWGBps = a100.hbmBandwidthGBps * 0.7,
    hbmWriteBWGBps = a100.hbmBandwidthGBps * 0.05,
    nvlinkUtilization = Utilization(0.1),
    currentFreqMHz = a100.frequencyMaxMHz
  )

  "MultiComponentGpuPower" should "return idle power for idle activity" in {
    val components = MultiComponentGpuPower.calculate(a100, idleActivity)
    components.idleWatts shouldBe a100.idleWatts
    components.computeWatts.value shouldBe 0.0 +- 0.01
    components.total.value shouldBe a100.idleWatts.value +- 1.0
  }

  it should "return higher power for full activity" in {
    val components = MultiComponentGpuPower.calculate(a100, fullActivity)
    components.total.value should be > a100.idleWatts.value
    components.total.value should be <= a100.tdpWatts.value * 1.1
  }

  it should "have compute as dominant component during prefill" in {
    val components = MultiComponentGpuPower.calculate(a100, prefillActivity)
    components.computeWatts.value should be > components.hbmWatts.value
  }

  it should "have HBM as significant component during decode" in {
    val components = MultiComponentGpuPower.calculate(a100, decodeActivity)
    // Decode is memory-bound: HBM should be a large fraction
    components.hbmWatts.value should be > components.computeWatts.value
  }

  it should "break down into five components that sum to total" in {
    val components = MultiComponentGpuPower.calculate(a100, fullActivity)
    val sum = components.computeWatts.value + components.hbmWatts.value +
      components.nvlinkWatts.value + components.pcieWatts.value + components.idleWatts.value
    components.total.value shouldBe sum +- 0.001
  }

  it should "scale power with frequency (DVFS)" in {
    val highFreq  = fullActivity.copy(currentFreqMHz = a100.frequencyMaxMHz)
    val lowFreq   = fullActivity.copy(currentFreqMHz = a100.frequencyMinMHz)
    val highPower = MultiComponentGpuPower.totalPower(a100, highFreq)
    val lowPower  = MultiComponentGpuPower.totalPower(a100, lowFreq)
    highPower.value should be > lowPower.value
  }

  "energy" should "calculate watt-hours correctly" in {
    val energyWh = MultiComponentGpuPower.energy(a100, fullActivity, 3600.0)
    val power    = MultiComponentGpuPower.totalPower(a100, fullActivity)
    energyWh.value shouldBe power.value +- 0.1
  }

  it should "return zero for zero duration" in {
    val energyWh = MultiComponentGpuPower.energy(a100, fullActivity, 0.0)
    energyWh.value shouldBe 0.0
  }

  "H100 power" should "be higher than A100 at same utilization" in {
    val h100 = GpuDeviceSpec.h100Sxm
    val activity = fullActivity.copy(
      hbmReadBWGBps = h100.hbmBandwidthGBps * 0.7,
      hbmWriteBWGBps = h100.hbmBandwidthGBps * 0.1,
      currentFreqMHz = h100.frequencyMaxMHz
    )
    val h100Power = MultiComponentGpuPower.totalPower(h100, activity)
    val a100Power = MultiComponentGpuPower.totalPower(a100, fullActivity)
    h100Power.value should be > a100Power.value
  }
