// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BootModelSpec extends AnyFlatSpec with Matchers:

  // ─── HostBootConfig Presets ─────────────────────────────────────────

  "HostBootConfig.fast" should "have shorter delay than standard" in {
    HostBootConfig.fast.startupDelay.value should be < HostBootConfig.standard.startupDelay.value
  }

  "HostBootConfig.slow" should "have longer delay than standard" in {
    HostBootConfig.slow.startupDelay.value should be > HostBootConfig.standard.startupDelay.value
  }

  "HostBootConfig.instant" should "have zero delays" in {
    HostBootConfig.instant.startupDelay shouldBe SimTime.Zero
    HostBootConfig.instant.shutdownDelay shouldBe SimTime.Zero
    HostBootConfig.instant.bootPowerFraction shouldBe 0.0
  }

  "HostBootConfig.standard" should "have reasonable defaults" in {
    HostBootConfig.standard.startupDelay.value shouldBe 60.0
    HostBootConfig.standard.shutdownDelay.value shouldBe 10.0
    HostBootConfig.standard.bootPowerFraction shouldBe 0.5
  }

  // ─── VmBootConfig Presets ───────────────────────────────────────────

  "VmBootConfig.light" should "have shorter delay than standard" in {
    VmBootConfig.light.startupDelay.value should be < VmBootConfig.standard.startupDelay.value
  }

  "VmBootConfig.heavy" should "have longer delay than standard" in {
    VmBootConfig.heavy.startupDelay.value should be > VmBootConfig.standard.startupDelay.value
  }

  "VmBootConfig.instant" should "have zero delays" in {
    VmBootConfig.instant.startupDelay shouldBe SimTime.Zero
    VmBootConfig.instant.shutdownDelay shouldBe SimTime.Zero
  }

  // ─── HostLifecycleState ─────────────────────────────────────────────

  "HostLifecycleState.isReady" should "be true only for Running" in {
    HostLifecycleState.isReady(HostLifecycleState.Running) shouldBe true
    HostLifecycleState.isReady(HostLifecycleState.Off) shouldBe false
    HostLifecycleState.isReady(HostLifecycleState.Booting(SimTime(0.0), SimTime(60.0))) shouldBe false
    HostLifecycleState.isReady(HostLifecycleState.ShuttingDown(SimTime(100.0), SimTime(110.0))) shouldBe false
  }

  "HostLifecycleState.isPowered" should "be false only for Off" in {
    HostLifecycleState.isPowered(HostLifecycleState.Off) shouldBe false
    HostLifecycleState.isPowered(HostLifecycleState.Running) shouldBe true
    HostLifecycleState.isPowered(HostLifecycleState.Booting(SimTime(0.0), SimTime(60.0))) shouldBe true
    HostLifecycleState.isPowered(HostLifecycleState.ShuttingDown(SimTime(100.0), SimTime(110.0))) shouldBe true
  }

  "HostLifecycleState.powerFraction" should "return boot fraction during booting" in {
    val config = HostBootConfig.standard
    val fraction = HostLifecycleState.powerFraction(
      HostLifecycleState.Booting(SimTime(0.0), SimTime(60.0)),
      config
    )
    fraction shouldBe 0.5
  }

  it should "return 1.0 when running" in {
    HostLifecycleState.powerFraction(HostLifecycleState.Running, HostBootConfig.standard) shouldBe 1.0
  }

  it should "return 0.0 when off" in {
    HostLifecycleState.powerFraction(HostLifecycleState.Off, HostBootConfig.standard) shouldBe 0.0
  }

  it should "return reduced fraction during shutdown" in {
    val config = HostBootConfig.standard
    val fraction = HostLifecycleState.powerFraction(
      HostLifecycleState.ShuttingDown(SimTime(100.0), SimTime(110.0)),
      config
    )
    fraction should be < config.bootPowerFraction
  }

  // ─── VmLifecycleState ──────────────────────────────────────────────

  "VmLifecycleState.isReady" should "be true only for Running" in {
    VmLifecycleState.isReady(VmLifecycleState.Running) shouldBe true
    VmLifecycleState.isReady(VmLifecycleState.Creating(SimTime(0.0), SimTime(10.0))) shouldBe false
    VmLifecycleState.isReady(VmLifecycleState.Destroyed) shouldBe false
  }

  // ─── Energy Calculations ───────────────────────────────────────────

  "bootEnergy" should "compute energy during boot" in {
    val maxPower = Watts(300.0)
    val config   = HostBootConfig.standard // 60s boot, 50% power
    val energy   = io.aura.core.types.bootEnergy(maxPower, config)
    // 300W * 0.5 * 60s / 3600 = 2.5 Wh
    energy.value shouldBe 2.5 +- 0.01
  }

  it should "return zero for instant boot" in {
    val energy = io.aura.core.types.bootEnergy(Watts(300.0), HostBootConfig.instant)
    energy.value shouldBe 0.0
  }

  "shutdownEnergy" should "be less than boot energy" in {
    val maxPower  = Watts(300.0)
    val config    = HostBootConfig.standard
    val bootE     = io.aura.core.types.bootEnergy(maxPower, config)
    val shutdownE = io.aura.core.types.shutdownEnergy(maxPower, config)
    shutdownE.value should be < bootE.value
  }

  it should "return zero for instant shutdown" in {
    val energy = io.aura.core.types.shutdownEnergy(Watts(300.0), HostBootConfig.instant)
    energy.value shouldBe 0.0
  }
