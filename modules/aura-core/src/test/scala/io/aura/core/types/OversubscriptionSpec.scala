// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OversubscriptionSpec extends AnyFlatSpec with Matchers:

  private val hostSpec = ResourceSpec(PEs(8), MIPS(10000.0), MegaBytes(16384.0), Mbps(10000.0), MegaBytes(500000.0))
  private val vmSpec   = ResourceSpec(PEs(2), MIPS(2500.0), MegaBytes(4096.0), Mbps(2500.0), MegaBytes(100000.0))

  // ─── OversubscriptionConfig Presets ────────────────────────────────

  "OversubscriptionConfig.none" should "have ratio 1.0 for all resources" in {
    val config = OversubscriptionConfig.none
    config.ramOversubscriptionRatio shouldBe 1.0
    config.bwOversubscriptionRatio shouldBe 1.0
    config.cpuOversubscriptionRatio shouldBe 1.0
  }

  "OversubscriptionConfig.conservative" should "have moderate ratios" in {
    val config = OversubscriptionConfig.conservative
    config.ramOversubscriptionRatio shouldBe 1.25
    config.cpuOversubscriptionRatio shouldBe 1.5
  }

  "OversubscriptionConfig.aggressive" should "have high ratios" in {
    val config = OversubscriptionConfig.aggressive
    config.ramOversubscriptionRatio shouldBe 2.0
    config.cpuOversubscriptionRatio shouldBe 4.0
  }

  // ─── No Oversubscription ───────────────────────────────────────────

  "OversubscribableResources without oversubscription" should "behave like standard resources" in {
    val res = OversubscribableResources.fromSpec(hostSpec)
    res.canFit(vmSpec) shouldBe true
  }

  it should "reject when physical capacity exceeded" in {
    val res       = OversubscribableResources.fromSpec(hostSpec)
    val allocated = res.allocate(vmSpec).allocate(vmSpec).allocate(vmSpec).allocate(vmSpec) // 4 VMs = 16GB
    allocated.canFit(vmSpec) shouldBe false // 5th VM would need 20GB > 16GB
  }

  // ─── With Oversubscription ─────────────────────────────────────────

  "OversubscribableResources with conservative oversubscription" should "allow more VMs than physical capacity" in {
    val res       = OversubscribableResources.fromSpec(hostSpec, OversubscriptionConfig.conservative)
    val allocated = res.allocate(vmSpec).allocate(vmSpec).allocate(vmSpec).allocate(vmSpec)
    // 4 VMs = 16GB RAM, physical = 16GB, effective = 16 * 1.25 = 20GB
    allocated.canFit(vmSpec) shouldBe true // 5th VM fits with oversub
  }

  "OversubscribableResources with aggressive oversubscription" should "allow many more VMs" in {
    // Use small storage so it doesn't hit the non-oversubscribed storage limit
    val smallVm = vmSpec.copy(storage = MegaBytes(10000.0))
    val res     = OversubscribableResources.fromSpec(hostSpec, OversubscriptionConfig.aggressive)
    // RAM oversub 2x: 16GB * 2 = 32GB effective → 8 VMs of 4GB each
    var current = res
    for _ <- 0 until 7 do current = current.allocate(smallVm)
    current.canFit(smallVm) shouldBe true // 8th VM fits
  }

  // ─── Effective Capacity ────────────────────────────────────────────

  "effectiveCapacity" should "scale with oversubscription ratio" in {
    val res = OversubscribableResources.fromSpec(hostSpec, OversubscriptionConfig.aggressive)
    val cap = res.effectiveCapacity
    cap.ram.value shouldBe 32768.0 +- 0.1 // 16384 * 2.0
    cap.pes.value shouldBe 32             // 8 * 4.0
  }

  it should "not scale storage" in {
    val res = OversubscribableResources.fromSpec(hostSpec, OversubscriptionConfig.aggressive)
    res.effectiveCapacity.storage shouldBe hostSpec.storage
  }

  // ─── Oversubscription Detection ────────────────────────────────────

  "isRamOversubscribed" should "detect when allocated exceeds physical" in {
    val res = OversubscribableResources.fromSpec(hostSpec, OversubscriptionConfig.aggressive)
    val allocated =
      res.allocate(vmSpec).allocate(vmSpec).allocate(vmSpec).allocate(vmSpec).allocate(vmSpec) // 5 VMs = 20GB > 16GB
    allocated.isRamOversubscribed shouldBe true
  }

  it should "return false when within physical capacity" in {
    val res       = OversubscribableResources.fromSpec(hostSpec, OversubscriptionConfig.aggressive)
    val allocated = res.allocate(vmSpec).allocate(vmSpec).allocate(vmSpec) // 3 VMs = 12GB < 16GB
    allocated.isRamOversubscribed shouldBe false
  }

  "isCpuOversubscribed" should "detect CPU overcommit" in {
    val res     = OversubscribableResources.fromSpec(hostSpec, OversubscriptionConfig.aggressive)
    var current = res
    for _ <- 0 until 5 do current = current.allocate(vmSpec) // 5 * 2 PEs = 10 > 8 physical
    current.isCpuOversubscribed shouldBe true
  }

  // ─── Performance Degradation ───────────────────────────────────────

  "memoryPerformanceFactor" should "return 1.0 when not oversubscribed" in {
    val res       = OversubscribableResources.fromSpec(hostSpec, OversubscriptionConfig.conservative)
    val allocated = res.allocate(vmSpec) // 4GB < 16GB
    allocated.memoryPerformanceFactor shouldBe 1.0
  }

  it should "return less than 1.0 when RAM is oversubscribed" in {
    val res     = OversubscribableResources.fromSpec(hostSpec, OversubscriptionConfig.aggressive)
    var current = res
    for _ <- 0 until 5 do current = current.allocate(vmSpec) // 20GB > 16GB
    current.memoryPerformanceFactor should be < 1.0
  }

  it should "decrease with more oversubscription" in {
    val res    = OversubscribableResources.fromSpec(hostSpec, OversubscriptionConfig.aggressive)
    var slight = res
    for _ <- 0 until 5 do slight = slight.allocate(vmSpec) // 20GB
    var heavy = res
    for _ <- 0 until 7 do heavy = heavy.allocate(vmSpec) // 28GB
    heavy.memoryPerformanceFactor should be < slight.memoryPerformanceFactor
  }

  // ─── Effective MIPS ────────────────────────────────────────────────

  "effectiveMips" should "return full MIPS when not oversubscribed" in {
    val res       = OversubscribableResources.fromSpec(hostSpec, OversubscriptionConfig.conservative)
    val allocated = res.allocate(vmSpec)
    allocated.effectiveMips(vmSpec) shouldBe vmSpec.mips
  }

  it should "reduce MIPS proportionally when CPU oversubscribed" in {
    val res     = OversubscribableResources.fromSpec(hostSpec, OversubscriptionConfig.aggressive)
    var current = res
    for _ <- 0 until 5 do current = current.allocate(vmSpec) // 5 * 2500 = 12500 MIPS > 10000
    val effective = current.effectiveMips(vmSpec)
    effective.value should be < vmSpec.mips.value
    effective.value should be > 0.0
  }

  // ─── Effective Bandwidth ───────────────────────────────────────────

  "effectiveBandwidth" should "return full BW when not oversubscribed" in {
    val res       = OversubscribableResources.fromSpec(hostSpec, OversubscriptionConfig.conservative)
    val allocated = res.allocate(vmSpec)
    allocated.effectiveBandwidth(vmSpec) shouldBe vmSpec.bw
  }

  it should "reduce BW when oversubscribed" in {
    val res     = OversubscribableResources.fromSpec(hostSpec, OversubscriptionConfig.aggressive)
    var current = res
    for _ <- 0 until 5 do current = current.allocate(vmSpec) // 5 * 2500 = 12500 > 10000
    val effective = current.effectiveBandwidth(vmSpec)
    effective.value should be < vmSpec.bw.value
  }

  // ─── Allocate / Release ────────────────────────────────────────────

  "allocate then release" should "restore to original state" in {
    val res       = OversubscribableResources.fromSpec(hostSpec, OversubscriptionConfig.conservative)
    val allocated = res.allocate(vmSpec)
    val released  = allocated.release(vmSpec)
    released.allocated.ram.value shouldBe 0.0 +- 0.001
    released.allocated.pes.value shouldBe 0
  }

  // ─── RAM Utilization ───────────────────────────────────────────────

  "ramUtilization" should "be zero with no allocation" in {
    val res = OversubscribableResources.fromSpec(hostSpec)
    res.ramUtilization shouldBe 0.0
  }

  it should "exceed 1.0 when oversubscribed" in {
    val res     = OversubscribableResources.fromSpec(hostSpec, OversubscriptionConfig.aggressive)
    var current = res
    for _ <- 0 until 5 do current = current.allocate(vmSpec)
    current.ramUtilization should be > 1.0
  }
