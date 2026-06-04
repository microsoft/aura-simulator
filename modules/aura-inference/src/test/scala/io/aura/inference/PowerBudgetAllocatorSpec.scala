// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.inference

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.inference.energy.PowerBudgetAllocator

class PowerBudgetAllocatorSpec extends AnyFlatSpec with Matchers:

  val a100   = GpuDeviceSpec.a100Sxm
  val devIds = (0 until 4).map(i => GpuDeviceId(i.toLong)).toVector

  "equalAllocation" should "distribute budget evenly" in {
    val allocs = PowerBudgetAllocator.equalAllocation(Watts(1200.0), 4, a100, devIds)
    allocs.size shouldBe 4
    allocs.foreach { a =>
      a.allocatedWatts.value shouldBe 300.0 +- 1.0
    }
  }

  it should "cap per-GPU budget at TDP" in {
    val allocs = PowerBudgetAllocator.equalAllocation(Watts(10000.0), 4, a100, devIds)
    allocs.foreach { a =>
      a.allocatedWatts.value should be <= a100.tdpWatts.value
    }
  }

  it should "set frequency within valid range" in {
    val allocs = PowerBudgetAllocator.equalAllocation(Watts(800.0), 4, a100, devIds)
    allocs.foreach { a =>
      a.targetFreqMHz should be >= a100.frequencyMinMHz
      a.targetFreqMHz should be <= a100.frequencyMaxMHz
    }
  }

  it should "handle empty device list" in {
    PowerBudgetAllocator.equalAllocation(Watts(400.0), 0, a100, Vector.empty) shouldBe empty
  }

  "proportionalAllocation" should "allocate more to higher-demand GPUs" in {
    val demands = Vector(
      (GpuDeviceId(0), 1.0),
      (GpuDeviceId(1), 3.0) // 3x more demand
    )
    val allocs = PowerBudgetAllocator.proportionalAllocation(Watts(800.0), demands, a100)
    allocs.size shouldBe 2
    allocs(1).allocatedWatts.value should be > allocs(0).allocatedWatts.value
  }

  it should "handle empty demands" in {
    PowerBudgetAllocator.proportionalAllocation(Watts(400.0), Vector.empty, a100) shouldBe empty
  }
