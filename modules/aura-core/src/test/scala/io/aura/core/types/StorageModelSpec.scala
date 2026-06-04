// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StorageModelSpec extends AnyFlatSpec with Matchers:

  // ─── StoragePerformance basics ────────────────────────────────────────

  "StoragePerformance.nvme" should "have highest throughput" in {
    StoragePerformance.nvme.readThroughputMBps should be > StoragePerformance.ssd.readThroughputMBps
    StoragePerformance.ssd.readThroughputMBps should be > StoragePerformance.hdd.readThroughputMBps
  }

  it should "have lowest latency" in {
    StoragePerformance.nvme.readLatencyUs should be < StoragePerformance.ssd.readLatencyUs
    StoragePerformance.ssd.readLatencyUs should be < StoragePerformance.hdd.readLatencyUs
  }

  "StoragePerformance.hdd" should "have lowest cost per GB" in {
    StoragePerformance.hdd.costPerGBMonth.value should be < StoragePerformance.ssd.costPerGBMonth.value
  }

  // ─── Sequential I/O time ─────────────────────────────────────────────

  "StoragePerformance.sequentialReadTime" should "compute time from throughput" in {
    val time = StoragePerformance.nvme.sequentialReadTime(MegaBytes(7000.0))
    // 7000 MB / 7000 MB/s = 1.0 second
    time.value shouldBe 1.0 +- 0.001
  }

  "StoragePerformance.sequentialWriteTime" should "be slower on HDD than NVMe" in {
    val data     = MegaBytes(1000.0)
    val nvmeTime = StoragePerformance.nvme.sequentialWriteTime(data)
    val hddTime  = StoragePerformance.hdd.sequentialWriteTime(data)
    hddTime.value should be > nvmeTime.value
  }

  // ─── Random I/O time ────────────────────────────────────────────────

  "StoragePerformance.randomReadTime" should "compute time from IOPS" in {
    val time = StoragePerformance.ssd.randomReadTime(100_000L)
    // 100,000 ops / 100,000 IOPS = 1.0 second
    time.value shouldBe 1.0 +- 0.001
  }

  it should "be much slower on HDD" in {
    val ops     = 10_000L
    val ssdTime = StoragePerformance.ssd.randomReadTime(ops)
    val hddTime = StoragePerformance.hdd.randomReadTime(ops)
    hddTime.value should be > (ssdTime.value * 100) // HDD ~600x slower for random IO
  }

  // ─── Cost ───────────────────────────────────────────────────────────

  "StoragePerformance.monthlyCost" should "compute cost per GB used" in {
    val cost = StoragePerformance.hdd.monthlyCost(MegaBytes(1024.0 * 100)) // 100 GB
    cost.value shouldBe 2.0 +- 0.001 // 100 GB * $0.02/GB
  }

  // ─── StorageWorkload ────────────────────────────────────────────────

  "StorageWorkload.estimateTime" should "sum all I/O times" in {
    val workload = StorageWorkload(
      sequentialReadMB = MegaBytes(1000.0),
      sequentialWriteMB = MegaBytes(500.0),
      randomReadOps = 10_000L,
      randomWriteOps = 5_000L
    )
    val time = workload.estimateTime(StoragePerformance.ssd)
    time.value should be > 0.0
  }

  it should "be faster on NVMe than HDD" in {
    val workload = StorageWorkload(
      sequentialReadMB = MegaBytes(500.0),
      randomReadOps = 50_000L
    )
    val nvmeTime = workload.estimateTime(StoragePerformance.nvme)
    val hddTime  = workload.estimateTime(StoragePerformance.hdd)
    nvmeTime.value should be < hddTime.value
  }

  it should "return zero for empty workload" in {
    val workload = StorageWorkload()
    workload.estimateTime(StoragePerformance.ssd).value shouldBe 0.0
  }

  // ─── TieredStorage ─────────────────────────────────────────────────

  "TieredStorage.standard" should "have 3 tiers" in {
    TieredStorage.standard.tiers should have size 3
  }

  "TieredStorage.bestTierForCapacity" should "return fastest tier with sufficient capacity" in {
    val tier = TieredStorage.standard.bestTierForCapacity(MegaBytes(1_000_000.0))
    tier shouldBe defined
    tier.get.tier shouldBe StorageTier.NVMe // fastest with enough capacity
  }

  it should "fall back to larger tier when capacity needed" in {
    val tier = TieredStorage.standard.bestTierForCapacity(MegaBytes(10_000_000.0))
    tier shouldBe defined
    tier.get.tier shouldBe StorageTier.HDD // only HDD has 16TB
  }

  it should "return None when no tier has enough capacity" in {
    val tier = TieredStorage.standard.bestTierForCapacity(MegaBytes(100_000_000.0))
    tier shouldBe None
  }

  "TieredStorage.cheapestTierForCapacity" should "return cheapest suitable tier" in {
    val tier = TieredStorage.standard.cheapestTierForCapacity(MegaBytes(1_000_000.0))
    tier shouldBe defined
    tier.get.tier shouldBe StorageTier.HDD // cheapest
  }

  "TieredStorage.totalCapacity" should "sum all tier capacities" in {
    val total = TieredStorage.standard.totalCapacity
    // 2TB + 4TB + 16TB = 22TB
    total.value shouldBe 22_000_000.0
  }
