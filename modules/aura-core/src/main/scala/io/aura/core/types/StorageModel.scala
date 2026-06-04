// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

/** Storage tier representing different storage technologies. */
enum StorageTier:
  case NVMe, SSD, HDD, NetworkAttached

/** Storage performance characteristics for a tier. */
final case class StoragePerformance(
    tier: StorageTier,
    readIops: Long,              // Random read IOPS
    writeIops: Long,             // Random write IOPS
    readThroughputMBps: Double,  // Sequential read MB/s
    writeThroughputMBps: Double, // Sequential write MB/s
    readLatencyUs: Double,       // Average read latency in microseconds
    writeLatencyUs: Double,      // Average write latency in microseconds
    capacityMB: MegaBytes,
    costPerGBMonth: Cost // $/GB/month
):
  /** Estimate time to read a given amount of data sequentially. */
  def sequentialReadTime(dataMB: MegaBytes): SimTime =
    if readThroughputMBps > 0 then SimTime(dataMB.value / readThroughputMBps)
    else SimTime.MaxValue

  /** Estimate time to write a given amount of data sequentially. */
  def sequentialWriteTime(dataMB: MegaBytes): SimTime =
    if writeThroughputMBps > 0 then SimTime(dataMB.value / writeThroughputMBps)
    else SimTime.MaxValue

  /** Estimate time for N random read operations. */
  def randomReadTime(ops: Long): SimTime =
    if readIops > 0 then SimTime(ops.toDouble / readIops)
    else SimTime.MaxValue

  /** Estimate time for N random write operations. */
  def randomWriteTime(ops: Long): SimTime =
    if writeIops > 0 then SimTime(ops.toDouble / writeIops)
    else SimTime.MaxValue

  /** Monthly storage cost for given capacity used. */
  def monthlyCost(usedMB: MegaBytes): Cost =
    Cost(usedMB.value / 1024.0 * costPerGBMonth.value)

object StoragePerformance:

  /** NVMe SSD (e.g. Samsung 990 Pro, datacenter NVMe). */
  val nvme: StoragePerformance = StoragePerformance(
    tier = StorageTier.NVMe,
    readIops = 1_000_000L,
    writeIops = 800_000L,
    readThroughputMBps = 7000.0,
    writeThroughputMBps = 5000.0,
    readLatencyUs = 10.0,
    writeLatencyUs = 20.0,
    capacityMB = MegaBytes(2_000_000.0), // 2 TB
    costPerGBMonth = Cost(0.08)
  )

  /** SATA SSD (e.g. datacenter SATA SSD). */
  val ssd: StoragePerformance = StoragePerformance(
    tier = StorageTier.SSD,
    readIops = 100_000L,
    writeIops = 80_000L,
    readThroughputMBps = 550.0,
    writeThroughputMBps = 520.0,
    readLatencyUs = 50.0,
    writeLatencyUs = 100.0,
    capacityMB = MegaBytes(4_000_000.0), // 4 TB
    costPerGBMonth = Cost(0.05)
  )

  /** Enterprise HDD (e.g. 7200 RPM datacenter HDD). */
  val hdd: StoragePerformance = StoragePerformance(
    tier = StorageTier.HDD,
    readIops = 150L,
    writeIops = 150L,
    readThroughputMBps = 200.0,
    writeThroughputMBps = 200.0,
    readLatencyUs = 4000.0,
    writeLatencyUs = 4500.0,
    capacityMB = MegaBytes(16_000_000.0), // 16 TB
    costPerGBMonth = Cost(0.02)
  )

  /** Network-attached storage (e.g. AWS EBS gp3). */
  val networkAttached: StoragePerformance = StoragePerformance(
    tier = StorageTier.NetworkAttached,
    readIops = 16_000L,
    writeIops = 16_000L,
    readThroughputMBps = 1000.0,
    writeThroughputMBps = 1000.0,
    readLatencyUs = 200.0,
    writeLatencyUs = 300.0,
    capacityMB = MegaBytes(64_000_000.0), // 64 TB
    costPerGBMonth = Cost(0.08)
  )

  /** Custom storage performance from parameters. */
  def custom(
      tier: StorageTier,
      readIops: Long,
      writeIops: Long,
      readMBps: Double,
      writeMBps: Double,
      readLatencyUs: Double,
      writeLatencyUs: Double,
      capacityMB: MegaBytes = MegaBytes(1_000_000.0),
      costPerGBMonth: Cost = Cost(0.05)
  ): StoragePerformance =
    StoragePerformance(
      tier,
      readIops,
      writeIops,
      readMBps,
      writeMBps,
      readLatencyUs,
      writeLatencyUs,
      capacityMB,
      costPerGBMonth
    )

/** Storage I/O workload specification. */
final case class StorageWorkload(
    sequentialReadMB: MegaBytes = MegaBytes.Zero,
    sequentialWriteMB: MegaBytes = MegaBytes.Zero,
    randomReadOps: Long = 0L,
    randomWriteOps: Long = 0L
):
  /** Estimate total I/O time on given storage. */
  def estimateTime(storage: StoragePerformance): SimTime =
    val seqRead   = storage.sequentialReadTime(sequentialReadMB)
    val seqWrite  = storage.sequentialWriteTime(sequentialWriteMB)
    val randRead  = storage.randomReadTime(randomReadOps)
    val randWrite = storage.randomWriteTime(randomWriteOps)
    SimTime(seqRead.value + seqWrite.value + randRead.value + randWrite.value)

/** Tiered storage system with automatic data placement. */
final case class TieredStorage(
    tiers: Vector[StoragePerformance]
):
  /** Find the fastest tier that has enough capacity. */
  def bestTierForCapacity(requiredMB: MegaBytes): Option[StoragePerformance] =
    tiers
      .filter(_.capacityMB >= requiredMB)
      .sortBy(_.readLatencyUs)
      .headOption

  /** Find the cheapest tier that has enough capacity. */
  def cheapestTierForCapacity(requiredMB: MegaBytes): Option[StoragePerformance] =
    tiers
      .filter(_.capacityMB >= requiredMB)
      .sortBy(_.costPerGBMonth.value)
      .headOption

  /** Total capacity across all tiers. */
  def totalCapacity: MegaBytes =
    MegaBytes(tiers.map(_.capacityMB.value).sum)

object TieredStorage:
  /** Standard 3-tier storage: NVMe + SSD + HDD. */
  val standard: TieredStorage = TieredStorage(
    Vector(
      StoragePerformance.nvme,
      StoragePerformance.ssd,
      StoragePerformance.hdd
    )
  )

  /** Cloud storage tiers: NVMe + Network-attached. */
  val cloud: TieredStorage = TieredStorage(
    Vector(
      StoragePerformance.nvme,
      StoragePerformance.networkAttached
    )
  )
