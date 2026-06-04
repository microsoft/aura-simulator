// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

/** Storage replication models: replication strategies, consistency levels, RAID configurations, and quorum-based writes
  * — as immutable data and pure functions.
  */

// ── Replication Strategy ────────────────────────────────────────────

/** How data is replicated across storage nodes. */
enum ReplicationStrategy:
  /** Synchronous replication: write acknowledged only after all replicas confirmed. */
  case Synchronous(replicationFactor: Int)

  /** Asynchronous replication: write acknowledged after primary, replicas update in background. */
  case Asynchronous(replicationFactor: Int)

  /** Quorum-based: write acknowledged after W replicas, reads require R replicas (W + R > N). */
  case Quorum(replicationFactor: Int, writeQuorum: Int, readQuorum: Int)

  /** No replication (single copy). */
  case None

object ReplicationStrategy:
  /** Standard 3-way synchronous replication. */
  val threeWaySync: ReplicationStrategy = Synchronous(3)

  /** Standard 3-way async replication. */
  val threeWayAsync: ReplicationStrategy = Asynchronous(3)

  /** Quorum with N=3, W=2, R=2 (strong consistency). */
  val quorumStrong: ReplicationStrategy = Quorum(3, 2, 2)

  /** Quorum with N=3, W=1, R=1 (eventual consistency, fast). */
  val quorumEventual: ReplicationStrategy = Quorum(3, 1, 1)

// ── Consistency Level ───────────────────────────────────────────────

/** Read/write consistency guarantees. */
enum ConsistencyLevel:
  case Strong                             // All replicas agree (linearizable)
  case Eventual                           // Replicas converge over time
  case ReadYourWrites                     // Client sees its own writes immediately
  case BoundedStaleness(maxLagMs: Double) // Max staleness bound

// ── RAID Configuration ──────────────────────────────────────────────

/** RAID level modeling. */
enum RaidLevel:
  case Raid0(disks: Int)  // Striping, no redundancy
  case Raid1(disks: Int)  // Mirroring
  case Raid5(disks: Int)  // Striping + distributed parity
  case Raid6(disks: Int)  // Striping + double parity
  case Raid10(disks: Int) // Mirrored stripes

// ── Replicated Volume ───────────────────────────────────────────────

/** A replicated storage volume. */
final case class ReplicatedVolume(
    volumeId: Long,
    capacityMB: MegaBytes,
    storage: StoragePerformance,
    strategy: ReplicationStrategy,
    consistency: ConsistencyLevel = ConsistencyLevel.Strong
):
  def replicationFactor: Int = strategy match
    case ReplicationStrategy.Synchronous(rf)  => rf
    case ReplicationStrategy.Asynchronous(rf) => rf
    case ReplicationStrategy.Quorum(rf, _, _) => rf
    case ReplicationStrategy.None             => 1

  def totalStorageUsed: MegaBytes = MegaBytes(capacityMB.value * replicationFactor)

  def monthlyCost: Cost =
    Cost(storage.monthlyCost(capacityMB).value * replicationFactor)

// ── Storage Replication Engine ──────────────────────────────────────

object StorageReplicationEngine:

  /** Estimate write latency for a given replication strategy.
    * @param dataMB
    *   amount of data to write
    * @param storage
    *   base storage performance
    * @param strategy
    *   replication strategy
    * @param networkLatencyMs
    *   inter-node network latency in milliseconds
    * @return
    *   total write time in simulation seconds
    */
  def writeLatency(
      dataMB: MegaBytes,
      storage: StoragePerformance,
      strategy: ReplicationStrategy,
      networkLatencyMs: Double = 1.0
  ): SimTime =
    val baseWrite         = storage.sequentialWriteTime(dataMB)
    val networkLatencySec = networkLatencyMs / 1000.0
    strategy match
      case ReplicationStrategy.Synchronous(rf) =>
        // Must wait for all replicas; slowest replica = base + network RTT
        SimTime(baseWrite.value + (rf - 1) * (baseWrite.value + networkLatencySec))
      case ReplicationStrategy.Asynchronous(_) =>
        // Only wait for primary write
        baseWrite
      case ReplicationStrategy.Quorum(_, w, _) =>
        // Wait for W replicas (including primary)
        val replicaWrites = w - 1
        SimTime(baseWrite.value + replicaWrites * networkLatencySec)
      case ReplicationStrategy.None =>
        baseWrite

  /** Estimate read latency for a given replication strategy. */
  def readLatency(
      dataMB: MegaBytes,
      storage: StoragePerformance,
      strategy: ReplicationStrategy,
      networkLatencyMs: Double = 1.0
  ): SimTime =
    val baseRead = storage.sequentialReadTime(dataMB)
    strategy match
      case ReplicationStrategy.Quorum(_, _, r) if r > 1 =>
        // Read from R replicas, take fastest + network overhead
        val networkLatencySec = networkLatencyMs / 1000.0
        SimTime(baseRead.value + networkLatencySec)
      case _ =>
        // Read from primary or single replica
        baseRead

  /** Calculate RAID effective capacity and performance. */
  def raidEffectiveCapacity(
      diskCapacityMB: MegaBytes,
      raidLevel: RaidLevel
  ): MegaBytes = raidLevel match
    case RaidLevel.Raid0(n)  => MegaBytes(diskCapacityMB.value * n)
    case RaidLevel.Raid1(n)  => diskCapacityMB // mirrored = capacity of one disk
    case RaidLevel.Raid5(n)  => MegaBytes(diskCapacityMB.value * (n - 1))
    case RaidLevel.Raid6(n)  => MegaBytes(diskCapacityMB.value * (n - 2))
    case RaidLevel.Raid10(n) => MegaBytes(diskCapacityMB.value * (n / 2))

  /** Calculate RAID performance multiplier for reads. */
  def raidReadMultiplier(raidLevel: RaidLevel): Double = raidLevel match
    case RaidLevel.Raid0(n)  => n.toDouble       // reads from all disks
    case RaidLevel.Raid1(n)  => n.toDouble       // reads from any mirror
    case RaidLevel.Raid5(n)  => (n - 1).toDouble // reads from data disks
    case RaidLevel.Raid6(n)  => (n - 2).toDouble // reads from data disks
    case RaidLevel.Raid10(n) => n.toDouble       // reads from all disks

  /** Calculate RAID performance multiplier for writes. */
  def raidWriteMultiplier(raidLevel: RaidLevel): Double = raidLevel match
    case RaidLevel.Raid0(n)  => n.toDouble             // writes to all disks
    case RaidLevel.Raid1(n)  => 1.0                    // writes to all mirrors (limited by slowest)
    case RaidLevel.Raid5(n)  => (n - 1).toDouble / 2.0 // read-modify-write for parity
    case RaidLevel.Raid6(n)  => (n - 2).toDouble / 3.0 // double parity overhead
    case RaidLevel.Raid10(n) => (n / 2).toDouble       // writes to stripe, mirror overhead

  /** Estimate RAID read time. */
  def raidReadTime(dataMB: MegaBytes, disk: StoragePerformance, raidLevel: RaidLevel): SimTime =
    val baseTime   = disk.sequentialReadTime(dataMB)
    val multiplier = raidReadMultiplier(raidLevel)
    if multiplier > 0 then SimTime(baseTime.value / multiplier)
    else baseTime

  /** Estimate RAID write time. */
  def raidWriteTime(dataMB: MegaBytes, disk: StoragePerformance, raidLevel: RaidLevel): SimTime =
    val baseTime   = disk.sequentialWriteTime(dataMB)
    val multiplier = raidWriteMultiplier(raidLevel)
    if multiplier > 0 then SimTime(baseTime.value / multiplier)
    else baseTime

  /** Number of disk failures a RAID level can tolerate. */
  def raidFaultTolerance(raidLevel: RaidLevel): Int = raidLevel match
    case RaidLevel.Raid0(_)  => 0
    case RaidLevel.Raid1(n)  => n - 1
    case RaidLevel.Raid5(_)  => 1
    case RaidLevel.Raid6(_)  => 2
    case RaidLevel.Raid10(_) => 1 // per mirror group

  /** Check if a quorum configuration satisfies strong consistency (W + R > N). */
  def isStronglyConsistent(strategy: ReplicationStrategy): Boolean = strategy match
    case ReplicationStrategy.Quorum(n, w, r) => w + r > n
    case ReplicationStrategy.Synchronous(_)  => true
    case _                                   => false

  /** Estimate replication bandwidth required (MB/s) for a given write rate. */
  def replicationBandwidthMBps(
      writeMBps: Double,
      strategy: ReplicationStrategy
  ): Double = strategy match
    case ReplicationStrategy.Synchronous(rf)  => writeMBps * (rf - 1)
    case ReplicationStrategy.Asynchronous(rf) => writeMBps * (rf - 1)
    case ReplicationStrategy.Quorum(_, w, _)  => writeMBps * (w - 1)
    case ReplicationStrategy.None             => 0.0

  /** Estimate the probability of data loss given disk failure probability. */
  def dataLossProbability(
      diskFailureProbability: Double,
      strategy: ReplicationStrategy
  ): Double =
    val rf = strategy match
      case ReplicationStrategy.Synchronous(r)  => r
      case ReplicationStrategy.Asynchronous(r) => r
      case ReplicationStrategy.Quorum(r, _, _) => r
      case ReplicationStrategy.None            => 1
    // Probability all replicas fail simultaneously
    math.pow(diskFailureProbability, rf)
