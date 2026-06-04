// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StorageReplicationSpec extends AnyFlatSpec with Matchers:

  private val ssd       = StoragePerformance.ssd
  private val data100MB = MegaBytes(100.0)

  // ── ReplicationStrategy presets ───────────────────────────────────

  "ReplicationStrategy" should "provide standard presets" in {
    ReplicationStrategy.threeWaySync shouldBe ReplicationStrategy.Synchronous(3)
    ReplicationStrategy.quorumStrong shouldBe ReplicationStrategy.Quorum(3, 2, 2)
  }

  // ── ReplicatedVolume ──────────────────────────────────────────────

  "ReplicatedVolume" should "compute total storage used with replication factor" in {
    val vol = ReplicatedVolume(1L, MegaBytes(1000), ssd, ReplicationStrategy.Synchronous(3))
    vol.totalStorageUsed.value shouldBe 3000.0
    vol.replicationFactor shouldBe 3
  }

  it should "compute monthly cost scaled by replication factor" in {
    val singleVol = ReplicatedVolume(1L, MegaBytes(1024), ssd, ReplicationStrategy.None)
    val replVol   = ReplicatedVolume(2L, MegaBytes(1024), ssd, ReplicationStrategy.Synchronous(3))
    replVol.monthlyCost.value shouldBe (singleVol.monthlyCost.value * 3) +- 0.001
  }

  it should "treat None strategy as replication factor 1" in {
    val vol = ReplicatedVolume(1L, MegaBytes(1000), ssd, ReplicationStrategy.None)
    vol.replicationFactor shouldBe 1
    vol.totalStorageUsed.value shouldBe 1000.0
  }

  // ── Write Latency ────────────────────────────────────────────────

  "StorageReplicationEngine.writeLatency" should "be slowest for synchronous replication" in {
    val syncLatency  = StorageReplicationEngine.writeLatency(data100MB, ssd, ReplicationStrategy.Synchronous(3))
    val asyncLatency = StorageReplicationEngine.writeLatency(data100MB, ssd, ReplicationStrategy.Asynchronous(3))
    val noneLatency  = StorageReplicationEngine.writeLatency(data100MB, ssd, ReplicationStrategy.None)

    syncLatency.value should be > asyncLatency.value
    asyncLatency.value shouldBe noneLatency.value // async = just primary write
  }

  it should "scale with quorum write count" in {
    val q1 = StorageReplicationEngine.writeLatency(data100MB, ssd, ReplicationStrategy.Quorum(3, 1, 3))
    val q2 = StorageReplicationEngine.writeLatency(data100MB, ssd, ReplicationStrategy.Quorum(3, 2, 2))
    val q3 = StorageReplicationEngine.writeLatency(data100MB, ssd, ReplicationStrategy.Quorum(3, 3, 1))
    // Higher write quorum → more replicas to wait for → higher latency
    q3.value should be > q2.value
    q2.value should be > q1.value
  }

  // ── Read Latency ─────────────────────────────────────────────────

  "StorageReplicationEngine.readLatency" should "add overhead for quorum reads" in {
    val singleRead = StorageReplicationEngine.readLatency(data100MB, ssd, ReplicationStrategy.None)
    val quorumRead = StorageReplicationEngine.readLatency(data100MB, ssd, ReplicationStrategy.Quorum(3, 2, 2))
    quorumRead.value should be > singleRead.value
  }

  // ── RAID Capacity ────────────────────────────────────────────────

  "StorageReplicationEngine.raidEffectiveCapacity" should "compute correct capacity" in {
    val diskMB = MegaBytes(1000)
    StorageReplicationEngine.raidEffectiveCapacity(diskMB, RaidLevel.Raid0(4)).value shouldBe 4000.0
    StorageReplicationEngine.raidEffectiveCapacity(diskMB, RaidLevel.Raid1(2)).value shouldBe 1000.0
    StorageReplicationEngine.raidEffectiveCapacity(diskMB, RaidLevel.Raid5(4)).value shouldBe 3000.0
    StorageReplicationEngine.raidEffectiveCapacity(diskMB, RaidLevel.Raid6(4)).value shouldBe 2000.0
    StorageReplicationEngine.raidEffectiveCapacity(diskMB, RaidLevel.Raid10(4)).value shouldBe 2000.0
  }

  // ── RAID Performance ─────────────────────────────────────────────

  "StorageReplicationEngine.raidReadTime" should "improve with striping" in {
    val raid0  = StorageReplicationEngine.raidReadTime(data100MB, ssd, RaidLevel.Raid0(4))
    val single = ssd.sequentialReadTime(data100MB)
    raid0.value should be < single.value // RAID0 is faster
  }

  "StorageReplicationEngine.raidWriteTime" should "be computed for each level" in {
    val raid0 = StorageReplicationEngine.raidWriteTime(data100MB, ssd, RaidLevel.Raid0(4))
    val raid5 = StorageReplicationEngine.raidWriteTime(data100MB, ssd, RaidLevel.Raid5(4))
    // RAID5 has parity overhead, slower than RAID0
    raid5.value should be > raid0.value
  }

  // ── RAID Fault Tolerance ──────────────────────────────────────────

  "StorageReplicationEngine.raidFaultTolerance" should "return correct tolerance" in {
    StorageReplicationEngine.raidFaultTolerance(RaidLevel.Raid0(4)) shouldBe 0
    StorageReplicationEngine.raidFaultTolerance(RaidLevel.Raid1(2)) shouldBe 1
    StorageReplicationEngine.raidFaultTolerance(RaidLevel.Raid5(4)) shouldBe 1
    StorageReplicationEngine.raidFaultTolerance(RaidLevel.Raid6(4)) shouldBe 2
    StorageReplicationEngine.raidFaultTolerance(RaidLevel.Raid10(4)) shouldBe 1
  }

  // ── Consistency Check ─────────────────────────────────────────────

  "StorageReplicationEngine.isStronglyConsistent" should "check W + R > N" in {
    StorageReplicationEngine.isStronglyConsistent(ReplicationStrategy.quorumStrong) shouldBe true
    StorageReplicationEngine.isStronglyConsistent(ReplicationStrategy.quorumEventual) shouldBe false
    StorageReplicationEngine.isStronglyConsistent(ReplicationStrategy.threeWaySync) shouldBe true
  }

  // ── Replication Bandwidth ─────────────────────────────────────────

  "StorageReplicationEngine.replicationBandwidthMBps" should "scale with replication factor" in {
    val bwSync = StorageReplicationEngine.replicationBandwidthMBps(100.0, ReplicationStrategy.Synchronous(3))
    bwSync shouldBe 200.0 // 100 * (3-1)
    val bwNone = StorageReplicationEngine.replicationBandwidthMBps(100.0, ReplicationStrategy.None)
    bwNone shouldBe 0.0
  }

  it should "use write quorum for quorum strategy" in {
    val bw = StorageReplicationEngine.replicationBandwidthMBps(100.0, ReplicationStrategy.Quorum(3, 2, 2))
    bw shouldBe 100.0 // 100 * (2-1)
  }

  // ── Data Loss Probability ─────────────────────────────────────────

  "StorageReplicationEngine.dataLossProbability" should "decrease with replication" in {
    val p1 = StorageReplicationEngine.dataLossProbability(0.01, ReplicationStrategy.None)
    val p3 = StorageReplicationEngine.dataLossProbability(0.01, ReplicationStrategy.Synchronous(3))
    p1 shouldBe 0.01
    p3 shouldBe 1e-6 +- 1e-8 // 0.01^3
    p3 should be < p1
  }

  it should "return failure probability for single replica" in {
    val p = StorageReplicationEngine.dataLossProbability(0.05, ReplicationStrategy.None)
    p shouldBe 0.05
  }

  // ── RAID Read Multiplier ──────────────────────────────────────────

  "StorageReplicationEngine.raidReadMultiplier" should "scale with disk count" in {
    StorageReplicationEngine.raidReadMultiplier(RaidLevel.Raid0(8)) shouldBe 8.0
    StorageReplicationEngine.raidReadMultiplier(RaidLevel.Raid5(6)) shouldBe 5.0
  }
