// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SimulationCheckpointSpec extends AnyFlatSpec with Matchers:

  private val metrics1 = MetricsState(100, 5, 3, 500.0, 120.0)
  private val metrics2 = MetricsState(200, 8, 7, 900.0, 250.0)

  private val snapshot1 = CheckpointManager.createSnapshot(
    CheckpointId(1),
    SimTime(100.0),
    hosts = Vector(
      (HostId(0), true, 2, 0.6, 5000.0, 32000.0),
      (HostId(1), true, 1, 0.3, 8000.0, 48000.0)
    ),
    vms = Vector(
      (VmId(0), HostId(0), "Running", 3, 0.7),
      (VmId(1), HostId(0), "Running", 1, 0.4),
      (VmId(2), HostId(1), "Running", 2, 0.5)
    ),
    pendingEvents = 50,
    metrics = metrics1,
    seed = 42L
  )

  private val snapshot2 = CheckpointManager.createSnapshot(
    CheckpointId(2),
    SimTime(200.0),
    hosts = Vector(
      (HostId(0), true, 3, 0.8, 3000.0, 24000.0),
      (HostId(1), false, 0, 0.0, 10000.0, 64000.0),
      (HostId(2), true, 1, 0.2, 9000.0, 60000.0)
    ),
    vms = Vector(
      (VmId(0), HostId(0), "Running", 4, 0.9),
      (VmId(1), HostId(0), "Running", 1, 0.4),
      (VmId(3), HostId(0), "Running", 2, 0.6),
      (VmId(4), HostId(2), "Running", 1, 0.3)
    ),
    pendingEvents = 30,
    metrics = metrics2
  )

  // ── SimulationSnapshot ──────────────────────────────────────────────────

  "SimulationSnapshot" should "report correct counts" in {
    snapshot1.hostCount shouldBe 2
    snapshot1.vmCount shouldBe 3
    snapshot1.totalRunningWorkloads shouldBe 6 // 3 + 1 + 2
  }

  it should "preserve random seed" in {
    snapshot1.randomSeed shouldBe 42L
  }

  // ── createSnapshot ──────────────────────────────────────────────────────

  "CheckpointManager.createSnapshot" should "store host states" in {
    snapshot1.hostStates(HostId(0)).cpuUtilization shouldBe 0.6
    snapshot1.hostStates(HostId(1)).active shouldBe true
  }

  it should "store VM states" in {
    snapshot1.vmStates(VmId(0)).hostId shouldBe HostId(0)
    snapshot1.vmStates(VmId(2)).runningWorkloadCount shouldBe 2
  }

  // ── diffSnapshots ───────────────────────────────────────────────────────

  "CheckpointManager.diffSnapshots" should "detect added hosts" in {
    val diff = CheckpointManager.diffSnapshots(snapshot1, snapshot2)
    diff.addedHosts should contain(HostId(2))
  }

  it should "detect removed VMs" in {
    val diff = CheckpointManager.diffSnapshots(snapshot1, snapshot2)
    diff.removedVms should contain(VmId(2))
  }

  it should "detect added VMs" in {
    val diff = CheckpointManager.diffSnapshots(snapshot1, snapshot2)
    diff.addedVms should contain allOf (VmId(3), VmId(4))
  }

  it should "detect changed hosts" in {
    val diff = CheckpointManager.diffSnapshots(snapshot1, snapshot2)
    // Host 0 changed (more VMs, higher CPU), Host 1 changed (deactivated)
    diff.changedHosts should contain(HostId(0))
    diff.changedHosts should contain(HostId(1))
  }

  it should "compute metrics deltas" in {
    val diff = CheckpointManager.diffSnapshots(snapshot1, snapshot2)
    diff.metricsDeltas.workloadsCompletedDelta shouldBe 100
    diff.metricsDeltas.workloadsFailedDelta shouldBe 3
    diff.metricsDeltas.migrationsDelta shouldBe 4
    diff.metricsDeltas.energyDeltaWh shouldBe 400.0 +- 0.1
    diff.metricsDeltas.costDelta shouldBe 130.0 +- 0.1
  }

  it should "record time range" in {
    val diff = CheckpointManager.diffSnapshots(snapshot1, snapshot2)
    diff.fromTime.value shouldBe 100.0
    diff.toTime.value shouldBe 200.0
  }

  // ── shouldCheckpoint ────────────────────────────────────────────────────

  "CheckpointManager.shouldCheckpoint Periodic" should "trigger at interval" in {
    val policy = CheckpointPolicy.Periodic(SimTime(50.0))
    CheckpointManager.shouldCheckpoint(policy, SimTime(100.0), SimTime(50.0)) shouldBe true
    CheckpointManager.shouldCheckpoint(policy, SimTime(80.0), SimTime(50.0)) shouldBe false
  }

  "CheckpointManager.shouldCheckpoint EventBased" should "trigger at event count" in {
    val policy = CheckpointPolicy.EventBased(1000)
    CheckpointManager.shouldCheckpoint(policy, SimTime(0.0), SimTime(0.0), 1000) shouldBe true
    CheckpointManager.shouldCheckpoint(policy, SimTime(0.0), SimTime(0.0), 999) shouldBe false
  }

  "CheckpointManager.shouldCheckpoint Manual" should "never trigger automatically" in {
    CheckpointManager.shouldCheckpoint(CheckpointPolicy.Manual, SimTime(1000.0), SimTime(0.0)) shouldBe false
  }

  // ── pruneCheckpoints ────────────────────────────────────────────────────

  "CheckpointManager.pruneCheckpoints" should "keep most recent checkpoints" in {
    val cps = (1 to 10)
      .map(i => CheckpointMetadata(CheckpointId(i.toLong), SimTime(i * 10.0), s"cp-$i", None, 2, 3, 1024))
      .toVector
    val pruned = CheckpointManager.pruneCheckpoints(cps, 3)
    pruned should have size 3
    pruned.map(_.checkpointId.value) should contain allOf (10L, 9L, 8L)
  }

  it should "keep all when under limit" in {
    val cps = Vector(
      CheckpointMetadata(CheckpointId(1), SimTime(10.0), "cp-1", None, 2, 3, 1024)
    )
    CheckpointManager.pruneCheckpoints(cps, 5) should have size 1
  }

  // ── estimateSnapshotSize ────────────────────────────────────────────────

  "CheckpointManager.estimateSnapshotSize" should "scale with entity counts" in {
    val small = CheckpointManager.estimateSnapshotSize(10, 20)
    val large = CheckpointManager.estimateSnapshotSize(100, 200)
    large should be > small
    small should be > 0L
  }

  // ── toMetadata ──────────────────────────────────────────────────────────

  "CheckpointManager.toMetadata" should "capture snapshot info" in {
    val meta = CheckpointManager.toMetadata(snapshot1, "test checkpoint", Some(CheckpointId(0)))
    meta.checkpointId shouldBe CheckpointId(1)
    meta.hostCount shouldBe 2
    meta.vmCount shouldBe 3
    meta.parentCheckpointId shouldBe Some(CheckpointId(0))
    meta.estimatedSizeBytes should be > 0L
  }

  // ── buildChain ──────────────────────────────────────────────────────────

  "CheckpointManager.buildChain" should "link parent checkpoints" in {
    val meta1 = CheckpointManager.toMetadata(snapshot1, "first")
    val meta2 = CheckpointManager.toMetadata(snapshot2, "second", Some(CheckpointId(1)))
    val chain = CheckpointManager.buildChain(Vector(meta2, meta1))
    chain should have size 2
    chain(0)._2 shouldBe None    // First has no parent
    chain(1)._2 shouldBe defined // Second links to first
  }
