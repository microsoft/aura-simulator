// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.types.*
import io.aura.core.engine.{FaultRecord, MigrationRecord, SimulationResults, WorkloadResult}

class SimulationMonitorSpec extends AnyFlatSpec with Matchers:

  private def resultsAt(time: Double, workloads: Int = 0): SimulationResults =
    SimulationResults.empty.copy(
      simulationEndTime = SimTime(time),
      workloadResults = (0 until workloads)
        .map(i => WorkloadResult(WorkloadId(i.toLong), VmId(0), HostId(0), SimTime(time), MI(1000.0)))
        .toVector,
      totalEventsProcessed = workloads.toLong
    )

  // ─── MetricsSnapshot ──────────────────────────────────────────────

  "MetricsSnapshot.empty" should "have zero values" in {
    val snap = MetricsSnapshot.empty
    snap.completedWorkloads shouldBe 0
    snap.totalEnergyWh shouldBe 0.0
    snap.timestamp shouldBe SimTime.Zero
  }

  "MetricsSnapshot.toJSON" should "produce valid JSON" in {
    val snap = MetricsSnapshot.empty.copy(completedWorkloads = 5, totalEnergyWh = 12.5)
    val json = snap.toJSON
    json should include("\"completedWorkloads\": 5")
    json should include("\"totalEnergyWh\": 12.5")
  }

  // ─── SimulationMonitor ─────────────────────────────────────────────

  "SimulationMonitor.empty" should "have no snapshots" in {
    SimulationMonitor.empty.snapshots shouldBe empty
  }

  "SimulationMonitor.record" should "add a snapshot" in {
    val monitor = SimulationMonitor.empty.record(resultsAt(10.0, workloads = 3))
    monitor.snapshots should have size 1
    monitor.latest.get.completedWorkloads shouldBe 3
  }

  it should "accumulate multiple snapshots" in {
    val monitor = SimulationMonitor.empty
      .record(resultsAt(10.0, workloads = 2))
      .record(resultsAt(20.0, workloads = 5))
      .record(resultsAt(30.0, workloads = 10))
    monitor.snapshots should have size 3
    monitor.latest.get.completedWorkloads shouldBe 10
  }

  it should "respect maxSnapshots limit" in {
    val monitor = SimulationMonitor(maxSnapshots = 3)
      .record(resultsAt(1.0, workloads = 1))
      .record(resultsAt(2.0, workloads = 2))
      .record(resultsAt(3.0, workloads = 3))
      .record(resultsAt(4.0, workloads = 4))
    monitor.snapshots should have size 3
    monitor.snapshots.head.completedWorkloads shouldBe 2 // oldest dropped
    monitor.latest.get.completedWorkloads shouldBe 4
  }

  // ─── snapshotFrom ──────────────────────────────────────────────────

  "SimulationMonitor.snapshotFrom" should "capture workload count" in {
    val results = resultsAt(50.0, workloads = 10)
    val snap    = SimulationMonitor.snapshotFrom(results)
    snap.completedWorkloads shouldBe 10
    snap.timestamp shouldBe SimTime(50.0)
  }

  it should "compute throughput" in {
    val results = resultsAt(10.0, workloads = 20)
    val snap    = SimulationMonitor.snapshotFrom(results)
    snap.throughput shouldBe 2.0 +- 0.001
  }

  it should "capture active VMs and utilization" in {
    val snap = SimulationMonitor.snapshotFrom(resultsAt(10.0), activeVms = 5, currentUtilization = 75.0)
    snap.activeVms shouldBe 5
    snap.currentUtilization shouldBe 75.0
  }

  // ─── snapshotsBetween ──────────────────────────────────────────────

  "SimulationMonitor.snapshotsBetween" should "filter by time range" in {
    val monitor = SimulationMonitor.empty
      .record(resultsAt(10.0))
      .record(resultsAt(20.0))
      .record(resultsAt(30.0))
      .record(resultsAt(40.0))
    val filtered = monitor.snapshotsBetween(SimTime(15.0), SimTime(35.0))
    filtered should have size 2
    filtered.head.timestamp shouldBe SimTime(20.0)
    filtered.last.timestamp shouldBe SimTime(30.0)
  }

  // ─── deltas ────────────────────────────────────────────────────────

  "SimulationMonitor.deltas" should "compute differences between snapshots" in {
    val monitor = SimulationMonitor.empty
      .record(resultsAt(10.0, workloads = 5))
      .record(resultsAt(20.0, workloads = 15))
    val d = monitor.deltas
    d should have size 1
    d.head.workloadsDelta shouldBe 10
    d.head.duration shouldBe 10.0 +- 0.001
    d.head.workloadRate shouldBe 1.0 +- 0.001
  }

  it should "return empty for single snapshot" in {
    val monitor = SimulationMonitor.empty.record(resultsAt(10.0))
    monitor.deltas shouldBe empty
  }

  // ─── toJSON ────────────────────────────────────────────────────────

  "SimulationMonitor.toJSON" should "produce valid JSON array" in {
    val monitor = SimulationMonitor.empty
      .record(resultsAt(10.0, workloads = 3))
      .record(resultsAt(20.0, workloads = 7))
    val json = monitor.toJSON
    json should startWith("[")
    json should endWith("]")
    json should include("\"completedWorkloads\": 3")
    json should include("\"completedWorkloads\": 7")
  }

  // ─── formatSummary ─────────────────────────────────────────────────

  "SimulationMonitor.formatSummary" should "produce readable output" in {
    val monitor = SimulationMonitor.empty
      .record(resultsAt(0.0))
      .record(resultsAt(100.0, workloads = 50))
    val summary = monitor.formatSummary
    summary should include("Duration")
    summary should include("50")
    summary should include("Snapshots: 2")
  }

  it should "handle empty monitor" in {
    SimulationMonitor.empty.formatSummary should include("No monitoring data")
  }

  // ─── latest ────────────────────────────────────────────────────────

  "SimulationMonitor.latest" should "return None for empty" in {
    SimulationMonitor.empty.latest shouldBe None
  }

  it should "return most recent snapshot" in {
    val monitor = SimulationMonitor.empty
      .record(resultsAt(10.0, workloads = 1))
      .record(resultsAt(20.0, workloads = 2))
    monitor.latest.get.completedWorkloads shouldBe 2
  }
