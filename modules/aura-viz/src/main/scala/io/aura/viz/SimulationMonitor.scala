// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz

import io.aura.core.types.*
import io.aura.core.engine.SimulationResults

/** Real-time simulation metrics snapshot for live monitoring.
  *
  * Captures a point-in-time view of simulation progress, designed to be periodically sampled and exported for dashboard
  * visualization.
  */
final case class MetricsSnapshot(
    timestamp: SimTime,
    completedWorkloads: Int,
    failedWorkloads: Int,
    activeVms: Int,
    totalMigrations: Int,
    totalFaults: Int,
    totalEnergyWh: Double,
    avgCompletionTime: Double,
    throughput: Double,         // workloads per second
    currentUtilization: Double, // 0.0 to 1.0
    totalCost: Double,
    eventCount: Long
):
  /** Export snapshot as JSON string for dashboard consumption. */
  def toJSON: String =
    s"""{
  "timestamp": ${timestamp.value},
  "completedWorkloads": $completedWorkloads,
  "failedWorkloads": $failedWorkloads,
  "activeVms": $activeVms,
  "totalMigrations": $totalMigrations,
  "totalFaults": $totalFaults,
  "totalEnergyWh": $totalEnergyWh,
  "avgCompletionTime": $avgCompletionTime,
  "throughput": $throughput,
  "currentUtilization": $currentUtilization,
  "totalCost": $totalCost,
  "eventCount": $eventCount
}"""

object MetricsSnapshot:
  val empty: MetricsSnapshot = MetricsSnapshot(
    SimTime.Zero,
    0,
    0,
    0,
    0,
    0,
    0.0,
    0.0,
    0.0,
    0.0,
    0.0,
    0L
  )

/** Immutable monitor that tracks simulation progress over time.
  *
  * Takes periodic snapshots of simulation state and maintains a time-series history for live dashboard visualization.
  */
final case class SimulationMonitor(
    snapshots: Vector[MetricsSnapshot],
    maxSnapshots: Int = 1000
):

  /** Record a new snapshot from current simulation results. */
  def record(results: SimulationResults, activeVms: Int = 0, currentUtilization: Double = 0.0): SimulationMonitor =
    val snapshot = SimulationMonitor.snapshotFrom(results, activeVms, currentUtilization)
    val updated =
      if snapshots.size >= maxSnapshots then snapshots.drop(1) :+ snapshot
      else snapshots :+ snapshot
    copy(snapshots = updated)

  /** Get the latest snapshot, if any. */
  def latest: Option[MetricsSnapshot] = snapshots.lastOption

  /** Get all snapshots as a JSON array for dashboard consumption. */
  def toJSON: String =
    val items = snapshots.map(_.toJSON)
    s"[${items.mkString(",\n")}]"

  /** Get snapshots within a time range. */
  def snapshotsBetween(from: SimTime, to: SimTime): Vector[MetricsSnapshot] =
    snapshots.filter(s => s.timestamp.value >= from.value && s.timestamp.value <= to.value)

  /** Compute deltas between consecutive snapshots for rate-of-change analysis. */
  def deltas: Vector[MetricsDelta] =
    if snapshots.size < 2 then Vector.empty
    else
      snapshots
        .sliding(2)
        .collect { case Vector(a, b) =>
          MetricsDelta(
            fromTime = a.timestamp,
            toTime = b.timestamp,
            workloadsDelta = b.completedWorkloads - a.completedWorkloads,
            energyDelta = b.totalEnergyWh - a.totalEnergyWh,
            migrationsDelta = b.totalMigrations - a.totalMigrations,
            costDelta = b.totalCost - a.totalCost
          )
        }
        .toVector

  /** Format a summary of the monitoring session. */
  def formatSummary: String =
    if snapshots.isEmpty then "No monitoring data collected."
    else
      val first         = snapshots.head
      val last          = snapshots.last
      val duration      = last.timestamp.value - first.timestamp.value
      val avgThroughput = if duration > 0 then last.completedWorkloads.toDouble / duration else 0.0
      f"""Monitoring Summary:
  Duration: ${duration}%.1f seconds
  Snapshots: ${snapshots.size}
  Final workloads: ${last.completedWorkloads}
  Final energy: ${last.totalEnergyWh}%.2f Wh
  Avg throughput: ${avgThroughput}%.2f workloads/sec
  Peak utilization: ${snapshots.map(_.currentUtilization).max}%.1f%%"""

object SimulationMonitor:
  val empty: SimulationMonitor = SimulationMonitor(Vector.empty)

  def apply(maxSnapshots: Int): SimulationMonitor =
    SimulationMonitor(Vector.empty, maxSnapshots)

  /** Create a metrics snapshot from simulation results. */
  def snapshotFrom(
      results: SimulationResults,
      activeVms: Int = 0,
      currentUtilization: Double = 0.0
  ): MetricsSnapshot =
    val avgTime = if results.workloadResults.isEmpty then 0.0 else results.avgCompletionTime.value
    val throughput =
      if results.simulationEndTime.value > 0 then
        results.workloadResults.size.toDouble / results.simulationEndTime.value
      else 0.0
    MetricsSnapshot(
      timestamp = results.simulationEndTime,
      completedWorkloads = results.workloadResults.size,
      failedWorkloads = results.failedWorkloads.size,
      activeVms = activeVms,
      totalMigrations = results.migrationRecords.size,
      totalFaults = results.faultRecords.size,
      totalEnergyWh = results.totalEnergyWh.value,
      avgCompletionTime = avgTime,
      throughput = throughput,
      currentUtilization = currentUtilization,
      totalCost = results.totalCost.value,
      eventCount = results.totalEventsProcessed
    )

/** Delta between consecutive snapshots for rate analysis. */
final case class MetricsDelta(
    fromTime: SimTime,
    toTime: SimTime,
    workloadsDelta: Int,
    energyDelta: Double,
    migrationsDelta: Int,
    costDelta: Double
):
  def duration: Double     = toTime.value - fromTime.value
  def workloadRate: Double = if duration > 0 then workloadsDelta.toDouble / duration else 0.0
  def energyRate: Double   = if duration > 0 then energyDelta / duration else 0.0
