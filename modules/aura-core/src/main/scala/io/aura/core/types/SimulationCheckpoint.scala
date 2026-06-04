// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

/** Simulation checkpointing and replay: snapshot capture, delta computation, checkpoint policies, and state restoration
  * — as immutable data and pure functions.
  */

opaque type CheckpointId = Long
object CheckpointId:
  def apply(value: Long): CheckpointId         = value
  extension (id: CheckpointId) def value: Long = id
  given Ordering[CheckpointId] with
    def compare(x: CheckpointId, y: CheckpointId): Int = java.lang.Long.compare(x, y)

/** A captured state of the simulation at a point in time. */
final case class SimulationSnapshot(
    checkpointId: CheckpointId,
    time: SimTime,
    hostStates: Map[HostId, HostSnapshot],
    vmStates: Map[VmId, VmSnapshot],
    pendingEventCount: Int,
    metricsSnapshot: MetricsState,
    randomSeed: Long = 0L
):
  def hostCount: Int             = hostStates.size
  def vmCount: Int               = vmStates.size
  def totalRunningWorkloads: Int = vmStates.values.map(_.runningWorkloadCount).sum

/** Lightweight host state for checkpointing. */
final case class HostSnapshot(
    hostId: HostId,
    active: Boolean,
    vmCount: Int,
    cpuUtilization: Double,
    availableMips: Double,
    availableRamMB: Double
)

/** Lightweight VM state for checkpointing. */
final case class VmSnapshot(
    vmId: VmId,
    hostId: HostId,
    status: String,
    runningWorkloadCount: Int,
    cpuUtilization: Double
)

/** Aggregated metrics at checkpoint time. */
final case class MetricsState(
    totalWorkloadsCompleted: Long,
    totalWorkloadsFailed: Long,
    totalMigrations: Long,
    totalEnergyWh: Double,
    totalCost: Double
)

object MetricsState:
  val zero: MetricsState = MetricsState(0, 0, 0, 0.0, 0.0)

/** When to take checkpoints. */
enum CheckpointPolicy:
  /** Take checkpoint every `interval` simulation time units. */
  case Periodic(interval: SimTime)

  /** Take checkpoint when a specific event count threshold is reached. */
  case EventBased(everyNEvents: Int)

  /** Only checkpoint when explicitly requested. */
  case Manual

/** Metadata about a stored checkpoint (without the full state). */
final case class CheckpointMetadata(
    checkpointId: CheckpointId,
    time: SimTime,
    description: String,
    parentCheckpointId: Option[CheckpointId],
    hostCount: Int,
    vmCount: Int,
    estimatedSizeBytes: Long,
    createdAt: Long = System.currentTimeMillis()
)

/** Delta between two simulation snapshots. */
final case class DiffSnapshot(
    fromCheckpoint: CheckpointId,
    toCheckpoint: CheckpointId,
    fromTime: SimTime,
    toTime: SimTime,
    addedHosts: Set[HostId],
    removedHosts: Set[HostId],
    changedHosts: Set[HostId],
    addedVms: Set[VmId],
    removedVms: Set[VmId],
    changedVms: Set[VmId],
    metricsDeltas: MetricsDelta
)

/** Change in metrics between two checkpoints. */
final case class MetricsDelta(
    workloadsCompletedDelta: Long,
    workloadsFailedDelta: Long,
    migrationsDelta: Long,
    energyDeltaWh: Double,
    costDelta: Double
)

/** Pure-function checkpoint manager. */
object CheckpointManager:

  /** Create a snapshot from current simulation state. */
  def createSnapshot(
      checkpointId: CheckpointId,
      time: SimTime,
      hosts: Vector[(HostId, Boolean, Int, Double, Double, Double)],
      vms: Vector[(VmId, HostId, String, Int, Double)],
      pendingEvents: Int,
      metrics: MetricsState,
      seed: Long = 0L
  ): SimulationSnapshot =
    SimulationSnapshot(
      checkpointId = checkpointId,
      time = time,
      hostStates = hosts.map { case (id, active, vmCount, cpu, mips, ram) =>
        id -> HostSnapshot(id, active, vmCount, cpu, mips, ram)
      }.toMap,
      vmStates = vms.map { case (id, hid, status, wl, cpu) =>
        id -> VmSnapshot(id, hid, status, wl, cpu)
      }.toMap,
      pendingEventCount = pendingEvents,
      metricsSnapshot = metrics,
      randomSeed = seed
    )

  /** Compute the diff between two snapshots. */
  def diffSnapshots(before: SimulationSnapshot, after: SimulationSnapshot): DiffSnapshot =
    val beforeHostIds = before.hostStates.keySet
    val afterHostIds  = after.hostStates.keySet
    val commonHosts   = beforeHostIds.intersect(afterHostIds)
    val changedHosts = commonHosts.filter { hid =>
      before.hostStates(hid) != after.hostStates(hid)
    }

    val beforeVmIds = before.vmStates.keySet
    val afterVmIds  = after.vmStates.keySet
    val commonVms   = beforeVmIds.intersect(afterVmIds)
    val changedVms = commonVms.filter { vid =>
      before.vmStates(vid) != after.vmStates(vid)
    }

    DiffSnapshot(
      fromCheckpoint = before.checkpointId,
      toCheckpoint = after.checkpointId,
      fromTime = before.time,
      toTime = after.time,
      addedHosts = afterHostIds -- beforeHostIds,
      removedHosts = beforeHostIds -- afterHostIds,
      changedHosts = changedHosts,
      addedVms = afterVmIds -- beforeVmIds,
      removedVms = beforeVmIds -- afterVmIds,
      changedVms = changedVms,
      metricsDeltas = MetricsDelta(
        workloadsCompletedDelta =
          after.metricsSnapshot.totalWorkloadsCompleted - before.metricsSnapshot.totalWorkloadsCompleted,
        workloadsFailedDelta = after.metricsSnapshot.totalWorkloadsFailed - before.metricsSnapshot.totalWorkloadsFailed,
        migrationsDelta = after.metricsSnapshot.totalMigrations - before.metricsSnapshot.totalMigrations,
        energyDeltaWh = after.metricsSnapshot.totalEnergyWh - before.metricsSnapshot.totalEnergyWh,
        costDelta = after.metricsSnapshot.totalCost - before.metricsSnapshot.totalCost
      )
    )

  /** Check if a checkpoint should be taken based on policy. */
  def shouldCheckpoint(
      policy: CheckpointPolicy,
      currentTime: SimTime,
      lastCheckpointTime: SimTime,
      eventsSinceLastCheckpoint: Int = 0
  ): Boolean = policy match
    case CheckpointPolicy.Periodic(interval) =>
      (currentTime.value - lastCheckpointTime.value) >= interval.value
    case CheckpointPolicy.EventBased(everyN) =>
      eventsSinceLastCheckpoint >= everyN
    case CheckpointPolicy.Manual =>
      false

  /** Prune old checkpoints, keeping at most `maxKeep` most recent. */
  def pruneCheckpoints(
      checkpoints: Vector[CheckpointMetadata],
      maxKeep: Int
  ): Vector[CheckpointMetadata] =
    if checkpoints.size <= maxKeep then checkpoints
    else checkpoints.sortBy(-_.time.value).take(maxKeep)

  /** Estimate snapshot size in bytes. */
  def estimateSnapshotSize(hostCount: Int, vmCount: Int, pendingEvents: Int = 0): Long =
    val hostBytes  = hostCount * 128L // ~128 bytes per host snapshot
    val vmBytes    = vmCount * 256L   // ~256 bytes per VM snapshot
    val eventBytes = pendingEvents * 64L
    val overhead   = 1024L            // metadata overhead
    hostBytes + vmBytes + eventBytes + overhead

  /** Build metadata from a snapshot. */
  def toMetadata(
      snapshot: SimulationSnapshot,
      description: String,
      parentId: Option[CheckpointId] = None
  ): CheckpointMetadata =
    CheckpointMetadata(
      checkpointId = snapshot.checkpointId,
      time = snapshot.time,
      description = description,
      parentCheckpointId = parentId,
      hostCount = snapshot.hostCount,
      vmCount = snapshot.vmCount,
      estimatedSizeBytes = estimateSnapshotSize(snapshot.hostCount, snapshot.vmCount, snapshot.pendingEventCount)
    )

  /** Create a checkpoint chain: list of checkpoints with parent links. */
  def buildChain(
      checkpoints: Vector[CheckpointMetadata]
  ): Vector[(CheckpointMetadata, Option[CheckpointMetadata])] =
    val byId = checkpoints.map(c => c.checkpointId -> c).toMap
    checkpoints.sortBy(_.time.value).map { cp =>
      cp -> cp.parentCheckpointId.flatMap(byId.get)
    }
