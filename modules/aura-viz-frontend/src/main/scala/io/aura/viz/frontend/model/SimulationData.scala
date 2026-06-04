// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz.frontend.model

import upickle.default.*

/** Domain model mirroring the JSON structure from SimulationResults.toJSON. */
final case class SimulationData(
    simulationEndTime: Double,
    totalEventsProcessed: Long,
    totalEnergyWh: Double,
    avgCompletionTime: Double,
    totalCost: Double,
    workloadResults: Vector[WorkloadResult],
    failedWorkloads: Vector[FailedWorkload],
    vmPlacements: Vector[VmPlacement],
    energyRecords: Vector[EnergyRecord],
    migrationRecords: Vector[MigrationRecord],
    invocationResults: Vector[InvocationResult],
    throttledInvocations: Vector[ThrottledInvocation],
    timedOutInvocations: Vector[TimedOutInvocation],
    podResults: Vector[PodResult],
    podSchedulingRecords: Vector[PodSchedulingRecord],
    edgeTaskResults: Vector[EdgeTaskResult],
    failedEdgeTasks: Vector[FailedEdgeTask],
    federatedTaskResults: Vector[FederatedTaskResult],
    failedFederatedTasks: Vector[FailedFederatedTask],
    costRecords: Vector[VmCostRecord],
    faultRecords: Vector[FaultRecord],
    vmDestroyedRecords: Vector[VmDestroyedRecord]
) derives ReadWriter:

  def hasServerlessData: Boolean =
    invocationResults.nonEmpty || throttledInvocations.nonEmpty || timedOutInvocations.nonEmpty

  def hasContainerData: Boolean =
    podResults.nonEmpty || podSchedulingRecords.nonEmpty

  def hasEdgeData: Boolean =
    edgeTaskResults.nonEmpty || failedEdgeTasks.nonEmpty

  def hasFederatedData: Boolean =
    federatedTaskResults.nonEmpty || failedFederatedTasks.nonEmpty

final case class WorkloadResult(
    workloadId: Int,
    vmId: Int,
    hostId: Int,
    finishTime: Double,
    executedMI: Double
) derives ReadWriter

final case class FailedWorkload(
    workloadId: Int,
    reason: String,
    time: Double
) derives ReadWriter

final case class VmPlacement(
    vmId: Int,
    hostId: Int,
    datacenterId: Int,
    time: Double
) derives ReadWriter

final case class EnergyRecord(
    hostId: Int,
    watts: Double,
    fromTime: Double,
    toTime: Double,
    energyWh: Double
) derives ReadWriter

final case class MigrationRecord(
    vmId: Int,
    sourceHostId: Int,
    targetHostId: Int,
    startTime: Double,
    duration: Double,
    dataTransferred: Double
) derives ReadWriter

final case class InvocationResult(
    invocationId: Int,
    functionId: Int,
    startTime: Double,
    finishTime: Double,
    billedGBSeconds: Double,
    coldStart: Boolean
) derives ReadWriter

final case class ThrottledInvocation(
    invocationId: Int,
    functionId: Int,
    reason: String,
    time: Double
) derives ReadWriter

final case class TimedOutInvocation(
    invocationId: Int,
    functionId: Int,
    reason: String,
    time: Double
) derives ReadWriter

final case class PodResult(
    podId: Int,
    hostId: Int,
    startTime: Double,
    finishTime: Option[Double],
    phase: String
) derives ReadWriter

final case class PodSchedulingRecord(
    podId: Int,
    hostId: Int,
    nodeName: String,
    time: Double
) derives ReadWriter

final case class EdgeTaskResult(
    taskId: Int,
    sourceNodeName: String,
    executionNodeName: String,
    offloaded: Boolean,
    networkLatency: Double,
    startTime: Double,
    finishTime: Double,
    offloadingReason: String
) derives ReadWriter

final case class FailedEdgeTask(
    taskId: Int,
    reason: String,
    time: Double
) derives ReadWriter

final case class FederatedTaskResult(
    taskId: Int,
    initialTier: String,
    finalTier: String,
    escalations: Int,
    startTime: Double,
    finishTime: Double
) derives ReadWriter

final case class FailedFederatedTask(
    taskId: Int,
    reason: String,
    tiersAttempted: Int,
    time: Double
) derives ReadWriter

final case class VmCostRecord(
    vmId: Int,
    cpuCost: Double,
    ramCost: Double,
    bwCost: Double,
    storageCost: Double,
    totalCost: Double,
    time: Double
) derives ReadWriter

final case class FaultRecord(
    hostId: Int,
    failedPEs: Int,
    time: Double
) derives ReadWriter

final case class VmDestroyedRecord(
    vmId: Int,
    hostId: Int,
    time: Double,
    reason: String
) derives ReadWriter
