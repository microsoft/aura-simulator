// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.engine

import scala.collection.mutable.ArrayBuffer
import io.aura.core.types.*
import io.aura.core.events.*
import io.aura.core.events.SimEventPayload.*

/** Metrics collector that accumulates simulation statistics.
  *
  * Uses ArrayBuffers internally for O(1) amortized append. This is safe because the collector lives within the
  * TimeCoordinator actor (single-threaded). Call `snapshot()` to obtain immutable results at simulation end.
  */
final class MetricsCollector private (
    private val _workloadResults: ArrayBuffer[WorkloadResult],
    private val _failedWorkloads: ArrayBuffer[FailedWorkload],
    private val _vmPlacements: ArrayBuffer[VmPlacement],
    private val _hostUtilizations: ArrayBuffer[HostUtilizationRecord],
    private val _energyRecords: ArrayBuffer[EnergyRecord],
    private val _migrationRecords: ArrayBuffer[MigrationRecord],
    private val _costRecords: ArrayBuffer[VmCostRecord],
    private val _faultRecords: ArrayBuffer[FaultRecord],
    private val _vmDestroyedRecords: ArrayBuffer[VmDestroyedRecord],
    private val _invocationResults: ArrayBuffer[InvocationResult],
    private val _throttledInvocations: ArrayBuffer[ThrottledInvocation],
    private val _timedOutInvocations: ArrayBuffer[TimedOutInvocation],
    private val _podResults: ArrayBuffer[PodResult],
    private val _podSchedulingRecords: ArrayBuffer[PodSchedulingRecord],
    private val _unschedulablePods: ArrayBuffer[UnschedulablePod],
    private val _edgeTaskResults: ArrayBuffer[EdgeTaskResult],
    private val _failedEdgeTasks: ArrayBuffer[FailedEdgeTask],
    private val _federatedTaskResults: ArrayBuffer[FederatedTaskResult],
    private val _failedFederatedTasks: ArrayBuffer[FailedFederatedTask],
    private val _slaViolationRecords: ArrayBuffer[SlaViolationRecord],
    private val _consolidationRecords: ArrayBuffer[ConsolidationRecord],
    private val _batchJobRecords: ArrayBuffer[BatchJobRecord],
    private val _inferenceResults: ArrayBuffer[InferenceResult],
    private val _failedInferenceRequests: ArrayBuffer[FailedInferenceRequest],
    private val _gpuEnergyRecords: ArrayBuffer[GpuEnergyRecord],
    private val _dvfsRecords: ArrayBuffer[DvfsRecord],
    private var _eventCount: Long
):
  def eventCount: Long = _eventCount

  def recordEvent(event: SimEvent): MetricsCollector =
    event.payload match
      case WorkloadFinished(wId, vmId, hostId, finishTime, executedMI) =>
        _workloadResults += WorkloadResult(wId, vmId, hostId, finishTime, executedMI)
      case WorkloadFailed(wId, reason) =>
        _failedWorkloads += FailedWorkload(wId, reason, event.time)
      case VmCreated(vmId, hostId, dcId) =>
        _vmPlacements += VmPlacement(vmId, hostId, dcId, event.time)
      case HostUtilizationUpdate(hostId, util, time) =>
        _hostUtilizations += HostUtilizationRecord(hostId, util, time)
        if util.value > 0.9 then
          _slaViolationRecords += SlaViolationRecord(hostId, util, time, "CPU utilization exceeds 90%")
      case EnergyReport(hostId, watts, fromTime, toTime, energyWh) =>
        _energyRecords += EnergyRecord(hostId, watts, fromTime, toTime, energyWh)
      case VmMigrationStart(vmId, sourceHostId, targetHostId, duration, dataTransferred) =>
        _migrationRecords += MigrationRecord(vmId, sourceHostId, targetHostId, event.time, duration, dataTransferred)
      case VmDestroyed(vmId, hostId) =>
        _vmDestroyedRecords += VmDestroyedRecord(vmId, hostId, event.time, "destroyed")
      case VmCostReport(vmId, cpuCost, ramCost, bwCost, storageCost, totalCost) =>
        _costRecords += VmCostRecord(vmId, cpuCost, ramCost, bwCost, storageCost, totalCost, event.time)
      case HostFaultEvent(hostId, failedPEs) =>
        _faultRecords += FaultRecord(hostId, failedPEs, event.time)
      case InvocationComplete(invId, funcId, startTime, finishTime, billedGBs, coldStart) =>
        if event.destination.entityType != EntityType.FaasPlatform then
          _invocationResults += InvocationResult(invId, funcId, startTime, finishTime, billedGBs, coldStart)
      case InvocationThrottled(invId, funcId, reason) =>
        _throttledInvocations += ThrottledInvocation(invId, funcId, reason, event.time)
      case InvocationTimedOut(invId, funcId, reason) =>
        _timedOutInvocations += TimedOutInvocation(invId, funcId, reason, event.time)
      case PodScheduled(podId, hostId, nodeName) =>
        _podSchedulingRecords += PodSchedulingRecord(podId, hostId, nodeName, event.time)
      case PodStarted(podId, hostId, startTime) =>
        _podResults += PodResult(podId, hostId, startTime, None, "Running")
      case PodCompleted(podId, hostId, finishTime) =>
        val idx = _podResults.indexWhere(_.podId == podId)
        if idx == -1 then _podResults += PodResult(podId, hostId, finishTime, Some(finishTime), "Succeeded")
        else _podResults(idx) = _podResults(idx).copy(finishTime = Some(finishTime), phase = "Succeeded")
      case PodFailed(podId, hostId, _) =>
        val idx = _podResults.indexWhere(_.podId == podId)
        if idx == -1 then _podResults += PodResult(podId, hostId, event.time, None, "Failed")
        else _podResults(idx) = _podResults(idx).copy(phase = "Failed")
      case PodUnschedulable(podId, reason) =>
        _unschedulablePods += UnschedulablePod(podId, reason, event.time)
      case EdgeTaskCompleted(
            taskId,
            sourceNodeName,
            executionNodeName,
            offloaded,
            networkLatency,
            startTime,
            finishTime,
            offloadingReason
          ) =>
        if event.source != event.destination then
          _edgeTaskResults += EdgeTaskResult(
            taskId,
            sourceNodeName,
            executionNodeName,
            offloaded,
            networkLatency,
            startTime,
            finishTime,
            offloadingReason
          )
      case EdgeTaskFailed(taskId, reason) =>
        _failedEdgeTasks += FailedEdgeTask(taskId, reason, event.time)
      case FederatedTaskCompleted(taskId, initialTier, finalTier, escalations, startTime, finishTime) =>
        _federatedTaskResults += FederatedTaskResult(taskId, initialTier, finalTier, escalations, startTime, finishTime)
      case FederatedTaskFailed(taskId, reason, tiersAttempted) =>
        _failedFederatedTasks += FailedFederatedTask(taskId, reason, tiersAttempted, event.time)
      case ConsolidationCheck(dcId, time) =>
        _consolidationRecords += ConsolidationRecord(dcId, time)
      case BatchJobCompleted(jobId, startTime, finishTime, turnaroundTime) =>
        _batchJobRecords += BatchJobRecord(jobId, startTime, finishTime, turnaroundTime)
      case InferenceRequestComplete(
            reqId,
            modelId,
            promptTokens,
            outputTokens,
            ttft,
            tpot,
            totalTime,
            totalEnergyWh,
            sloMet
          ) =>
        _inferenceResults += InferenceResult(
          reqId,
          modelId,
          promptTokens,
          outputTokens,
          ttft,
          tpot,
          totalTime,
          totalEnergyWh,
          sloMet
        )
      case InferenceRequestFailed(reqId, reason) =>
        _failedInferenceRequests += FailedInferenceRequest(reqId, reason, event.time)
      case GpuPowerReport(nodeId, deviceId, watts, fromTime, toTime, energyWh, computeWatts, memoryWatts) =>
        _gpuEnergyRecords += GpuEnergyRecord(
          nodeId,
          deviceId,
          watts,
          fromTime,
          toTime,
          energyWh,
          computeWatts,
          memoryWatts
        )
      case GpuDvfsChange(nodeId, deviceId, oldFreq, newFreq, reason) =>
        _dvfsRecords += DvfsRecord(nodeId, deviceId, oldFreq, newFreq, event.time, reason)
      case _ => ()
    _eventCount += 1
    this

  def recordEnergy(record: EnergyRecord): MetricsCollector =
    _energyRecords += record
    this

  def addCostRecord(record: VmCostRecord): MetricsCollector =
    _costRecords += record
    this

  /** Convert to immutable snapshot for SimulationResults. */
  def snapshot: MetricsSnapshot = MetricsSnapshot(
    workloadResults = _workloadResults.toVector,
    failedWorkloads = _failedWorkloads.toVector,
    vmPlacements = _vmPlacements.toVector,
    hostUtilizations = _hostUtilizations.toVector,
    energyRecords = _energyRecords.toVector,
    migrationRecords = _migrationRecords.toVector,
    costRecords = _costRecords.toVector,
    faultRecords = _faultRecords.toVector,
    vmDestroyedRecords = _vmDestroyedRecords.toVector,
    invocationResults = _invocationResults.toVector,
    throttledInvocations = _throttledInvocations.toVector,
    timedOutInvocations = _timedOutInvocations.toVector,
    podResults = _podResults.toVector,
    podSchedulingRecords = _podSchedulingRecords.toVector,
    unschedulablePods = _unschedulablePods.toVector,
    edgeTaskResults = _edgeTaskResults.toVector,
    failedEdgeTasks = _failedEdgeTasks.toVector,
    federatedTaskResults = _federatedTaskResults.toVector,
    failedFederatedTasks = _failedFederatedTasks.toVector,
    slaViolationRecords = _slaViolationRecords.toVector,
    consolidationRecords = _consolidationRecords.toVector,
    batchJobRecords = _batchJobRecords.toVector,
    inferenceResults = _inferenceResults.toVector,
    failedInferenceRequests = _failedInferenceRequests.toVector,
    gpuEnergyRecords = _gpuEnergyRecords.toVector,
    dvfsRecords = _dvfsRecords.toVector,
    eventCount = _eventCount
  )

object MetricsCollector:
  def empty: MetricsCollector = new MetricsCollector(
    _workloadResults = ArrayBuffer.empty,
    _failedWorkloads = ArrayBuffer.empty,
    _vmPlacements = ArrayBuffer.empty,
    _hostUtilizations = ArrayBuffer.empty,
    _energyRecords = ArrayBuffer.empty,
    _migrationRecords = ArrayBuffer.empty,
    _costRecords = ArrayBuffer.empty,
    _faultRecords = ArrayBuffer.empty,
    _vmDestroyedRecords = ArrayBuffer.empty,
    _invocationResults = ArrayBuffer.empty,
    _throttledInvocations = ArrayBuffer.empty,
    _timedOutInvocations = ArrayBuffer.empty,
    _podResults = ArrayBuffer.empty,
    _podSchedulingRecords = ArrayBuffer.empty,
    _unschedulablePods = ArrayBuffer.empty,
    _edgeTaskResults = ArrayBuffer.empty,
    _failedEdgeTasks = ArrayBuffer.empty,
    _federatedTaskResults = ArrayBuffer.empty,
    _failedFederatedTasks = ArrayBuffer.empty,
    _slaViolationRecords = ArrayBuffer.empty,
    _consolidationRecords = ArrayBuffer.empty,
    _batchJobRecords = ArrayBuffer.empty,
    _inferenceResults = ArrayBuffer.empty,
    _failedInferenceRequests = ArrayBuffer.empty,
    _gpuEnergyRecords = ArrayBuffer.empty,
    _dvfsRecords = ArrayBuffer.empty,
    _eventCount = 0L
  )

/** Immutable snapshot of metrics for SimulationResults. */
final case class MetricsSnapshot(
    workloadResults: Vector[WorkloadResult],
    failedWorkloads: Vector[FailedWorkload],
    vmPlacements: Vector[VmPlacement],
    hostUtilizations: Vector[HostUtilizationRecord],
    energyRecords: Vector[EnergyRecord],
    migrationRecords: Vector[MigrationRecord],
    costRecords: Vector[VmCostRecord],
    faultRecords: Vector[FaultRecord],
    vmDestroyedRecords: Vector[VmDestroyedRecord],
    invocationResults: Vector[InvocationResult],
    throttledInvocations: Vector[ThrottledInvocation],
    timedOutInvocations: Vector[TimedOutInvocation],
    podResults: Vector[PodResult],
    podSchedulingRecords: Vector[PodSchedulingRecord],
    unschedulablePods: Vector[UnschedulablePod],
    edgeTaskResults: Vector[EdgeTaskResult],
    failedEdgeTasks: Vector[FailedEdgeTask],
    federatedTaskResults: Vector[FederatedTaskResult],
    failedFederatedTasks: Vector[FailedFederatedTask],
    slaViolationRecords: Vector[SlaViolationRecord],
    consolidationRecords: Vector[ConsolidationRecord],
    batchJobRecords: Vector[BatchJobRecord],
    inferenceResults: Vector[InferenceResult],
    failedInferenceRequests: Vector[FailedInferenceRequest],
    gpuEnergyRecords: Vector[GpuEnergyRecord],
    dvfsRecords: Vector[DvfsRecord],
    eventCount: Long
)

final case class WorkloadResult(
    workloadId: WorkloadId,
    vmId: VmId,
    hostId: HostId,
    finishTime: SimTime,
    executedMI: MI
)

final case class FailedWorkload(
    workloadId: WorkloadId,
    reason: String,
    time: SimTime
)

final case class VmPlacement(
    vmId: VmId,
    hostId: HostId,
    datacenterId: DatacenterId,
    time: SimTime
)

final case class HostUtilizationRecord(
    hostId: HostId,
    utilization: Utilization,
    time: SimTime
)

final case class EnergyRecord(
    hostId: HostId,
    watts: Watts,
    fromTime: SimTime,
    toTime: SimTime,
    energyWh: WattHours
)

final case class MigrationRecord(
    vmId: VmId,
    sourceHostId: HostId,
    targetHostId: HostId,
    startTime: SimTime,
    duration: SimTime,
    dataTransferred: MegaBytes = MegaBytes.Zero
)

final case class InvocationResult(
    invocationId: InvocationId,
    functionId: FunctionId,
    startTime: SimTime,
    finishTime: SimTime,
    billedGBSeconds: GBSeconds,
    coldStart: Boolean
)

final case class ThrottledInvocation(
    invocationId: InvocationId,
    functionId: FunctionId,
    reason: String,
    time: SimTime
)

final case class TimedOutInvocation(
    invocationId: InvocationId,
    functionId: FunctionId,
    reason: String,
    time: SimTime
)

final case class PodResult(
    podId: PodId,
    hostId: HostId,
    startTime: SimTime,
    finishTime: Option[SimTime],
    phase: String
)

final case class PodSchedulingRecord(
    podId: PodId,
    hostId: HostId,
    nodeName: String,
    time: SimTime
)

final case class UnschedulablePod(
    podId: PodId,
    reason: String,
    time: SimTime
)

final case class EdgeTaskResult(
    taskId: EdgeTaskId,
    sourceNodeName: String,
    executionNodeName: String,
    offloaded: Boolean,
    networkLatency: SimTime,
    startTime: SimTime,
    finishTime: SimTime,
    offloadingReason: String
)

final case class FailedEdgeTask(
    taskId: EdgeTaskId,
    reason: String,
    time: SimTime
)

final case class FederatedTaskResult(
    taskId: FederatedTaskId,
    initialTier: ExecutionTier,
    finalTier: ExecutionTier,
    escalations: Int,
    startTime: SimTime,
    finishTime: SimTime
)

final case class FailedFederatedTask(
    taskId: FederatedTaskId,
    reason: String,
    tiersAttempted: Int,
    time: SimTime
)

final case class VmCostRecord(
    vmId: VmId,
    cpuCost: Cost,
    ramCost: Cost,
    bwCost: Cost,
    storageCost: Cost,
    totalCost: Cost,
    time: SimTime
)

final case class FaultRecord(
    hostId: HostId,
    failedPEs: PEs,
    time: SimTime
)

final case class VmDestroyedRecord(
    vmId: VmId,
    hostId: HostId,
    time: SimTime,
    reason: String
)

final case class SlaViolationRecord(
    hostId: HostId,
    utilization: Utilization,
    time: SimTime,
    reason: String
)

final case class ConsolidationRecord(
    datacenterId: DatacenterId,
    time: SimTime
)

final case class BatchJobRecord(
    jobId: JobId,
    startTime: SimTime,
    finishTime: SimTime,
    turnaroundTime: SimTime
)

// ─── Inference & GPU Energy Records ─────────────────────────────────────

final case class InferenceResult(
    requestId: InferenceRequestId,
    modelId: ModelId,
    promptTokens: Int,
    outputTokens: Int,
    ttft: SimTime,
    tpot: SimTime,
    totalTime: SimTime,
    totalEnergyWh: WattHours,
    sloMet: Boolean
)

final case class FailedInferenceRequest(
    requestId: InferenceRequestId,
    reason: String,
    time: SimTime
)

final case class GpuEnergyRecord(
    nodeId: GpuNodeId,
    deviceId: GpuDeviceId,
    watts: Watts,
    fromTime: SimTime,
    toTime: SimTime,
    energyWh: WattHours,
    computeWatts: Watts,
    memoryWatts: Watts
)

final case class DvfsRecord(
    nodeId: GpuNodeId,
    deviceId: GpuDeviceId,
    oldFreqMHz: Int,
    newFreqMHz: Int,
    time: SimTime,
    reason: String
)
