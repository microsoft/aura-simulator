// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.events

import io.aura.core.types.*

/** The core event abstraction for Aura DES.
  *
  * Uses a sealed ADT hierarchy for compile-time type safety and exhaustive pattern matching.
  */
final case class SimEvent(
    time: SimTime,
    source: EntityRef,
    destination: EntityRef,
    payload: SimEventPayload,
    serial: SerialNumber
)

object SimEvent:
  given Ordering[SimEvent] = Ordering.by(e => (e.time, e.serial))

/** Reference to a simulation entity (actor). */
final case class EntityRef(
    name: String,
    entityType: EntityType
)

enum EntityType:
  case TimeCoord, SimGuardian, Datacenter, Host, Broker, FaasPlatform, ServerlessBroker, K8sCluster, EdgeEnvironment,
    FederatedBroker, BatchBroker, GpuCluster, GpuNode, InferenceEngine, InferenceBroker, InferenceRouter

/** Sealed ADT for type-safe event payloads.
  *
  * Pattern matching on these is exhaustive at compile time.
  */
sealed trait SimEventPayload

object SimEventPayload:

  // ─── Simulation Lifecycle ────────────────────────────────────────────
  case object SimulationStart                       extends SimEventPayload
  case object SimulationEnd                         extends SimEventPayload
  case class TimeAdvance(newTime: SimTime)          extends SimEventPayload
  case class TimeStepComplete(entityRef: EntityRef) extends SimEventPayload

  // ─── VM Lifecycle ────────────────────────────────────────────────────
  case class VmCreateRequest(
      vmId: VmId,
      brokerId: BrokerId,
      spec: ResourceSpec
  ) extends SimEventPayload

  case class VmCreated(
      vmId: VmId,
      hostId: HostId,
      datacenterId: DatacenterId
  ) extends SimEventPayload

  case class VmCreateFailed(
      vmId: VmId,
      reason: String
  ) extends SimEventPayload

  case class VmDestroyRequest(vmId: VmId)            extends SimEventPayload
  case class VmDestroyed(vmId: VmId, hostId: HostId) extends SimEventPayload

  // ─── Workload Lifecycle ──────────────────────────────────────────────
  case class WorkloadSubmit(
      workload: WorkloadSpec
  ) extends SimEventPayload

  case class WorkloadAssign(
      workloadId: WorkloadId,
      vmId: VmId
  ) extends SimEventPayload

  case class WorkloadStarted(
      workloadId: WorkloadId,
      vmId: VmId,
      hostId: HostId,
      startTime: SimTime
  ) extends SimEventPayload

  case class WorkloadFinished(
      workloadId: WorkloadId,
      vmId: VmId,
      hostId: HostId,
      finishTime: SimTime,
      executedMI: MI
  ) extends SimEventPayload

  case class WorkloadUpdate(
      currentTime: SimTime
  ) extends SimEventPayload

  case class WorkloadFailed(
      workloadId: WorkloadId,
      reason: String
  ) extends SimEventPayload

  case class WorkloadPreempted(
      workloadId: WorkloadId,
      vmId: VmId,
      executedMI: MI
  ) extends SimEventPayload

  // ─── Host Events ─────────────────────────────────────────────────────
  case class HostUtilizationUpdate(
      hostId: HostId,
      utilization: Utilization,
      time: SimTime
  ) extends SimEventPayload

  case class EnergyReport(
      hostId: HostId,
      watts: Watts,
      fromTime: SimTime,
      toTime: SimTime,
      energyWh: WattHours
  ) extends SimEventPayload

  case class EnergySample(
      currentTime: SimTime
  ) extends SimEventPayload

  // ─── VM Migration Events ───────────────────────────────────────────
  case class VmMigrationRequest(
      vmId: VmId,
      sourceHostId: HostId,
      targetHostId: HostId
  ) extends SimEventPayload

  case class VmMigrationStart(
      vmId: VmId,
      sourceHostId: HostId,
      targetHostId: HostId,
      estimatedDuration: SimTime,
      dataTransferred: MegaBytes = MegaBytes.Zero
  ) extends SimEventPayload

  case class VmMigrationComplete(
      vmId: VmId,
      sourceHostId: HostId,
      targetHostId: HostId
  ) extends SimEventPayload

  case class VmMigrationFailed(
      vmId: VmId,
      reason: String
  ) extends SimEventPayload

  // ─── Vertical Scaling Events ───────────────────────────────────────
  case class VmScaleRequest(
      vmId: VmId,
      resource: ScalableResource,
      newAmount: Double,
      direction: ScalingDirection
  ) extends SimEventPayload

  case class VmScaled(
      vmId: VmId,
      resource: ScalableResource,
      oldAmount: Double,
      newAmount: Double
  ) extends SimEventPayload

  // ─── Datacenter Events ───────────────────────────────────────────────
  case class DatacenterRegistration(
      datacenterId: DatacenterId,
      entityRef: EntityRef
  ) extends SimEventPayload

  case class BrokerRegistration(
      brokerId: BrokerId,
      entityRef: EntityRef
  ) extends SimEventPayload

  // ─── Scheduling Events ───────────────────────────────────────────────
  case class ScheduleWorkloads(time: SimTime) extends SimEventPayload

  // ─── Horizontal Scaling Events ──────────────────────────────────────
  case class ScalingCheck(brokerId: BrokerId) extends SimEventPayload

  // ─── Cost Events ───────────────────────────────────────────────────
  case class VmCostReport(
      vmId: VmId,
      cpuCost: Cost,
      ramCost: Cost,
      bwCost: Cost,
      storageCost: Cost,
      totalCost: Cost
  ) extends SimEventPayload

  // ─── Consolidation Events ───────────────────────────────────────────
  case class ConsolidationCheck(datacenterId: DatacenterId, currentTime: SimTime) extends SimEventPayload

  // ─── Batch Scheduling Events ────────────────────────────────────────
  case class BatchJobSubmitted(
      jobId: JobId,
      name: String,
      priority: String,
      requiredNodes: Int,
      submitTime: SimTime
  ) extends SimEventPayload

  case class BatchJobStarted(
      jobId: JobId,
      startTime: SimTime,
      nodesAllocated: Int
  ) extends SimEventPayload

  case class BatchJobCompleted(
      jobId: JobId,
      startTime: SimTime,
      finishTime: SimTime,
      turnaroundTime: SimTime
  ) extends SimEventPayload

  case class BatchScheduleTick(currentTime: SimTime) extends SimEventPayload

  // ─── Spot Instance Events ──────────────────────────────────────────
  case class SpotInterruption(vmId: VmId, noticePeriod: SimTime) extends SimEventPayload

  // ─── Fault Injection Events ─────────────────────────────────────────
  case class HostFaultEvent(hostId: HostId, failedPEs: PEs) extends SimEventPayload

  // ─── Serverless Events ──────────────────────────────────────────────
  case class FunctionDeploy(
      functionId: FunctionId,
      spec: FunctionDeploySpec
  ) extends SimEventPayload

  case class FunctionInvoke(
      invocationId: InvocationId,
      functionId: FunctionId,
      executionLength: MI,
      inputSize: MegaBytes
  ) extends SimEventPayload

  case class InvocationStarted(
      invocationId: InvocationId,
      functionId: FunctionId,
      coldStart: Boolean,
      containerId: ContainerId
  ) extends SimEventPayload

  case class InvocationComplete(
      invocationId: InvocationId,
      functionId: FunctionId,
      startTime: SimTime,
      finishTime: SimTime,
      billedGBSeconds: GBSeconds,
      coldStart: Boolean
  ) extends SimEventPayload

  case class InvocationTimedOut(
      invocationId: InvocationId,
      functionId: FunctionId,
      reason: String
  ) extends SimEventPayload

  case class InvocationThrottled(
      invocationId: InvocationId,
      functionId: FunctionId,
      reason: String
  ) extends SimEventPayload

  case class ContainerCreated(
      containerId: ContainerId,
      functionId: FunctionId,
      warmUp: Boolean
  ) extends SimEventPayload

  case class ContainerEvicted(
      containerId: ContainerId,
      functionId: FunctionId
  ) extends SimEventPayload

  // ─── Container Orchestration Events ──────────────────────────────────
  case class PodScheduleRequest(
      podId: PodId,
      deploymentId: DeploymentId,
      spec: PodDeploySpec
  ) extends SimEventPayload

  case class PodScheduled(
      podId: PodId,
      hostId: HostId,
      nodeName: String
  ) extends SimEventPayload

  case class PodStarted(
      podId: PodId,
      hostId: HostId,
      startTime: SimTime
  ) extends SimEventPayload

  case class PodCompleted(
      podId: PodId,
      hostId: HostId,
      finishTime: SimTime
  ) extends SimEventPayload

  case class PodFailed(
      podId: PodId,
      hostId: HostId,
      reason: String
  ) extends SimEventPayload

  case class PodEvicted(
      podId: PodId,
      hostId: HostId,
      reason: String
  ) extends SimEventPayload

  case class PodUnschedulable(
      podId: PodId,
      reason: String
  ) extends SimEventPayload

  // ─── Edge Computing Events ─────────────────────────────────────────
  case class EdgeTaskSubmit(
      taskId: EdgeTaskId,
      sourceNodeName: String,
      cpuRequired: MIPS,
      memRequired: MegaBytes,
      taskLength: MI,
      deadline: SimTime
  ) extends SimEventPayload

  case class EdgeTaskStarted(
      taskId: EdgeTaskId,
      sourceNodeName: String,
      executionNodeName: String,
      offloaded: Boolean,
      networkLatency: SimTime,
      startTime: SimTime
  ) extends SimEventPayload

  case class EdgeTaskCompleted(
      taskId: EdgeTaskId,
      sourceNodeName: String,
      executionNodeName: String,
      offloaded: Boolean,
      networkLatency: SimTime,
      startTime: SimTime,
      finishTime: SimTime,
      offloadingReason: String
  ) extends SimEventPayload

  case class EdgeTaskFailed(
      taskId: EdgeTaskId,
      reason: String
  ) extends SimEventPayload

  // ─── Federated Orchestration Events ─────────────────────────────────
  case class FederatedTaskSubmit(
      taskId: FederatedTaskId,
      cpuRequired: MIPS,
      memRequired: MegaBytes,
      taskLength: MI,
      deadline: SimTime
  ) extends SimEventPayload

  case class FederatedTaskRouted(
      taskId: FederatedTaskId,
      tier: ExecutionTier,
      attempt: Int
  ) extends SimEventPayload

  case class FederatedTaskCompleted(
      taskId: FederatedTaskId,
      initialTier: ExecutionTier,
      finalTier: ExecutionTier,
      escalations: Int,
      startTime: SimTime,
      finishTime: SimTime
  ) extends SimEventPayload

  case class FederatedTaskFailed(
      taskId: FederatedTaskId,
      reason: String,
      tiersAttempted: Int
  ) extends SimEventPayload

  // ─── GPU Cluster Events ──────────────────────────────────────────────
  case class GpuNodeRegister(
      nodeId: GpuNodeId,
      gpuCount: Int,
      gpuMemoryMB: MegaBytes
  ) extends SimEventPayload

  case class GpuPowerReport(
      nodeId: GpuNodeId,
      deviceId: GpuDeviceId,
      watts: Watts,
      fromTime: SimTime,
      toTime: SimTime,
      energyWh: WattHours,
      computeWatts: Watts,
      memoryWatts: Watts
  ) extends SimEventPayload

  case class GpuDvfsChange(
      nodeId: GpuNodeId,
      deviceId: GpuDeviceId,
      oldFreqMHz: Int,
      newFreqMHz: Int,
      reason: String
  ) extends SimEventPayload

  // ─── LLM Inference Events ────────────────────────────────────────────
  case class InferenceRequestSubmit(
      requestId: InferenceRequestId,
      modelId: ModelId,
      promptTokens: Int,
      maxOutputTokens: Int,
      sloTtft: SimTime,
      sloTpot: SimTime
  ) extends SimEventPayload

  case class InferenceRequestAdmitted(
      requestId: InferenceRequestId,
      modelId: ModelId,
      engineName: String,
      admitTime: SimTime
  ) extends SimEventPayload

  case class InferencePrefillComplete(
      requestId: InferenceRequestId,
      prefillTime: SimTime,
      kvCachePages: Int,
      prefillWatts: Watts
  ) extends SimEventPayload

  case class InferenceDecodeStep(
      requestId: InferenceRequestId,
      tokensGenerated: Int,
      batchSize: Int,
      stepTime: SimTime,
      decodeWatts: Watts
  ) extends SimEventPayload

  case class InferenceRequestComplete(
      requestId: InferenceRequestId,
      modelId: ModelId,
      promptTokens: Int,
      outputTokens: Int,
      ttft: SimTime,
      tpot: SimTime,
      totalTime: SimTime,
      totalEnergyWh: WattHours,
      sloMet: Boolean
  ) extends SimEventPayload

  case class InferenceRequestFailed(
      requestId: InferenceRequestId,
      reason: String
  ) extends SimEventPayload

  case class InferenceBatchTick(
      engineName: String,
      currentTime: SimTime
  ) extends SimEventPayload

  case class InferenceRequestPreempted(
      requestId: InferenceRequestId,
      reason: String,
      tokensGenerated: Int
  ) extends SimEventPayload

enum ScalableResource:
  case CPU, RAM, BW

enum ScalingDirection:
  case Up, Down

/** Specification for a workload. */
final case class WorkloadSpec(
    id: WorkloadId,
    length: MI,
    pes: PEs,
    requiredMips: MIPS,
    fileSize: MegaBytes,
    outputSize: MegaBytes,
    utilizationCpu: Utilization,
    utilizationRam: Utilization,
    utilizationBw: Utilization,
    submissionDelay: SimTime = SimTime.Zero,
    weight: Int = 1024,
    predecessors: Set[WorkloadId] = Set.empty
)

object WorkloadSpec:
  def simple(
      id: WorkloadId,
      length: MI,
      pes: PEs
  ): WorkloadSpec =
    WorkloadSpec(
      id = id,
      length = length,
      pes = pes,
      requiredMips = MIPS(1000.0),
      fileSize = MegaBytes(300.0),
      outputSize = MegaBytes(300.0),
      utilizationCpu = Utilization.Full,
      utilizationRam = Utilization(0.5),
      utilizationBw = Utilization(0.5)
    )

/** Specification for a serverless function deployment. Lives in core because it's a field on SimEventPayload. Uses
  * runtime: String (not enum) to avoid core→serverless dependency.
  */
final case class FunctionDeploySpec(
    name: String,
    runtime: String,
    memoryMB: MegaBytes,
    timeout: SimTime,
    concurrencyLimit: Int = 1000,
    reservedConcurrency: Option[Int] = None
)

/** Resource specification for a single container within a pod. Lives in core because it's a field on PodDeploySpec →
  * SimEventPayload.
  */
final case class ContainerResourceSpec(
    name: String,
    image: String,
    cpuRequest: MIPS,
    cpuLimit: MIPS,
    memoryRequest: MegaBytes,
    memoryLimit: MegaBytes
)

/** Specification for an LLM inference request. Lives in core because it's referenced in SimEventPayload.
  */
final case class InferenceRequestSpec(
    requestId: InferenceRequestId,
    modelId: ModelId,
    promptTokens: Int,
    maxOutputTokens: Int,
    sloTtft: SimTime,
    sloTpot: SimTime,
    priority: Int = 0
)

/** Specification for a pod deployment. Lives in core because it's a field on SimEventPayload. Uses restartPolicy:
  * String (not enum) to avoid core→containers dependency.
  */
final case class PodDeploySpec(
    name: String,
    namespace: String,
    containers: Vector[ContainerResourceSpec],
    restartPolicy: String,
    priority: Int,
    nodeSelector: Map[String, String]
)
