// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.dsl

import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext

import io.aura.core.types.*
import io.aura.core.types.BriteParser
import io.aura.core.events.*
import io.aura.core.engine.{
  SimulationConfig,
  SimulationError,
  SimulationGuardian,
  SimulationResults,
  SimulationRunner,
  TimeCoordinator
}
import io.aura.iaas.actors.{BatchBrokerActor, BrokerActor, DatacenterActor}
import io.aura.iaas.policies.{
  CostRates,
  FaultInjection,
  FaultInjectionConfig,
  FaultRecoveryPolicy,
  HorizontalScalingPolicy,
  MigrationModel,
  SpotInstanceConfig,
  VerticalScalingPolicy,
  VmAllocationPolicy,
  WorkloadScheduler
}
import io.aura.iaas.policies.{
  OverloadDetector as ConsolidationOverloadDetector,
  UnderloadDetector as ConsolidationUnderloadDetector,
  VmSelectionPolicy as ConsolidationVmSelection
}
import io.aura.power.PowerModel
import io.aura.serverless.*
import io.aura.serverless.actors.{FaasPlatformActor, ServerlessBrokerActor}
import io.aura.containers.*
import io.aura.containers.state.{DeploymentRecord, WorkloadConfig}
import io.aura.containers.actors.{K8sBrokerActor, K8sClusterActor}
import io.aura.edge.*
import io.aura.edge.actors.{EdgeBrokerActor, EdgeEnvironmentActor}
import io.aura.gpu.*
import io.aura.inference.model.*
import io.aura.inference.actors.{InferenceBrokerActor, InferenceEngineActor, InferenceRouterActor}
import io.aura.inference.traces.AzureLlmTraceReader

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.*

/** Context-function DSL for simulation configuration.
  *
  * Example:
  * {{{
  * val results = simulation("example", endTime = SimTime(1000.0)) {
  *   datacenter("dc-1") {
  *     allocationPolicy(VmAllocationPolicy.bestFit)
  *     scheduler(WorkloadScheduler.timeShared)
  *     hosts(count = 2, pes = PEs(4), mips = MIPS(1000.0),
  *           ram = MegaBytes(8192.0), bw = Mbps(1000.0),
  *           storage = MegaBytes(100000.0))
  *   }
  *   broker("broker-1") {
  *     vm(pes = PEs(2), mips = MIPS(1000.0), ram = MegaBytes(2048.0))
  *     workload(length = MI(10000.0), pes = PEs(2))
  *   }
  * }.run()
  * }}}
  */
object SimulationDsl:

  // ─── Builder types (mutable internals, immutable output) ──────────────

  class HostBuilder(hostId: Long):
    private var _spec: ResourceSpec =
      ResourceSpec(PEs(1), MIPS(1000.0), MegaBytes(2048.0), Mbps(1000.0), MegaBytes(10000.0))
    private var _powerModel: PowerModel = PowerModel.linear(Watts(120.0), Watts(80.0))

    def spec(v: ResourceSpec): Unit     = _spec = v
    def powerModel(v: PowerModel): Unit = _powerModel = v

    def build: DatacenterActor.HostConfig =
      DatacenterActor.HostConfig(HostId(hostId), _spec, _powerModel)

  class DatacenterBuilder:
    private var _name: String                           = ""
    private val _hosts                                  = ArrayBuffer.empty[DatacenterActor.HostConfig]
    private var _allocationPolicy: VmAllocationPolicy   = VmAllocationPolicy.firstFit
    private var _scheduler: WorkloadScheduler           = WorkloadScheduler.timeShared
    private var _defaultPowerModel: PowerModel          = PowerModel.linear(Watts(120.0), Watts(80.0))
    private var _nextHostId: Long                       = 0
    private var _hostIdOffset: Long                     = 0
    private var _migrationModel: Option[MigrationModel] = None
    private var _migrationBandwidth: Mbps               = Mbps(10000.0)
    private var _overloadDetector: Option[DatacenterActor.OverloadDetector] = None
    private var _vmSelector: Option[DatacenterActor.VmSelectionPolicy]      = None
    private var _verticalScalingPolicy: Option[VerticalScalingPolicy]       = None
    private var _faultInjection: Option[FaultInjectionConfig]               = None
    private var _consolidation: Option[DatacenterActor.ConsolidationConfig] = None

    def name(v: String): Unit = _name = v

    def setHostIdOffset(offset: Long): Unit =
      _hostIdOffset = offset
      _nextHostId = offset

    def nextHostId: Long = _nextHostId

    def allocationPolicy(p: VmAllocationPolicy): Unit = _allocationPolicy = p
    def scheduler(s: WorkloadScheduler): Unit         = _scheduler = s
    def defaultPowerModel(p: PowerModel): Unit        = _defaultPowerModel = p

    def migrationPolicy(
        overloadDetector: DatacenterActor.OverloadDetector,
        vmSelector: DatacenterActor.VmSelectionPolicy,
        migrationModel: MigrationModel = io.aura.iaas.policies.MigrationModel.preCopy,
        bandwidth: Mbps = Mbps(10000.0)
    ): Unit =
      _overloadDetector = Some(overloadDetector)
      _vmSelector = Some(vmSelector)
      _migrationModel = Some(migrationModel)
      _migrationBandwidth = bandwidth

    def verticalScaling(policy: VerticalScalingPolicy): Unit =
      _verticalScalingPolicy = Some(policy)

    def faultInjection(config: FaultInjectionConfig): Unit =
      _faultInjection = Some(config)

    def consolidation(
        overloadDetector: ConsolidationOverloadDetector,
        underloadDetector: ConsolidationUnderloadDetector,
        vmSelector: ConsolidationVmSelection,
        interval: SimTime = SimTime(100.0),
        migrationModel: MigrationModel = io.aura.iaas.policies.MigrationModel.preCopy,
        bandwidth: Mbps = Mbps(10000.0)
    ): Unit =
      _consolidation = Some(
        DatacenterActor.ConsolidationConfig(overloadDetector, underloadDetector, vmSelector, interval)
      )
      _migrationModel = Some(migrationModel)
      _migrationBandwidth = bandwidth

    def host(
        pes: PEs = PEs(4),
        mips: MIPS = MIPS(1000.0),
        ram: MegaBytes = MegaBytes(8192.0),
        bw: Mbps = Mbps(1000.0),
        storage: MegaBytes = MegaBytes(100000.0),
        powerModel: Option[PowerModel] = None
    ): Unit =
      val pm = powerModel.getOrElse(_defaultPowerModel)
      _hosts += DatacenterActor.HostConfig(
        HostId(_nextHostId),
        ResourceSpec(pes, mips, ram, bw, storage),
        pm
      )
      _nextHostId += 1

    def hosts(
        count: Int,
        pes: PEs = PEs(4),
        mips: MIPS = MIPS(1000.0),
        ram: MegaBytes = MegaBytes(8192.0),
        bw: Mbps = Mbps(1000.0),
        storage: MegaBytes = MegaBytes(100000.0),
        powerModel: Option[PowerModel] = None
    ): Unit =
      val pm = powerModel.getOrElse(_defaultPowerModel)
      for _ <- 0 until count do
        _hosts += DatacenterActor.HostConfig(
          HostId(_nextHostId),
          ResourceSpec(pes, mips, ram, bw, storage),
          pm
        )
        _nextHostId += 1

    def build(id: Long): DatacenterConfig =
      DatacenterConfig(
        datacenterId = DatacenterId(id),
        name = _name,
        hosts = _hosts.toVector,
        allocationPolicy = _allocationPolicy,
        scheduler = _scheduler,
        migrationModel = _migrationModel,
        migrationBandwidth = _migrationBandwidth,
        overloadDetector = _overloadDetector,
        vmSelector = _vmSelector,
        faultInjection = _faultInjection,
        consolidation = _consolidation
      )

  class BrokerBuilder:
    private var _name: String                                       = ""
    private val _vms                                                = ArrayBuffer.empty[BrokerActor.VmRequest]
    private val _workloads                                          = ArrayBuffer.empty[WorkloadSpec]
    private var _nextVmId: Long                                     = 0
    private var _nextWorkloadId: Long                               = 0
    private var _targetDcIndex: Int                                 = 0
    private var _horizontalScaling: Option[HorizontalScalingPolicy] = None
    private var _maxVms: Int                                        = Int.MaxValue
    private var _costRates: Option[CostRates]                       = None

    private var _spotConfig: Option[SpotInstanceConfig] = None
    private var _faultRecovery: FaultRecoveryPolicy     = FaultRecoveryPolicy.none
    private var _vmBootConfig: VmBootConfig             = VmBootConfig.instant

    def name(v: String): Unit                                    = _name = v
    def targetDatacenter(index: Int): Unit                       = _targetDcIndex = index
    def horizontalScaling(policy: HorizontalScalingPolicy): Unit = _horizontalScaling = Some(policy)
    def maxVms(count: Int): Unit                                 = _maxVms = count
    def costRates(rates: CostRates): Unit                        = _costRates = Some(rates)
    def spotConfig(config: SpotInstanceConfig): Unit             = _spotConfig = Some(config)
    def faultRecovery(policy: FaultRecoveryPolicy): Unit         = _faultRecovery = policy
    def vmBootConfig(config: VmBootConfig): Unit                 = _vmBootConfig = config

    def setIdOffsets(vmOffset: Long, workloadOffset: Long): Unit =
      _nextVmId = vmOffset
      _nextWorkloadId = workloadOffset

    def nextVmId: Long       = _nextVmId
    def nextWorkloadId: Long = _nextWorkloadId

    def vm(
        pes: PEs = PEs(2),
        mips: MIPS = MIPS(1000.0),
        ram: MegaBytes = MegaBytes(2048.0),
        bw: Mbps = Mbps(500.0),
        storage: MegaBytes = MegaBytes(10000.0),
        submissionTime: SimTime = SimTime.Zero
    ): Unit =
      _vms += BrokerActor.VmRequest(
        VmId(_nextVmId),
        ResourceSpec(pes, mips, ram, bw, storage),
        submissionTime
      )
      _nextVmId += 1

    def vms(
        count: Int,
        pes: PEs = PEs(2),
        mips: MIPS = MIPS(1000.0),
        ram: MegaBytes = MegaBytes(2048.0),
        bw: Mbps = Mbps(500.0),
        storage: MegaBytes = MegaBytes(10000.0),
        submissionTime: SimTime = SimTime.Zero
    ): Unit =
      for _ <- 0 until count do vm(pes, mips, ram, bw, storage, submissionTime)

    def workload(
        length: MI = MI(10000.0),
        pes: PEs = PEs(1),
        requiredMips: MIPS = MIPS(1000.0),
        fileSize: MegaBytes = MegaBytes(300.0),
        outputSize: MegaBytes = MegaBytes(300.0),
        submissionDelay: SimTime = SimTime.Zero,
        weight: Int = 1024,
        dependsOn: Set[WorkloadId] = Set.empty
    ): WorkloadId =
      val wlId = WorkloadId(_nextWorkloadId)
      _workloads += WorkloadSpec(
        id = wlId,
        length = length,
        pes = pes,
        requiredMips = requiredMips,
        fileSize = fileSize,
        outputSize = outputSize,
        utilizationCpu = Utilization.Full,
        utilizationRam = Utilization(0.5),
        utilizationBw = Utilization(0.5),
        submissionDelay = submissionDelay,
        weight = weight,
        predecessors = dependsOn
      )
      _nextWorkloadId += 1
      wlId

    def workloads(
        count: Int,
        length: MI = MI(10000.0),
        pes: PEs = PEs(1),
        weight: Int = 1024
    ): Unit =
      for _ <- 0 until count do workload(length = length, pes = pes, weight = weight)

    def build(id: Long): BrokerConfig =
      BrokerConfig(
        brokerId = BrokerId(id),
        name = _name,
        vmRequests = _vms.toVector,
        workloads = _workloads.toVector,
        targetDcIndex = _targetDcIndex,
        horizontalScaling = _horizontalScaling,
        maxVms = _maxVms,
        costRates = _costRates,
        spotConfig = _spotConfig,
        faultRecovery = _faultRecovery,
        vmBootConfig = _vmBootConfig
      )

  class NetworkBuilder:
    private val _links = ArrayBuffer.empty[PendingLink]

    final case class PendingLink(
        fromName: String,
        toName: String,
        latency: SimTime,
        bandwidth: Mbps
    )

    def link(
        from: String,
        to: String,
        latency: SimTime,
        bandwidth: Mbps = Mbps(10000.0)
    ): Unit =
      _links += PendingLink(from, to, latency, bandwidth)

    def build(dcNameToId: Map[String, DatacenterId]): NetworkTopology =
      val networkLinks = _links.toVector.flatMap { pl =>
        for
          fromId <- dcNameToId.get(pl.fromName)
          toId   <- dcNameToId.get(pl.toName)
        yield NetworkLink(fromId, toId, pl.latency, pl.bandwidth)
      }
      NetworkTopology.fromLinks(networkLinks)

  // ─── Serverless Builders ──────────────────────────────────────────────

  class FunctionBuilder:
    private var _runtime: Runtime                 = Runtime.Python
    private var _memory: MegaBytes                = MegaBytes(256.0)
    private var _timeout: SimTime                 = SimTime(30.0)
    private var _concurrencyLimit: Int            = 1000
    private var _reservedConcurrency: Option[Int] = None

    def runtime(v: Runtime): Unit         = _runtime = v
    def memory(v: MegaBytes): Unit        = _memory = v
    def timeout(v: SimTime): Unit         = _timeout = v
    def concurrencyLimit(v: Int): Unit    = _concurrencyLimit = v
    def reservedConcurrency(v: Int): Unit = _reservedConcurrency = Some(v)

    def build(id: FunctionId, name: String): FunctionDefinition =
      FunctionDefinition(
        functionId = id,
        name = name,
        memoryMB = _memory,
        timeout = _timeout,
        runtime = _runtime,
        concurrencyLimit = _concurrencyLimit,
        reservedConcurrency = _reservedConcurrency
      )

  class FaasPlatformBuilder:
    private var _name: String                                  = ""
    private var _coldStartModel: ColdStartModel.ColdStartModel = ColdStartModel.byRuntime
    private var _billingModel: BillingModel.BillingModel       = BillingModel.awsLambda
    private var _scalingPolicy: ScalingPolicy.ScalingPolicy    = ScalingPolicy.reactive
    private var _containerTtl: SimTime                         = SimTime(600.0)
    private var _executionMips: MIPS                           = MIPS(1000.0)
    private val _functions                                     = ArrayBuffer.empty[(String, FunctionBuilder)]

    def name(v: String): Unit                                  = _name = v
    def coldStartModel(v: ColdStartModel.ColdStartModel): Unit = _coldStartModel = v
    def billingModel(v: BillingModel.BillingModel): Unit       = _billingModel = v
    def scalingPolicy(v: ScalingPolicy.ScalingPolicy): Unit    = _scalingPolicy = v
    def containerTtl(v: SimTime): Unit                         = _containerTtl = v
    def executionMips(v: MIPS): Unit                           = _executionMips = v

    def function(funcName: String)(configure: FunctionBuilder ?=> Unit): Unit =
      val builder = new FunctionBuilder
      configure(using builder)
      _functions += (funcName -> builder)

    def functionBuilders: Vector[(String, FunctionBuilder)] = _functions.toVector

    def build(functions: Vector[FunctionDefinition]): FaasPlatformConfig =
      FaasPlatformConfig(
        name = _name,
        coldStartModel = _coldStartModel,
        billingModel = _billingModel,
        scalingPolicy = _scalingPolicy,
        containerTtl = _containerTtl,
        executionMips = _executionMips,
        functions = functions
      )

  class ServerlessBrokerBuilder:
    private var _name: String             = ""
    private val _batches                  = ArrayBuffer.empty[InvocationBatchConfig]
    private var _targetPlatformIndex: Int = 0
    private var _startTime: SimTime       = SimTime.Zero

    def name(v: String): Unit            = _name = v
    def targetPlatform(index: Int): Unit = _targetPlatformIndex = index
    def startTime(v: SimTime): Unit      = _startTime = v

    def invocations(
        functionName: String,
        count: Int,
        executionLength: MI = MI(1000.0),
        inputSize: MegaBytes = MegaBytes(1.0),
        arrivalPattern: ArrivalPattern.ArrivalPattern = ArrivalPattern.uniform(SimTime(1.0))
    ): Unit =
      _batches += InvocationBatchConfig(functionName, count, executionLength, inputSize, arrivalPattern)

    def build(id: String): ServerlessBrokerConfig =
      ServerlessBrokerConfig(
        brokerId = id,
        name = _name,
        batches = _batches.toVector,
        targetPlatformIndex = _targetPlatformIndex,
        startTime = _startTime
      )

  // ─── Container Orchestration Builders ────────────────────────────────

  class ContainerBuilder:
    private var _image: String            = "default:latest"
    private var _cpuRequest: MIPS         = MIPS(500.0)
    private var _cpuLimit: MIPS           = MIPS(1000.0)
    private var _memoryRequest: MegaBytes = MegaBytes(256.0)
    private var _memoryLimit: MegaBytes   = MegaBytes(512.0)

    def image(v: String): Unit            = _image = v
    def cpuRequest(v: MIPS): Unit         = _cpuRequest = v
    def cpuLimit(v: MIPS): Unit           = _cpuLimit = v
    def memoryRequest(v: MegaBytes): Unit = _memoryRequest = v
    def memoryLimit(v: MegaBytes): Unit   = _memoryLimit = v

    def build(name: String): ContainerConfig =
      ContainerConfig(name, _image, _cpuRequest, _cpuLimit, _memoryRequest, _memoryLimit)

  class DeploymentBuilder:
    private var _replicas: Int                          = 1
    private val _containers                             = ArrayBuffer.empty[(String, ContainerBuilder)]
    private var _restartPolicy: RestartPolicy           = RestartPolicy.Never
    private var _priority: Int                          = 0
    private var _nodeSelector: Map[String, String]      = Map.empty
    private var _workloadConfig: Option[WorkloadConfig] = None

    def replicas(v: Int): Unit                     = _replicas = v
    def restartPolicy(v: RestartPolicy): Unit      = _restartPolicy = v
    def priority(v: Int): Unit                     = _priority = v
    def nodeSelector(v: Map[String, String]): Unit = _nodeSelector = v
    def workloadPerPod(length: MI, pes: PEs): Unit =
      _workloadConfig = Some(WorkloadConfig(length, pes))

    def container(name: String)(configure: ContainerBuilder ?=> Unit): Unit =
      val builder = new ContainerBuilder
      configure(using builder)
      _containers += (name -> builder)

    def build(id: DeploymentId, name: String): DeploymentConfig =
      val containers = _containers.toVector.map { case (n, b) => b.build(n) }
      DeploymentConfig(id, name, _replicas, containers, _restartPolicy, _priority, _nodeSelector, _workloadConfig)

  class K8sClusterBuilder:
    private var _name: String                          = ""
    private var _schedulingPolicy: K8sSchedulingPolicy = K8sScheduler.leastRequested
    private var _targetDcIndex: Int                    = 0
    private var _startupDelay: SimTime                 = SimTime.Zero
    private val _deployments                           = ArrayBuffer.empty[(String, DeploymentBuilder)]

    def name(v: String): Unit                          = _name = v
    def schedulingPolicy(v: K8sSchedulingPolicy): Unit = _schedulingPolicy = v
    def targetDatacenter(index: Int): Unit             = _targetDcIndex = index
    def startupDelay(v: SimTime): Unit                 = _startupDelay = v

    def deployment(depName: String)(configure: DeploymentBuilder ?=> Unit): Unit =
      val builder = new DeploymentBuilder
      configure(using builder)
      _deployments += (depName -> builder)

    def deploymentBuilders: Vector[(String, DeploymentBuilder)] = _deployments.toVector

    def build(deployments: Vector[DeploymentConfig]): K8sClusterConfig =
      K8sClusterConfig(_name, _schedulingPolicy, _targetDcIndex, _startupDelay, deployments)

  // ─── Edge Computing Builders ───────────────────────────────────────

  class EdgeNodeBuilder:
    private var _location: GeoLocation     = GeoLocation(0.0, 0.0)
    private var _tier: EdgeTier            = EdgeTier.EdgeMicro
    private var _pes: PEs                  = PEs(1)
    private var _mips: MIPS                = MIPS(1000.0)
    private var _ram: MegaBytes            = MegaBytes(1024.0)
    private val _coverageRadiusKm: Double  = 10.0
    private val _maxDeviceConnections: Int = 1000

    def location(lat: Double, lon: Double): Unit = _location = GeoLocation(lat, lon)
    def tier(t: EdgeTier): Unit                  = _tier = t
    def resources(pes: PEs, mips: MIPS, ram: MegaBytes): Unit =
      _pes = pes
      _mips = mips
      _ram = ram

    def build(name: String): EdgeNodeSpec =
      EdgeNodeSpec(
        name = name,
        location = _location,
        spec = ResourceSpec(_pes, _mips, _ram, Mbps(1000.0), MegaBytes(10000.0)),
        tier = _tier,
        coverageRadiusKm = _coverageRadiusKm,
        maxDeviceConnections = _maxDeviceConnections
      )

  class TaskSourceBuilder:
    private var _sourceNode: String        = ""
    private var _count: Int                = 1
    private var _cpuRequired: MIPS         = MIPS(100.0)
    private var _memRequired: MegaBytes    = MegaBytes(64.0)
    private var _taskLength: MI            = MI(500.0)
    private var _deadline: SimTime         = SimTime(1.0)
    private var _interArrivalTime: SimTime = SimTime(1.0)

    def sourceNode(name: String): Unit = _sourceNode = name
    def tasks(count: Int, cpuRequired: MIPS, memRequired: MegaBytes, taskLength: MI, deadline: SimTime): Unit =
      _count = count
      _cpuRequired = cpuRequired
      _memRequired = memRequired
      _taskLength = taskLength
      _deadline = deadline
    def interArrivalTime(t: SimTime): Unit = _interArrivalTime = t

    def build: TaskSourceConfig =
      TaskSourceConfig(_sourceNode, _count, _cpuRequired, _memRequired, _taskLength, _deadline, _interArrivalTime)

  class EdgeEnvironmentBuilder:
    private var _name: String                       = ""
    private var _latencyModel: LatencyModel         = LatencyModel.combined()
    private var _offloadingPolicy: OffloadingPolicy = OffloadingPolicy.latencyAware
    private val _nodes                              = ArrayBuffer.empty[(String, EdgeNodeBuilder)]
    private val _taskSources                        = ArrayBuffer.empty[TaskSourceBuilder]

    def name(v: String): Unit                       = _name = v
    def latencyModel(v: LatencyModel): Unit         = _latencyModel = v
    def offloadingPolicy(v: OffloadingPolicy): Unit = _offloadingPolicy = v

    def edgeNode(nodeName: String)(configure: EdgeNodeBuilder ?=> Unit): Unit =
      val builder = new EdgeNodeBuilder
      configure(using builder)
      _nodes += (nodeName -> builder)

    def taskSource(sourceName: String)(configure: TaskSourceBuilder ?=> Unit): Unit =
      val builder = new TaskSourceBuilder
      configure(using builder)
      _taskSources += builder

    def nodeBuilders: Vector[(String, EdgeNodeBuilder)] = _nodes.toVector
    def taskSourceBuilders: Vector[TaskSourceBuilder]   = _taskSources.toVector

    def build(nodeSpecs: Vector[EdgeNodeSpec], taskSources: Vector[TaskSourceConfig]): EdgeEnvironmentConfig =
      EdgeEnvironmentConfig(_name, _latencyModel, _offloadingPolicy, nodeSpecs, taskSources)

  // ─── Batch Scheduling Builder ─────────────────────────────────────

  class BatchJobBuilder:
    private var _name: String              = "job"
    private var _priority: JobPriority     = JobPriority.Normal
    private var _requiredNodes: Int        = 1
    private var _cpuPerNode: MIPS          = MIPS(1000.0)
    private var _ramPerNode: MegaBytes     = MegaBytes(2048.0)
    private var _estimatedRuntime: SimTime = SimTime(100.0)
    private var _submitTime: SimTime       = SimTime.Zero
    private var _isGang: Boolean           = false

    def name(v: String): Unit              = _name = v
    def priority(v: JobPriority): Unit     = _priority = v
    def requiredNodes(v: Int): Unit        = _requiredNodes = v
    def cpuPerNode(v: MIPS): Unit          = _cpuPerNode = v
    def ramPerNode(v: MegaBytes): Unit     = _ramPerNode = v
    def estimatedRuntime(v: SimTime): Unit = _estimatedRuntime = v
    def submitTime(v: SimTime): Unit       = _submitTime = v
    def gang(v: Boolean): Unit             = _isGang = v

    def build(id: JobId): BatchJob =
      BatchJob(
        id = id,
        name = _name,
        priority = _priority,
        requiredNodes = _requiredNodes,
        cpuPerNode = _cpuPerNode,
        ramPerNode = _ramPerNode,
        estimatedRuntime = _estimatedRuntime,
        submitTime = _submitTime,
        isGang = _isGang
      )

  class BatchBrokerBuilder:
    private var _name: String                                         = ""
    private var _algorithm: BatchBrokerActor.BatchSchedulingAlgorithm = BatchScheduler.fcfs
    private var _tickInterval: SimTime                                = SimTime(10.0)
    private var _targetDcIndex: Int                                   = 0
    private val _jobs                                                 = ArrayBuffer.empty[BatchJobBuilder]
    private var _nodeCount: Int                                       = 4
    private var _nodeCpu: MIPS                                        = MIPS(1000.0)
    private var _nodeRam: MegaBytes                                   = MegaBytes(8192.0)

    def name(v: String): Unit                                         = _name = v
    def algorithm(v: BatchBrokerActor.BatchSchedulingAlgorithm): Unit = _algorithm = v
    def tickInterval(v: SimTime): Unit                                = _tickInterval = v
    def targetDatacenter(index: Int): Unit                            = _targetDcIndex = index
    def computeNodes(count: Int, cpu: MIPS = MIPS(1000.0), ram: MegaBytes = MegaBytes(8192.0)): Unit =
      _nodeCount = count
      _nodeCpu = cpu
      _nodeRam = ram

    def job(jobName: String)(configure: BatchJobBuilder ?=> Unit): Unit =
      val builder = new BatchJobBuilder
      builder.name(jobName)
      configure(using builder)
      _jobs += builder

    def jobs(
        count: Int,
        namePrefix: String = "job",
        priority: JobPriority = JobPriority.Normal,
        requiredNodes: Int = 1,
        cpuPerNode: MIPS = MIPS(1000.0),
        ramPerNode: MegaBytes = MegaBytes(2048.0),
        estimatedRuntime: SimTime = SimTime(100.0),
        submitTime: SimTime = SimTime.Zero,
        isGang: Boolean = false
    ): Unit =
      for i <- 0 until count do
        val builder = new BatchJobBuilder
        builder.name(s"$namePrefix-$i")
        builder.priority(priority)
        builder.requiredNodes(requiredNodes)
        builder.cpuPerNode(cpuPerNode)
        builder.ramPerNode(ramPerNode)
        builder.estimatedRuntime(estimatedRuntime)
        builder.submitTime(submitTime)
        builder.gang(isGang)
        _jobs += builder

    def jobBuilders: Vector[BatchJobBuilder] = _jobs.toVector

    def build(nodes: Vector[ComputeNode], builtJobs: Vector[BatchJob]): BatchBrokerConfig =
      BatchBrokerConfig(
        name = _name,
        algorithm = _algorithm,
        tickInterval = _tickInterval,
        targetDcIndex = _targetDcIndex,
        nodeCount = _nodeCount,
        nodeCpu = _nodeCpu,
        nodeRam = _nodeRam,
        jobs = builtJobs
      )

  // ─── Federated Workflow Builder ────────────────────────────────────

  class FederatedWorkflowBuilder:
    private var _tierSelection: TierSelectionPolicy       = TierSelectionPolicy.edgeFirst
    private var _escalation: EscalationPolicy             = EscalationPolicy.cascade
    private var _edgeTier: Option[(String, String)]       = None
    private var _serverlessTier: Option[(String, String)] = None
    private var _k8sTier: Option[String]                  = None
    private var _count: Int                               = 10
    private var _cpuRequired: MIPS                        = MIPS(100.0)
    private var _memRequired: MegaBytes                   = MegaBytes(64.0)
    private var _taskLength: MI                           = MI(500.0)
    private var _deadline: SimTime                        = SimTime(1.0)
    private var _interArrivalTime: SimTime                = SimTime(1.0)

    def tierSelection(v: TierSelectionPolicy): Unit                  = _tierSelection = v
    def escalation(v: EscalationPolicy): Unit                        = _escalation = v
    def edgeTier(environment: String, sourceNode: String): Unit      = _edgeTier = Some((environment, sourceNode))
    def serverlessTier(platform: String, functionName: String): Unit = _serverlessTier = Some((platform, functionName))
    def k8sTier(cluster: String): Unit                               = _k8sTier = Some(cluster)
    def tasks(count: Int, cpuRequired: MIPS, memRequired: MegaBytes, taskLength: MI, deadline: SimTime): Unit =
      _count = count
      _cpuRequired = cpuRequired
      _memRequired = memRequired
      _taskLength = taskLength
      _deadline = deadline
    def interArrivalTime(t: SimTime): Unit = _interArrivalTime = t

    def build(name: String): FederatedWorkflowConfig =
      FederatedWorkflowConfig(
        name = name,
        tierSelection = _tierSelection,
        escalation = _escalation,
        edgeTier = _edgeTier,
        serverlessTier = _serverlessTier,
        k8sTier = _k8sTier,
        count = _count,
        cpuRequired = _cpuRequired,
        memRequired = _memRequired,
        taskLength = _taskLength,
        deadline = _deadline,
        interArrivalTime = _interArrivalTime
      )

  // ─── Inference Builders ──────────────────────────────────────────────

  class InferenceEngineBuilder:
    private var _name: String               = ""
    private var _model: LlmModelSpec        = LlmModelSpec.llama2_7b
    private var _deviceSpec: GpuDeviceSpec  = GpuDeviceSpec.a100Sxm
    private var _tpDegree: Int              = 1
    private var _ppStages: Int              = 1
    private var _epDegree: Int              = 1
    private var _maxBatchSize: Int          = 256
    private var _iterationInterval: SimTime = SimTime(0.01)
    private var _dvfsPolicy: DvfsPolicy     = DvfsPolicy.maxPerformance
    private var _powerBudget: Watts         = Watts(400.0)
    private var _schedulingStrategy: InferenceEngineActor.SchedulingStrategy =
      InferenceEngineActor.SchedulingStrategy.ContinuousBatching

    def name(v: String): Unit                                                = _name = v
    def model(v: LlmModelSpec): Unit                                         = _model = v
    def deviceSpec(v: GpuDeviceSpec): Unit                                   = _deviceSpec = v
    def tpDegree(v: Int): Unit                                               = _tpDegree = v
    def ppStages(v: Int): Unit                                               = _ppStages = v
    def epDegree(v: Int): Unit                                               = _epDegree = v
    def maxBatchSize(v: Int): Unit                                           = _maxBatchSize = v
    def iterationInterval(v: SimTime): Unit                                  = _iterationInterval = v
    def dvfsPolicy(v: DvfsPolicy): Unit                                      = _dvfsPolicy = v
    def powerBudget(v: Watts): Unit                                          = _powerBudget = v
    def schedulingStrategy(v: InferenceEngineActor.SchedulingStrategy): Unit = _schedulingStrategy = v

    def build: InferenceEngineConfig =
      InferenceEngineConfig(
        name = _name,
        model = _model,
        deviceSpec = _deviceSpec,
        tpDegree = _tpDegree,
        ppStages = _ppStages,
        epDegree = _epDegree,
        maxBatchSize = _maxBatchSize,
        iterationInterval = _iterationInterval,
        dvfsPolicy = _dvfsPolicy,
        powerBudget = _powerBudget,
        schedulingStrategy = _schedulingStrategy
      )

  class InferenceBrokerBuilder:
    private var _name: String                     = ""
    private var _targetEngineIndex: Int           = 0
    private var _targetRouterName: Option[String] = None
    private var _sloTtft: SimTime                 = SimTime(0.5)
    private var _sloTpot: SimTime                 = SimTime(0.05)
    private val _batches                          = ArrayBuffer.empty[InferenceRequestBatchConfig]
    private var _traceEntries: Option[Vector[AzureLlmTraceReader.TraceEntry]] = None

    def name(v: String): Unit            = _name = v
    def targetEngine(index: Int): Unit   = _targetEngineIndex = index
    def targetRouter(name: String): Unit = _targetRouterName = Some(name)
    def sloTtft(v: SimTime): Unit        = _sloTtft = v
    def sloTpot(v: SimTime): Unit        = _sloTpot = v

    def requests(
        count: Int,
        promptTokens: Int = 512,
        maxOutputTokens: Int = 128,
        arrivalRate: Double = 10.0,
        startTime: SimTime = SimTime.Zero
    ): Unit =
      _batches += InferenceRequestBatchConfig(count, promptTokens, maxOutputTokens, arrivalRate, startTime)

    /** Use trace entries directly as request source. */
    def traceRequests(entries: Vector[AzureLlmTraceReader.TraceEntry]): Unit =
      _traceEntries = Some(entries)

    /** Generate synthetic trace with given parameters. */
    def syntheticTrace(
        count: Int,
        ratePerSecond: Double,
        avgPromptTokens: Int = 512,
        avgOutputTokens: Int = 128,
        seed: Long = 42L
    ): Unit =
      _traceEntries = Some(
        AzureLlmTraceReader.generateSyntheticTrace(
          count,
          ratePerSecond,
          avgPromptTokens,
          avgOutputTokens,
          seed
        )
      )

    def build(id: String): InferenceBrokerConfig =
      // Convert trace entries to batches if provided
      _traceEntries.foreach { entries =>
        entries.foreach { entry =>
          _batches += InferenceRequestBatchConfig(
            count = 1,
            promptTokens = entry.promptTokens,
            maxOutputTokens = entry.completionTokens,
            arrivalRate = 0.0, // arrival time comes from trace
            startTime = entry.arrivalTime
          )
        }
      }
      InferenceBrokerConfig(
        brokerId = id,
        name = _name,
        targetEngineIndex = _targetEngineIndex,
        targetRouterName = _targetRouterName,
        sloTtft = _sloTtft,
        sloTpot = _sloTpot,
        batches = _batches.toVector
      )

  class SimulationBuilder:
    private var _name: String                           = "simulation"
    private var _endTime: SimTime                       = SimTime(1000.0)
    def name(v: String): Unit                           = _name = v
    private val _datacenters                            = ArrayBuffer.empty[DatacenterConfig]
    private val _brokers                                = ArrayBuffer.empty[BrokerConfig]
    private val _faasPlatforms                          = ArrayBuffer.empty[FaasPlatformConfig]
    private val _serverlessBrokers                      = ArrayBuffer.empty[ServerlessBrokerConfig]
    private var _nextDcId: Long                         = 0
    private var _nextBrokerId: Long                     = 0
    private var _nextGlobalHostId: Long                 = 0
    private var _nextGlobalVmId: Long                   = 0
    private var _nextGlobalWorkloadId: Long             = 0
    private var _nextGlobalFunctionId: Long             = 0
    private var _nextGlobalInvocationId: Long           = 0
    private var _nextServerlessBrokerId: Long           = 0
    private val _k8sClusters                            = ArrayBuffer.empty[K8sClusterConfig]
    private var _nextGlobalPodId: Long                  = 0
    private var _nextGlobalDeploymentId: Long           = 0
    private var _nextK8sClusterId: Long                 = 0
    private val _edgeEnvironments                       = ArrayBuffer.empty[EdgeEnvironmentConfig]
    private var _nextGlobalEdgeTaskId: Long             = 0
    private val _federatedWorkflows                     = ArrayBuffer.empty[FederatedWorkflowConfig]
    private var _nextGlobalFederatedTaskId: Long        = 0
    private val _batchBrokers                           = ArrayBuffer.empty[BatchBrokerConfig]
    private var _nextGlobalJobId: Long                  = 0
    private val _inferenceEngines                       = ArrayBuffer.empty[InferenceEngineConfig]
    private val _inferenceBrokers                       = ArrayBuffer.empty[InferenceBrokerConfig]
    private val _inferenceRouters                       = ArrayBuffer.empty[InferenceRouterConfig]
    private var _nextGlobalInferenceRequestId: Long     = 0
    private var _nextInferenceBrokerId: Long            = 0
    private var _networkBuilder: Option[NetworkBuilder] = None
    private val _dcNames                                = ArrayBuffer.empty[(String, DatacenterId)]
    private var _briteTopology: Option[NetworkTopology] = None

    def endTime(t: SimTime): Unit = _endTime = t

    def datacenter(name: String)(configure: DatacenterBuilder ?=> Unit): Unit =
      val builder = new DatacenterBuilder
      builder.name(name)
      builder.setHostIdOffset(_nextGlobalHostId)
      configure(using builder)
      val dcId = DatacenterId(_nextDcId)
      _datacenters += builder.build(_nextDcId)
      _nextGlobalHostId = builder.nextHostId
      _dcNames += (name -> dcId)
      _nextDcId += 1

    def broker(name: String)(configure: BrokerBuilder ?=> Unit): Unit =
      val builder = new BrokerBuilder
      builder.name(name)
      builder.setIdOffsets(_nextGlobalVmId, _nextGlobalWorkloadId)
      configure(using builder)
      val config = builder.build(_nextBrokerId)
      _nextGlobalVmId = builder.nextVmId
      _nextGlobalWorkloadId = builder.nextWorkloadId
      _brokers += config
      _nextBrokerId += 1

    def network(configure: NetworkBuilder ?=> Unit): Unit =
      val builder = new NetworkBuilder
      configure(using builder)
      _networkBuilder = Some(builder)

    def faasPlatform(platformName: String)(configure: FaasPlatformBuilder ?=> Unit): Unit =
      val builder = new FaasPlatformBuilder
      builder.name(platformName)
      configure(using builder)
      // Assign globally unique function IDs
      val functions = builder.functionBuilders.map { case (funcName, funcBuilder) =>
        val funcId = FunctionId(_nextGlobalFunctionId)
        _nextGlobalFunctionId += 1
        funcBuilder.build(funcId, funcName)
      }
      _faasPlatforms += builder.build(functions)

    def serverlessBroker(brokerName: String)(configure: ServerlessBrokerBuilder ?=> Unit): Unit =
      val builder = new ServerlessBrokerBuilder
      builder.name(brokerName)
      configure(using builder)
      val brokerId = s"serverless-broker-${_nextServerlessBrokerId}"
      _nextServerlessBrokerId += 1
      val config = builder.build(brokerId)
      // Advance invocation ID counter for each batch
      config.batches.foreach(b => _nextGlobalInvocationId += b.count)
      _serverlessBrokers += config

    def k8sCluster(clusterName: String)(configure: K8sClusterBuilder ?=> Unit): Unit =
      val builder = new K8sClusterBuilder
      builder.name(clusterName)
      configure(using builder)
      // Assign globally unique deployment IDs and build configs
      val deployments = builder.deploymentBuilders.map { case (depName, depBuilder) =>
        val depId = DeploymentId(_nextGlobalDeploymentId)
        _nextGlobalDeploymentId += 1
        val config = depBuilder.build(depId, depName)
        // Advance pod ID counter for each replica
        _nextGlobalPodId += config.replicas
        config
      }
      _k8sClusters += builder.build(deployments)
      _nextK8sClusterId += 1

    def edgeEnvironment(envName: String)(configure: EdgeEnvironmentBuilder ?=> Unit): Unit =
      val builder = new EdgeEnvironmentBuilder
      builder.name(envName)
      configure(using builder)
      val nodeSpecs   = builder.nodeBuilders.map { case (name, nb) => nb.build(name) }
      val taskSources = builder.taskSourceBuilders.map(_.build)
      // Advance edge task ID counter
      taskSources.foreach(ts => _nextGlobalEdgeTaskId += ts.count)
      _edgeEnvironments += builder.build(nodeSpecs, taskSources)

    def federatedWorkflow(name: String)(configure: FederatedWorkflowBuilder ?=> Unit): Unit =
      val builder = new FederatedWorkflowBuilder
      configure(using builder)
      val config = builder.build(name)
      _nextGlobalFederatedTaskId += config.count
      _federatedWorkflows += config

    def batchBroker(brokerName: String)(configure: BatchBrokerBuilder ?=> Unit): Unit =
      val builder = new BatchBrokerBuilder
      builder.name(brokerName)
      configure(using builder)
      val builtJobs = builder.jobBuilders.map { jb =>
        val jobId = JobId(_nextGlobalJobId)
        _nextGlobalJobId += 1
        jb.build(jobId)
      }
      _batchBrokers += builder.build(Vector.empty, builtJobs)

    def inferenceRouter(
        routerName: String,
        routingStrategy: InferenceRouterActor.RoutingStrategy,
        engineIndices: Vector[Int],
        engineRegions: Vector[String],
        maxBatchSizes: Vector[Int]
    ): Unit =
      _inferenceRouters += InferenceRouterConfig(
        routerName,
        routingStrategy,
        engineIndices,
        engineRegions,
        maxBatchSizes
      )

    def inferenceEngine(engineName: String)(configure: InferenceEngineBuilder ?=> Unit): Unit =
      val builder = new InferenceEngineBuilder
      builder.name(engineName)
      configure(using builder)
      _inferenceEngines += builder.build

    def inferenceBroker(brokerName: String)(configure: InferenceBrokerBuilder ?=> Unit): Unit =
      val builder = new InferenceBrokerBuilder
      builder.name(brokerName)
      configure(using builder)
      val brokerId = s"inference-broker-${_nextInferenceBrokerId}"
      _nextInferenceBrokerId += 1
      val config = builder.build(brokerId)
      config.batches.foreach(b => _nextGlobalInferenceRequestId += b.count)
      _inferenceBrokers += config

    def briteTopology(content: String): Unit =
      BriteParser.parse(content) match
        case Right(topo) => _briteTopology = Some(topo)
        case Left(err)   => throw IllegalArgumentException(s"Invalid BRITE topology: $err")

    def build: DslSimulationConfig =
      val dcNameToId       = _dcNames.toMap
      val explicitTopology = _networkBuilder.map(_.build(dcNameToId)).getOrElse(NetworkTopology.empty)
      // Merge BRITE topology with explicit links
      val topology = _briteTopology match
        case Some(brite) if explicitTopology.links.nonEmpty =>
          NetworkTopology.fromLinks(brite.links ++ explicitTopology.links)
        case Some(brite) => brite
        case None        => explicitTopology
      DslSimulationConfig(
        _name,
        _endTime,
        _datacenters.toVector,
        _brokers.toVector,
        topology,
        _faasPlatforms.toVector,
        _serverlessBrokers.toVector,
        _nextGlobalFunctionId,
        _nextGlobalInvocationId,
        _k8sClusters.toVector,
        _nextGlobalPodId,
        _nextGlobalDeploymentId,
        _edgeEnvironments.toVector,
        _nextGlobalEdgeTaskId,
        _federatedWorkflows.toVector,
        _nextGlobalFederatedTaskId,
        _batchBrokers.toVector,
        _inferenceEngines.toVector,
        _inferenceBrokers.toVector,
        _inferenceRouters.toVector,
        _nextGlobalInferenceRequestId
      )

  // ─── Context function DSL entry point ─────────────────────────────────

  def simulation(name: String, endTime: SimTime = SimTime(1000.0))(
      configure: SimulationBuilder ?=> Unit
  ): DslSimulationConfig =
    val builder = new SimulationBuilder
    builder.name(name)
    builder.endTime(endTime)
    configure(using builder)
    builder.build

  // Convenience extensions for DSL usage with context functions
  def datacenter(name: String)(configure: DatacenterBuilder ?=> Unit)(using sb: SimulationBuilder): Unit =
    sb.datacenter(name)(configure)

  def broker(name: String)(configure: BrokerBuilder ?=> Unit)(using sb: SimulationBuilder): Unit =
    sb.broker(name)(configure)

  def network(configure: NetworkBuilder ?=> Unit)(using sb: SimulationBuilder): Unit =
    sb.network(configure)

  def briteTopology(content: String)(using sb: SimulationBuilder): Unit =
    sb.briteTopology(content)

  def faasPlatform(name: String)(configure: FaasPlatformBuilder ?=> Unit)(using sb: SimulationBuilder): Unit =
    sb.faasPlatform(name)(configure)

  def serverlessBroker(name: String)(configure: ServerlessBrokerBuilder ?=> Unit)(using sb: SimulationBuilder): Unit =
    sb.serverlessBroker(name)(configure)

  def allocationPolicy(p: VmAllocationPolicy)(using db: DatacenterBuilder): Unit =
    db.allocationPolicy(p)

  def scheduler(s: WorkloadScheduler)(using db: DatacenterBuilder): Unit =
    db.scheduler(s)

  def migrationPolicy(
      overloadDetector: DatacenterActor.OverloadDetector,
      vmSelector: DatacenterActor.VmSelectionPolicy,
      migrationModel: MigrationModel = io.aura.iaas.policies.MigrationModel.preCopy,
      bandwidth: Mbps = Mbps(10000.0)
  )(using db: DatacenterBuilder): Unit =
    db.migrationPolicy(overloadDetector, vmSelector, migrationModel, bandwidth)

  def verticalScaling(policy: VerticalScalingPolicy)(using db: DatacenterBuilder): Unit =
    db.verticalScaling(policy)

  def faultInjection(config: FaultInjectionConfig)(using db: DatacenterBuilder): Unit =
    db.faultInjection(config)

  def consolidation(
      overloadDetector: ConsolidationOverloadDetector,
      underloadDetector: ConsolidationUnderloadDetector,
      vmSelector: ConsolidationVmSelection,
      interval: SimTime = SimTime(100.0),
      migrationModel: MigrationModel = io.aura.iaas.policies.MigrationModel.preCopy,
      bandwidth: Mbps = Mbps(10000.0)
  )(using db: DatacenterBuilder): Unit =
    db.consolidation(overloadDetector, underloadDetector, vmSelector, interval, migrationModel, bandwidth)

  def horizontalScaling(policy: HorizontalScalingPolicy)(using bb: BrokerBuilder): Unit =
    bb.horizontalScaling(policy)

  def maxVms(count: Int)(using bb: BrokerBuilder): Unit =
    bb.maxVms(count)

  def costRates(rates: CostRates)(using bb: BrokerBuilder): Unit =
    bb.costRates(rates)

  def spotConfig(config: SpotInstanceConfig)(using bb: BrokerBuilder): Unit =
    bb.spotConfig(config)

  def faultRecovery(policy: FaultRecoveryPolicy)(using bb: BrokerBuilder): Unit =
    bb.faultRecovery(policy)

  def vmBootConfig(config: VmBootConfig)(using bb: BrokerBuilder): Unit =
    bb.vmBootConfig(config)

  def link(
      from: String,
      to: String,
      latency: SimTime,
      bandwidth: Mbps = Mbps(10000.0)
  )(using nb: NetworkBuilder): Unit =
    nb.link(from, to, latency, bandwidth)

  def host(
      pes: PEs = PEs(4),
      mips: MIPS = MIPS(1000.0),
      ram: MegaBytes = MegaBytes(8192.0),
      bw: Mbps = Mbps(1000.0),
      storage: MegaBytes = MegaBytes(100000.0),
      powerModel: Option[PowerModel] = None
  )(using db: DatacenterBuilder): Unit =
    db.host(pes, mips, ram, bw, storage, powerModel)

  def hosts(
      count: Int,
      pes: PEs = PEs(4),
      mips: MIPS = MIPS(1000.0),
      ram: MegaBytes = MegaBytes(8192.0),
      bw: Mbps = Mbps(1000.0),
      storage: MegaBytes = MegaBytes(100000.0),
      powerModel: Option[PowerModel] = None
  )(using db: DatacenterBuilder): Unit =
    db.hosts(count, pes, mips, ram, bw, storage, powerModel)

  def vm(
      pes: PEs = PEs(2),
      mips: MIPS = MIPS(1000.0),
      ram: MegaBytes = MegaBytes(2048.0),
      bw: Mbps = Mbps(500.0),
      storage: MegaBytes = MegaBytes(10000.0),
      submissionTime: SimTime = SimTime.Zero
  )(using bb: BrokerBuilder): Unit =
    bb.vm(pes, mips, ram, bw, storage, submissionTime)

  def vms(
      count: Int,
      pes: PEs = PEs(2),
      mips: MIPS = MIPS(1000.0),
      ram: MegaBytes = MegaBytes(2048.0),
      bw: Mbps = Mbps(500.0),
      storage: MegaBytes = MegaBytes(10000.0),
      submissionTime: SimTime = SimTime.Zero
  )(using bb: BrokerBuilder): Unit =
    bb.vms(count, pes, mips, ram, bw, storage, submissionTime)

  def workload(
      length: MI = MI(10000.0),
      pes: PEs = PEs(1),
      requiredMips: MIPS = MIPS(1000.0),
      weight: Int = 1024,
      dependsOn: Set[WorkloadId] = Set.empty
  )(using bb: BrokerBuilder): WorkloadId =
    bb.workload(length = length, pes = pes, requiredMips = requiredMips, weight = weight, dependsOn = dependsOn)

  def targetDatacenter(index: Int)(using bb: BrokerBuilder): Unit =
    bb.targetDatacenter(index)

  def workloads(
      count: Int,
      length: MI = MI(10000.0),
      pes: PEs = PEs(1),
      weight: Int = 1024
  )(using bb: BrokerBuilder): Unit =
    bb.workloads(count, length, pes, weight)

  // ─── Batch broker context-function wrappers ───────────────────────────

  def batchBroker(name: String)(configure: BatchBrokerBuilder ?=> Unit)(using sb: SimulationBuilder): Unit =
    sb.batchBroker(name)(configure)

  def batchAlgorithm(v: BatchBrokerActor.BatchSchedulingAlgorithm)(using bb: BatchBrokerBuilder): Unit =
    bb.algorithm(v)

  def computeNodes(count: Int, cpu: MIPS = MIPS(1000.0), ram: MegaBytes = MegaBytes(8192.0))(using
      bb: BatchBrokerBuilder
  ): Unit =
    bb.computeNodes(count, cpu, ram)

  def batchTickInterval(v: SimTime)(using bb: BatchBrokerBuilder): Unit =
    bb.tickInterval(v)

  def batchJob(jobName: String)(configure: BatchJobBuilder ?=> Unit)(using bb: BatchBrokerBuilder): Unit =
    bb.job(jobName)(configure)

  def batchJobs(
      count: Int,
      namePrefix: String = "job",
      priority: JobPriority = JobPriority.Normal,
      requiredNodes: Int = 1,
      cpuPerNode: MIPS = MIPS(1000.0),
      ramPerNode: MegaBytes = MegaBytes(2048.0),
      estimatedRuntime: SimTime = SimTime(100.0),
      submitTime: SimTime = SimTime.Zero,
      isGang: Boolean = false
  )(using bb: BatchBrokerBuilder): Unit =
    bb.jobs(count, namePrefix, priority, requiredNodes, cpuPerNode, ramPerNode, estimatedRuntime, submitTime, isGang)

  // ─── FaaS platform context-function wrappers ──────────────────────────

  def coldStartModel(v: ColdStartModel.ColdStartModel)(using fb: FaasPlatformBuilder): Unit =
    fb.coldStartModel(v)

  def billingModel(v: BillingModel.BillingModel)(using fb: FaasPlatformBuilder): Unit =
    fb.billingModel(v)

  def containerTtl(v: SimTime)(using fb: FaasPlatformBuilder): Unit =
    fb.containerTtl(v)

  def function(name: String)(configure: FunctionBuilder ?=> Unit)(using fb: FaasPlatformBuilder): Unit =
    fb.function(name)(configure)

  // ─── Function builder context-function wrappers ───────────────────────

  def runtime(v: Runtime)(using fb: FunctionBuilder): Unit =
    fb.runtime(v)

  def memory(v: MegaBytes)(using fb: FunctionBuilder): Unit =
    fb.memory(v)

  def timeout(v: SimTime)(using fb: FunctionBuilder): Unit =
    fb.timeout(v)

  def concurrencyLimit(v: Int)(using fb: FunctionBuilder): Unit =
    fb.concurrencyLimit(v)

  // ─── Serverless broker context-function wrappers ──────────────────────

  def invocations(
      functionName: String,
      count: Int,
      executionLength: MI = MI(1000.0),
      inputSize: MegaBytes = MegaBytes(1.0),
      arrivalPattern: ArrivalPattern.ArrivalPattern = ArrivalPattern.uniform(SimTime(1.0))
  )(using sb: ServerlessBrokerBuilder): Unit =
    sb.invocations(functionName, count, executionLength, inputSize, arrivalPattern)

  // ─── K8s cluster context-function wrappers ─────────────────────────────

  def k8sCluster(name: String)(configure: K8sClusterBuilder ?=> Unit)(using sb: SimulationBuilder): Unit =
    sb.k8sCluster(name)(configure)

  def schedulingPolicy(v: K8sSchedulingPolicy)(using kb: K8sClusterBuilder): Unit =
    kb.schedulingPolicy(v)

  def deployment(name: String)(configure: DeploymentBuilder ?=> Unit)(using kb: K8sClusterBuilder): Unit =
    kb.deployment(name)(configure)

  def replicas(v: Int)(using db: DeploymentBuilder): Unit =
    db.replicas(v)

  def container(name: String)(configure: ContainerBuilder ?=> Unit)(using db: DeploymentBuilder): Unit =
    db.container(name)(configure)

  def restartPolicy(v: RestartPolicy)(using db: DeploymentBuilder): Unit =
    db.restartPolicy(v)

  def workloadPerPod(length: MI, pes: PEs)(using db: DeploymentBuilder): Unit =
    db.workloadPerPod(length, pes)

  def image(v: String)(using cb: ContainerBuilder): Unit =
    cb.image(v)

  def cpuRequest(v: MIPS)(using cb: ContainerBuilder): Unit =
    cb.cpuRequest(v)

  def cpuLimit(v: MIPS)(using cb: ContainerBuilder): Unit =
    cb.cpuLimit(v)

  def memoryRequest(v: MegaBytes)(using cb: ContainerBuilder): Unit =
    cb.memoryRequest(v)

  def memoryLimit(v: MegaBytes)(using cb: ContainerBuilder): Unit =
    cb.memoryLimit(v)

  // ─── Edge environment context-function wrappers ────────────────────────

  def edgeEnvironment(name: String)(configure: EdgeEnvironmentBuilder ?=> Unit)(using sb: SimulationBuilder): Unit =
    sb.edgeEnvironment(name)(configure)

  def latencyModel(v: LatencyModel)(using eb: EdgeEnvironmentBuilder): Unit =
    eb.latencyModel(v)

  def offloadingPolicy(v: OffloadingPolicy)(using eb: EdgeEnvironmentBuilder): Unit =
    eb.offloadingPolicy(v)

  def edgeNode(name: String)(configure: EdgeNodeBuilder ?=> Unit)(using eb: EdgeEnvironmentBuilder): Unit =
    eb.edgeNode(name)(configure)

  def taskSource(name: String)(configure: TaskSourceBuilder ?=> Unit)(using eb: EdgeEnvironmentBuilder): Unit =
    eb.taskSource(name)(configure)

  def location(lat: Double, lon: Double)(using nb: EdgeNodeBuilder): Unit =
    nb.location(lat, lon)

  def tier(t: EdgeTier)(using nb: EdgeNodeBuilder): Unit =
    nb.tier(t)

  def resources(pes: PEs, mips: MIPS, ram: MegaBytes)(using nb: EdgeNodeBuilder): Unit =
    nb.resources(pes, mips, ram)

  def sourceNode(name: String)(using tsb: TaskSourceBuilder): Unit =
    tsb.sourceNode(name)

  def tasks(count: Int, cpuRequired: MIPS, memRequired: MegaBytes, taskLength: MI, deadline: SimTime)(using
      tsb: TaskSourceBuilder
  ): Unit =
    tsb.tasks(count, cpuRequired, memRequired, taskLength, deadline)

  def interArrivalTime(t: SimTime)(using tsb: TaskSourceBuilder): Unit =
    tsb.interArrivalTime(t)

  // ─── Federated workflow context-function wrappers ──────────────────

  def federatedWorkflow(name: String)(configure: FederatedWorkflowBuilder ?=> Unit)(using sb: SimulationBuilder): Unit =
    sb.federatedWorkflow(name)(configure)

  def tierSelection(v: TierSelectionPolicy)(using fb: FederatedWorkflowBuilder): Unit =
    fb.tierSelection(v)

  def escalation(v: EscalationPolicy)(using fb: FederatedWorkflowBuilder): Unit =
    fb.escalation(v)

  def edgeTier(environment: String, sourceNode: String)(using fb: FederatedWorkflowBuilder): Unit =
    fb.edgeTier(environment, sourceNode)

  def serverlessTier(platform: String, functionName: String)(using fb: FederatedWorkflowBuilder): Unit =
    fb.serverlessTier(platform, functionName)

  def k8sTier(cluster: String)(using fb: FederatedWorkflowBuilder): Unit =
    fb.k8sTier(cluster)

  // ─── Inference context-function wrappers ─────────────────────────────

  def inferenceEngine(name: String)(configure: InferenceEngineBuilder ?=> Unit)(using sb: SimulationBuilder): Unit =
    sb.inferenceEngine(name)(configure)

  def inferenceBroker(name: String)(configure: InferenceBrokerBuilder ?=> Unit)(using sb: SimulationBuilder): Unit =
    sb.inferenceBroker(name)(configure)

  def inferenceRouter(
      routerName: String,
      routingStrategy: InferenceRouterActor.RoutingStrategy,
      engineIndices: Vector[Int],
      engineRegions: Vector[String],
      maxBatchSizes: Vector[Int]
  )(using sb: SimulationBuilder): Unit =
    sb.inferenceRouter(routerName, routingStrategy, engineIndices, engineRegions, maxBatchSizes)

  def model(v: LlmModelSpec)(using eb: InferenceEngineBuilder): Unit =
    eb.model(v)

  def deviceSpec(v: GpuDeviceSpec)(using eb: InferenceEngineBuilder): Unit =
    eb.deviceSpec(v)

  def tpDegree(v: Int)(using eb: InferenceEngineBuilder): Unit =
    eb.tpDegree(v)

  def ppStages(v: Int)(using eb: InferenceEngineBuilder): Unit =
    eb.ppStages(v)

  def epDegree(v: Int)(using eb: InferenceEngineBuilder): Unit =
    eb.epDegree(v)

  def maxBatchSize(v: Int)(using eb: InferenceEngineBuilder): Unit =
    eb.maxBatchSize(v)

  def iterationInterval(v: SimTime)(using eb: InferenceEngineBuilder): Unit =
    eb.iterationInterval(v)

  def dvfsPolicy(v: DvfsPolicy)(using eb: InferenceEngineBuilder): Unit =
    eb.dvfsPolicy(v)

  def powerBudget(v: Watts)(using eb: InferenceEngineBuilder): Unit =
    eb.powerBudget(v)

  def schedulingStrategy(v: InferenceEngineActor.SchedulingStrategy)(using eb: InferenceEngineBuilder): Unit =
    eb.schedulingStrategy(v)

  def targetEngine(index: Int)(using ib: InferenceBrokerBuilder): Unit =
    ib.targetEngine(index)

  def targetRouter(name: String)(using ib: InferenceBrokerBuilder): Unit =
    ib.targetRouter(name)

  def sloTtft(v: SimTime)(using ib: InferenceBrokerBuilder): Unit =
    ib.sloTtft(v)

  def sloTpot(v: SimTime)(using ib: InferenceBrokerBuilder): Unit =
    ib.sloTpot(v)

  def requests(
      count: Int,
      promptTokens: Int = 512,
      maxOutputTokens: Int = 128,
      arrivalRate: Double = 10.0,
      startTime: SimTime = SimTime.Zero
  )(using ib: InferenceBrokerBuilder): Unit =
    ib.requests(count, promptTokens, maxOutputTokens, arrivalRate, startTime)

  def traceRequests(entries: Vector[AzureLlmTraceReader.TraceEntry])(using ib: InferenceBrokerBuilder): Unit =
    ib.traceRequests(entries)

  def syntheticTrace(
      count: Int,
      ratePerSecond: Double,
      avgPromptTokens: Int = 512,
      avgOutputTokens: Int = 128,
      seed: Long = 42L
  )(using ib: InferenceBrokerBuilder): Unit =
    ib.syntheticTrace(count, ratePerSecond, avgPromptTokens, avgOutputTokens, seed)

// ─── Config case classes ──────────────────────────────────────────────────

final case class DatacenterConfig(
    datacenterId: DatacenterId,
    name: String,
    hosts: Vector[DatacenterActor.HostConfig],
    allocationPolicy: VmAllocationPolicy,
    scheduler: WorkloadScheduler,
    migrationModel: Option[MigrationModel] = None,
    migrationBandwidth: Mbps = Mbps(10000.0),
    overloadDetector: Option[DatacenterActor.OverloadDetector] = None,
    vmSelector: Option[DatacenterActor.VmSelectionPolicy] = None,
    faultInjection: Option[FaultInjectionConfig] = None,
    consolidation: Option[DatacenterActor.ConsolidationConfig] = None
)

final case class BrokerConfig(
    brokerId: BrokerId,
    name: String,
    vmRequests: Vector[BrokerActor.VmRequest],
    workloads: Vector[WorkloadSpec],
    targetDcIndex: Int,
    horizontalScaling: Option[HorizontalScalingPolicy] = None,
    maxVms: Int = Int.MaxValue,
    costRates: Option[CostRates] = None,
    spotConfig: Option[SpotInstanceConfig] = None,
    faultRecovery: FaultRecoveryPolicy = FaultRecoveryPolicy.none,
    vmBootConfig: VmBootConfig = VmBootConfig.instant
)

// ─── Serverless Config case classes ─────────────────────────────────────

final case class FunctionDefinition(
    functionId: FunctionId,
    name: String,
    memoryMB: MegaBytes,
    timeout: SimTime,
    runtime: Runtime,
    concurrencyLimit: Int,
    reservedConcurrency: Option[Int]
)

final case class InvocationBatchConfig(
    functionName: String,
    count: Int,
    executionLength: MI,
    inputSize: MegaBytes,
    arrivalPattern: ArrivalPattern.ArrivalPattern
)

final case class FaasPlatformConfig(
    name: String,
    coldStartModel: ColdStartModel.ColdStartModel,
    billingModel: BillingModel.BillingModel,
    scalingPolicy: ScalingPolicy.ScalingPolicy,
    containerTtl: SimTime,
    executionMips: MIPS,
    functions: Vector[FunctionDefinition]
)

final case class ServerlessBrokerConfig(
    brokerId: String,
    name: String,
    batches: Vector[InvocationBatchConfig],
    targetPlatformIndex: Int,
    startTime: SimTime
)

// ─── Container Config case classes ───────────────────────────────────────

final case class ContainerConfig(
    name: String,
    image: String,
    cpuRequest: MIPS,
    cpuLimit: MIPS,
    memoryRequest: MegaBytes,
    memoryLimit: MegaBytes
)

final case class DeploymentConfig(
    deploymentId: DeploymentId,
    name: String,
    replicas: Int,
    containers: Vector[ContainerConfig],
    restartPolicy: RestartPolicy,
    priority: Int,
    nodeSelector: Map[String, String],
    workloadConfig: Option[WorkloadConfig]
)

final case class K8sClusterConfig(
    name: String,
    schedulingPolicy: K8sSchedulingPolicy,
    targetDcIndex: Int,
    startupDelay: SimTime,
    deployments: Vector[DeploymentConfig]
)

// ─── Edge Config case classes ──────────────────────────────────────────────

final case class TaskSourceConfig(
    sourceNodeName: String,
    count: Int,
    cpuRequired: MIPS,
    memRequired: MegaBytes,
    taskLength: MI,
    deadline: SimTime,
    interArrivalTime: SimTime
)

final case class EdgeEnvironmentConfig(
    name: String,
    latencyModel: LatencyModel,
    offloadingPolicy: OffloadingPolicy,
    nodeSpecs: Vector[EdgeNodeSpec],
    taskSources: Vector[TaskSourceConfig]
)

// ─── Federated Config case classes ──────────────────────────────────────

// ─── Batch Scheduling Config case classes ─────────────────────────────────

final case class BatchBrokerConfig(
    name: String,
    algorithm: BatchBrokerActor.BatchSchedulingAlgorithm,
    tickInterval: SimTime,
    targetDcIndex: Int,
    nodeCount: Int,
    nodeCpu: MIPS,
    nodeRam: MegaBytes,
    jobs: Vector[BatchJob]
)

// ─── Federated Config case classes ──────────────────────────────────────

final case class FederatedWorkflowConfig(
    name: String,
    tierSelection: TierSelectionPolicy,
    escalation: EscalationPolicy,
    edgeTier: Option[(String, String)],
    serverlessTier: Option[(String, String)],
    k8sTier: Option[String],
    count: Int,
    cpuRequired: MIPS,
    memRequired: MegaBytes,
    taskLength: MI,
    deadline: SimTime,
    interArrivalTime: SimTime
)

// ─── Inference Config case classes ─────────────────────────────────────

final case class InferenceRequestBatchConfig(
    count: Int,
    promptTokens: Int,
    maxOutputTokens: Int,
    arrivalRate: Double,
    startTime: SimTime
)

final case class InferenceEngineConfig(
    name: String,
    model: LlmModelSpec,
    deviceSpec: GpuDeviceSpec,
    tpDegree: Int,
    ppStages: Int,
    epDegree: Int,
    maxBatchSize: Int,
    iterationInterval: SimTime,
    dvfsPolicy: DvfsPolicy,
    powerBudget: Watts,
    schedulingStrategy: InferenceEngineActor.SchedulingStrategy =
      InferenceEngineActor.SchedulingStrategy.ContinuousBatching
)

final case class InferenceBrokerConfig(
    brokerId: String,
    name: String,
    targetEngineIndex: Int,
    targetRouterName: Option[String] = None,
    sloTtft: SimTime,
    sloTpot: SimTime,
    batches: Vector[InferenceRequestBatchConfig]
)

/** Configuration for a multi-engine router with CAGR or load-balancing. */
final case class InferenceRouterConfig(
    name: String,
    routingStrategy: InferenceRouterActor.RoutingStrategy,
    engineIndices: Vector[Int],
    engineRegions: Vector[String],
    maxBatchSizes: Vector[Int]
)

final case class DslSimulationConfig(
    name: String,
    endTime: SimTime,
    datacenters: Vector[DatacenterConfig],
    brokers: Vector[BrokerConfig],
    networkTopology: NetworkTopology = NetworkTopology.empty,
    faasPlatforms: Vector[FaasPlatformConfig] = Vector.empty,
    serverlessBrokers: Vector[ServerlessBrokerConfig] = Vector.empty,
    nextFunctionId: Long = 0L,
    nextInvocationId: Long = 0L,
    k8sClusters: Vector[K8sClusterConfig] = Vector.empty,
    nextPodId: Long = 0L,
    nextDeploymentId: Long = 0L,
    edgeEnvironments: Vector[EdgeEnvironmentConfig] = Vector.empty,
    nextEdgeTaskId: Long = 0L,
    federatedWorkflows: Vector[FederatedWorkflowConfig] = Vector.empty,
    nextFederatedTaskId: Long = 0L,
    batchBrokers: Vector[BatchBrokerConfig] = Vector.empty,
    inferenceEngines: Vector[InferenceEngineConfig] = Vector.empty,
    inferenceBrokers: Vector[InferenceBrokerConfig] = Vector.empty,
    inferenceRouters: Vector[InferenceRouterConfig] = Vector.empty,
    nextInferenceRequestId: Long = 0L
) extends SimulationConfig:

  private val K8sVmIdOffset     = 10_000L
  private val FederatedIdOffset = 10_000L

  override def brokerCount: Int =
    brokers.size + serverlessBrokers.size + k8sClusters.size + edgeEnvironments.size + federatedWorkflows.size + batchBrokers.size + inferenceBrokers.size + inferenceRouters.size

  /** Enumerate every `EntityRef` this config will spawn. The coordinator uses this to gate `StartSimulation` on a
    * registration barrier — see `TimeCoordinator.ExpectEntities`. The names below MUST match the names each actor uses
    * in its own `RegisterEntity` call (which in turn derive from the `brokerId` / `name` strings the DSL constructs in
    * `spawnEntities`). If a new entity type is added there, mirror it here or the coordinator will hang waiting for an
    * entity that never registers.
    */
  private def expectedEntityRefs: Set[EntityRef] =
    val dcs            = datacenters.map(dc => EntityRef(s"dc-${dc.datacenterId.value}", EntityType.Datacenter))
    val hosts          = datacenters.flatMap(_.hosts.map(h => EntityRef(s"host-${h.hostId.value}", EntityType.Host)))
    val brokerRefs     = brokers.map(b => EntityRef(s"broker-${b.brokerId.value}", EntityType.Broker))
    val faasRefs       = faasPlatforms.map(p => EntityRef(p.name, EntityType.FaasPlatform))
    val serverlessRefs = serverlessBrokers.map(b => EntityRef(b.brokerId, EntityType.ServerlessBroker))
    val k8sRefs        = k8sClusters.map(c => EntityRef(s"k8s-broker-${c.name}", EntityType.K8sCluster))
    val edgeEnvRefs    = edgeEnvironments.map(e => EntityRef(e.name, EntityType.EdgeEnvironment))
    val edgeBrokerRefs = edgeEnvironments.map(e => EntityRef(s"edge-broker-${e.name}", EntityType.EdgeEnvironment))
    val federatedRefs =
      federatedWorkflows.map(f => EntityRef(s"federated-broker-${f.name}", EntityType.FederatedBroker))
    val batchRefs     = batchBrokers.map(b => EntityRef(s"batch-${b.name}", EntityType.BatchBroker))
    val infEngineRefs = inferenceEngines.map(e => EntityRef(e.name, EntityType.InferenceEngine))
    val infBrokerRefs = inferenceBrokers.map(b => EntityRef(b.brokerId, EntityType.InferenceBroker))
    val infRouterRefs = inferenceRouters.map(r => EntityRef(r.name, EntityType.InferenceRouter))
    (dcs ++ hosts ++ brokerRefs ++ faasRefs ++ serverlessRefs ++ k8sRefs ++ edgeEnvRefs ++
      edgeBrokerRefs ++ federatedRefs ++ batchRefs ++ infEngineRefs ++ infBrokerRefs ++ infRouterRefs).toSet

  /** Convenience method: run this config and return results. */
  def run(timeout: FiniteDuration = 10.minutes): Either[SimulationError, SimulationResults] =
    SimulationRunner.run(this, timeout)

  override def spawnEntities(
      context: ActorContext[SimulationGuardian.Command],
      coordinator: ActorRef[TimeCoordinator.Command]
  ): Unit =
    // Compute the set of EntityRefs we will spawn, and announce them to the
    // coordinator BEFORE any spawn. The coordinator buffers `StartSimulation`
    // until every expected entity has called `RegisterEntity` from its
    // (asynchronous) setup block, eliminating the race where dispatch could
    // begin while slow-spawning actors had not yet registered.
    val expected = expectedEntityRefs
    if expected.nonEmpty then coordinator ! TimeCoordinator.ExpectEntities(expected)

    // Set network topology
    if networkTopology.links.nonEmpty then coordinator ! TimeCoordinator.SetNetworkTopology(networkTopology)

    // Spawn datacenters
    val dcRefs = datacenters.map { dc =>
      context.spawn(
        DatacenterActor(
          DatacenterActor.Config(
            datacenterId = dc.datacenterId,
            hosts = dc.hosts,
            allocationPolicy = dc.allocationPolicy,
            scheduler = dc.scheduler,
            coordinator = coordinator,
            migrationModel = dc.migrationModel,
            migrationBandwidth = dc.migrationBandwidth,
            overloadDetector = dc.overloadDetector,
            vmSelector = dc.vmSelector,
            networkTopology = if networkTopology.links.nonEmpty then Some(networkTopology) else None,
            consolidation = dc.consolidation
          )
        ),
        s"dc-${dc.datacenterId.value}"
      )
      val dcEntityRef = EntityRef(s"dc-${dc.datacenterId.value}", EntityType.Datacenter)

      // Register DC entity's datacenter mapping
      coordinator ! TimeCoordinator.RegisterEntityDc(dcEntityRef, dc.datacenterId)

      // Register host entities' datacenter mapping
      dc.hosts.foreach { hc =>
        val hostRef = EntityRef(s"host-${hc.hostId.value}", EntityType.Host)
        coordinator ! TimeCoordinator.RegisterEntityDc(hostRef, dc.datacenterId)
      }

      // Schedule fault injection events if configured
      dc.faultInjection.foreach { fiConfig =>
        val hostIds = dc.hosts.map(_.hostId)
        val faults  = FaultInjection.generateFaults(hostIds, fiConfig, endTime)
        if faults.nonEmpty then
          val faultEvents = faults.map { fault =>
            SimEvent(
              time = fault.time,
              source = dcEntityRef,
              destination = EntityRef(s"host-${fault.hostId.value}", EntityType.Host),
              payload = SimEventPayload.HostFaultEvent(fault.hostId, fault.failedPEs),
              serial = SerialNumber.Zero
            )
          }
          coordinator ! TimeCoordinator.ScheduleEvents(faultEvents)
      }

      dc.datacenterId -> dcEntityRef
    }.toMap

    // Spawn IaaS brokers (pre-generate events on guardian thread to avoid race with StartSimulation)
    brokers.foreach { br =>
      val dcRef = dcRefs.values.toVector
        .lift(br.targetDcIndex)
        .orElse(dcRefs.values.headOption)
        .getOrElse(throw IllegalStateException("No datacenters configured — cannot assign broker target"))

      val brokerConfig = BrokerActor.Config(
        brokerId = br.brokerId,
        vmRequests = br.vmRequests,
        workloads = br.workloads,
        datacenterRef = dcRef,
        coordinator = coordinator,
        horizontalScaling = br.horizontalScaling,
        maxVms = br.maxVms,
        costRates = br.costRates,
        spotConfig = br.spotConfig,
        faultRecovery = br.faultRecovery,
        vmBootConfig = br.vmBootConfig
      )

      val brokerEvents = BrokerActor.generateEvents(brokerConfig)
      if brokerEvents.nonEmpty then coordinator ! TimeCoordinator.ScheduleEvents(brokerEvents)

      context.spawn(BrokerActor(brokerConfig), s"broker-${br.brokerId.value}")

      // Register broker entity's datacenter mapping (associate with target DC)
      val brokerEntityRef = EntityRef(s"broker-${br.brokerId.value}", EntityType.Broker)
      val targetDcId = dcRefs.keys.toVector
        .lift(br.targetDcIndex)
        .orElse(dcRefs.keys.headOption)
      targetDcId.foreach { dcId =>
        coordinator ! TimeCoordinator.RegisterEntityDc(brokerEntityRef, dcId)
      }
    }

    // Build function name→ID lookup from all platforms
    val functionNameToId: Map[String, FunctionId] = faasPlatforms.flatMap { pc =>
      pc.functions.map(f => f.name -> f.functionId)
    }.toMap

    // Spawn FaaS platforms
    val platformRefs = faasPlatforms.map { pc =>
      val platformRef = EntityRef(pc.name, EntityType.FaasPlatform)

      context.spawn(
        FaasPlatformActor(
          FaasPlatformActor.Config(
            platformName = pc.name,
            coldStartModel = pc.coldStartModel,
            billingModel = pc.billingModel,
            scalingPolicy = pc.scalingPolicy,
            containerTtl = pc.containerTtl,
            coordinator = coordinator,
            executionMips = pc.executionMips
          )
        ),
        pc.name
      )

      // Schedule FunctionDeploy events for each function
      val deployEvents = pc.functions.map { fd =>
        SimEvent(
          time = SimTime.Zero,
          source = platformRef,
          destination = platformRef,
          payload = SimEventPayload.FunctionDeploy(
            fd.functionId,
            FunctionDeploySpec(
              name = fd.name,
              runtime = fd.runtime.toString,
              memoryMB = fd.memoryMB,
              timeout = fd.timeout,
              concurrencyLimit = fd.concurrencyLimit,
              reservedConcurrency = fd.reservedConcurrency
            )
          ),
          serial = SerialNumber.Zero
        )
      }
      if deployEvents.nonEmpty then coordinator ! TimeCoordinator.ScheduleEvents(deployEvents)

      platformRef
    }

    // Spawn serverless brokers — pre-generate events on guardian thread to avoid
    // race conditions with the coordinator (events must be queued before StartSimulation)
    val finalInvocationId = serverlessBrokers.foldLeft(nextInvocationId) { (currentInvocationId, sbc) =>
      val platformRef = platformRefs
        .lift(sbc.targetPlatformIndex)
        .orElse(platformRefs.headOption)
        .getOrElse(throw IllegalStateException("No FaaS platforms configured — cannot assign serverless broker target"))

      // Resolve function names to IDs
      val batches = sbc.batches.map { batch =>
        val funcId = functionNameToId.getOrElse(
          batch.functionName,
          throw IllegalStateException(s"Unknown function '${batch.functionName}' in serverless broker '${sbc.name}'")
        )
        ServerlessBrokerActor.InvocationBatch(
          functionId = funcId,
          count = batch.count,
          executionLength = batch.executionLength,
          inputSize = batch.inputSize,
          arrivalPattern = batch.arrivalPattern
        )
      }

      val brokerConfig = ServerlessBrokerActor.Config(
        brokerId = sbc.brokerId,
        platformRef = platformRef,
        batches = batches,
        coordinator = coordinator,
        startTime = sbc.startTime
      )

      // Pre-generate events synchronously and schedule them before spawning the actor
      val (allEvents, pendingIds) = ServerlessBrokerActor.generateEvents(brokerConfig, currentInvocationId)
      if allEvents.nonEmpty then coordinator ! TimeCoordinator.ScheduleEvents(allEvents)

      context.spawn(
        ServerlessBrokerActor(brokerConfig, pendingIds),
        sbc.brokerId
      )

      currentInvocationId + batches.map(_.count.toLong).sum
    }

    // Spawn K8s clusters and their brokers
    val (finalPodId, finalK8sVmId) = k8sClusters.foldLeft((nextPodId, K8sVmIdOffset)) {
      case ((currentPodId, currentK8sVmId), kc) =>
        val dcConfig = datacenters
          .lift(kc.targetDcIndex)
          .orElse(datacenters.headOption)
          .getOrElse(throw IllegalStateException("No datacenters configured — cannot assign K8s cluster target"))
        val dcRef = dcRefs.values.toVector
          .lift(kc.targetDcIndex)
          .orElse(dcRefs.values.headOption)
          .getOrElse(throw IllegalStateException("No datacenters configured — cannot assign K8s cluster target"))

        // Map datacenter hosts to cluster nodes
        val hostMappings = dcConfig.hosts.map { hc =>
          K8sClusterActor.HostNodeMapping(
            nodeName = s"node-${hc.hostId.value}",
            hostId = hc.hostId,
            totalCpu = hc.spec.mips,
            totalMemory = hc.spec.ram
          )
        }

        val clusterRef = EntityRef(kc.name, EntityType.K8sCluster)

        // Build deployment records for the cluster actor
        val deploymentRecords = kc.deployments.map { dc =>
          val containerSpecs = dc.containers.map(c =>
            ContainerSpec(c.name, c.image, c.cpuRequest, c.cpuLimit, c.memoryRequest, c.memoryLimit)
          )
          val podTemplate = PodSpec(
            name = dc.name,
            containers = containerSpecs,
            restartPolicy = dc.restartPolicy,
            priority = dc.priority,
            nodeSelector = dc.nodeSelector
          )
          DeploymentRecord(
            deploymentId = dc.deploymentId,
            name = dc.name,
            replicaCount = dc.replicas,
            podTemplate = podTemplate,
            workloadSpec = dc.workloadConfig
          )
        }

        // Spawn K8sClusterActor with pre-populated deployment state
        context.spawn(
          K8sClusterActor(
            K8sClusterActor.Config(
              clusterName = kc.name,
              schedulingPolicy = kc.schedulingPolicy,
              datacenterRef = dcRef,
              hostConfigs = hostMappings,
              coordinator = coordinator,
              startupDelay = kc.startupDelay,
              initialDeployments = deploymentRecords
            ),
            nextVmId = currentK8sVmId
          ),
          kc.name
        )

        // Build deployment batches for the broker (reuse deployment records)
        val batches = deploymentRecords.map { dr =>
          K8sBrokerActor.DeploymentBatch(
            deploymentId = dr.deploymentId,
            name = dr.name,
            podSpec = dr.podTemplate,
            replicas = dr.replicaCount,
            workloadConfig = dr.workloadSpec,
            submitDelay = kc.startupDelay
          )
        }

        val brokerName = s"k8s-broker-${kc.name}"
        val brokerConfig = K8sBrokerActor.Config(
          brokerId = brokerName,
          clusterRef = clusterRef,
          deployments = batches,
          coordinator = coordinator
        )

        // Pre-generate events synchronously and schedule before spawning the actor
        val (k8sEvents, pendingPodIds) = K8sBrokerActor.generateEvents(brokerConfig, currentPodId)
        if k8sEvents.nonEmpty then coordinator ! TimeCoordinator.ScheduleEvents(k8sEvents)

        context.spawn(
          K8sBrokerActor(brokerConfig, pendingPodIds),
          brokerName
        )

        val totalPods = kc.deployments.map(_.replicas.toLong).sum
        (currentPodId + totalPods, currentK8sVmId + totalPods * 2)
    }

    // Spawn edge environments and their brokers
    val finalEdgeTaskId = edgeEnvironments.foldLeft(nextEdgeTaskId) { (currentEdgeTaskId, ec) =>
      val envRef = EntityRef(ec.name, EntityType.EdgeEnvironment)

      context.spawn(
        EdgeEnvironmentActor(
          EdgeEnvironmentActor.Config(
            environmentName = ec.name,
            nodeSpecs = ec.nodeSpecs,
            latencyModel = ec.latencyModel,
            offloadingPolicy = ec.offloadingPolicy,
            coordinator = coordinator
          )
        ),
        ec.name
      )

      // Build task batches from task sources
      val batches = ec.taskSources.map { ts =>
        EdgeBrokerActor.TaskBatch(
          sourceNodeName = ts.sourceNodeName,
          count = ts.count,
          cpuRequired = ts.cpuRequired,
          memRequired = ts.memRequired,
          taskLength = ts.taskLength,
          deadline = ts.deadline,
          interArrivalTime = ts.interArrivalTime
        )
      }

      val brokerName      = s"edge-broker-${ec.name}"
      val brokerEntityRef = EntityRef(brokerName, EntityType.EdgeEnvironment)

      // Generate events from guardian to avoid race with coordinator startup
      val (taskEvents, pendingIds) = EdgeBrokerActor.generateEvents(
        brokerEntityRef,
        envRef,
        batches,
        SimTime.Zero,
        currentEdgeTaskId
      )

      // Schedule events on coordinator from guardian (synchronous). The
      // empty-tasks case needs no special handling — the coordinator reaches
      // quiescence naturally when no events are scheduled.
      if taskEvents.nonEmpty then coordinator ! TimeCoordinator.ScheduleEvents(taskEvents)

      context.spawn(
        EdgeBrokerActor(
          EdgeBrokerActor.Config(
            brokerId = brokerName,
            environmentRef = envRef,
            coordinator = coordinator
          ),
          pendingTaskIds = pendingIds
        ),
        brokerName
      )

      val totalTasks = ec.taskSources.map(_.count.toLong).sum
      currentEdgeTaskId + totalTasks
    }

    // Spawn federated workflow brokers
    // Build name→ref lookups for tier resolution
    val edgeEnvRefs: Map[String, EntityRef] =
      edgeEnvironments.map(e => e.name -> EntityRef(e.name, EntityType.EdgeEnvironment)).toMap
    val platformRefMap: Map[String, EntityRef] =
      faasPlatforms.map(p => p.name -> EntityRef(p.name, EntityType.FaasPlatform)).toMap
    val clusterRefMap: Map[String, EntityRef] =
      k8sClusters.map(k => k.name -> EntityRef(k.name, EntityType.K8sCluster)).toMap

    federatedWorkflows.foldLeft(
      (
        nextFederatedTaskId,
        finalEdgeTaskId + FederatedIdOffset,
        finalInvocationId + FederatedIdOffset,
        finalPodId + FederatedIdOffset
      )
    ) { case ((currentFederatedTaskId, fedEdgeTaskId, fedInvocationId, fedPodId), fw) =>
      val brokerName      = s"federated-broker-${fw.name}"
      val brokerEntityRef = EntityRef(brokerName, EntityType.FederatedBroker)

      // Build tier mappings immutably
      val edgeTierMapping = fw.edgeTier.flatMap { case (envName, _) =>
        edgeEnvRefs.get(envName).map { ref =>
          val edgeIdGen = new java.util.concurrent.atomic.AtomicLong(fedEdgeTaskId)
          ExecutionTier.Edge -> FederatedBrokerActor.TierMapping(
            tier = ExecutionTier.Edge,
            targetRef = ref,
            localIdGenerator = () => edgeIdGen.getAndIncrement()
          )
        }
      }

      val serverlessTierMapping = fw.serverlessTier.flatMap { case (platformName, funcName) =>
        platformRefMap.get(platformName).map { ref =>
          val invIdGen = new java.util.concurrent.atomic.AtomicLong(fedInvocationId)
          ExecutionTier.Serverless -> FederatedBrokerActor.TierMapping(
            tier = ExecutionTier.Serverless,
            targetRef = ref,
            localIdGenerator = () => invIdGen.getAndIncrement()
          )
        }
      }

      val k8sTierMapping = fw.k8sTier.flatMap { clusterName =>
        clusterRefMap.get(clusterName).map { ref =>
          val podIdGen = new java.util.concurrent.atomic.AtomicLong(fedPodId)
          ExecutionTier.K8s -> FederatedBrokerActor.TierMapping(
            tier = ExecutionTier.K8s,
            targetRef = ref,
            localIdGenerator = () => podIdGen.getAndIncrement()
          )
        }
      }

      val tierMappings = Vector(edgeTierMapping, serverlessTierMapping, k8sTierMapping).flatten.toMap

      // Resolve serverless function ID
      val serverlessFuncId = fw.serverlessTier
        .flatMap { case (_, funcName) =>
          functionNameToId.get(funcName)
        }
        .getOrElse(FunctionId(0L))

      // Resolve K8s deployment ID (use first deployment in target cluster)
      val k8sDeploymentId = fw.k8sTier
        .flatMap { clusterName =>
          k8sClusters.find(_.name == clusterName).flatMap(_.deployments.headOption).map(_.deploymentId)
        }
        .getOrElse(DeploymentId(0L))

      val edgeSourceNode = fw.edgeTier.map(_._2).getOrElse("")

      // Generate FederatedTaskSubmit events
      val (taskEvents, taskCount) = FederatedBrokerActor.generateEvents(
        brokerEntityRef,
        fw.count,
        SimTime.Zero,
        fw.interArrivalTime,
        currentFederatedTaskId,
        fw.cpuRequired,
        fw.memRequired,
        fw.taskLength,
        fw.deadline
      )

      if taskEvents.nonEmpty then coordinator ! TimeCoordinator.ScheduleEvents(taskEvents)

      context.spawn(
        FederatedBrokerActor(
          FederatedBrokerActor.Config(
            brokerId = brokerName,
            tierSelection = fw.tierSelection,
            escalation = fw.escalation,
            tierMappings = tierMappings,
            coordinator = coordinator,
            cpuRequired = fw.cpuRequired,
            memRequired = fw.memRequired,
            taskLength = fw.taskLength,
            deadline = fw.deadline,
            edgeSourceNode = edgeSourceNode,
            serverlessFunctionId = serverlessFuncId,
            k8sDeploymentId = k8sDeploymentId
          ),
          totalTasks = taskCount
        ),
        brokerName
      )

      (
        currentFederatedTaskId + fw.count,
        fedEdgeTaskId + (if fw.edgeTier.isDefined then fw.count * 3 else 0),
        fedInvocationId + (if fw.serverlessTier.isDefined then fw.count * 3 else 0),
        fedPodId + (if fw.k8sTier.isDefined then fw.count * 3 else 0)
      )
    }

    // Spawn batch brokers (each actor schedules its own initial events after
    // registering with the coordinator — see BatchBrokerActor.apply)
    batchBrokers.foreach { bc =>
      val nodes = (0 until bc.nodeCount).map { i =>
        ComputeNode(i.toLong, bc.nodeCpu, bc.nodeRam)
      }.toVector

      val batchConfig = BatchBrokerActor.Config(
        brokerId = bc.name,
        nodes = nodes,
        jobs = bc.jobs,
        algorithm = bc.algorithm,
        coordinator = coordinator,
        tickInterval = bc.tickInterval
      )

      context.spawn(BatchBrokerActor(batchConfig), s"batch-${bc.name}")
    }

    // Spawn inference engines
    val inferenceEngineRefs = inferenceEngines.map { ec =>
      val engineRef = EntityRef(ec.name, EntityType.InferenceEngine)

      context.spawn(
        InferenceEngineActor(
          InferenceEngineActor.Config(
            engineName = ec.name,
            model = ec.model,
            deviceSpec = ec.deviceSpec,
            tpDegree = ec.tpDegree,
            ppStages = ec.ppStages,
            epDegree = ec.epDegree,
            maxBatchSize = ec.maxBatchSize,
            iterationInterval = ec.iterationInterval,
            dvfsPolicy = ec.dvfsPolicy,
            powerBudget = ec.powerBudget,
            schedulingStrategy = ec.schedulingStrategy,
            coordinator = coordinator
          )
        ),
        ec.name
      )

      engineRef
    }

    // Spawn inference routers (before brokers, so router refs are available)
    val routerRefs: Map[String, EntityRef] = inferenceRouters.map { rc =>
      val routerRef = EntityRef(rc.name, EntityType.InferenceRouter)
      val routerEngines = rc.engineIndices.zipWithIndex.map { (engIdx, i) =>
        val engRef = inferenceEngineRefs
          .lift(engIdx)
          .getOrElse(throw IllegalStateException(s"Router ${rc.name}: engine index $engIdx out of range"))
        InferenceRouterActor.EngineInfo(
          engineRef = engRef,
          regionId = rc.engineRegions.lift(i).getOrElse("default"),
          currentBatchSize = 0,
          maxBatchSize = rc.maxBatchSizes.lift(i).getOrElse(64),
          completedCount = 0
        )
      }
      context.spawn(
        InferenceRouterActor(
          InferenceRouterActor.Config(
            routerName = rc.name,
            engines = routerEngines,
            routingStrategy = rc.routingStrategy,
            coordinator = coordinator
          )
        ),
        rc.name
      )
      rc.name -> routerRef
    }.toMap

    // Spawn inference brokers (pre-generate events on guardian thread)
    inferenceBrokers.foldLeft(nextInferenceRequestId) { (currentReqId, ibc) =>
      // If broker targets a router, use router ref; otherwise use engine ref directly
      val targetRef = ibc.targetRouterName
        .flatMap(routerRefs.get)
        .orElse(inferenceEngineRefs.lift(ibc.targetEngineIndex))
        .orElse(inferenceEngineRefs.headOption)
        .getOrElse(throw IllegalStateException("No inference engines or routers configured"))

      val brokerConfig = InferenceBrokerActor.Config(
        brokerId = ibc.brokerId,
        engineRef = targetRef,
        modelId = ModelId(0L),
        coordinator = coordinator,
        sloTtft = ibc.sloTtft,
        sloTpot = ibc.sloTpot
      )

      val batches = ibc.batches.map { b =>
        InferenceBrokerActor.RequestBatch(
          count = b.count,
          promptTokens = b.promptTokens,
          maxOutputTokens = b.maxOutputTokens,
          arrivalRatePerSecond = b.arrivalRate,
          startTime = b.startTime
        )
      }

      val (allEvents, pendingIds) = InferenceBrokerActor.generateEvents(brokerConfig, batches, currentReqId)
      if allEvents.nonEmpty then coordinator ! TimeCoordinator.ScheduleEvents(allEvents)

      context.spawn(
        InferenceBrokerActor(brokerConfig, pendingIds),
        ibc.brokerId
      )

      currentReqId + ibc.batches.map(_.count.toLong).sum
    }
