# Aura

> A high-performance, multi-paradigm cloud simulation framework.

[![CI](https://github.com/microsoft/aura-simulator/actions/workflows/ci.yml/badge.svg)](https://github.com/microsoft/aura-simulator/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Scala 3.4](https://img.shields.io/badge/Scala-3.4-red.svg)](https://www.scala-lang.org/)

## Overview

Aura is an actor-based discrete event simulation (DES) framework for cloud computing research. Built in Scala 3 with Apache Pekko typed actors, it supports IaaS, serverless, container orchestration, edge computing, and batch scheduling workloads in a single unified simulation. All state is immutable, all policies are pure functions, and the type-safe DSL catches configuration errors at compile time.

## Key Features

- **Parallel DES engine.** Event dispatch via Apache Pekko typed actors with barrier synchronization.
- **Type-safe units.** Opaque types for `MIPS`, `PEs`, `MegaBytes`, `SimTime`, `MI`, `Watts`, etc. prevent unit confusion at compile time.
- **Context-function DSL.** Declarative simulation configuration using Scala 3 context functions.
- **Multi-paradigm.** IaaS VMs, serverless functions, Kubernetes pods, edge tasks, GPU inference, federated workflows, and HPC batch jobs in a single simulation.
- **Immutable state.** All actor state transitions produce new immutable values. No shared mutable state.
- **Pure-function policies.** Allocation, scheduling, scaling, consolidation, and migration policies are plain functions.
- **GPU inference simulation.** LLM inference with prefill/decode phases, KV cache, continuous batching, and energy-aware scheduling.
- **Real-world trace replay.** Google Cluster and Azure VM trace parsers for realistic workload generation.
- **Interactive visualization.** HTML reports with Chart.js dashboards.

## Quick Start

### Prerequisites

- JDK 17+
- sbt 2.0+

### Build & Test

```bash
sbt compile    # compile all modules
sbt test       # run 1,100+ tests
```

### Your First Simulation

```bash
sbt "auraExamples/runMain io.aura.examples.BasicIaaSExample"
```

```scala
import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.iaas.policies.{VmAllocationPolicy, WorkloadScheduler}

val config = simulation("my-sim", endTime = SimTime(1000.0)) {
  datacenter("dc-1") {
    allocationPolicy(VmAllocationPolicy.bestFit)
    scheduler(WorkloadScheduler.timeShared)
    host(pes = PEs(4), mips = MIPS(10000.0), ram = MegaBytes(16384.0))
  }
  broker("broker-1") {
    vm(pes = PEs(2), mips = MIPS(10000.0), ram = MegaBytes(4096.0))
    workloads(count = 4, length = MI(10000.0), pes = PEs(1))
  }
}

config.run() match
  case Right(results) =>
    println(results.formatWorkloadTable)
    println(s"Avg completion: ${results.avgCompletionTime.value}s")
  case Left(error) =>
    println(s"Failed: $error")
```

Expected output:
```
Workload  VM  Host  Status     Start     Finish    Duration  MI
0         0   0     SUCCESS    0.00      1.00      1.00      10000
1         0   0     SUCCESS    0.00      1.00      1.00      10000
2         0   0     SUCCESS    0.00      1.00      1.00      10000
3         0   0     SUCCESS    0.00      1.00      1.00      10000
```

## Multi-Paradigm Simulations

### IaaS

```scala
val config = simulation("iaas", endTime = SimTime(1000.0)) {
  datacenter("dc-1") {
    allocationPolicy(VmAllocationPolicy.bestFit)
    scheduler(WorkloadScheduler.timeShared)
    hosts(count = 10, pes = PEs(16), mips = MIPS(20000.0), ram = MegaBytes(65536.0),
      powerModel = Some(PowerModel.linear(Watts(400.0), Watts(150.0))))
  }
  broker("broker-1") {
    costRates(CostRates.awsM5)
    vms(count = 50, pes = PEs(2), mips = MIPS(2000.0), ram = MegaBytes(4096.0))
    workloads(count = 500, length = MI(20000.0), pes = PEs(1))
  }
}
```

### Serverless

```scala
import io.aura.serverless.*

val config = simulation("faas", endTime = SimTime(300.0)) {
  faasPlatform("lambda") {
    coldStartModel(ColdStartModel.byRuntime)
    billingModel(BillingModel.awsLambda)
    function("handler") {
      runtime(Runtime.Python)
      memory(MegaBytes(256.0))
      timeout(SimTime(30.0))
    }
  }
  serverlessBroker("client") {
    invocations("handler", count = 100, executionLength = MI(1000.0),
      arrivalPattern = ArrivalPattern.poisson(rate = 10.0))
  }
}
```

### Kubernetes

```scala
import io.aura.containers.*

val config = simulation("k8s", endTime = SimTime(500.0)) {
  datacenter("dc-1") {
    hosts(count = 4, pes = PEs(8), mips = MIPS(20000.0), ram = MegaBytes(32768.0))
  }
  k8sCluster("cluster") {
    schedulingPolicy(K8sScheduler.leastRequested)
    deployment("web") {
      replicas(3)
      container("nginx") {
        cpuRequest(MIPS(500.0))
        memoryRequest(MegaBytes(256.0))
      }
      workloadPerPod(length = MI(5000.0))
    }
  }
}
```

### Edge Computing

```scala
import io.aura.edge.*

val config = simulation("edge", endTime = SimTime(500.0)) {
  edgeEnvironment("metro") {
    latencyModel(LatencyModel.combined())
    offloadingPolicy(OffloadingPolicy.latencyAware)
    edgeNode("sensor-hub") {
      location(40.7128, -74.0060)
      tier(EdgeTier.EdgeMicro)
      resources(PEs(2), MIPS(2000.0), MegaBytes(2048.0))
    }
    taskSource("sensors") {
      sourceNode("sensor-hub")
      tasks(count = 20, cpuRequired = MIPS(100.0), taskLength = MI(500.0))
    }
  }
}
```

### Federated Multi-Tier

```scala
val config = simulation("federated", endTime = SimTime(5000.0)) {
  edgeEnvironment("edge-env") { /* edge nodes */ }
  faasPlatform("functions") { /* serverless tier */ }
  k8sCluster("k8s-prod") { /* container tier */ }
  federatedWorkflow("pipeline") {
    tierSelection(TierSelectionPolicy.edgeFirst)
    escalation(EscalationPolicy.cascade)
    edgeTier(environment = "edge-env", sourceNode = "edge-1")
    serverlessTier(platform = "functions", functionName = "process")
    k8sTier(cluster = "k8s-prod")
  }
}
```

### LLM Inference

```scala
import io.aura.inference.*
import io.aura.gpu.*

val config = simulation("inference", endTime = SimTime(100.0)) {
  inferenceEngine("vllm-0") {
    model(LlmModelSpec.llama2_7B)
    gpu(GpuDeviceSpec.a100_80g)
    gpuCount(1)
    maxBatchSize(32)
    kvCachePages(2048)
  }
  inferenceBroker("client") {
    targetEngine(0)
    requestBatch(count = 100, inputTokens = 512, outputTokens = 128,
      arrivalPattern = ArrivalPattern.poisson(rate = 10.0))
    sloTarget(SimTime(2.0))
  }
}
```

### Batch Scheduling

```scala
val config = simulation("hpc", endTime = SimTime(1000.0)) {
  datacenter("dc-1") { host(pes = PEs(4), mips = MIPS(10000.0), ram = MegaBytes(16384.0)) }
  broker("iaas") { vm(); workload() }
  batchBroker("hpc") {
    computeNodes(8, cpu = MIPS(2000.0), ram = MegaBytes(4096.0))
    batchAlgorithm(BatchScheduler.backfill)
    batchJobs(count = 10, estimatedRuntime = SimTime(50.0))
  }
}
```

## Architecture

| Module | Description |
|---|---|
| `aura-core` | DES engine, opaque types, event system |
| `aura-iaas` | Datacenter/Host/VM/Broker actors, policies |
| `aura-power` | Compositional power consumption models |
| `aura-serverless` | FaaS platform, cold starts, billing models |
| `aura-containers` | Kubernetes-style pod scheduling |
| `aura-edge` | Edge computing, latency models, offloading |
| `aura-gpu` | GPU hardware substrate, DVFS, multi-component power models |
| `aura-inference` | LLM inference simulation, energy-aware scheduling |
| `aura-dsl` | High-level simulation configuration DSL |
| `aura-traces` | Google Cluster & Azure VM trace parsers |
| `aura-bench` | JMH benchmarks |
| `aura-viz` | HTML report generation with Chart.js |
| `aura-viz-frontend` | Scala.js + Laminar SPA dashboard |
| `aura-examples` | 20 example simulations |

## Policies

All policies are pure functions, making them easy to compose and test:

| Category | Policies |
|---|---|
| VM Allocation | `firstFit`, `bestFit`, `worstFit`, `roundRobin` |
| Workload Scheduling | `timeShared`, `spaceShared`, `cfs` |
| Migration | `preCopy`, `iterativePreCopy`, `postCopy`, `nonLive` |
| Horizontal Scaling | `cpuThreshold`, `predictive` |
| Consolidation | `staticThreshold`, `iqrThreshold`, `madThreshold` |
| Batch Scheduling | `fcfs`, `sjf`, `priorityBased`, `gangSchedule`, `backfill` |
| K8s Scheduling | `leastRequested`, `mostRequested`, `priorityBased` |
| Edge Offloading | `latencyAware`, `costAware`, `energyAware` |
| Power Models | `linear`, `nonLinear`, `hpProLiantG4`, `hpProLiantG5` |
| GPU DVFS | `maxPerformance`, `powerCapped`, `energyProportional` |
| Inference Scheduling | `continuousBatching`, `phaseAwareDvfs`, `powerCappedAdmission`, `energySloPareto` |
| Cost Models | `awsM5`, `awsSpot`, `awsReserved` |

## Results & Output

`SimulationResults` provides structured access to all simulation metrics:

```scala
results.formatWorkloadTable     // tabular workload completion data
results.formatBatchSummary      // HPC batch job statistics
results.formatCostReport        // per-VM cost breakdown
results.formatEdgeSummary       // edge task completion metrics
results.toCSV                   // CSV export
results.toJSON                  // JSON export
```

## Examples

The `aura-examples` module contains 20 runnable examples:

| Example | Description |
|---|---|
| `BasicIaaSExample` | Single DC, 1 host, 4 workloads |
| `MultiDcNetworkExample` | Multi-datacenter with network topology |
| `VmMigrationExample` | Live VM migration with consolidation |
| `DagSchedulingExample` | DAG task dependency scheduling |
| `SpotInstanceExample` | Spot instance pricing and preemption |
| `FaultToleranceExample` | Fault injection and recovery |
| `VerticalScalingExample` | Dynamic VM vertical scaling |
| `CfsSchedulerExample` | Completely Fair Scheduler workload scheduling |
| `DatacenterEfficiencyExample` | Datacenter PUE and efficiency metrics |
| `EnergyAwareExample` | Power model tracking and energy reports |
| `ServerlessBasicExample` | FaaS with cold starts and billing |
| `ServerlessBurstExample` | Burst traffic with auto-scaling |
| `ContainerBasicExample` | K8s cluster with deployments |
| `ContainerBinPackExample` | Bin-packing pod scheduler |
| `EdgeBasicExample` | 3-tier edge topology |
| `EdgeOffloadingExample` | Cost-aware edge offloading |
| `FederatedExample` | Multi-tier federated workflow |
| `FederatedCostExample` | Federated orchestration with cost tracking |
| `InferenceBasicExample` | LLM inference on GPU with energy tracking |
| `AzureTraceReplayExample` | Azure VM trace replay |

Run any example:

```bash
sbt "auraExamples/runMain io.aura.examples.<ExampleName>"
```

## Trace Replay

Parse real-world cloud traces for realistic workload generation:

```scala
import io.aura.traces.*

// Google Cluster Data
val tasks = GoogleClusterTraceParser.parse(traceContent)

// Azure VM Traces
val vms = AzureVmTraceParser.parse(traceContent)
```

## Visualization

Generate HTML reports with interactive Chart.js dashboards:

```scala
import io.aura.viz.ReportGenerator
ReportGenerator.generate(results, outputDir)
```

Reports include workload timelines, host utilization heatmaps, energy consumption charts, cost breakdowns, and edge task latency distributions.

## Technology Stack

| Component | Version |
|---|---|
| Scala | 3.4.2 |
| Apache Pekko | 1.1.3 (typed actors) |
| ScalaTest | 3.2.19 |
| ScalaCheck | 1.18.1 |
| JMH | 0.4.7 |
| Chart.js | 4.4.0 |

## Citation

If you use Aura in academic work, please cite:

```bibtex
@inproceedings{cappelletti2026aura,
  title     = {Aura: A Type-Safe, Multi-Paradigm Cloud Simulation
               Framework in Scala 3},
  author    = {Cappelletti, Andrea},
  booktitle = {Proceedings of the 2026 IEEE International Conference
               on Cloud Computing (CLOUD)},
  year      = {2026}
}
```

## Contributing

Contributions are welcome. Please open an issue to discuss proposed changes before submitting a pull request.

```bash
sbt compile    # compile
sbt test       # run tests
sbt scalafmtAll  # format code
```

## License

MIT License. See [LICENSE](LICENSE) for details.

## Trademarks

This project may contain trademarks or logos for projects, products, or services. Authorized use of Microsoft trademarks or logos is subject to and must follow [Microsoft's Trademark & Brand Guidelines](https://www.microsoft.com/en-us/legal/intellectualproperty/trademarks/usage/general). Use of Microsoft trademarks or logos in modified versions of this project must not cause confusion or imply Microsoft sponsorship. Any use of third-party trademarks or logos are subject to those third-party's policies.
