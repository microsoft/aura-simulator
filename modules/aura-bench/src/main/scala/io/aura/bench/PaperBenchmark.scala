// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.bench

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit
import scala.compiletime.uninitialized

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.iaas.policies.{CostRates, VmAllocationPolicy, WorkloadScheduler}
import io.aura.power.PowerModel
import io.aura.serverless.*
import io.aura.containers.*
import io.aura.edge.*

// =============================================================================
// IEEE CLOUD 2026 Paper Benchmarks — IaaS Scenarios S1–S4
//
// Measures end-to-end simulation execution time and throughput for four
// progressively larger IaaS scenarios, intended for direct comparison with
// corresponding baseline configurations.
//
// Run with:
//   sbt "auraBench/Jmh/run -f 1 -wi 5 -i 10 -rff paper-iaas.csv .*PaperIaasBenchmark.*"
// =============================================================================

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 2)
@Measurement(iterations = 10, time = 2)
@Fork(1)
class PaperIaasBenchmark:

  /** S1: Small — 1 DC, 10 hosts, 100 VMs, 1,000 workloads. */
  @Benchmark
  def s1_small(bh: Blackhole): Unit =
    val config = simulation("paper-s1", endTime = SimTime(5000.0)) {
      datacenter("dc-1") {
        allocationPolicy(VmAllocationPolicy.bestFit)
        scheduler(WorkloadScheduler.timeShared)
        hosts(
          count = 10,
          pes = PEs(16),
          mips = MIPS(20000.0),
          ram = MegaBytes(65536.0),
          bw = Mbps(10000.0),
          storage = MegaBytes(1000000.0),
          powerModel = Some(PowerModel.linear(Watts(400.0), Watts(150.0)))
        )
      }
      broker("broker-1") {
        costRates(CostRates.awsM5)
        vms(
          count = 100,
          pes = PEs(2),
          mips = MIPS(2000.0),
          ram = MegaBytes(4096.0),
          bw = Mbps(1000.0),
          storage = MegaBytes(10000.0)
        )
        workloads(count = 1000, length = MI(20000.0), pes = PEs(1))
      }
    }
    bh.consume(config.run())

  /** S2: Medium — 1 DC, 100 hosts, 500 VMs, 5,000 workloads. */
  @Benchmark
  def s2_medium(bh: Blackhole): Unit =
    val config = simulation("paper-s2", endTime = SimTime(10000.0)) {
      datacenter("dc-1") {
        allocationPolicy(VmAllocationPolicy.bestFit)
        scheduler(WorkloadScheduler.timeShared)
        hosts(
          count = 100,
          pes = PEs(16),
          mips = MIPS(20000.0),
          ram = MegaBytes(65536.0),
          bw = Mbps(10000.0),
          storage = MegaBytes(1000000.0),
          powerModel = Some(PowerModel.linear(Watts(400.0), Watts(150.0)))
        )
      }
      broker("broker-1") {
        costRates(CostRates.awsM5)
        vms(
          count = 500,
          pes = PEs(2),
          mips = MIPS(2000.0),
          ram = MegaBytes(4096.0),
          bw = Mbps(1000.0),
          storage = MegaBytes(10000.0)
        )
        workloads(count = 5000, length = MI(20000.0), pes = PEs(1))
      }
    }
    bh.consume(config.run())

  /** S3: Large — 1 DC, 1000 hosts, 5,000 VMs, 50,000 workloads. */
  @Benchmark
  def s3_large(bh: Blackhole): Unit =
    val config = simulation("paper-s3", endTime = SimTime(50000.0)) {
      datacenter("dc-1") {
        allocationPolicy(VmAllocationPolicy.bestFit)
        scheduler(WorkloadScheduler.timeShared)
        hosts(
          count = 1000,
          pes = PEs(16),
          mips = MIPS(20000.0),
          ram = MegaBytes(65536.0),
          bw = Mbps(10000.0),
          storage = MegaBytes(1000000.0),
          powerModel = Some(PowerModel.linear(Watts(400.0), Watts(150.0)))
        )
      }
      broker("broker-1") {
        costRates(CostRates.awsM5)
        vms(
          count = 5000,
          pes = PEs(2),
          mips = MIPS(2000.0),
          ram = MegaBytes(4096.0),
          bw = Mbps(1000.0),
          storage = MegaBytes(10000.0)
        )
        workloads(count = 50000, length = MI(20000.0), pes = PEs(1))
      }
    }
    bh.consume(config.run())

  /** S4: Multi-DC — 5 DCs, 100 hosts each, 2,000 VMs, 10,000 workloads. */
  @Benchmark
  def s4_multiDc(bh: Blackhole): Unit =
    val config = simulation("paper-s4", endTime = SimTime(10000.0)) {
      for i <- 1 to 5 do
        datacenter(s"dc-$i") {
          allocationPolicy(VmAllocationPolicy.bestFit)
          scheduler(WorkloadScheduler.timeShared)
          hosts(
            count = 100,
            pes = PEs(16),
            mips = MIPS(20000.0),
            ram = MegaBytes(65536.0),
            bw = Mbps(10000.0),
            storage = MegaBytes(1000000.0),
            powerModel = Some(PowerModel.linear(Watts(400.0), Watts(150.0)))
          )
        }
      broker("broker-1") {
        costRates(CostRates.awsM5)
        vms(
          count = 2000,
          pes = PEs(2),
          mips = MIPS(2000.0),
          ram = MegaBytes(4096.0),
          bw = Mbps(1000.0),
          storage = MegaBytes(10000.0)
        )
        workloads(count = 10000, length = MI(20000.0), pes = PEs(1))
      }
    }
    bh.consume(config.run())

// =============================================================================
// IEEE CLOUD 2026 Paper Benchmarks — Scalability Analysis
//
// Measures execution time as workload count increases from 100 to 100,000
// on fixed infrastructure (100 hosts, 500 VMs).
//
// Run with:
//   sbt "auraBench/Jmh/run -f 1 -wi 5 -i 10 -rff paper-scale.csv .*PaperScalabilityBenchmark.*"
// =============================================================================

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 2)
@Measurement(iterations = 10, time = 2)
@Fork(1)
class PaperScalabilityBenchmark:

  @Param(Array("100", "500", "1000", "5000", "10000", "50000"))
  var workloadCount: Int = uninitialized

  @Benchmark
  def scalability(bh: Blackhole): Unit =
    val config = simulation("paper-scale", endTime = SimTime(50000.0)) {
      datacenter("dc-1") {
        allocationPolicy(VmAllocationPolicy.bestFit)
        scheduler(WorkloadScheduler.timeShared)
        hosts(
          count = 100,
          pes = PEs(16),
          mips = MIPS(20000.0),
          ram = MegaBytes(65536.0),
          bw = Mbps(10000.0),
          storage = MegaBytes(1000000.0),
          powerModel = Some(PowerModel.linear(Watts(400.0), Watts(150.0)))
        )
      }
      broker("broker-1") {
        vms(count = 500, pes = PEs(2), mips = MIPS(2000.0), ram = MegaBytes(4096.0))
        workloads(count = workloadCount, length = MI(20000.0), pes = PEs(1))
      }
    }
    bh.consume(config.run())

// =============================================================================
// IEEE CLOUD 2026 Paper Benchmarks — Multi-Paradigm Scenarios F1–F5
//
// Demonstrates multi-paradigm scenarios unique to Aura.
//
// Run with:
//   sbt "auraBench/Jmh/run -f 1 -wi 5 -i 10 -rff paper-multi.csv .*PaperMultiParadigmBenchmark.*"
// =============================================================================

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 2)
@Measurement(iterations = 10, time = 2)
@Fork(1)
class PaperMultiParadigmBenchmark:

  /** F1: Serverless — 10,000 invocations, cold starts, auto-scaling. */
  @Benchmark
  def f1_serverless(bh: Blackhole): Unit =
    val config = simulation("paper-f1", endTime = SimTime(2000.0)) {
      faasPlatform("lambda") {
        coldStartModel(ColdStartModel.byRuntime)
        billingModel(BillingModel.awsLambda)
        containerTtl(SimTime(600.0))

        function("compute") {
          runtime(Runtime.Python)
          memory(MegaBytes(512.0))
          timeout(SimTime(30.0))
          concurrencyLimit(100)
        }
        function("api") {
          runtime(Runtime.Java)
          memory(MegaBytes(256.0))
          timeout(SimTime(10.0))
          concurrencyLimit(200)
        }
        function("transform") {
          runtime(Runtime.Go)
          memory(MegaBytes(128.0))
          timeout(SimTime(5.0))
        }
      }

      serverlessBroker("client") {
        invocations(
          "compute",
          count = 4000,
          executionLength = MI(5000.0),
          arrivalPattern = ArrivalPattern.poisson(rate = 20.0)
        )
        invocations(
          "api",
          count = 4000,
          executionLength = MI(1000.0),
          arrivalPattern = ArrivalPattern.poisson(rate = 30.0)
        )
        invocations(
          "transform",
          count = 2000,
          executionLength = MI(2000.0),
          arrivalPattern = ArrivalPattern.poisson(rate = 10.0)
        )
      }
    }
    bh.consume(config.run())

  /** F2: Containers — 500 pods, K8s BinPack scheduling. */
  @Benchmark
  def f2_containers(bh: Blackhole): Unit =
    val config = simulation("paper-f2", endTime = SimTime(3000.0)) {
      datacenter("dc-1") {
        allocationPolicy(VmAllocationPolicy.firstFit)
        scheduler(WorkloadScheduler.timeShared)
        hosts(count = 50, pes = PEs(16), mips = MIPS(20000.0), ram = MegaBytes(65536.0))
      }

      k8sCluster("production") {
        schedulingPolicy(K8sScheduler.mostRequested)

        deployment("web") {
          replicas(200)
          container("nginx") {
            cpuRequest(MIPS(500.0))
            cpuLimit(MIPS(1000.0))
            memoryRequest(MegaBytes(256.0))
            memoryLimit(MegaBytes(512.0))
          }
          workloadPerPod(length = MI(10000.0), pes = PEs(1))
        }

        deployment("api") {
          replicas(200)
          container("api") {
            cpuRequest(MIPS(1000.0))
            cpuLimit(MIPS(2000.0))
            memoryRequest(MegaBytes(512.0))
            memoryLimit(MegaBytes(1024.0))
          }
          workloadPerPod(length = MI(20000.0), pes = PEs(1))
        }

        deployment("worker") {
          replicas(100)
          container("worker") {
            cpuRequest(MIPS(2000.0))
            cpuLimit(MIPS(4000.0))
            memoryRequest(MegaBytes(1024.0))
            memoryLimit(MegaBytes(2048.0))
          }
          workloadPerPod(length = MI(50000.0), pes = PEs(2))
        }
      }
    }
    bh.consume(config.run())

  /** F3: Edge — 100 edge nodes, latency-aware offloading. */
  @Benchmark
  def f3_edge(bh: Blackhole): Unit =
    val config = simulation("paper-f3", endTime = SimTime(1000.0)) {
      edgeEnvironment("metro") {
        latencyModel(LatencyModel.combined())
        offloadingPolicy(OffloadingPolicy.latencyAware)

        // 30 device-tier sensors
        for i <- 1 to 30 do
          edgeNode(s"sensor-$i") {
            location(37.77 + (i * 0.001), -122.42 + (i * 0.001))
            tier(EdgeTier.Device)
            resources(pes = PEs(1), mips = MIPS(100.0), ram = MegaBytes(256.0))
          }

        // 20 edge-micro gateways
        for i <- 1 to 20 do
          edgeNode(s"gateway-$i") {
            location(37.78 + (i * 0.002), -122.41 + (i * 0.002))
            tier(EdgeTier.EdgeMicro)
            resources(pes = PEs(4), mips = MIPS(2000.0), ram = MegaBytes(4096.0))
          }

        // 10 edge-medium nodes
        for i <- 1 to 10 do
          edgeNode(s"edge-med-$i") {
            location(37.79 + (i * 0.005), -122.40 + (i * 0.005))
            tier(EdgeTier.EdgeMedium)
            resources(pes = PEs(8), mips = MIPS(10000.0), ram = MegaBytes(16384.0))
          }

        // 5 fog nodes
        for i <- 1 to 5 do
          edgeNode(s"fog-$i") {
            location(37.80 + (i * 0.01), -122.39 + (i * 0.01))
            tier(EdgeTier.Fog)
            resources(pes = PEs(16), mips = MIPS(30000.0), ram = MegaBytes(32768.0))
          }

        // 5 cloud DCs
        for i <- 1 to 5 do
          edgeNode(s"cloud-$i") {
            location(37.39 + (i * 0.02), -122.08 + (i * 0.02))
            tier(EdgeTier.Cloud)
            resources(pes = PEs(32), mips = MIPS(50000.0), ram = MegaBytes(65536.0))
          }

        // tasks from each sensor
        for i <- 1 to 30 do
          taskSource(s"stream-$i") {
            sourceNode(s"sensor-$i")
            tasks(
              count = 100,
              cpuRequired = MIPS(50.0),
              memRequired = MegaBytes(64.0),
              taskLength = MI(500.0),
              deadline = SimTime(0.1)
            )
            interArrivalTime(SimTime(0.3))
          }
      }
    }
    bh.consume(config.run())

  /** F4: Federated — 3-tier, 5,000 tasks, cascade escalation. */
  @Benchmark
  def f4_federated(bh: Blackhole): Unit =
    val config = simulation("paper-f4", endTime = SimTime(5000.0)) {
      // Edge tier
      edgeEnvironment("edge-env") {
        latencyModel(LatencyModel.combined())
        offloadingPolicy(OffloadingPolicy.latencyAware)

        for i <- 1 to 3 do
          edgeNode(s"edge-$i") {
            location(37.77 + (i * 0.005), -122.42 + (i * 0.005))
            tier(EdgeTier.EdgeMicro)
            resources(pes = PEs(1), mips = MIPS(300.0), ram = MegaBytes(512.0))
          }
        edgeNode("cloud-back") {
          location(37.39, -122.08)
          tier(EdgeTier.Cloud)
          resources(pes = PEs(1), mips = MIPS(300.0), ram = MegaBytes(512.0))
        }
      }

      // Serverless tier
      faasPlatform("functions") {
        coldStartModel(ColdStartModel.byRuntime)
        billingModel(BillingModel.awsLambda)
        containerTtl(SimTime(600.0))

        function("process") {
          runtime(Runtime.Go)
          memory(MegaBytes(256.0))
          timeout(SimTime(30.0))
          concurrencyLimit(500)
        }
      }

      // K8s tier
      datacenter("dc-k8s") {
        allocationPolicy(VmAllocationPolicy.bestFit)
        scheduler(WorkloadScheduler.timeShared)
        hosts(count = 20, pes = PEs(16), mips = MIPS(20000.0), ram = MegaBytes(65536.0))
      }

      k8sCluster("k8s-prod") {
        schedulingPolicy(K8sScheduler.leastRequested)
        deployment("worker") {
          replicas(50)
          container("app") {
            cpuRequest(MIPS(500.0))
            memoryRequest(MegaBytes(256.0))
          }
          workloadPerPod(MI(2000.0), PEs(1))
        }
      }

      // Federated orchestration
      federatedWorkflow("pipeline") {
        tierSelection(TierSelectionPolicy.edgeFirst)
        escalation(EscalationPolicy.cascade)

        edgeTier(environment = "edge-env", sourceNode = "edge-1")
        serverlessTier(platform = "functions", functionName = "process")
        k8sTier(cluster = "k8s-prod")

        summon[FederatedWorkflowBuilder].tasks(
          count = 5000,
          cpuRequired = MIPS(100.0),
          memRequired = MegaBytes(64.0),
          taskLength = MI(3000.0),
          deadline = SimTime(30.0)
        )
        summon[FederatedWorkflowBuilder].interArrivalTime(SimTime(0.5))
      }
    }
    bh.consume(config.run())

  /** F5: Mixed — IaaS infrastructure + Serverless + Containers in single simulation. */
  @Benchmark
  def f5_mixed(bh: Blackhole): Unit =
    val config = simulation("paper-f5", endTime = SimTime(5000.0)) {
      // IaaS infrastructure layer (power-modeled hosts)
      datacenter("dc-infra") {
        allocationPolicy(VmAllocationPolicy.firstFit)
        scheduler(WorkloadScheduler.timeShared)
        hosts(
          count = 20,
          pes = PEs(16),
          mips = MIPS(20000.0),
          ram = MegaBytes(65536.0),
          bw = Mbps(10000.0),
          storage = MegaBytes(1000000.0),
          powerModel = Some(PowerModel.linear(Watts(400.0), Watts(150.0)))
        )
      }

      // Serverless layer
      faasPlatform("lambda") {
        coldStartModel(ColdStartModel.byRuntime)
        billingModel(BillingModel.awsLambda)
        containerTtl(SimTime(600.0))

        function("event-handler") {
          runtime(Runtime.NodeJs)
          memory(MegaBytes(256.0))
          timeout(SimTime(10.0))
          concurrencyLimit(200)
        }
      }
      serverlessBroker("event-client") {
        invocations(
          "event-handler",
          count = 2000,
          executionLength = MI(1000.0),
          arrivalPattern = ArrivalPattern.poisson(rate = 10.0)
        )
      }

      // Container layer
      k8sCluster("k8s") {
        schedulingPolicy(K8sScheduler.mostRequested)
        deployment("microservice") {
          replicas(30)
          container("svc") {
            cpuRequest(MIPS(1000.0))
            cpuLimit(MIPS(2000.0))
            memoryRequest(MegaBytes(512.0))
            memoryLimit(MegaBytes(1024.0))
          }
          workloadPerPod(length = MI(20000.0), pes = PEs(1))
        }
      }
    }
    bh.consume(config.run())
