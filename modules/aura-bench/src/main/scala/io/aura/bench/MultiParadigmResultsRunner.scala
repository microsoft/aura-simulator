// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.bench

import io.aura.core.types.*
import io.aura.core.engine.SimulationResults
import io.aura.dsl.{DslSimulationConfig, SimulationDsl}
import io.aura.dsl.SimulationDsl.*
import io.aura.iaas.policies.{VmAllocationPolicy, WorkloadScheduler}
import io.aura.power.PowerModel
import io.aura.serverless.*
import io.aura.containers.*
import io.aura.edge.*

/** Standalone runner for multi-paradigm scenarios F1–F5.
  *
  * Executes each scenario once and extracts key metrics for the IEEE CLOUD 2026 paper results table. Unlike the JMH
  * benchmarks, this captures simulation outputs (not just execution time).
  *
  * Run with: sbt "auraBench/runMain io.aura.bench.MultiParadigmResultsRunner"
  */
object MultiParadigmResultsRunner:

  private def runSim(config: DslSimulationConfig): SimulationResults =
    config.run() match
      case Right(r)  => r
      case Left(err) => throw RuntimeException(s"Simulation failed: $err")

  private def timed[A](block: => A): (A, Long) =
    val start   = System.nanoTime()
    val result  = block
    val elapsed = (System.nanoTime() - start) / 1_000_000
    (result, elapsed)

  // ─── F1: Serverless ──────────────────────────────────────────────────

  private def runF1(): (SimulationResults, Long) =
    val config = simulation("f1-serverless", endTime = SimTime(2000.0)) {
      // Minimal IaaS footprint to avoid actor race condition
      datacenter("dc") {
        allocationPolicy(VmAllocationPolicy.firstFit)
        scheduler(WorkloadScheduler.timeShared)
        hosts(count = 1, pes = PEs(4), mips = MIPS(10000.0), ram = MegaBytes(8192.0))
      }
      broker("b") {
        vms(count = 1, pes = PEs(1), mips = MIPS(1000.0), ram = MegaBytes(1024.0))
        workloads(count = 1, length = MI(1000.0))
      }

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
    timed(runSim(config))

  // ─── F2: Containers ──────────────────────────────────────────────────

  private def runF2(): (SimulationResults, Long) =
    val config = simulation("f2-containers", endTime = SimTime(3000.0)) {
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
    timed(runSim(config))

  // ─── F3: Edge ────────────────────────────────────────────────────────

  private def runF3(): (SimulationResults, Long) =
    val config = simulation("f3-edge", endTime = SimTime(1000.0)) {
      edgeEnvironment("metro") {
        latencyModel(LatencyModel.combined())
        offloadingPolicy(OffloadingPolicy.latencyAware)

        for i <- 1 to 30 do
          edgeNode(s"sensor-$i") {
            location(37.77 + (i * 0.001), -122.42 + (i * 0.001))
            tier(EdgeTier.Device)
            resources(pes = PEs(1), mips = MIPS(100.0), ram = MegaBytes(256.0))
          }

        for i <- 1 to 20 do
          edgeNode(s"gateway-$i") {
            location(37.78 + (i * 0.002), -122.41 + (i * 0.002))
            tier(EdgeTier.EdgeMicro)
            resources(pes = PEs(4), mips = MIPS(2000.0), ram = MegaBytes(4096.0))
          }

        for i <- 1 to 10 do
          edgeNode(s"edge-med-$i") {
            location(37.79 + (i * 0.005), -122.40 + (i * 0.005))
            tier(EdgeTier.EdgeMedium)
            resources(pes = PEs(8), mips = MIPS(10000.0), ram = MegaBytes(16384.0))
          }

        for i <- 1 to 5 do
          edgeNode(s"fog-$i") {
            location(37.80 + (i * 0.01), -122.39 + (i * 0.01))
            tier(EdgeTier.Fog)
            resources(pes = PEs(16), mips = MIPS(30000.0), ram = MegaBytes(32768.0))
          }

        for i <- 1 to 5 do
          edgeNode(s"cloud-$i") {
            location(37.39 + (i * 0.02), -122.08 + (i * 0.02))
            tier(EdgeTier.Cloud)
            resources(pes = PEs(32), mips = MIPS(50000.0), ram = MegaBytes(65536.0))
          }

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
    timed(runSim(config))

  // ─── F4: Federated ───────────────────────────────────────────────────

  private def runF4(): (SimulationResults, Long) =
    val config = simulation("f4-federated", endTime = SimTime(5000.0)) {
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
    timed(runSim(config))

  // ─── F5: Mixed ───────────────────────────────────────────────────────

  private def runF5(): (SimulationResults, Long) =
    val config = simulation("f5-mixed", endTime = SimTime(5000.0)) {
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
    timed(runSim(config))

  // ─── Main ────────────────────────────────────────────────────────────

  def main(args: Array[String]): Unit =
    println()
    println("*" * 70)
    println("  Multi-Paradigm Results Runner (F1–F5)")
    println("  IEEE CLOUD 2026 — Extracting metrics for paper table")
    println("*" * 70)
    println()

    // Warmup
    print("Warming up JIT... ")
    val warmup = simulation("warmup", endTime = SimTime(500.0)) {
      datacenter("dc") {
        allocationPolicy(VmAllocationPolicy.firstFit)
        scheduler(WorkloadScheduler.timeShared)
        hosts(count = 2, pes = PEs(4), mips = MIPS(10000.0), ram = MegaBytes(8192.0))
      }
      broker("b") {
        vms(count = 2, pes = PEs(1), mips = MIPS(1000.0), ram = MegaBytes(1024.0))
        workloads(count = 10, length = MI(5000.0))
      }
    }
    for _ <- 1 to 3 do warmup.run()
    println("done.")
    println()

    // ── F1: Serverless ──
    println("=" * 70)
    println("F1: Serverless — 10,000 invocations, 3 functions, Poisson arrivals")
    println("=" * 70)
    val (f1, f1Time) = runF1()
    val f1Total      = f1.invocationResults.size
    val f1Cold       = f1.invocationResults.count(_.coldStart)
    val f1Throttled  = f1.throttledInvocations.size
    val f1TimedOut   = f1.timedOutInvocations.size
    val f1ColdPct    = if f1Total > 0 then f1Cold.toDouble / f1Total * 100 else 0.0
    val f1BilledGBs  = f1.invocationResults.map(_.billedGBSeconds.value).sum
    println(f"  Invocations completed: $f1Total")
    println(f"  Cold starts: $f1Cold ($f1ColdPct%.1f%%)")
    println(f"  Throttled: $f1Throttled")
    println(f"  Timed out: $f1TimedOut")
    println(f"  Billed GB-seconds: $f1BilledGBs%.2f")
    println(f"  Execution time: ${f1Time}ms")
    println()

    // ── F2: Containers ──
    println("=" * 70)
    println("F2: Containers — 500 pods, 3 deployments, mostRequested scheduling")
    println("=" * 70)
    val (f2, f2Time) = runF2()
    val f2TotalPods  = f2.podResults.size
    val f2Succeeded  = f2.podResults.count(_.phase == "Succeeded")
    val f2Running    = f2.podResults.count(_.phase == "Running")
    val f2Failed     = f2.podResults.count(_.phase == "Failed")
    val f2Unsched    = f2.unschedulablePods.size
    println(f"  Pods total: $f2TotalPods")
    println(f"  Succeeded: $f2Succeeded")
    println(f"  Running: $f2Running")
    println(f"  Failed: $f2Failed")
    println(f"  Unschedulable: $f2Unsched")
    println(f"  Execution time: ${f2Time}ms")
    println()

    // ── F3: Edge ──
    println("=" * 70)
    println("F3: Edge — 3,000 tasks, 70 nodes, 5 tiers, latency-aware")
    println("=" * 70)
    val (f3, f3Time) = runF3()
    val f3Total      = f3.edgeTaskResults.size
    val f3Failed     = f3.failedEdgeTasks.size
    val f3Offloaded  = f3.edgeTaskResults.count(_.offloaded)
    val f3AvgLatency =
      if f3Total > 0 then f3.edgeTaskResults.map(_.networkLatency.value).sum / f3Total * 1000
      else 0.0
    println(f"  Tasks completed: $f3Total")
    println(f"  Tasks failed: $f3Failed")
    println(f"  Tasks offloaded: $f3Offloaded")
    println(f"  Avg network latency: ${f3AvgLatency}%.1f ms")
    println(f"  Execution time: ${f3Time}ms")
    println()

    // ── F4: Federated ──
    println("=" * 70)
    println("F4: Federated — 5,000 tasks, 3-tier cascade, edgeFirst")
    println("=" * 70)
    val (f4, f4Time) = runF4()
    val f4Total      = f4.federatedTaskResults.size
    val f4FailedFed  = f4.failedFederatedTasks.size
    val f4Escalated  = f4.federatedTaskResults.count(_.escalations > 0)
    val f4EscPct     = if f4Total > 0 then f4Escalated.toDouble / f4Total * 100 else 0.0
    val f4AvgEsc =
      if f4Total > 0 then f4.federatedTaskResults.map(_.escalations).sum.toDouble / f4Total
      else 0.0
    println(f"  Tasks completed: $f4Total")
    println(f"  Tasks failed: $f4FailedFed")
    println(f"  Tasks escalated: $f4Escalated ($f4EscPct%.1f%%)")
    println(f"  Avg escalations: $f4AvgEsc%.2f")
    println(f"  Execution time: ${f4Time}ms")
    println()

    // ── F5: Mixed ──
    println("=" * 70)
    println("F5: Mixed — IaaS infrastructure + Serverless + Containers")
    println("=" * 70)
    val (f5, f5Time)  = runF5()
    val f5Invocations = f5.invocationResults.size
    val f5Pods        = f5.podResults.size
    val f5InvCold     = f5.invocationResults.count(_.coldStart)
    val f5InvColdPct  = if f5Invocations > 0 then f5InvCold.toDouble / f5Invocations * 100 else 0.0
    val f5Energy      = f5.totalEnergyWh.value
    val f5Cost        = f5.totalCost.value
    println(f"  IaaS infrastructure: 20 hosts (power-modeled)")
    println(f"  Serverless invocations: $f5Invocations (cold: $f5InvCold, $f5InvColdPct%.1f%%)")
    println(f"  Container pods: $f5Pods")
    println(f"  Total energy: ${f5Energy}%.2f Wh")
    println(f"  Total cost: $$$f5Cost%.4f")
    println(f"  Execution time: ${f5Time}ms")
    println()

    // ── Summary Table ──
    println("=" * 70)
    println("Summary Table for Paper")
    println("=" * 70)
    println(f"${"Scenario"}%-12s ${"Paradigm"}%-12s ${"Scale"}%-24s ${"Key Metric"}%-28s ${"Time (ms)"}%-10s")
    println("-" * 86)
    println(
      f"${"F1"}%-12s ${"Serverless"}%-12s ${"10K invocations, 3 funcs"}%-24s ${f"Cold start: $f1ColdPct%.1f%%"}%-28s ${f1Time}%-10d"
    )
    println(
      f"${"F2"}%-12s ${"Containers"}%-12s ${"500 pods, 3 deployments"}%-24s ${f"Scheduled: ${f2Succeeded + f2Running}/$f2TotalPods"}%-28s ${f2Time}%-10d"
    )
    println(
      f"${"F3"}%-12s ${"Edge"}%-12s ${"3K tasks, 70 nodes"}%-24s ${f"Offloaded: $f3Offloaded, ${f3AvgLatency}%.0fms lat"}%-28s ${f3Time}%-10d"
    )
    println(
      f"${"F4"}%-12s ${"Federated"}%-12s ${"5K tasks, 3 tiers"}%-24s ${f"Escalated: $f4EscPct%.0f%%, avg $f4AvgEsc%.1f"}%-28s ${f4Time}%-10d"
    )
    println(
      f"${"F5"}%-12s ${"Mixed"}%-12s ${s"20h+${f5Invocations}inv+${f5Pods}pod"}%-24s ${f"Energy: ${f5Energy}%.1f Wh"}%-28s ${f5Time}%-10d"
    )
    println()

    // ── CSV ──
    println("CSV:")
    println("scenario,paradigm,entities,key_metric,key_value,exec_time_ms")
    println(s"F1,Serverless,${f1Total},cold_start_pct,${f1ColdPct},${f1Time}")
    println(s"F2,Containers,${f2TotalPods},scheduled_pct,${(f2Succeeded + f2Running).toDouble / math
        .max(f2TotalPods, 1) * 100},${f2Time}")
    println(
      s"F3,Edge,${f3Total},offload_pct,${if f3Total > 0 then f3Offloaded.toDouble / f3Total * 100 else 0.0},${f3Time}"
    )
    println(s"F4,Federated,${f4Total},escalation_pct,${f4EscPct},${f4Time}")
    println(s"F5,Mixed,${20 + f5Invocations + f5Pods},energy_wh,${f5Energy},${f5Time}")

    println()
    println("Runner complete.")
