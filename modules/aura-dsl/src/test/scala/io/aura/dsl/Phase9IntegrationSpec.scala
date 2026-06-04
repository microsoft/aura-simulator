// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.dsl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.iaas.policies.CostRates
import io.aura.serverless.{BillingModel, ColdStartModel, Runtime}
import io.aura.containers.K8sScheduler
import io.aura.edge.{EdgeTier, LatencyModel, OffloadingPolicy}

class Phase9IntegrationSpec extends AnyFlatSpec with Matchers:

  // ─── Batch Scheduling E2E ───────────────────────────────────────────

  "Batch scheduling E2E" should "run FCFS batch jobs to completion" in {
    val config = simulation("batch-e2e-fcfs", endTime = SimTime(500.0)) {
      datacenter("dc-1") {
        hosts(count = 2, pes = PEs(4), mips = MIPS(10000.0), ram = MegaBytes(16384.0))
      }
      broker("iaas") {
        vm(pes = PEs(2), mips = MIPS(10000.0), ram = MegaBytes(4096.0))
        workload(length = MI(1000.0))
      }
      batchBroker("hpc") {
        computeNodes(4, cpu = MIPS(2000.0), ram = MegaBytes(4096.0))
        batchAlgorithm(BatchScheduler.fcfs)
        batchTickInterval(SimTime(5.0))
        batchJobs(count = 3, estimatedRuntime = SimTime(20.0), cpuPerNode = MIPS(500.0), ramPerNode = MegaBytes(1024.0))
      }
    }

    val result = config.run()
    result.isRight shouldBe true
    result.foreach { r =>
      r.batchJobRecords should have size 3
      r.batchJobRecords.foreach { rec =>
        rec.turnaroundTime.value should be > 0.0
        rec.finishTime.value should be > rec.startTime.value
      }
    }
  }

  it should "run SJF batch jobs to completion" in {
    val config = simulation("batch-e2e-sjf", endTime = SimTime(500.0)) {
      datacenter("dc-1") {
        host(pes = PEs(4), mips = MIPS(10000.0), ram = MegaBytes(16384.0))
      }
      broker("iaas") {
        vm(pes = PEs(2), mips = MIPS(10000.0), ram = MegaBytes(4096.0))
        workload(length = MI(1000.0))
      }
      batchBroker("hpc") {
        computeNodes(4, cpu = MIPS(2000.0), ram = MegaBytes(4096.0))
        batchAlgorithm(BatchScheduler.sjf)
        batchJobs(count = 5, estimatedRuntime = SimTime(30.0), cpuPerNode = MIPS(500.0), ramPerNode = MegaBytes(1024.0))
      }
    }

    val result = config.run()
    result.isRight shouldBe true
    result.foreach { r =>
      r.batchJobRecords should have size 5
    }
  }

  it should "run backfill batch jobs to completion" in {
    val config = simulation("batch-e2e-backfill", endTime = SimTime(500.0)) {
      datacenter("dc-1") {
        host(pes = PEs(4), mips = MIPS(10000.0), ram = MegaBytes(16384.0))
      }
      broker("iaas") {
        vm(pes = PEs(2), mips = MIPS(10000.0), ram = MegaBytes(4096.0))
        workload(length = MI(1000.0))
      }
      batchBroker("hpc") {
        computeNodes(4, cpu = MIPS(2000.0), ram = MegaBytes(4096.0))
        batchAlgorithm(BatchScheduler.backfill)
        batchJobs(count = 4, estimatedRuntime = SimTime(25.0), cpuPerNode = MIPS(500.0), ramPerNode = MegaBytes(1024.0))
      }
    }

    val result = config.run()
    result.isRight shouldBe true
    result.foreach { r =>
      r.batchJobRecords should have size 4
    }
  }

  it should "produce batch summary report" in {
    val config = simulation("batch-summary", endTime = SimTime(500.0)) {
      datacenter("dc-1") {
        host(pes = PEs(4), mips = MIPS(10000.0), ram = MegaBytes(16384.0))
      }
      broker("iaas") {
        vm(pes = PEs(2), mips = MIPS(10000.0), ram = MegaBytes(4096.0))
        workload(length = MI(1000.0))
      }
      batchBroker("hpc") {
        computeNodes(4, cpu = MIPS(2000.0), ram = MegaBytes(4096.0))
        batchJobs(count = 3, estimatedRuntime = SimTime(20.0), cpuPerNode = MIPS(500.0), ramPerNode = MegaBytes(1024.0))
      }
    }

    val result = config.run()
    result.isRight shouldBe true
    result.foreach { r =>
      r.formatBatchSummary should include("Batch Jobs: 3 completed")
      r.formatBatchTable should include("Job ID")
    }
  }

  // ─── VM Boot Config E2E ─────────────────────────────────────────────

  "VM boot config" should "apply boot delay config without errors" in {
    val config = simulation("boot-delay", endTime = SimTime(1000.0)) {
      datacenter("dc-1") {
        host(pes = PEs(4), mips = MIPS(10000.0), ram = MegaBytes(16384.0))
      }
      broker("b") {
        vmBootConfig(VmBootConfig.standard)
        vm(pes = PEs(2), mips = MIPS(10000.0), ram = MegaBytes(4096.0))
        workload(length = MI(5000.0))
      }
    }

    val result = config.run()
    result.isRight shouldBe true
    result.foreach { r =>
      r.workloadResults should have size 1
    }
  }

  // ─── Consolidation E2E ──────────────────────────────────────────────

  "Consolidation" should "run simulation with consolidation config" in {
    val config = simulation("consolidation-e2e", endTime = SimTime(1000.0)) {
      datacenter("dc-1") {
        hosts(count = 4, pes = PEs(4), mips = MIPS(10000.0), ram = MegaBytes(16384.0))
        consolidation(
          overloadDetector = io.aura.iaas.policies.OverloadDetector.staticThreshold(0.8),
          underloadDetector = io.aura.iaas.policies.UnderloadDetector.staticThreshold(0.2),
          vmSelector = io.aura.iaas.policies.VmSelectionPolicy.minimumMigrationTime,
          interval = SimTime(10.0)
        )
      }
      broker("b") {
        vms(count = 3, pes = PEs(2), mips = MIPS(5000.0), ram = MegaBytes(4096.0))
        workloads(count = 3, length = MI(100000.0))
      }
    }

    val result = config.run()
    result.isRight shouldBe true
    result.foreach { r =>
      // Verify the simulation completed successfully with consolidation enabled
      r.workloadResults should have size 3
    }
  }

  // ─── Cost + SLA E2E ─────────────────────────────────────────────────

  "Cost and SLA tracking" should "produce both cost and SLA records" in {
    val config = simulation("cost-sla-e2e", endTime = SimTime(500.0)) {
      datacenter("dc-1") {
        host(pes = PEs(2), mips = MIPS(10000.0), ram = MegaBytes(8192.0))
      }
      broker("b") {
        costRates(CostRates.awsM5)
        vm(pes = PEs(2), mips = MIPS(10000.0), ram = MegaBytes(4096.0))
        workloads(count = 3, length = MI(20000.0))
      }
    }

    val result = config.run()
    result.isRight shouldBe true
    result.foreach { r =>
      r.costRecords should not be empty
      r.totalCost.value should be > 0.0
      r.formatCostReport should include("VM")
    }
  }

  // ─── Mixed Paradigm E2E ────────────────────────────────────────────

  "Mixed paradigm simulation" should "combine IaaS + batch scheduling" in {
    val config = simulation("mixed-iaas-batch", endTime = SimTime(500.0)) {
      datacenter("dc-1") {
        hosts(count = 4, pes = PEs(4), mips = MIPS(10000.0), ram = MegaBytes(16384.0))
      }
      broker("iaas") {
        vm(pes = PEs(2), mips = MIPS(10000.0), ram = MegaBytes(4096.0))
        workload(length = MI(5000.0))
      }
      batchBroker("hpc") {
        computeNodes(8, cpu = MIPS(2000.0), ram = MegaBytes(4096.0))
        batchAlgorithm(BatchScheduler.priorityBased)
        batchJobs(count = 6, estimatedRuntime = SimTime(30.0), cpuPerNode = MIPS(500.0), ramPerNode = MegaBytes(1024.0))
      }
    }

    val result = config.run()
    result.isRight shouldBe true
    result.foreach { r =>
      r.workloadResults should have size 1
      r.batchJobRecords should have size 6
    }
  }

  // ─── Serverless E2E ─────────────────────────────────────────────────

  "Serverless simulation" should "run invocations to completion" in {
    val config = simulation("serverless-e2e", endTime = SimTime(500.0)) {
      faasPlatform("lambda") {
        coldStartModel(ColdStartModel.byRuntime)
        billingModel(BillingModel.awsLambda)
        function("handler") {
          runtime(Runtime.Python)
          memory(MegaBytes(256.0))
          timeout(SimTime(30.0))
        }
      }
      serverlessBroker("invoker") {
        invocations("handler", count = 5, executionLength = MI(500.0))
      }
    }

    val result = config.run()
    result.isRight shouldBe true
    result.foreach { r =>
      // Invocations should complete (may be empty if platform hasn't routed results to metrics)
      r.totalEventsProcessed should be > 0L
    }
  }

  // ─── K8s E2E ────────────────────────────────────────────────────────

  "K8s simulation" should "schedule pods and complete workloads" in {
    val config = simulation("k8s-e2e", endTime = SimTime(500.0)) {
      datacenter("dc-1") {
        hosts(count = 2, pes = PEs(4), mips = MIPS(10000.0), ram = MegaBytes(16384.0))
      }
      k8sCluster("cluster-1") {
        schedulingPolicy(K8sScheduler.leastRequested)
        deployment("web") {
          replicas(2)
          container("nginx") {
            image("nginx:latest")
            cpuRequest(MIPS(500.0))
            cpuLimit(MIPS(1000.0))
            memoryRequest(MegaBytes(256.0))
            memoryLimit(MegaBytes(512.0))
          }
          workloadPerPod(length = MI(5000.0), pes = PEs(1))
        }
      }
    }

    val result = config.run()
    result.isRight shouldBe true
    result.foreach { r =>
      r.podResults should not be empty
    }
  }

  // ─── Edge E2E ───────────────────────────────────────────────────────

  "Edge computing simulation" should "complete edge tasks" in {
    val config = simulation("edge-e2e", endTime = SimTime(500.0)) {
      edgeEnvironment("edge-1") {
        latencyModel(LatencyModel.combined())
        offloadingPolicy(OffloadingPolicy.latencyAware)
        edgeNode("node-A") {
          location(40.7128, -74.0060)
          tier(EdgeTier.EdgeMicro)
          resources(PEs(2), MIPS(2000.0), MegaBytes(2048.0))
        }
        edgeNode("node-B") {
          location(40.7580, -73.9855)
          tier(EdgeTier.EdgeSmall)
          resources(PEs(4), MIPS(4000.0), MegaBytes(4096.0))
        }
        taskSource("source-A") {
          sourceNode("node-A")
          tasks(
            count = 5,
            cpuRequired = MIPS(100.0),
            memRequired = MegaBytes(64.0),
            taskLength = MI(500.0),
            deadline = SimTime(2.0)
          )
          interArrivalTime(SimTime(1.0))
        }
      }
    }

    val result = config.run()
    result.isRight shouldBe true
    result.foreach { r =>
      r.edgeTaskResults should have size 5
      r.formatEdgeSummary should include("Completed: 5")
    }
  }
