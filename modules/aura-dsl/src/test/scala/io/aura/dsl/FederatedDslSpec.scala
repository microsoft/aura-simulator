// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.dsl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.edge.*

class FederatedDslSpec extends AnyFlatSpec with Matchers:

  "Federated DSL" should "create a valid federated workflow config" in {
    val config = simulation("fed-test", endTime = SimTime(200.0)) {
      edgeEnvironment("smart-city") {
        latencyModel(LatencyModel.combined())
        offloadingPolicy(OffloadingPolicy.latencyAware)
        edgeNode("sensor-1") {
          location(37.7749, -122.4194)
          tier(EdgeTier.Device)
          resources(pes = PEs(1), mips = MIPS(100.0), ram = MegaBytes(256.0))
        }
        edgeNode("gateway-1") {
          location(37.7849, -122.4094)
          tier(EdgeTier.EdgeMicro)
          resources(pes = PEs(4), mips = MIPS(2000.0), ram = MegaBytes(4096.0))
        }
      }

      faasPlatform("lambda") {
        function("process-data") {
          runtime(io.aura.serverless.Runtime.Python)
          memory(MegaBytes(256.0))
          timeout(SimTime(30.0))
        }
      }

      datacenter("dc-1") {
        hosts(count = 4, pes = PEs(8), mips = MIPS(10000.0), ram = MegaBytes(16384.0))
      }
      k8sCluster("k8s-prod") {
        deployment("worker") {
          replicas(2)
          container("app") {
            cpuRequest(MIPS(500.0))
            memoryRequest(MegaBytes(256.0))
          }
          workloadPerPod(MI(1000.0), PEs(1))
        }
      }

      federatedWorkflow("iot-pipeline") {
        tierSelection(TierSelectionPolicy.edgeFirst)
        escalation(EscalationPolicy.cascade)
        edgeTier(environment = "smart-city", sourceNode = "sensor-1")
        serverlessTier(platform = "lambda", functionName = "process-data")
        k8sTier(cluster = "k8s-prod")
        summon[FederatedWorkflowBuilder].tasks(
          count = 30,
          cpuRequired = MIPS(100.0),
          memRequired = MegaBytes(64.0),
          taskLength = MI(500.0),
          deadline = SimTime(1.0)
        )
        summon[FederatedWorkflowBuilder].interArrivalTime(SimTime(2.0))
      }
    }

    config.federatedWorkflows should have size 1
    val fw = config.federatedWorkflows.head
    fw.name shouldBe "iot-pipeline"
    fw.count shouldBe 30
    fw.edgeTier shouldBe Some(("smart-city", "sensor-1"))
    fw.serverlessTier shouldBe Some(("lambda", "process-data"))
    fw.k8sTier shouldBe Some("k8s-prod")
    fw.cpuRequired.value shouldBe 100.0
    fw.memRequired.value shouldBe 64.0
    fw.taskLength.value shouldBe 500.0
    fw.deadline.value shouldBe 1.0
    fw.interArrivalTime.value shouldBe 2.0
  }

  it should "include federated workflows in brokerCount" in {
    val config = simulation("fed-count-test", endTime = SimTime(100.0)) {
      edgeEnvironment("env-1") {
        edgeNode("node-1") {
          location(0.0, 0.0)
          tier(EdgeTier.EdgeMicro)
          resources(pes = PEs(4), mips = MIPS(2000.0), ram = MegaBytes(4096.0))
        }
        taskSource("src") {
          sourceNode("node-1")
          tasks(
            count = 5,
            cpuRequired = MIPS(50.0),
            memRequired = MegaBytes(64.0),
            taskLength = MI(500.0),
            deadline = SimTime(1.0)
          )
          interArrivalTime(SimTime(1.0))
        }
      }

      federatedWorkflow("workflow-1") {
        edgeTier(environment = "env-1", sourceNode = "node-1")
        summon[FederatedWorkflowBuilder].tasks(
          count = 10,
          cpuRequired = MIPS(100.0),
          memRequired = MegaBytes(64.0),
          taskLength = MI(500.0),
          deadline = SimTime(1.0)
        )
        summon[FederatedWorkflowBuilder].interArrivalTime(SimTime(1.0))
      }
    }

    // 1 edge environment + 1 federated workflow = 2
    config.brokerCount shouldBe 2
  }

  it should "assign globally unique federated task IDs" in {
    val config = simulation("fed-ids", endTime = SimTime(100.0)) {
      edgeEnvironment("env-1") {
        edgeNode("node-1") {
          location(0.0, 0.0)
          tier(EdgeTier.EdgeMicro)
          resources(pes = PEs(4), mips = MIPS(2000.0), ram = MegaBytes(4096.0))
        }
      }

      federatedWorkflow("wf-1") {
        edgeTier(environment = "env-1", sourceNode = "node-1")
        summon[FederatedWorkflowBuilder].tasks(
          count = 10,
          cpuRequired = MIPS(100.0),
          memRequired = MegaBytes(64.0),
          taskLength = MI(500.0),
          deadline = SimTime(1.0)
        )
      }

      federatedWorkflow("wf-2") {
        edgeTier(environment = "env-1", sourceNode = "node-1")
        summon[FederatedWorkflowBuilder].tasks(
          count = 5,
          cpuRequired = MIPS(100.0),
          memRequired = MegaBytes(64.0),
          taskLength = MI(500.0),
          deadline = SimTime(1.0)
        )
      }
    }

    config.federatedWorkflows should have size 2
    config.nextFederatedTaskId shouldBe 15
  }

  it should "support mixed paradigms + federated workflow" in {
    val config = simulation("full-stack-fed", endTime = SimTime(1000.0)) {
      datacenter("dc-1") {
        hosts(count = 4, pes = PEs(8), mips = MIPS(10000.0), ram = MegaBytes(16384.0))
      }

      broker("iaas-broker") {
        vm()
        workload()
      }

      faasPlatform("lambda") {
        function("api")(runtime(io.aura.serverless.Runtime.Python))
      }
      serverlessBroker("faas-client") {
        invocations("api", count = 10)
      }

      k8sCluster("production") {
        deployment("web") {
          replicas(2)
          container("nginx") {
            cpuRequest(MIPS(500.0))
            memoryRequest(MegaBytes(256.0))
          }
        }
      }

      edgeEnvironment("iot-edge") {
        edgeNode("sensor") {
          location(37.7749, -122.4194)
          tier(EdgeTier.Device)
          resources(pes = PEs(1), mips = MIPS(100.0), ram = MegaBytes(256.0))
        }
        taskSource("data") {
          sourceNode("sensor")
          tasks(
            count = 5,
            cpuRequired = MIPS(50.0),
            memRequired = MegaBytes(32.0),
            taskLength = MI(100.0),
            deadline = SimTime(0.5)
          )
          interArrivalTime(SimTime(3.0))
        }
      }

      federatedWorkflow("cross-tier") {
        tierSelection(TierSelectionPolicy.costFirst)
        escalation(EscalationPolicy.cascade)
        edgeTier(environment = "iot-edge", sourceNode = "sensor")
        serverlessTier(platform = "lambda", functionName = "api")
        k8sTier(cluster = "production")
        summon[FederatedWorkflowBuilder].tasks(
          count = 20,
          cpuRequired = MIPS(100.0),
          memRequired = MegaBytes(64.0),
          taskLength = MI(500.0),
          deadline = SimTime(1.0)
        )
        summon[FederatedWorkflowBuilder].interArrivalTime(SimTime(2.0))
      }
    }

    config.datacenters should have size 1
    config.brokers should have size 1
    config.faasPlatforms should have size 1
    config.serverlessBrokers should have size 1
    config.k8sClusters should have size 1
    config.edgeEnvironments should have size 1
    config.federatedWorkflows should have size 1
    // 1 IaaS + 1 serverless + 1 K8s + 1 edge + 1 federated = 5
    config.brokerCount shouldBe 5
  }
