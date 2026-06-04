// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.dsl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.edge.*

class EdgeDslSpec extends AnyFlatSpec with Matchers:

  "Edge DSL" should "create a valid edge environment config" in {
    val config = simulation("edge-test", endTime = SimTime(100.0)) {
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
        edgeNode("cloud-dc") {
          location(37.3861, -122.0839)
          tier(EdgeTier.Cloud)
          resources(pes = PEs(32), mips = MIPS(50000.0), ram = MegaBytes(65536.0))
        }

        taskSource("sensor-tasks") {
          sourceNode("sensor-1")
          tasks(
            count = 20,
            cpuRequired = MIPS(50.0),
            memRequired = MegaBytes(64.0),
            taskLength = MI(500.0),
            deadline = SimTime(0.1)
          )
          interArrivalTime(SimTime(2.0))
        }
      }
    }

    config.edgeEnvironments should have size 1
    config.edgeEnvironments.head.name shouldBe "smart-city"
    config.edgeEnvironments.head.nodeSpecs should have size 3
    config.edgeEnvironments.head.taskSources should have size 1
    config.edgeEnvironments.head.taskSources.head.count shouldBe 20
    config.edgeEnvironments.head.nodeSpecs(0).tier shouldBe EdgeTier.Device
    config.edgeEnvironments.head.nodeSpecs(1).tier shouldBe EdgeTier.EdgeMicro
    config.edgeEnvironments.head.nodeSpecs(2).tier shouldBe EdgeTier.Cloud
  }

  it should "assign globally unique edge task IDs" in {
    val config = simulation("multi-edge", endTime = SimTime(100.0)) {
      edgeEnvironment("env-1") {
        edgeNode("node-1") {
          location(0.0, 0.0)
          tier(EdgeTier.EdgeMicro)
          resources(pes = PEs(4), mips = MIPS(2000.0), ram = MegaBytes(4096.0))
        }
        taskSource("src-1") {
          sourceNode("node-1")
          tasks(
            count = 10,
            cpuRequired = MIPS(50.0),
            memRequired = MegaBytes(64.0),
            taskLength = MI(500.0),
            deadline = SimTime(1.0)
          )
          interArrivalTime(SimTime(1.0))
        }
      }

      edgeEnvironment("env-2") {
        edgeNode("node-2") {
          location(1.0, 1.0)
          tier(EdgeTier.EdgeMicro)
          resources(pes = PEs(4), mips = MIPS(2000.0), ram = MegaBytes(4096.0))
        }
        taskSource("src-2") {
          sourceNode("node-2")
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
    }

    config.edgeEnvironments should have size 2
    // Total tasks: 10 + 5 = 15
    config.nextEdgeTaskId shouldBe 15
  }

  it should "include edge environments in brokerCount" in {
    val config = simulation("edge-only", endTime = SimTime(100.0)) {
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

      edgeEnvironment("env-2") {
        edgeNode("node-2") {
          location(1.0, 1.0)
          tier(EdgeTier.Cloud)
          resources(pes = PEs(32), mips = MIPS(50000.0), ram = MegaBytes(65536.0))
        }
        taskSource("src") {
          sourceNode("node-2")
          tasks(
            count = 3,
            cpuRequired = MIPS(100.0),
            memRequired = MegaBytes(128.0),
            taskLength = MI(1000.0),
            deadline = SimTime(0.5)
          )
          interArrivalTime(SimTime(2.0))
        }
      }
    }

    config.brokerCount shouldBe 2 // 0 IaaS + 0 serverless + 0 K8s + 2 edge
  }

  it should "support mixed IaaS + serverless + containers + edge" in {
    val config = simulation("full-stack", endTime = SimTime(1000.0)) {
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
    }

    config.datacenters should have size 1
    config.brokers should have size 1
    config.faasPlatforms should have size 1
    config.serverlessBrokers should have size 1
    config.k8sClusters should have size 1
    config.edgeEnvironments should have size 1
    config.brokerCount shouldBe 4 // 1 IaaS + 1 serverless + 1 K8s + 1 edge
  }
