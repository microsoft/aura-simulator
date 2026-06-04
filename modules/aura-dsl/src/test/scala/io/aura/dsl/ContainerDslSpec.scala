// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.dsl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.containers.*

class ContainerDslSpec extends AnyFlatSpec with Matchers:

  "Container DSL" should "create a valid K8s cluster configuration" in {
    val config = simulation("k8s-test", endTime = SimTime(600.0)) {
      datacenter("dc-1") {
        hosts(count = 4, pes = PEs(8), mips = MIPS(10000.0), ram = MegaBytes(16384.0))
      }

      k8sCluster("production") {
        schedulingPolicy(K8sScheduler.leastRequested)
        summon[SimulationDsl.K8sClusterBuilder].targetDatacenter(0)

        deployment("web-frontend") {
          replicas(3)
          container("nginx") {
            image("nginx:1.25")
            cpuRequest(MIPS(500.0))
            cpuLimit(MIPS(1000.0))
            memoryRequest(MegaBytes(256.0))
            memoryLimit(MegaBytes(512.0))
          }
          restartPolicy(RestartPolicy.Never)
        }

        deployment("api-backend") {
          replicas(2)
          container("api") {
            image("api:latest")
            cpuRequest(MIPS(1000.0))
            cpuLimit(MIPS(2000.0))
            memoryRequest(MegaBytes(512.0))
            memoryLimit(MegaBytes(1024.0))
          }
          workloadPerPod(length = MI(50000.0), pes = PEs(1))
        }
      }
    }

    config.k8sClusters should have size 1
    config.k8sClusters.head.name shouldBe "production"
    config.k8sClusters.head.deployments should have size 2
    config.k8sClusters.head.deployments(0).replicas shouldBe 3
    config.k8sClusters.head.deployments(1).replicas shouldBe 2
    config.k8sClusters.head.deployments(0).containers should have size 1
    config.k8sClusters.head.deployments(0).containers.head.image shouldBe "nginx:1.25"
    config.k8sClusters.head.deployments(1).workloadConfig shouldBe defined
  }

  it should "assign globally unique pod IDs" in {
    val config = simulation("multi-cluster", endTime = SimTime(100.0)) {
      datacenter("dc-1") {
        hosts(count = 4, pes = PEs(4), mips = MIPS(10000.0), ram = MegaBytes(16384.0))
      }

      k8sCluster("cluster-1") {
        deployment("app-a") {
          replicas(3)
          container("c") {
            cpuRequest(MIPS(500.0))
            memoryRequest(MegaBytes(256.0))
          }
        }
      }

      k8sCluster("cluster-2") {
        deployment("app-b") {
          replicas(2)
          container("c") {
            cpuRequest(MIPS(500.0))
            memoryRequest(MegaBytes(256.0))
          }
        }
      }
    }

    config.k8sClusters should have size 2
    // Total pods: 3 + 2 = 5
    config.nextPodId shouldBe 5
    // Deployment IDs should be unique
    val allDepIds = config.k8sClusters.flatMap(_.deployments.map(_.deploymentId.value))
    allDepIds.distinct should have size 2
  }

  it should "include K8s clusters in brokerCount" in {
    val config = simulation("mixed", endTime = SimTime(100.0)) {
      datacenter("dc-1") {
        hosts(count = 2, pes = PEs(4), mips = MIPS(10000.0), ram = MegaBytes(16384.0))
      }

      k8sCluster("k8s-1") {
        deployment("app") {
          replicas(1)
          container("c") {
            cpuRequest(MIPS(500.0))
            memoryRequest(MegaBytes(256.0))
          }
        }
      }

      k8sCluster("k8s-2") {
        deployment("app") {
          replicas(1)
          container("c") {
            cpuRequest(MIPS(500.0))
            memoryRequest(MegaBytes(256.0))
          }
        }
      }
    }

    config.brokerCount shouldBe 2 // 0 IaaS + 0 serverless + 2 K8s
  }

  it should "support mixed IaaS + serverless + containers" in {
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
    }

    config.datacenters should have size 1
    config.brokers should have size 1
    config.faasPlatforms should have size 1
    config.serverlessBrokers should have size 1
    config.k8sClusters should have size 1
    config.brokerCount shouldBe 3 // 1 IaaS + 1 serverless + 1 K8s
  }

  it should "use default container values" in {
    val config: DslSimulationConfig = simulation("defaults", endTime = SimTime(100.0)) {
      datacenter("dc-1") {
        hosts(count = 1, pes = PEs(4), mips = MIPS(10000.0), ram = MegaBytes(16384.0))
      }

      k8sCluster("cluster") {
        deployment("app") {
          container("c") {
            image("default:latest")
          }
        }
      }
    }

    val cc = config.k8sClusters.head.deployments.head.containers.head
    cc.image shouldBe "default:latest"
    cc.cpuRequest.value shouldBe 500.0
    cc.cpuLimit.value shouldBe 1000.0
    cc.memoryRequest.value shouldBe 256.0
    cc.memoryLimit.value shouldBe 512.0
  }
