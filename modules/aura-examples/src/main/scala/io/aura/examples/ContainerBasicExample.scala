// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.examples

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.containers.*

/** Basic container orchestration example with leastRequested scheduling.
  *
  * Demonstrates K8s-style pod scheduling across 4 hosts with two deployments (web-frontend: 3 replicas, api-backend: 2
  * replicas). Uses leastRequested scheduling to distribute pods evenly.
  */
object ContainerBasicExample:
  def main(args: Array[String]): Unit =

    println("=" * 70)
    println("Aura Cloud Simulator - Container Orchestration (Least Requested)")
    println("=" * 70)

    val config = simulation("container-basic", endTime = SimTime(600.0)) {
      datacenter("dc-1") {
        hosts(count = 4, pes = PEs(8), mips = MIPS(10000.0), ram = MegaBytes(16384.0))
      }

      k8sCluster("production") {
        schedulingPolicy(K8sScheduler.leastRequested)

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

    println(s"\nCluster: production")
    println(s"Deployments: ${config.k8sClusters.head.deployments.map(_.name).mkString(", ")}")
    println(s"Total pods: ${config.k8sClusters.head.deployments.map(_.replicas).sum}")
    println(s"Hosts: ${config.datacenters.head.hosts.size}")
    println(s"Scheduling: leastRequested")
    println(s"Broker count: ${config.brokerCount}")

    println("\nStarting simulation...")
    config.run() match
      case Right(results) =>
        println("\nSimulation completed!")
        println(s"End time: ${results.simulationEndTime.value}")
        println(s"Total events: ${results.totalEventsProcessed}")
        println()
        println("=== Pod Summary ===")
        println(results.formatPodSummary)
        println()
        println("=== Pod Table ===")
        println(results.formatPodTable)
        println()
        println("=== Scheduling Report ===")
        println(results.formatSchedulingReport)
      case Left(error) =>
        println(s"\nSimulation failed: $error")
