// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.examples

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.edge.*

/** Federated cross-tier orchestration example.
  *
  * Demonstrates a smart city IoT pipeline where 30 tasks are federated across Edge (3 nodes), Serverless (1 function),
  * and K8s (1 cluster). Uses edgeFirst tier selection with cascade escalation — tasks start at the edge and escalate to
  * serverless then K8s on failure.
  */
object FederatedExample:
  def main(args: Array[String]): Unit =

    println("=" * 70)
    println("Aura Cloud Simulator - Federated Cross-Tier Orchestration")
    println("=" * 70)

    val config = simulation("federated-iot", endTime = SimTime(200.0)) {
      // ── Edge infrastructure ──────────────────────────────────────────
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
      }

      // ── Serverless infrastructure ────────────────────────────────────
      faasPlatform("lambda") {
        function("process-data") {
          runtime(io.aura.serverless.Runtime.Python)
          memory(MegaBytes(256.0))
          timeout(SimTime(30.0))
        }
      }

      // ── IaaS datacenter for K8s backing ──────────────────────────────
      datacenter("dc-1") {
        hosts(count = 4, pes = PEs(8), mips = MIPS(10000.0), ram = MegaBytes(16384.0))
      }

      // ── K8s cluster ──────────────────────────────────────────────────
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

      // ── Federated workflow: edgeFirst + cascade ──────────────────────
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

    println(s"\nPolicy: edgeFirst + cascade escalation")
    println(s"Tasks: 30 federated across Edge / Serverless / K8s")
    println(s"Broker count: ${config.brokerCount}")

    println("\nStarting simulation...")
    config.run() match
      case Right(results) =>
        println("\nSimulation completed!")
        println(s"End time: ${results.simulationEndTime.value}")
        println(s"Total events: ${results.totalEventsProcessed}")
        println()
        println("=== Federated Summary ===")
        println(results.formatFederatedSummary)
        println()
        println("=== Federated Task Table ===")
        println(results.formatFederatedTable)
        println()
        println("=== Tier Distribution ===")
        println(results.formatTierDistribution)
      case Left(error) =>
        println(s"\nSimulation failed: $error")
