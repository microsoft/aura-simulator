// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.examples

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.edge.*

/** Edge computing example with local-first offloading.
  *
  * Same topology as EdgeBasicExample but uses localFirst policy to contrast offloading behavior — tasks stay local
  * unless overloaded.
  */
object EdgeOffloadingExample:
  def main(args: Array[String]): Unit =

    println("=" * 70)
    println("Aura Cloud Simulator - Edge Computing (Local First)")
    println("=" * 70)

    val config = simulation("edge-offloading", endTime = SimTime(100.0)) {
      edgeEnvironment("smart-city") {
        latencyModel(LatencyModel.combined())
        offloadingPolicy(OffloadingPolicy.localFirst)

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

    println(s"\nEnvironment: smart-city")
    println(s"Nodes: ${config.edgeEnvironments.head.nodeSpecs.map(_.name).mkString(", ")}")
    println(s"Tasks: ${config.edgeEnvironments.head.taskSources.map(_.count).sum}")
    println(s"Policy: localFirst")
    println(s"Broker count: ${config.brokerCount}")

    println("\nStarting simulation...")
    config.run() match
      case Right(results) =>
        println("\nSimulation completed!")
        println(s"End time: ${results.simulationEndTime.value}")
        println(s"Total events: ${results.totalEventsProcessed}")
        println()
        println("=== Edge Summary ===")
        println(results.formatEdgeSummary)
        println()
        println("=== Edge Task Table ===")
        println(results.formatEdgeTable)
        println()
        println("=== Offloading Report ===")
        println(results.formatOffloadingReport)
      case Left(error) =>
        println(s"\nSimulation failed: $error")
