// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.examples

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.iaas.policies.{VerticalScalingPolicy, VmAllocationPolicy, WorkloadScheduler}
import io.aura.power.PowerModel

/** Vertical scaling example showing dynamic VM resource resizing.
  *
  * Single host with a VM that processes workloads. Demonstrates the gradual scaling policy configuration in the DSL.
  */
object VerticalScalingExample:
  def main(args: Array[String]): Unit =

    println("=" * 70)
    println("Aura Cloud Simulator - Vertical Scaling Example")
    println("=" * 70)

    val config = simulation("vertical-scaling", endTime = SimTime(3600.0)) {
      datacenter("dc-1") {
        allocationPolicy(VmAllocationPolicy.bestFit)
        scheduler(WorkloadScheduler.timeShared)
        verticalScaling(VerticalScalingPolicy.gradual(0.8, 0.2, 1.5))

        host(
          pes = PEs(16),
          mips = MIPS(40000.0),
          ram = MegaBytes(65536.0),
          bw = Mbps(20000.0),
          storage = MegaBytes(2000000.0),
          powerModel = Some(PowerModel.hpProLiantG5)
        )
      }

      broker("client") {
        vm(
          pes = PEs(4),
          mips = MIPS(10000.0),
          ram = MegaBytes(8192.0),
          bw = Mbps(5000.0),
          storage = MegaBytes(200000.0)
        )

        // Burst of workloads that will trigger high utilization
        workloads(count = 8, length = MI(100000.0), pes = PEs(1))
      }
    }

    println("\nStarting simulation with gradual vertical scaling (0.8/0.2 thresholds, 1.5x factor)...")

    config.run() match
      case Right(results) =>
        println("\n" + results.formatSummary)
        println()
        println("Workload Results:")
        println(results.formatWorkloadTable)
        println()
        println("Energy Report:")
        println(results.formatEnergyReport)
      case Left(error) =>
        println(s"\nSimulation failed: $error")
