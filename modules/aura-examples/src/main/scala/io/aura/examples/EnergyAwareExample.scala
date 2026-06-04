// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.examples

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.iaas.policies.{VmAllocationPolicy, WorkloadScheduler}
import io.aura.power.PowerModel

/** Energy-aware simulation demonstrating power tracking with SPECpower models.
  *
  *   - 5 hosts with HP ProLiant G5 power model
  *   - 10 VMs
  *   - 50 workloads
  *   - Shows formatSummary, formatEnergyReport
  */
object EnergyAwareExample:
  def main(args: Array[String]): Unit =

    println("=" * 70)
    println("Aura Cloud Simulator - Energy-Aware Example")
    println("=" * 70)

    val config = simulation("energy-aware", endTime = SimTime(3600.0)) {
      datacenter("us-east") {
        allocationPolicy(VmAllocationPolicy.bestFit)
        scheduler(WorkloadScheduler.timeShared)

        hosts(
          count = 5,
          pes = PEs(8),
          mips = MIPS(20000.0),
          ram = MegaBytes(32768.0),
          bw = Mbps(10000.0),
          storage = MegaBytes(1000000.0),
          powerModel = Some(PowerModel.hpProLiantG5)
        )
      }

      broker("client") {
        vms(
          count = 10,
          pes = PEs(4),
          mips = MIPS(10000.0),
          ram = MegaBytes(8192.0),
          bw = Mbps(5000.0),
          storage = MegaBytes(200000.0)
        )

        workloads(count = 50, length = MI(50000.0), pes = PEs(2))
      }
    }

    println("\nStarting simulation...")
    config.run() match
      case Right(results) =>
        println("\n" + results.formatSummary)
        println()
        println("Energy Report:")
        println(results.formatEnergyReport)
        println()
        println("Workload Results:")
        println(results.formatWorkloadTable)
      case Left(error) =>
        println(s"\nSimulation failed: $error")
