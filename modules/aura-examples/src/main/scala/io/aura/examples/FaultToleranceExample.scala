// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.examples

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.iaas.policies.{FaultInjectionConfig, FaultRecoveryPolicy, VmAllocationPolicy, WorkloadScheduler}
import io.aura.core.types.Distribution
import io.aura.power.PowerModel

/** Fault tolerance example with checkpoint-based recovery.
  *
  *   - 3 hosts with fault injection (exponential inter-arrival)
  *   - 4 VMs with long-running workloads
  *   - Checkpoint-based recovery: workloads resume from last checkpoint after host failure
  */
object FaultToleranceExample:
  def main(args: Array[String]): Unit =

    println("=" * 70)
    println("Aura Cloud Simulator - Fault Tolerance Example")
    println("=" * 70)

    val config = simulation("fault-tolerance", endTime = SimTime(3600.0)) {
      datacenter("dc-1") {
        allocationPolicy(VmAllocationPolicy.bestFit)
        scheduler(WorkloadScheduler.timeShared)
        faultInjection(
          FaultInjectionConfig(
            faultArrivalDistribution = Distribution.exponential(0.5), // ~2 faults per hour
            pesPerFault = Distribution.constant(4.0),
            maxFaultTime = Some(SimTime(1800.0))
          )
        )

        hosts(
          count = 3,
          pes = PEs(8),
          mips = MIPS(20000.0),
          ram = MegaBytes(32768.0),
          bw = Mbps(10000.0),
          storage = MegaBytes(1000000.0),
          powerModel = Some(PowerModel.hpProLiantG5)
        )
      }

      broker("client") {
        faultRecovery(
          FaultRecoveryPolicy.checkpointResubmit(
            checkpointInterval = MI(50000.0),
            maxRetries = 3
          )
        )

        vms(
          count = 4,
          pes = PEs(4),
          mips = MIPS(10000.0),
          ram = MegaBytes(8192.0),
          bw = Mbps(5000.0),
          storage = MegaBytes(200000.0)
        )

        workloads(count = 8, length = MI(200000.0), pes = PEs(2))
      }
    }

    println("\nStarting simulation with fault injection at t=500s...")
    config.run() match
      case Right(results) =>
        println("\n" + results.formatSummary)
        println()
        println("Fault Report:")
        println(results.formatFaultReport)
        println()
        println("Energy Report:")
        println(results.formatEnergyReport)
      case Left(error) =>
        println(s"\nSimulation failed: $error")
