// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.examples

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.iaas.policies.{VmAllocationPolicy, WorkloadScheduler}
import io.aura.power.PowerModel

/** CFS Scheduler example demonstrating weight-proportional MIPS sharing.
  *
  * Runs the same workloads with different priority weights (512, 1024, 2048). Higher weight = more CPU time = faster
  * completion.
  */
object CfsSchedulerExample:
  def main(args: Array[String]): Unit =

    println("=" * 70)
    println("Aura Cloud Simulator - CFS Scheduler Example")
    println("=" * 70)

    val config = simulation("cfs-scheduler", endTime = SimTime(3600.0)) {
      datacenter("dc-1") {
        allocationPolicy(VmAllocationPolicy.bestFit)
        scheduler(WorkloadScheduler.cfs)

        host(
          pes = PEs(8),
          mips = MIPS(20000.0),
          ram = MegaBytes(32768.0),
          bw = Mbps(10000.0),
          storage = MegaBytes(1000000.0),
          powerModel = Some(PowerModel.hpProLiantG5)
        )
      }

      broker("client") {
        vm(
          pes = PEs(4),
          mips = MIPS(12000.0),
          ram = MegaBytes(8192.0),
          bw = Mbps(5000.0),
          storage = MegaBytes(200000.0)
        )

        // Three workloads with different weights
        workload(length = MI(60000.0), pes = PEs(1), weight = 512)  // low priority
        workload(length = MI(60000.0), pes = PEs(1), weight = 1024) // normal priority
        workload(length = MI(60000.0), pes = PEs(1), weight = 2048) // high priority
      }
    }

    println("\nStarting simulation...")
    println("Running 3 workloads of equal length with weights 512, 1024, 2048")
    println("Expected: high-weight workload finishes first\n")

    config.run() match
      case Right(results) =>
        println(results.formatSummary)
        println()
        println("Workload Results:")
        println(results.formatWorkloadTable)
        println()

        // Show completion order
        val sorted = results.workloadResults.sortBy(_.finishTime)
        println("Completion Order (fastest to slowest):")
        sorted.foreach { r =>
          println(f"  Workload ${r.workloadId.value}: finished at ${r.finishTime.value}%.2f s")
        }
      case Left(error) =>
        println(s"\nSimulation failed: $error")
