// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.examples

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.iaas.policies.{VmAllocationPolicy, WorkloadScheduler}
import io.aura.power.PowerModel

/** Multi-datacenter example comparing performance across two DCs.
  *
  *   - us-east: 3 hosts @ 20,000 MIPS (standard tier), VMs @ 10K MIPS
  *   - eu-west: 2 hosts @ 30,000 MIPS (high-performance tier), VMs @ 15K MIPS
  *   - Network link: 50ms latency between DCs
  *   - Two independent brokers, one per DC, running identical workloads
  *
  * Demonstrates:
  *   - Different completion times due to different VM MIPS tiers
  *   - Energy consumption across both DCs
  *   - Network topology configuration (latency applies to cross-DC events)
  */
object MultiDcNetworkExample:
  def main(args: Array[String]): Unit =

    println("=" * 70)
    println("Aura Cloud Simulator - Multi-DC Network Example")
    println("=" * 70)

    // us-east hosts have IDs 0,1,2; eu-west hosts have IDs 3,4 (globally unique)
    val usEastHostCount = 3

    val config = simulation("multi-dc-network", endTime = SimTime(3600.0)) {
      datacenter("us-east") {
        allocationPolicy(VmAllocationPolicy.bestFit)
        scheduler(WorkloadScheduler.timeShared)

        hosts(
          count = usEastHostCount,
          pes = PEs(8),
          mips = MIPS(20000.0),
          ram = MegaBytes(32768.0),
          bw = Mbps(10000.0),
          storage = MegaBytes(1000000.0),
          powerModel = Some(PowerModel.hpProLiantG5)
        )
      }

      datacenter("eu-west") {
        allocationPolicy(VmAllocationPolicy.worstFit)
        scheduler(WorkloadScheduler.timeShared)

        hosts(
          count = 2,
          pes = PEs(16),
          mips = MIPS(30000.0),
          ram = MegaBytes(65536.0),
          bw = Mbps(20000.0),
          storage = MegaBytes(2000000.0),
          powerModel = Some(PowerModel.hpProLiantG4)
        )
      }

      network {
        link("us-east", "eu-west", latency = SimTime(0.050), bandwidth = Mbps(10000.0))
      }

      // Broker in us-east (DC index 0) — standard tier VMs @ 10K MIPS
      broker("us-client") {
        targetDatacenter(0)
        vms(count = 2, pes = PEs(4), mips = MIPS(10000.0), ram = MegaBytes(8192.0))
        workloads(count = 10, length = MI(50000.0), pes = PEs(2))
      }

      // Broker in eu-west (DC index 1) — high-performance VMs @ 15K MIPS
      broker("eu-client") {
        targetDatacenter(1)
        vms(count = 2, pes = PEs(4), mips = MIPS(15000.0), ram = MegaBytes(8192.0))
        workloads(count = 10, length = MI(50000.0), pes = PEs(2))
      }
    }

    println("\nRunning identical workloads in us-east (VMs @ 10K MIPS) vs eu-west (VMs @ 15K MIPS)...")
    println("Network link: 50ms latency (applies to cross-DC events)")

    config.run() match
      case Right(results) =>
        println("\n" + results.formatSummary)
        println()

        // Separate results by host to show per-DC performance
        val usEastWorkloads = results.workloadResults.filter(_.hostId.value < usEastHostCount)
        val euWestWorkloads = results.workloadResults.filter(_.hostId.value >= usEastHostCount)

        val usAvg =
          if usEastWorkloads.nonEmpty then usEastWorkloads.map(_.finishTime.value).sum / usEastWorkloads.size else 0.0
        val euAvg =
          if euWestWorkloads.nonEmpty then euWestWorkloads.map(_.finishTime.value).sum / euWestWorkloads.size else 0.0

        println(f"us-east: ${usEastWorkloads.size} workloads, avg completion ${usAvg}%.2fs (standard tier)")
        println(f"eu-west: ${euWestWorkloads.size} workloads, avg completion ${euAvg}%.2fs (high-performance tier)")
        if usAvg > 0 && euAvg > 0 then println(f"Speedup: ${usAvg / euAvg}%.2fx (eu-west VMs have 50%% more MIPS)")
        println()

        println("Workload Results:")
        println(results.formatWorkloadTable)
        println()
        println("Energy Report:")
        println(results.formatEnergyReport)
      case Left(error) =>
        println(s"\nSimulation failed: $error")
