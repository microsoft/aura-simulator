// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.examples

import io.aura.core.types.*
import io.aura.core.engine.SimulationRunner
import io.aura.dsl.SimulationDsl.*
import io.aura.iaas.policies.{VmAllocationPolicy, WorkloadScheduler}
import io.aura.power.PowerModel

/** Basic IaaS simulation demonstrating Aura end-to-end.
  *
  * Demonstrates a minimal IaaS scenario:
  *   - 1 Datacenter with 1 Host
  *   - 1 Broker creating 1 VM
  *   - 4 Workloads running on the VM
  *
  * Demonstrates:
  *   - Context-function DSL configuration
  *   - Type-safe units (MIPS, PEs, MegaBytes, etc.)
  *   - Actor-based parallel DES execution
  *   - Immutable simulation results
  */
object BasicIaaSExample:
  def main(args: Array[String]): Unit =

    println("=" * 70)
    println("Aura Cloud Simulator - Basic IaaS Example")
    println("=" * 70)

    val config = simulation("basic-iaas", endTime = SimTime(1000.0)) {
      datacenter("datacenter-1") {
        allocationPolicy(VmAllocationPolicy.bestFit)
        scheduler(WorkloadScheduler.timeShared)

        host(
          pes = PEs(4),
          mips = MIPS(10000.0),
          ram = MegaBytes(16384.0),
          bw = Mbps(10000.0),
          storage = MegaBytes(1000000.0),
          powerModel = Some(PowerModel.linear(Watts(120.0), Watts(70.0)))
        )
      }

      broker("broker-1") {
        vm(
          pes = PEs(2),
          mips = MIPS(10000.0),
          ram = MegaBytes(4096.0),
          bw = Mbps(1000.0),
          storage = MegaBytes(100000.0)
        )

        workload(length = MI(10000.0), pes = PEs(1))
        workload(length = MI(20000.0), pes = PEs(1))
        workload(length = MI(30000.0), pes = PEs(1))
        workload(length = MI(40000.0), pes = PEs(1))
      }
    }

    println("\nStarting simulation...")
    SimulationRunner.run(config) match
      case Right(results) =>
        println("\nSimulation completed!")
        println(s"End time: ${results.simulationEndTime.value}")
        println(s"Total events: ${results.totalEventsProcessed}")
        println()
        println("Workload Results:")
        println(results.formatWorkloadTable)
        println()
        println(s"Average completion time: ${results.avgCompletionTime.value}")
      case Left(error) =>
        println(s"\nSimulation failed: $error")

/** Multi-datacenter example with multiple brokers. */
object MultiDcExample:
  def main(args: Array[String]): Unit =

    println("=" * 70)
    println("Aura Cloud Simulator - Multi-Datacenter Example")
    println("=" * 70)

    val config = simulation("multi-dc", endTime = SimTime(2000.0)) {
      datacenter("us-east") {
        allocationPolicy(VmAllocationPolicy.worstFit)
        scheduler(WorkloadScheduler.spaceShared)

        hosts(
          count = 4,
          pes = PEs(8),
          mips = MIPS(20000.0),
          ram = MegaBytes(32768.0),
          bw = Mbps(10000.0),
          storage = MegaBytes(1000000.0),
          powerModel = Some(PowerModel.hpProLiantG5)
        )
      }

      datacenter("eu-west") {
        allocationPolicy(VmAllocationPolicy.bestFit)
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

      broker("enterprise-broker") {
        vms(
          count = 4,
          pes = PEs(4),
          mips = MIPS(10000.0),
          ram = MegaBytes(8192.0),
          bw = Mbps(5000.0),
          storage = MegaBytes(200000.0)
        )

        workloads(count = 8, length = MI(50000.0), pes = PEs(2))
      }
    }

    println("\nStarting simulation...")
    SimulationRunner.run(config) match
      case Right(results) =>
        println("\nSimulation completed!")
        println(s"End time: ${results.simulationEndTime.value}")
        println(s"Total events: ${results.totalEventsProcessed}")
        println(s"VMs placed: ${results.vmPlacements.size}")
        println()
        println("Workload Results:")
        println(results.formatWorkloadTable)
        println()
        println("CSV Export:")
        println(results.toCSV)
      case Left(error) =>
        println(s"\nSimulation failed: $error")
