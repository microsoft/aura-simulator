// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.examples

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.iaas.policies.{MigrationModel, VmAllocationPolicy, WorkloadScheduler}
import io.aura.iaas.policies.{
  OverloadDetector as ConsOverload,
  UnderloadDetector as ConsUnderload,
  VmSelectionPolicy as ConsVmSelect
}
import io.aura.power.PowerModel

/** VM migration example with overload detection.
  *
  * Creates an intentionally overloaded host by placing many VMs, then uses static threshold overload detection +
  * minimum migration time VM selection to trigger migrations.
  */
object VmMigrationExample:
  def main(args: Array[String]): Unit =

    println("=" * 70)
    println("Aura Cloud Simulator - VM Migration Example")
    println("=" * 70)

    val config = simulation("vm-migration", endTime = SimTime(3600.0)) {
      datacenter("dc-1") {
        allocationPolicy(VmAllocationPolicy.firstFit)
        scheduler(WorkloadScheduler.timeShared)
        consolidation(
          overloadDetector = ConsOverload.staticThreshold(0.8),
          underloadDetector = ConsUnderload.staticThreshold(0.2),
          vmSelector = ConsVmSelect.minimumMigrationTime,
          migrationModel = MigrationModel.preCopy,
          bandwidth = Mbps(10000.0)
        )

        // 3 hosts: first one will get overloaded
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
        // Many VMs to overload first host
        vms(
          count = 6,
          pes = PEs(2),
          mips = MIPS(8000.0),
          ram = MegaBytes(4096.0),
          bw = Mbps(2000.0),
          storage = MegaBytes(50000.0)
        )

        workloads(count = 12, length = MI(100000.0), pes = PEs(1))
      }
    }

    println("\nStarting simulation...")
    config.run() match
      case Right(results) =>
        println("\n" + results.formatSummary)
        println()
        println("Migration Log:")
        println(results.formatMigrationLog)
        println()
        println("Energy Report:")
        println(results.formatEnergyReport)
      case Left(error) =>
        println(s"\nSimulation failed: $error")
