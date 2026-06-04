// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.examples

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.iaas.policies.{VmAllocationPolicy, WorkloadScheduler}
import io.aura.power.{CarbonIntensity, EfficiencyMetrics, PowerModel, PueModel}

/** Data center efficiency example comparing regions and PUE models.
  *
  * Runs the same workload and computes:
  *   - PUE (Power Usage Effectiveness)
  *   - Carbon emissions by region
  *   - Cooling overhead
  */
object DatacenterEfficiencyExample:
  def main(args: Array[String]): Unit =

    println("=" * 70)
    println("Aura Cloud Simulator - Data Center Efficiency Example")
    println("=" * 70)

    val config = simulation("dc-efficiency", endTime = SimTime(3600.0)) {
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
          count = 8,
          pes = PEs(4),
          mips = MIPS(10000.0),
          ram = MegaBytes(8192.0),
          bw = Mbps(5000.0),
          storage = MegaBytes(200000.0)
        )

        workloads(count = 40, length = MI(80000.0), pes = PEs(2))
      }
    }

    println("\nStarting simulation...")
    config.run() match
      case Right(results) =>
        println("\n" + results.formatSummary)
        println()
        println("Energy Report:")
        println(results.formatEnergyReport)

        val itEnergy = results.totalEnergyWh

        // Compare different PUE models
        println("\n" + "=" * 50)
        println("Efficiency Comparison by Data Center Type")
        println("=" * 50)

        val scenarios = Vector(
          ("Hyperscale (PUE 1.1)", PueModel.hyperscale, CarbonIntensity.usWest),
          ("Colocation (PUE 1.4)", PueModel.colocation, CarbonIntensity.usEast),
          ("Enterprise (PUE 1.6)", PueModel.enterprise, CarbonIntensity.euWest),
          ("Legacy (PUE 2.0)", PueModel.legacy, CarbonIntensity.india)
        )

        for (label, pue, carbon) <- scenarios do
          val metrics = EfficiencyMetrics.compute(itEnergy, pue, carbon)
          println(f"\n--- $label ---")
          println(EfficiencyMetrics.formatReport(metrics))

        // Compare regions with same PUE
        println("\n" + "=" * 50)
        println("Carbon Comparison by Region (Hyperscale PUE)")
        println("=" * 50)

        val regions = Vector(
          ("US West (CA)", CarbonIntensity.usWest),
          ("US East (VA)", CarbonIntensity.usEast),
          ("EU North (SE)", CarbonIntensity.euNorth),
          ("EU West (IE)", CarbonIntensity.euWest),
          ("Asia Pacific", CarbonIntensity.asiaPacific),
          ("India", CarbonIntensity.india),
          ("Brazil", CarbonIntensity.brazil)
        )

        println(f"\n${"Region"}%-20s ${"gCO2/kWh"}%-12s ${"CO2 (kg)"}%-12s")
        println("-" * 44)
        for (name, ci) <- regions do
          val metrics = EfficiencyMetrics.compute(itEnergy, PueModel.hyperscale, ci)
          println(f"$name%-20s ${ci.gCO2PerKWh}%-12.0f ${metrics.carbonEmissionsKg}%-12.6f")

      case Left(error) =>
        println(s"\nSimulation failed: $error")
