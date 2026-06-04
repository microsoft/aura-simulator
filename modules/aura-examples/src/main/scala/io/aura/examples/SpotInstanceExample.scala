// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.examples

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.iaas.policies.{CostRates, FaultRecoveryPolicy, SpotInstanceConfig, VmAllocationPolicy, WorkloadScheduler}
import io.aura.power.PowerModel

/** Spot instance example with interruption and on-demand fallback.
  *
  *   - Simulates spot instances with interruption at t=600s
  *   - Uses fault recovery to resubmit preempted workloads
  *   - Falls back to on-demand VMs when spot instances are interrupted
  */
object SpotInstanceExample:
  def main(args: Array[String]): Unit =

    println("=" * 70)
    println("Aura Cloud Simulator - Spot Instance Example")
    println("=" * 70)

    val config = simulation("spot-instances", endTime = SimTime(3600.0)) {
      datacenter("dc-1") {
        allocationPolicy(VmAllocationPolicy.bestFit)
        scheduler(WorkloadScheduler.timeShared)

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

      broker("client") {
        costRates(CostRates.awsSpot)
        spotConfig(
          SpotInstanceConfig(
            interruptionTimes = Vector(SimTime(600.0)),
            noticePeriod = SimTime(120.0),
            fallbackToOnDemand = true,
            onDemandRates = Some(CostRates.awsM5)
          )
        )
        faultRecovery(FaultRecoveryPolicy.resubmit(maxRetries = 2))

        vms(
          count = 4,
          pes = PEs(4),
          mips = MIPS(10000.0),
          ram = MegaBytes(8192.0),
          bw = Mbps(5000.0),
          storage = MegaBytes(200000.0)
        )

        workloads(count = 16, length = MI(100000.0), pes = PEs(2))
      }
    }

    println("\nStarting simulation with spot interruption at t=600s...")
    config.run() match
      case Right(results) =>
        println("\n" + results.formatSummary)
        println()
        println("Cost Report:")
        println(results.formatCostReport)
        println()
        println("Energy Report:")
        println(results.formatEnergyReport)
      case Left(error) =>
        println(s"\nSimulation failed: $error")
