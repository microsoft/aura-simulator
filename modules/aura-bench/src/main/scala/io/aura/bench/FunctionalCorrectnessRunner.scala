// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.bench

import io.aura.core.types.*
import io.aura.core.engine.SimulationResults
import io.aura.dsl.{DslSimulationConfig, SimulationDsl}
import io.aura.dsl.SimulationDsl.*
import io.aura.iaas.policies.{VmAllocationPolicy, WorkloadScheduler}
import io.aura.power.PowerModel

/** Functional correctness validation runner.
  *
  * Runs a controlled validation scenario and prints per-workload CSV results plus aggregate metrics.
  *
  * Config: 100 hosts (16 PEs, 20K MIPS total), 1,000 VMs (1 PE, 2K MIPS), 1,000 workloads (1 per VM, 20K MI). Expected:
  * all complete at 10s.
  *
  * Uses 1-PE VMs with exactly 1 workload per VM, eliminating any MIPS-per-PE vs total-MIPS interpretation difference
  * and scheduler scheduler dependency between frameworks.
  *
  * Run with: sbt "auraBench/runMain io.aura.bench.FunctionalCorrectnessRunner"
  */
object FunctionalCorrectnessRunner:

  private def runSim(config: DslSimulationConfig): SimulationResults =
    config.run() match
      case Right(r)  => r
      case Left(err) => throw RuntimeException(s"Simulation failed: $err")

  def main(args: Array[String]): Unit =
    println()
    println("Aura — Functional Correctness Validation")
    println("============================================")
    println("Config: 100 hosts, 1000 VMs (1-PE, 2000 MIPS), 1000 workloads (1 per VM)")
    println()

    // Validation config — 1-PE VMs for scheduler-independent results
    // 1-PE VMs with 1 workload each: scheduler-independent, no MIPS ambiguity
    // Host MIPS = 20000 total. 10 VMs/host × 2000 MIPS = 20000 → fits exactly.
    val config = simulation("validation", endTime = SimTime(5000.0)) {
      datacenter("dc-1") {
        allocationPolicy(VmAllocationPolicy.bestFit)
        scheduler(WorkloadScheduler.timeShared)
        hosts(
          count = 100,
          pes = PEs(16),
          mips = MIPS(20000.0),
          ram = MegaBytes(655360.0),
          bw = Mbps(100000.0),
          storage = MegaBytes(10000000.0),
          powerModel = Some(PowerModel.linear(Watts(400.0), Watts(150.0)))
        )
      }
      broker("broker-1") {
        vms(
          count = 1000,
          pes = PEs(1),
          mips = MIPS(2000.0),
          ram = MegaBytes(4096.0),
          bw = Mbps(1000.0),
          storage = MegaBytes(10000.0)
        )
        // 1 workload per VM — requiredMips = 2000 claims the full VM
        for _ <- 0 until 1000 do workload(length = MI(20000.0), pes = PEs(1), requiredMips = MIPS(2000.0))
      }
    }

    val startNs   = System.nanoTime()
    val results   = runSim(config)
    val elapsedMs = (System.nanoTime() - startNs) / 1_000_000

    // Per-workload CSV output
    println("workload_id,vm_id,host_id,finish_time,mi_executed,status")
    val sorted = results.workloadResults.sortBy(_.workloadId.value)
    for r <- sorted do
      println(
        f"${r.workloadId.value}%d,${r.vmId.value}%d,${r.hostId.value}%d,${r.finishTime.value}%.2f,${r.executedMI.value}%.0f,SUCCESS"
      )

    // Aggregate metrics
    val completed   = results.workloadResults.size
    val failed      = results.failedWorkloads.size
    val finishTimes = results.workloadResults.map(_.finishTime.value)
    val meanFinish  = if completed > 0 then finishTimes.sum / completed else 0.0
    val minFinish   = if completed > 0 then finishTimes.min else 0.0
    val maxFinish   = if completed > 0 then finishTimes.max else 0.0

    println()
    println("=== Aggregate Metrics ===")
    println(f"Total completed: $completed%d")
    println(f"Total failed:    $failed%d")
    println(f"Mean finish time: $meanFinish%.4f s")
    println(f"Min finish time:  $minFinish%.4f s")
    println(f"Max finish time:  $maxFinish%.4f s")
    println(f"Execution time:   $elapsedMs%d ms")
