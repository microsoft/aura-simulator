// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.examples

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.iaas.policies.{DagScheduler, VmAllocationPolicy, WorkloadScheduler}
import io.aura.power.PowerModel

/** DAG-based workflow scheduling example.
  *
  * Models a data processing pipeline: extract -> transform -> load -> validate /
  *
  * Tasks have dependencies and execute in topological order.
  */
object DagSchedulingExample:
  def main(args: Array[String]): Unit =

    println("=" * 70)
    println("Aura Cloud Simulator - DAG Scheduling Example")
    println("=" * 70)

    val config = simulation("dag-scheduling", endTime = SimTime(3600.0)) {
      datacenter("dc-1") {
        allocationPolicy(VmAllocationPolicy.bestFit)
        scheduler(WorkloadScheduler.timeShared)

        hosts(
          count = 2,
          pes = PEs(8),
          mips = MIPS(20000.0),
          ram = MegaBytes(32768.0),
          bw = Mbps(10000.0),
          storage = MegaBytes(1000000.0),
          powerModel = Some(PowerModel.hpProLiantG5)
        )
      }

      broker("pipeline") {
        vms(
          count = 2,
          pes = PEs(4),
          mips = MIPS(10000.0),
          ram = MegaBytes(8192.0),
          bw = Mbps(5000.0),
          storage = MegaBytes(200000.0)
        )

        // Stage 1: Extract (no deps)
        val extract = workload(length = MI(50000.0), pes = PEs(2))
        // Stage 2a: Transform (depends on extract)
        val transform = workload(length = MI(100000.0), pes = PEs(2), dependsOn = Set(extract))
        // Stage 2b: Validate (depends on extract)
        val validate = workload(length = MI(30000.0), pes = PEs(1), dependsOn = Set(extract))
        // Stage 3: Load (depends on transform + validate)
        val _ = workload(length = MI(40000.0), pes = PEs(2), dependsOn = Set(transform, validate))
      }
    }

    // Validate the DAG before running
    println("\nDAG Validation...")
    val wlSpecs = config.brokers.head.workloads
    DagScheduler.validate(wlSpecs) match
      case DagScheduler.DagValid =>
        println("  DAG is valid")
        val critPath = DagScheduler.criticalPathLength(wlSpecs)
        println(f"  Critical path length: ${critPath.value}%.0f MI")
        println(s"  Root tasks: ${DagScheduler.rootTasks(wlSpecs).map(_.id.value).mkString(", ")}")
        println(s"  Leaf tasks: ${DagScheduler.leafTasks(wlSpecs).map(_.id.value).mkString(", ")}")
      case other =>
        println(s"  DAG validation failed: $other")

    println("\nStarting simulation...")
    config.run() match
      case Right(results) =>
        println("\n" + results.formatSummary)
        println()
        println("Workload Results:")
        println(results.formatWorkloadTable)
      case Left(error) =>
        println(s"\nSimulation failed: $error")
