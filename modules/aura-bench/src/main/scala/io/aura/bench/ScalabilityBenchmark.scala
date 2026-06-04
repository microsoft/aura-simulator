// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.bench

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.iaas.policies.{VmAllocationPolicy, WorkloadScheduler}
import io.aura.power.PowerModel

/** Standalone scalability benchmark for IEEE CLOUD 2026 paper.
  *
  * Fixed infrastructure: 1 DC, 100 hosts, 500 VMs. Varies workload count: 100, 500, 1000, 2000, 5000, 10000, 20000,
  * 50000. Reports execution time (ms) and peak memory (MB) for each point.
  *
  * Run with: sbt "auraBench/runMain io.aura.bench.ScalabilityBenchmark"
  */
object ScalabilityBenchmark:

  private val workloadCounts = Seq(100, 500, 1000, 2000, 5000, 10000, 20000, 50000)

  private def runScenario(numWorkloads: Int): (Long, Long) =
    // Force GC
    System.gc()
    Thread.sleep(300)
    System.gc()

    val rt         = Runtime.getRuntime
    val beforeUsed = rt.totalMemory() - rt.freeMemory()
    val start      = System.nanoTime()

    val config = simulation(s"scale-$numWorkloads", endTime = SimTime(50000.0)) {
      datacenter("dc-1") {
        allocationPolicy(VmAllocationPolicy.bestFit)
        scheduler(WorkloadScheduler.timeShared)
        hosts(
          count = 100,
          pes = PEs(16),
          mips = MIPS(20000.0),
          ram = MegaBytes(65536.0),
          bw = Mbps(10000.0),
          storage = MegaBytes(1000000.0),
          powerModel = Some(PowerModel.linear(Watts(400.0), Watts(150.0)))
        )
      }
      broker("broker-1") {
        vms(
          count = 500,
          pes = PEs(2),
          mips = MIPS(2000.0),
          ram = MegaBytes(4096.0),
          bw = Mbps(1000.0),
          storage = MegaBytes(10000.0)
        )
        workloads(count = numWorkloads, length = MI(20000.0), pes = PEs(1))
      }
    }
    config.run()

    val elapsed = (System.nanoTime() - start) / 1_000_000
    System.gc()
    Thread.sleep(200)
    val afterUsed = rt.totalMemory() - rt.freeMemory()
    val peakMB    = Math.max(afterUsed, beforeUsed) / (1024 * 1024)
    (elapsed, peakMB)

  def main(args: Array[String]): Unit =
    println("=" * 70)
    println("Aura Scalability Benchmark")
    println("Fixed: 1 DC, 100 hosts, 500 VMs | Varying workloads")
    println("=" * 70)

    // Warmup run
    print("Warming up JIT... ")
    val warmup = simulation("warmup", endTime = SimTime(100.0)) {
      datacenter("dc") {
        hosts(count = 5, pes = PEs(8), mips = MIPS(20000.0), ram = MegaBytes(32768.0))
      }
      broker("b") {
        vms(count = 10, pes = PEs(2), mips = MIPS(2000.0), ram = MegaBytes(4096.0))
        workloads(count = 50, length = MI(5000.0))
      }
    }
    warmup.run()
    // Run warmup a few more times for JIT
    for _ <- 1 to 3 do warmup.run()
    println("done.")
    println()

    // 3 iterations per point, take median
    val iterations = 3
    println(f"${"Workloads"}%10s | ${"Time (ms)"}%12s | ${"Memory (MB)"}%12s")
    println("-" * 42)

    val results = for count <- workloadCounts yield
      val measurements = for _ <- 1 to iterations yield runScenario(count)
      val times        = measurements.map(_._1).sorted
      val mems         = measurements.map(_._2).sorted
      val medianTime   = times(times.size / 2)
      val medianMem    = mems(mems.size / 2)
      println(f"$count%10d | $medianTime%,12d | $medianMem%,12d")
      (count, medianTime, medianMem)

    println()
    println("CSV output:")
    println("Workloads,Time_ms,Peak_MB")
    results.foreach { case (w, t, m) =>
      println(s"$w,$t,$m")
    }
