// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.bench

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.iaas.policies.{CostRates, VmAllocationPolicy, WorkloadScheduler}
import io.aura.power.PowerModel

import java.lang.management.{ManagementFactory, MemoryPoolMXBean, MemoryType}
import scala.jdk.CollectionConverters.*

/** Standalone memory measurement runner for paper benchmarks.
  *
  * Measures true peak heap usage via JMX MemoryPoolMXBean (resetPeakUsage before run, then sum getPeakUsage().getUsed()
  * across all heap pools). For process-level peak RSS, wrap invocation with /usr/bin/time -v.
  *
  * Run as a regular main class: sbt "auraBench/runMain io.aura.bench.PaperMemoryBenchmark"
  */
object PaperMemoryBenchmark:

  private val heapPools: Seq[MemoryPoolMXBean] =
    ManagementFactory.getMemoryPoolMXBeans.asScala.toSeq
      .filter(_.getType == MemoryType.HEAP)

  private def resetPeak(): Unit =
    heapPools.foreach(_.resetPeakUsage())

  private def readPeakHeapBytes(): Long =
    heapPools.map(_.getPeakUsage.getUsed).sum

  private def measureMemory(name: String)(block: => Unit): (Long, Long, Long) =
    // Stabilize before measurement
    System.gc()
    Thread.sleep(500)
    System.gc()
    Thread.sleep(200)

    val rt           = Runtime.getRuntime
    val baselineUsed = rt.totalMemory() - rt.freeMemory()

    // Reset JMX peak counters AFTER GC so we capture allocation during the run only
    resetPeak()

    val startTime = System.nanoTime()
    block
    val endTime = System.nanoTime()

    val peakHeapBytes = readPeakHeapBytes()
    val peakHeapMb    = peakHeapBytes / (1024L * 1024L)
    val baselineMb    = baselineUsed / (1024L * 1024L)

    // Steady-state heap after GC (the old measurement, kept for comparison)
    System.gc()
    Thread.sleep(200)
    val steadyUsed = rt.totalMemory() - rt.freeMemory()
    val steadyMb   = steadyUsed / (1024L * 1024L)

    val elapsedMs = (endTime - startTime) / 1_000_000
    println(
      f"$name%-12s | Time: ${elapsedMs}%,8d ms | Baseline: ${baselineMb}%,6d MB | Peak heap (JMX): ${peakHeapMb}%,6d MB | Steady (post-GC): ${steadyMb}%,6d MB"
    )
    (elapsedMs, peakHeapMb, steadyMb)

  def main(args: Array[String]): Unit =
    println("=" * 80)
    println("Aura Paper Memory Benchmark")
    println("=" * 80)
    println(f"${"Scenario"}%-12s | ${"Time"}%12s | ${"Mem Before"}%14s | ${"Mem After"}%13s | ${"Peak Est"}%12s")
    println("-" * 80)

    // Warmup
    val warmup = simulation("warmup", endTime = SimTime(100.0)) {
      datacenter("dc") {
        hosts(count = 2, pes = PEs(8), mips = MIPS(20000.0), ram = MegaBytes(32768.0))
      }
      broker("b") {
        vms(count = 4, pes = PEs(2), mips = MIPS(2000.0), ram = MegaBytes(4096.0))
        workloads(count = 20, length = MI(5000.0))
      }
    }
    warmup.run() // JIT warmup

    // S1: Small
    val results = scala.collection.mutable.ArrayBuffer[(String, Long, Long, Long)]()

    val (t1, m1, s1) = measureMemory("S1-Small") {
      val config = simulation("s1", endTime = SimTime(5000.0)) {
        datacenter("dc-1") {
          allocationPolicy(VmAllocationPolicy.bestFit)
          scheduler(WorkloadScheduler.timeShared)
          hosts(
            count = 10,
            pes = PEs(16),
            mips = MIPS(20000.0),
            ram = MegaBytes(65536.0),
            bw = Mbps(10000.0),
            storage = MegaBytes(1000000.0),
            powerModel = Some(PowerModel.linear(Watts(400.0), Watts(150.0)))
          )
        }
        broker("broker-1") {
          costRates(CostRates.awsM5)
          vms(
            count = 100,
            pes = PEs(2),
            mips = MIPS(2000.0),
            ram = MegaBytes(4096.0),
            bw = Mbps(1000.0),
            storage = MegaBytes(10000.0)
          )
          workloads(count = 1000, length = MI(20000.0), pes = PEs(1))
        }
      }
      config.run()
    }
    results += (("S1", t1, m1, s1))

    // S2: Medium
    val (t2, m2, s2) = measureMemory("S2-Medium") {
      val config = simulation("s2", endTime = SimTime(10000.0)) {
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
          costRates(CostRates.awsM5)
          vms(
            count = 500,
            pes = PEs(2),
            mips = MIPS(2000.0),
            ram = MegaBytes(4096.0),
            bw = Mbps(1000.0),
            storage = MegaBytes(10000.0)
          )
          workloads(count = 5000, length = MI(20000.0), pes = PEs(1))
        }
      }
      config.run()
    }
    results += (("S2", t2, m2, s2))

    // S3: Large
    val (t3, m3, s3) = measureMemory("S3-Large") {
      val config = simulation("s3", endTime = SimTime(50000.0)) {
        datacenter("dc-1") {
          allocationPolicy(VmAllocationPolicy.bestFit)
          scheduler(WorkloadScheduler.timeShared)
          hosts(
            count = 1000,
            pes = PEs(16),
            mips = MIPS(20000.0),
            ram = MegaBytes(65536.0),
            bw = Mbps(10000.0),
            storage = MegaBytes(1000000.0),
            powerModel = Some(PowerModel.linear(Watts(400.0), Watts(150.0)))
          )
        }
        broker("broker-1") {
          costRates(CostRates.awsM5)
          vms(
            count = 5000,
            pes = PEs(2),
            mips = MIPS(2000.0),
            ram = MegaBytes(4096.0),
            bw = Mbps(1000.0),
            storage = MegaBytes(10000.0)
          )
          workloads(count = 50000, length = MI(20000.0), pes = PEs(1))
        }
      }
      config.run()
    }
    results += (("S3", t3, m3, s3))

    // S4: Multi-DC
    val (t4, m4, s4) = measureMemory("S4-MultiDC") {
      val config = simulation("s4", endTime = SimTime(10000.0)) {
        for i <- 1 to 5 do
          datacenter(s"dc-$i") {
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
          costRates(CostRates.awsM5)
          vms(
            count = 2000,
            pes = PEs(2),
            mips = MIPS(2000.0),
            ram = MegaBytes(4096.0),
            bw = Mbps(1000.0),
            storage = MegaBytes(10000.0)
          )
          workloads(count = 10000, length = MI(20000.0), pes = PEs(1))
        }
      }
      config.run()
    }
    results += (("S4", t4, m4, s4))

    println()
    println("=" * 80)
    println("Summary (CSV format):")
    println("Scenario,Time_ms,PeakHeap_MB,SteadyHeap_MB")
    results.foreach { case (name, time, peak, steady) =>
      println(s"$name,$time,$peak,$steady")
    }
