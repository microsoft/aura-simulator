// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.bench

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import scala.compiletime.uninitialized

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.iaas.policies.{CostRates, VmAllocationPolicy, WorkloadScheduler}

/** Full end-to-end simulation throughput benchmark.
  *
  * Measures how fast Aura can run a complete simulation from DSL configuration through actor execution to results.
  *
  * Run with: sbt "auraBench/Jmh/run -f 1 -wi 3 -i 5 .*FullSimulationBenchmark.*"
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 2)
@Measurement(iterations = 5, time = 2)
@Fork(1)
class FullSimulationBenchmark:

  @Param(Array("1", "10", "50"))
  var workloadCount: Int = uninitialized

  /** Benchmark: minimal single-DC simulation. */
  @Benchmark
  def benchMinimalSimulation(): Unit =
    val config = simulation("bench-minimal", endTime = SimTime(500.0)) {
      datacenter("dc") {
        allocationPolicy(VmAllocationPolicy.firstFit)
        scheduler(WorkloadScheduler.timeShared)
        host(pes = PEs(8), mips = MIPS(20000.0), ram = MegaBytes(32768.0))
      }
      broker("b") {
        vm(pes = PEs(2), mips = MIPS(10000.0), ram = MegaBytes(4096.0))
        workloads(count = workloadCount, length = MI(5000.0))
      }
    }
    val _ = config.run()

  /** Benchmark: simulation with cost tracking. */
  @Benchmark
  def benchWithCostTracking(): Unit =
    val config = simulation("bench-cost", endTime = SimTime(500.0)) {
      datacenter("dc") {
        allocationPolicy(VmAllocationPolicy.bestFit)
        scheduler(WorkloadScheduler.timeShared)
        hosts(count = 2, pes = PEs(8), mips = MIPS(20000.0), ram = MegaBytes(32768.0))
      }
      broker("b") {
        costRates(CostRates.awsM5)
        vms(count = 2, pes = PEs(2), mips = MIPS(10000.0), ram = MegaBytes(4096.0))
        workloads(count = workloadCount, length = MI(5000.0))
      }
    }
    val _ = config.run()

/** IaaS baseline performance benchmark.
  *
  * Standard IaaS scenario: 1 DC, 1 host (8 PEs @ 20000 MIPS), 2 VMs (4 PEs @ 10000 MIPS each), 4 workloads (10000 MI, 2
  * PEs each).
  *
  * This is the benchmark configuration for the academic paper comparison.
  *
  * Run with: sbt "auraBench/Jmh/run -f 1 -wi 3 -i 5 .*IaaSBaselineBenchmark.*"
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput, Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 2)
@Measurement(iterations = 10, time = 2)
@Fork(1)
class IaaSBaselineBenchmark:

  /** Basic IaaS scenario benchmark. */
  @Benchmark
  def benchBasicFirstExample(): Unit =
    val config = simulation("iaas-basic", endTime = SimTime(1000.0)) {
      datacenter("dc-0") {
        allocationPolicy(VmAllocationPolicy.firstFit)
        scheduler(WorkloadScheduler.timeShared)
        host(
          pes = PEs(8),
          mips = MIPS(20000.0),
          ram = MegaBytes(16384.0),
          bw = Mbps(10000.0),
          storage = MegaBytes(1000000.0)
        )
      }
      broker("broker-0") {
        vms(count = 2, pes = PEs(4), mips = MIPS(10000.0), ram = MegaBytes(2048.0))
        workloads(count = 4, length = MI(10000.0), pes = PEs(2))
      }
    }
    val _ = config.run()

  /** Scaled scenario: 4 hosts, 8 VMs, 32 workloads. */
  @Benchmark
  def benchScaledScenario(): Unit =
    val config = simulation("iaas-scaled", endTime = SimTime(1000.0)) {
      datacenter("dc-0") {
        allocationPolicy(VmAllocationPolicy.bestFit)
        scheduler(WorkloadScheduler.timeShared)
        hosts(
          count = 4,
          pes = PEs(8),
          mips = MIPS(20000.0),
          ram = MegaBytes(32768.0),
          bw = Mbps(10000.0),
          storage = MegaBytes(1000000.0)
        )
      }
      broker("broker-0") {
        vms(count = 8, pes = PEs(4), mips = MIPS(10000.0), ram = MegaBytes(4096.0))
        workloads(count = 32, length = MI(10000.0), pes = PEs(2))
      }
    }
    val _ = config.run()

  /** Large scenario: 8 DCs, 16 hosts each, 128 VMs, 512 workloads. */
  @Benchmark
  def benchLargeScenario(): Unit =
    val config = simulation("iaas-large", endTime = SimTime(2000.0)) {
      datacenter("dc-0") {
        allocationPolicy(VmAllocationPolicy.firstFit)
        scheduler(WorkloadScheduler.timeShared)
        hosts(
          count = 16,
          pes = PEs(8),
          mips = MIPS(20000.0),
          ram = MegaBytes(65536.0),
          bw = Mbps(10000.0),
          storage = MegaBytes(1000000.0)
        )
      }
      broker("broker-0") {
        vms(count = 32, pes = PEs(4), mips = MIPS(10000.0), ram = MegaBytes(4096.0))
        workloads(count = 128, length = MI(10000.0), pes = PEs(2))
      }
    }
    val _ = config.run()

/** Batch scheduling throughput benchmark.
  *
  * Measures batch job scheduling throughput for HPC-style workloads.
  *
  * Run with: sbt "auraBench/Jmh/run -f 1 -wi 3 -i 5 .*BatchSchedulingBenchmark.*"
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
class BatchSchedulingBenchmark:

  @Param(Array("10", "100", "1000"))
  var jobCount: Int = uninitialized

  var queue: JobQueue = uninitialized

  @Setup
  def setup(): Unit =
    val nodes = (0 until 16).map { i =>
      ComputeNode(i.toLong, MIPS(10000.0), MegaBytes(16384.0))
    }.toVector
    val jobs = (0 until jobCount).map { i =>
      BatchJob(
        id = JobId(i.toLong),
        name = s"job-$i",
        priority = JobPriority.Normal,
        requiredNodes = 1,
        cpuPerNode = MIPS(2000.0),
        ramPerNode = MegaBytes(4096.0),
        estimatedRuntime = SimTime(100.0),
        submitTime = SimTime(i.toDouble)
      )
    }.toVector
    queue = JobQueue(pending = jobs, nodes = nodes)

  @Benchmark
  def benchFcfs(): JobQueue =
    BatchScheduler.fcfs(queue, SimTime.Zero)

  @Benchmark
  def benchSjf(): JobQueue =
    BatchScheduler.sjf(queue, SimTime.Zero)

  @Benchmark
  def benchPriorityBased(): JobQueue =
    BatchScheduler.priorityBased(queue, SimTime.Zero)

  @Benchmark
  def benchBackfill(): JobQueue =
    BatchScheduler.backfill(queue, SimTime.Zero)
