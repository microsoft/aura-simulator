// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.bench

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import scala.collection.immutable.TreeMap
import scala.compiletime.uninitialized

import io.aura.core.types.*
import io.aura.core.events.*
import io.aura.iaas.policies.{VmAllocationPolicy, WorkloadScheduler}
import io.aura.iaas.state.{HostState, VmState}

/** JMH benchmarks for Aura simulation components.
  *
  * Run with: sbt "auraBench/Jmh/run"
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(1)
class SimulationBenchmark:

  @Param(Array("100", "1000", "10000"))
  var hostCount: Int = uninitialized

  var hosts: IndexedSeq[HostState] = uninitialized
  var vmSpec: ResourceSpec         = uninitialized

  @Setup
  def setup(): Unit =
    val hostSpec = ResourceSpec(PEs(8), MIPS(20000.0), MegaBytes(32768.0), Mbps(10000.0), MegaBytes(1000000.0))
    hosts = (0 until hostCount).map { i =>
      HostState.create(HostId(i.toLong), DatacenterId(0L), hostSpec)
    }
    vmSpec = ResourceSpec(PEs(2), MIPS(5000.0), MegaBytes(4096.0), Mbps(1000.0), MegaBytes(100000.0))

  @Benchmark
  def benchFirstFit(): Option[HostId] =
    VmAllocationPolicy.firstFit(hosts, vmSpec)

  @Benchmark
  def benchBestFit(): Option[HostId] =
    VmAllocationPolicy.bestFit(hosts, vmSpec)

  @Benchmark
  def benchWorstFit(): Option[HostId] =
    VmAllocationPolicy.worstFit(hosts, vmSpec)

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(1)
class EventQueueBenchmark:

  @Param(Array("1000", "10000", "100000"))
  var eventCount: Int = uninitialized

  var events: Vector[SimEvent] = uninitialized

  @Setup
  def setup(): Unit =
    val source = EntityRef("source", EntityType.Broker)
    val dest   = EntityRef("dest", EntityType.Host)
    events = (0 until eventCount).map { i =>
      SimEvent(
        SimTime(i.toDouble),
        source,
        dest,
        SimEventPayload.SimulationStart,
        SerialNumber(i.toLong)
      )
    }.toVector

  @Benchmark
  def benchTreeMapInsert(): TreeMap[SimTime, Vector[SimEvent]] =
    var queue = TreeMap.empty[SimTime, Vector[SimEvent]](using SimTime.given_Ordering_SimTime)
    for event <- events do
      queue = queue.updatedWith(event.time) {
        case Some(existing) => Some(existing :+ event)
        case None           => Some(Vector(event))
      }
    queue

  @Benchmark
  def benchSchedulerExecution(): Unit =
    val vmSpec = ResourceSpec(PEs(4), MIPS(10000.0), MegaBytes(4096.0), Mbps(1000.0), MegaBytes(10000.0))
    var vm     = VmState.create(VmId(0L), BrokerId(0L), vmSpec, SimTime.Zero)
    for i <- 0 until 10 do vm = vm.startWorkload(WorkloadId(i.toLong), MIPS(1000.0), SimTime.Zero, MI(10000.0))
    val _ = WorkloadScheduler.timeShared(vm, SimTime(5.0), SimTime.Zero)
