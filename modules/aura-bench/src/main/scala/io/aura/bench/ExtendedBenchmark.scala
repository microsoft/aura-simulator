// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.bench

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import scala.compiletime.uninitialized

import io.aura.core.types.*
import io.aura.core.events.*
import io.aura.core.engine.{MultiObjectiveAnalysis, MultiObjectiveResult, ObjectiveValue}
import io.aura.iaas.policies.{CostRates, DagScheduler, MigrationModel, MigrationParams, VmCostCalculator}
import io.aura.power.{CarbonIntensity, EfficiencyMetrics, PueModel}

/** JMH benchmarks for migration models.
  *
  * Run with: sbt "auraBench/Jmh/run -f 1 -wi 3 -i 5 .*MigrationBenchmark.*"
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
class MigrationBenchmark:

  @Param(Array("1024", "8192", "32768"))
  var ramMB: Double = uninitialized

  var params: MigrationParams = uninitialized

  @Setup
  def setup(): Unit =
    params = MigrationParams(MegaBytes(ramMB), Mbps(10000.0))

  @Benchmark
  def benchPreCopy(): Unit =
    val _ = MigrationModel.preCopy(params)

  @Benchmark
  def benchPreCopyIterative(): Unit =
    val _ = MigrationModel.preCopyIterative()(params)

  @Benchmark
  def benchPostCopy(): Unit =
    val _ = MigrationModel.postCopy()(params)

  @Benchmark
  def benchNonLive(): Unit =
    val _ = MigrationModel.nonLive()(params)

/** JMH benchmarks for cost calculations. */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
class CostBenchmark:

  @Param(Array("3600", "86400", "604800"))
  var durationSeconds: Double = uninitialized

  var spec: ResourceSpec = uninitialized

  @Setup
  def setup(): Unit =
    spec = ResourceSpec(PEs(4), MIPS(10000.0), MegaBytes(8192.0), Mbps(5000.0), MegaBytes(200000.0))

  @Benchmark
  def benchCostM5(): Unit =
    val calc = VmCostCalculator.fromRates(CostRates.awsM5)
    val _    = calc(spec, SimTime(durationSeconds))

  @Benchmark
  def benchCostSpot(): Unit =
    val calc = VmCostCalculator.fromRates(CostRates.awsSpot)
    val _    = calc(spec, SimTime(durationSeconds))

  @Benchmark
  def benchCostReserved(): Unit =
    val calc = VmCostCalculator.fromRates(CostRates.awsReserved)
    val _    = calc(spec, SimTime(durationSeconds))

/** JMH benchmarks for Pareto analysis / multi-objective optimization. */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
class ParetoBenchmark:

  @Param(Array("10", "100", "1000"))
  var resultCount: Int = uninitialized

  var results: Vector[MultiObjectiveResult] = uninitialized
  var weights: Map[String, Double]          = uninitialized

  @Setup
  def setup(): Unit =
    val rng = new java.util.Random(42L)
    results = (0 until resultCount).map { i =>
      MultiObjectiveResult(
        s"r$i",
        Vector(
          ObjectiveValue("cost", rng.nextDouble() * 100),
          ObjectiveValue("latency", rng.nextDouble() * 50),
          ObjectiveValue("energy", rng.nextDouble() * 200)
        )
      )
    }.toVector
    weights = Map("cost" -> 1.0, "latency" -> 2.0, "energy" -> 0.5)

  @Benchmark
  def benchParetoFront(): Vector[MultiObjectiveResult] =
    MultiObjectiveAnalysis.paretoFront(results)

  @Benchmark
  def benchNormalize(): Vector[MultiObjectiveResult] =
    MultiObjectiveAnalysis.normalize(results)

  @Benchmark
  def benchRankedByWeightedSum(): Vector[(MultiObjectiveResult, Double)] =
    MultiObjectiveAnalysis.rankedByWeightedSum(results, weights)

/** JMH benchmarks for DAG scheduling operations. */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
class DagBenchmark:

  @Param(Array("10", "100", "1000"))
  var taskCount: Int = uninitialized

  var workloads: Vector[WorkloadSpec] = uninitialized

  @Setup
  def setup(): Unit =
    // Create a linear chain DAG: 0→1→2→...→n-1
    workloads = (0 until taskCount).map { i =>
      val preds = if i == 0 then Set.empty[WorkloadId] else Set(WorkloadId((i - 1).toLong))
      WorkloadSpec.simple(WorkloadId(i.toLong), MI(1000.0), PEs(1)).copy(predecessors = preds)
    }.toVector

  @Benchmark
  def benchTopologicalSort(): Vector[WorkloadSpec] =
    DagScheduler.topologicalSort(workloads)

  @Benchmark
  def benchCriticalPath(): MI =
    DagScheduler.criticalPathLength(workloads)

  @Benchmark
  def benchValidate(): DagScheduler.DagValidation =
    DagScheduler.validate(workloads)

/** JMH benchmarks for PUE and efficiency metrics computation. */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
class EfficiencyBenchmark:

  var itEnergy: WattHours = uninitialized

  @Setup
  def setup(): Unit =
    itEnergy = WattHours(50000.0)

  @Benchmark
  def benchConstantPue(): EfficiencyMetrics =
    EfficiencyMetrics.compute(itEnergy, PueModel.constant(1.5), CarbonIntensity.usEast)

  @Benchmark
  def benchTempPue(): EfficiencyMetrics =
    val model = PueModel.temperatureBased()(35.0)
    EfficiencyMetrics.compute(itEnergy, model, CarbonIntensity.india)

  @Benchmark
  def benchFormatReport(): String =
    val metrics = EfficiencyMetrics.compute(itEnergy, PueModel.hyperscale, CarbonIntensity.euNorth)
    EfficiencyMetrics.formatReport(metrics)
