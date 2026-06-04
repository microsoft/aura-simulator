// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

/** Chaos engineering framework: fault injection experiments with steady-state hypothesis validation and recovery
  * measurement.
  */

/** Types of chaos that can be injected into the simulation. */
enum ChaosExperiment:
  case HostFailure(hostId: HostId, duration: SimTime)
  case NetworkPartition(zoneA: String, zoneB: String, duration: SimTime)
  case LatencyInjection(hostId: HostId, addedLatency: SimTime, duration: SimTime)
  case CpuStress(hostId: HostId, stressLevel: Double, duration: SimTime)
  case MemoryPressure(hostId: HostId, pressureMB: MegaBytes, duration: SimTime)
  case PacketLoss(hostId: HostId, lossRate: Double, duration: SimTime)

/** When to inject chaos. */
enum ChaosSchedule:
  /** Inject once at a specific time. */
  case OneShot(at: SimTime)

  /** Inject repeatedly at a fixed interval. */
  case Recurring(startAt: SimTime, interval: SimTime, count: Int)

  /** Inject at random times within a window (deterministic with seed). */
  case RandomWithin(windowStart: SimTime, windowEnd: SimTime, count: Int, seed: Long = 42L)

/** A planned chaos event: an experiment scheduled at a specific time. */
final case class ChaosEvent(time: SimTime, experiment: ChaosExperiment):
  def endTime: SimTime = SimTime(time.value + duration.value)

  def duration: SimTime = experiment match
    case ChaosExperiment.HostFailure(_, d)         => d
    case ChaosExperiment.NetworkPartition(_, _, d) => d
    case ChaosExperiment.LatencyInjection(_, _, d) => d
    case ChaosExperiment.CpuStress(_, _, d)        => d
    case ChaosExperiment.MemoryPressure(_, _, d)   => d
    case ChaosExperiment.PacketLoss(_, _, d)       => d

  def isActive(at: SimTime): Boolean =
    at.value >= time.value && at.value < endTime.value

/** Defines what "normal" looks like — assertions to validate after chaos. */
final case class SteadyStateHypothesis(
    maxFailureRate: Double = 0.05,
    minThroughput: Double = 0.0,
    maxLatencyP99: SimTime = SimTime(Double.MaxValue),
    maxRecoveryTime: SimTime = SimTime(Double.MaxValue),
    customChecks: Vector[String] = Vector.empty
)

object SteadyStateHypothesis:
  val relaxed: SteadyStateHypothesis = SteadyStateHypothesis(maxFailureRate = 0.2)
  val strict: SteadyStateHypothesis = SteadyStateHypothesis(
    maxFailureRate = 0.01,
    maxLatencyP99 = SimTime(1.0),
    maxRecoveryTime = SimTime(60.0)
  )

/** Metrics snapshot for hypothesis evaluation. */
final case class ChaosMetrics(
    totalRequests: Long,
    failedRequests: Long,
    throughput: Double,
    latencyP99: SimTime,
    recoveryTime: SimTime
):
  def failureRate: Double =
    if totalRequests <= 0 then 0.0
    else failedRequests.toDouble / totalRequests

/** Result of a chaos experiment. */
final case class ChaosResult(
    experiment: ChaosExperiment,
    schedule: ChaosSchedule,
    hypothesis: SteadyStateHypothesis,
    metricsBefore: ChaosMetrics,
    metricsDuring: ChaosMetrics,
    metricsAfter: ChaosMetrics,
    hypothesisHeld: Boolean,
    violations: Vector[String]
):
  def formatReport: String =
    val status = if hypothesisHeld then "PASSED" else "FAILED"
    val lines = Vector(
      s"Chaos Experiment Report: $status",
      s"  Experiment: $experiment",
      s"  Schedule: $schedule",
      s"  Before: failure_rate=${metricsBefore.failureRate}, throughput=${metricsBefore.throughput}",
      s"  During: failure_rate=${metricsDuring.failureRate}, throughput=${metricsDuring.throughput}",
      s"  After:  failure_rate=${metricsAfter.failureRate}, throughput=${metricsAfter.throughput}",
      s"  Recovery time: ${metricsAfter.recoveryTime.value}s"
    ) ++ (if violations.isEmpty then Vector.empty
          else Vector("  Violations:") ++ violations.map(v => s"    - $v"))
    lines.mkString("\n")

/** Pure-function chaos engine. */
object ChaosEngine:

  /** Generate a timeline of chaos events from an experiment and schedule. */
  def planExperiment(
      experiment: ChaosExperiment,
      schedule: ChaosSchedule
  ): Vector[ChaosEvent] = schedule match
    case ChaosSchedule.OneShot(at) =>
      Vector(ChaosEvent(at, experiment))

    case ChaosSchedule.Recurring(startAt, interval, count) =>
      (0 until count).map { i =>
        ChaosEvent(SimTime(startAt.value + i * interval.value), experiment)
      }.toVector

    case ChaosSchedule.RandomWithin(windowStart, windowEnd, count, seed) =>
      val rng  = new scala.util.Random(seed)
      val span = windowEnd.value - windowStart.value
      (0 until count)
        .map { _ =>
          val offset = rng.nextDouble() * span
          ChaosEvent(SimTime(windowStart.value + offset), experiment)
        }
        .toVector
        .sortBy(_.time.value)

  /** Evaluate whether the steady-state hypothesis held based on metrics. */
  def evaluateHypothesis(
      hypothesis: SteadyStateHypothesis,
      metricsDuring: ChaosMetrics,
      metricsAfter: ChaosMetrics
  ): (Boolean, Vector[String]) =
    val violations = Vector.newBuilder[String]

    if metricsDuring.failureRate > hypothesis.maxFailureRate then
      violations += s"Failure rate ${metricsDuring.failureRate} exceeded max ${hypothesis.maxFailureRate}"

    if metricsDuring.throughput < hypothesis.minThroughput then
      violations += s"Throughput ${metricsDuring.throughput} below min ${hypothesis.minThroughput}"

    if metricsDuring.latencyP99.value > hypothesis.maxLatencyP99.value then
      violations += s"P99 latency ${metricsDuring.latencyP99.value}s exceeded max ${hypothesis.maxLatencyP99.value}s"

    if metricsAfter.recoveryTime.value > hypothesis.maxRecoveryTime.value then
      violations += s"Recovery time ${metricsAfter.recoveryTime.value}s exceeded max ${hypothesis.maxRecoveryTime.value}s"

    val result = violations.result()
    (result.isEmpty, result)

  /** Build a full ChaosResult from an experiment run. */
  def buildResult(
      experiment: ChaosExperiment,
      schedule: ChaosSchedule,
      hypothesis: SteadyStateHypothesis,
      metricsBefore: ChaosMetrics,
      metricsDuring: ChaosMetrics,
      metricsAfter: ChaosMetrics
  ): ChaosResult =
    val (held, violations) = evaluateHypothesis(hypothesis, metricsDuring, metricsAfter)
    ChaosResult(experiment, schedule, hypothesis, metricsBefore, metricsDuring, metricsAfter, held, violations)

  /** Compute recovery time: time from chaos end until metrics return to baseline. */
  def recoveryTime(
      chaosEndTime: SimTime,
      recoveredAtTime: SimTime
  ): SimTime =
    SimTime(math.max(0.0, recoveredAtTime.value - chaosEndTime.value))

  /** Check which chaos events are active at a given time. */
  def activeEvents(events: Vector[ChaosEvent], at: SimTime): Vector[ChaosEvent] =
    events.filter(_.isActive(at))

  /** Find all hosts affected by active chaos at a given time. */
  def affectedHosts(events: Vector[ChaosEvent], at: SimTime): Set[HostId] =
    activeEvents(events, at).flatMap { event =>
      event.experiment match
        case ChaosExperiment.HostFailure(hid, _)         => Some(hid)
        case ChaosExperiment.LatencyInjection(hid, _, _) => Some(hid)
        case ChaosExperiment.CpuStress(hid, _, _)        => Some(hid)
        case ChaosExperiment.MemoryPressure(hid, _, _)   => Some(hid)
        case ChaosExperiment.PacketLoss(hid, _, _)       => Some(hid)
        case _: ChaosExperiment.NetworkPartition         => None
    }.toSet
