// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ChaosEngineeringSpec extends AnyFlatSpec with Matchers:

  import ChaosExperiment.*
  import ChaosSchedule.*

  private val hostFailure  = HostFailure(HostId(1), SimTime(30.0))
  private val netPartition = NetworkPartition("zone-a", "zone-b", SimTime(60.0))
  private val latencyInj   = LatencyInjection(HostId(2), SimTime(0.5), SimTime(20.0))

  // ── ChaosEvent ──────────────────────────────────────────────────────────

  "ChaosEvent" should "compute end time" in {
    val event = ChaosEvent(SimTime(100.0), hostFailure)
    event.endTime.value shouldBe 130.0
  }

  it should "report active status correctly" in {
    val event = ChaosEvent(SimTime(100.0), hostFailure) // active 100-130
    event.isActive(SimTime(99.0)) shouldBe false
    event.isActive(SimTime(100.0)) shouldBe true
    event.isActive(SimTime(115.0)) shouldBe true
    event.isActive(SimTime(130.0)) shouldBe false
  }

  // ── ChaosSchedule.OneShot ───────────────────────────────────────────────

  "ChaosEngine.planExperiment OneShot" should "produce single event" in {
    val events = ChaosEngine.planExperiment(hostFailure, OneShot(SimTime(50.0)))
    events should have size 1
    events.head.time.value shouldBe 50.0
  }

  // ── ChaosSchedule.Recurring ─────────────────────────────────────────────

  "ChaosEngine.planExperiment Recurring" should "produce events at fixed intervals" in {
    val events = ChaosEngine.planExperiment(hostFailure, Recurring(SimTime(10.0), SimTime(100.0), 3))
    events should have size 3
    events(0).time.value shouldBe 10.0
    events(1).time.value shouldBe 110.0
    events(2).time.value shouldBe 210.0
  }

  // ── ChaosSchedule.RandomWithin ──────────────────────────────────────────

  "ChaosEngine.planExperiment RandomWithin" should "produce events within window" in {
    val events = ChaosEngine.planExperiment(
      hostFailure,
      RandomWithin(SimTime(100.0), SimTime(200.0), 5)
    )
    events should have size 5
    events.foreach { e =>
      e.time.value should be >= 100.0
      e.time.value should be < 200.0
    }
  }

  it should "produce sorted events" in {
    val events = ChaosEngine.planExperiment(
      hostFailure,
      RandomWithin(SimTime(0.0), SimTime(1000.0), 10)
    )
    val times = events.map(_.time.value)
    times shouldBe sorted
  }

  it should "be deterministic with same seed" in {
    val e1 = ChaosEngine.planExperiment(latencyInj, RandomWithin(SimTime(0.0), SimTime(100.0), 5, seed = 99))
    val e2 = ChaosEngine.planExperiment(latencyInj, RandomWithin(SimTime(0.0), SimTime(100.0), 5, seed = 99))
    e1.map(_.time) shouldBe e2.map(_.time)
  }

  // ── Hypothesis evaluation ───────────────────────────────────────────────

  "ChaosEngine.evaluateHypothesis" should "pass when all metrics within bounds" in {
    val hypothesis         = SteadyStateHypothesis(maxFailureRate = 0.1, minThroughput = 50.0)
    val during             = ChaosMetrics(1000, 50, 80.0, SimTime(0.5), SimTime.Zero)
    val after              = ChaosMetrics(1000, 10, 100.0, SimTime(0.2), SimTime(5.0))
    val (held, violations) = ChaosEngine.evaluateHypothesis(hypothesis, during, after)
    held shouldBe true
    violations shouldBe empty
  }

  it should "fail when failure rate exceeded" in {
    val hypothesis         = SteadyStateHypothesis(maxFailureRate = 0.05)
    val during             = ChaosMetrics(100, 20, 80.0, SimTime(0.5), SimTime.Zero) // 20% failure
    val after              = ChaosMetrics(100, 0, 100.0, SimTime(0.1), SimTime(1.0))
    val (held, violations) = ChaosEngine.evaluateHypothesis(hypothesis, during, after)
    held shouldBe false
    violations.exists(_.contains("Failure rate")) shouldBe true
  }

  it should "fail when recovery time exceeded" in {
    val hypothesis         = SteadyStateHypothesis(maxRecoveryTime = SimTime(10.0))
    val during             = ChaosMetrics(100, 2, 80.0, SimTime(0.5), SimTime.Zero)
    val after              = ChaosMetrics(100, 0, 100.0, SimTime(0.1), SimTime(30.0))
    val (held, violations) = ChaosEngine.evaluateHypothesis(hypothesis, during, after)
    held shouldBe false
    violations.exists(_.contains("Recovery time")) shouldBe true
  }

  it should "fail when P99 latency exceeded" in {
    val hypothesis         = SteadyStateHypothesis(maxLatencyP99 = SimTime(1.0))
    val during             = ChaosMetrics(100, 0, 80.0, SimTime(5.0), SimTime.Zero)
    val after              = ChaosMetrics(100, 0, 100.0, SimTime(0.2), SimTime(1.0))
    val (held, violations) = ChaosEngine.evaluateHypothesis(hypothesis, during, after)
    held shouldBe false
    violations.exists(_.contains("P99 latency")) shouldBe true
  }

  // ── buildResult ─────────────────────────────────────────────────────────

  "ChaosEngine.buildResult" should "produce a complete result" in {
    val before     = ChaosMetrics(1000, 10, 100.0, SimTime(0.2), SimTime.Zero)
    val during     = ChaosMetrics(800, 200, 60.0, SimTime(2.0), SimTime.Zero)
    val after      = ChaosMetrics(1000, 5, 98.0, SimTime(0.3), SimTime(15.0))
    val hypothesis = SteadyStateHypothesis.strict
    val result     = ChaosEngine.buildResult(hostFailure, OneShot(SimTime(50.0)), hypothesis, before, during, after)
    result.hypothesisHeld shouldBe false
    result.violations should not be empty
  }

  // ── ChaosResult.formatReport ────────────────────────────────────────────

  "ChaosResult.formatReport" should "include status and violations" in {
    val before = ChaosMetrics(100, 0, 100.0, SimTime(0.1), SimTime.Zero)
    val during = ChaosMetrics(100, 50, 50.0, SimTime(3.0), SimTime.Zero)
    val after  = ChaosMetrics(100, 0, 100.0, SimTime(0.1), SimTime(5.0))
    val result =
      ChaosEngine.buildResult(hostFailure, OneShot(SimTime(10.0)), SteadyStateHypothesis.strict, before, during, after)
    val report = result.formatReport
    report should include("FAILED")
    report should include("Violations")
  }

  // ── activeEvents / affectedHosts ────────────────────────────────────────

  "ChaosEngine.activeEvents" should "return only active events at given time" in {
    val events = Vector(
      ChaosEvent(SimTime(10.0), hostFailure), // active 10-40
      ChaosEvent(SimTime(50.0), latencyInj)   // active 50-70
    )
    ChaosEngine.activeEvents(events, SimTime(25.0)) should have size 1
    ChaosEngine.activeEvents(events, SimTime(55.0)) should have size 1
    ChaosEngine.activeEvents(events, SimTime(45.0)) shouldBe empty
  }

  "ChaosEngine.affectedHosts" should "return host IDs under chaos" in {
    val events = Vector(
      ChaosEvent(SimTime(10.0), HostFailure(HostId(1), SimTime(30.0))),
      ChaosEvent(SimTime(10.0), CpuStress(HostId(2), 0.9, SimTime(30.0)))
    )
    val affected = ChaosEngine.affectedHosts(events, SimTime(15.0))
    affected shouldBe Set(HostId(1), HostId(2))
  }

  it should "not include network partition hosts" in {
    val events = Vector(ChaosEvent(SimTime(10.0), netPartition))
    ChaosEngine.affectedHosts(events, SimTime(15.0)) shouldBe empty
  }

  // ── recoveryTime ────────────────────────────────────────────────────────

  "ChaosEngine.recoveryTime" should "compute time from chaos end to recovery" in {
    ChaosEngine.recoveryTime(SimTime(100.0), SimTime(115.0)).value shouldBe 15.0
  }

  it should "return zero when recovered before chaos ends" in {
    ChaosEngine.recoveryTime(SimTime(100.0), SimTime(90.0)).value shouldBe 0.0
  }
