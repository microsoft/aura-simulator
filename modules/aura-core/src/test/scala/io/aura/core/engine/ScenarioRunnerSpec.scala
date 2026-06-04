// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.types.*

class ScenarioRunnerSpec extends AnyFlatSpec with Matchers:

  // ─── generateConfigs ────────────────────────────────────────────────

  "ScenarioRunner.generateConfigs" should "produce cartesian product of parameters" in {
    val params = Vector(
      ScenarioParam("hosts", Vector(2, 4, 8)),
      ScenarioParam("vms", Vector(5, 10))
    )
    val configs = ScenarioRunner.generateConfigs(params)
    configs should have size 6 // 3 * 2
  }

  it should "include all parameter values in configs" in {
    val params = Vector(
      ScenarioParam("policy", Vector("firstFit", "bestFit"))
    )
    val configs = ScenarioRunner.generateConfigs(params)
    configs should have size 2
    configs(0).get[String]("policy") shouldBe Some("firstFit")
    configs(1).get[String]("policy") shouldBe Some("bestFit")
  }

  it should "handle single parameter" in {
    val params = Vector(ScenarioParam("x", Vector(1, 2, 3)))
    ScenarioRunner.generateConfigs(params) should have size 3
  }

  it should "handle empty parameters" in {
    ScenarioRunner.generateConfigs(Vector.empty) should have size 1
  }

  it should "generate meaningful labels" in {
    val params = Vector(
      ScenarioParam("hosts", Vector(4)),
      ScenarioParam("vms", Vector(10))
    )
    val configs = ScenarioRunner.generateConfigs(params)
    configs.head.label should include("hosts=4")
    configs.head.label should include("vms=10")
  }

  // ─── ScenarioConfig ────────────────────────────────────────────────

  "ScenarioConfig.get" should "return typed parameter value" in {
    val config = ScenarioConfig("test", Map("count" -> 42, "name" -> "hello"))
    config.get[Int]("count") shouldBe Some(42)
    config.get[String]("name") shouldBe Some("hello")
    config.get[Int]("missing") shouldBe None
  }

  "ScenarioConfig.getOrElse" should "return default for missing params" in {
    val config = ScenarioConfig("test", Map("x" -> 10))
    config.getOrElse[Int]("x", 0) shouldBe 10
    config.getOrElse[Int]("y", 99) shouldBe 99
  }

  // ─── sweep with mock runner ────────────────────────────────────────

  "ScenarioRunner.sweep" should "run each config and collect results" in {
    val params = Vector(ScenarioParam("workloads", Vector(10, 20, 30)))
    val results = ScenarioRunner.sweep(
      params,
      config =>
        Right(
          SimulationResults.empty.copy(
            simulationEndTime = SimTime(config.getOrElse[Int]("workloads", 0).toDouble)
          )
        )
    )
    results should have size 3
    results.foreach(_ shouldBe a[Right[?, ?]])
  }

  "ScenarioRunner.sweepSuccessful" should "filter out failures" in {
    val params = Vector(ScenarioParam("n", Vector(1, 2, 3)))
    val results = ScenarioRunner.sweepSuccessful(
      params,
      config =>
        val n = config.getOrElse[Int]("n", 0)
        if n == 2 then Left(SimulationError.Timeout("test timeout"))
        else Right(SimulationResults.empty)
    )
    results should have size 2
  }

  // ─── formatComparison ─────────────────────────────────────────────

  "ScenarioRunner.formatComparison" should "produce a readable table" in {
    val results = Vector(
      ScenarioResult(
        ScenarioConfig("small", Map.empty),
        SimulationResults.empty,
        MultiObjectiveAnalysis.fromResults("small", SimulationResults.empty)
      ),
      ScenarioResult(
        ScenarioConfig("large", Map.empty),
        SimulationResults.empty,
        MultiObjectiveAnalysis.fromResults("large", SimulationResults.empty)
      )
    )
    val table = ScenarioRunner.formatComparison(results)
    table should include("Scenario")
    table should include("small")
    table should include("large")
  }

  it should "handle empty results" in {
    ScenarioRunner.formatComparison(Vector.empty) should include("No scenario results")
  }

  // ─── paretoAnalysis ───────────────────────────────────────────────

  "ScenarioRunner.paretoAnalysis" should "return Pareto-optimal scenarios" in {
    val r1 = ScenarioResult(
      ScenarioConfig("fast", Map.empty),
      SimulationResults.empty.copy(simulationEndTime = SimTime(10.0)),
      MultiObjectiveResult(
        "fast",
        Vector(
          ObjectiveValue("cost", 100.0),
          ObjectiveValue("latency", 1.0),
          ObjectiveValue("energy", 50.0),
          ObjectiveValue("failures", 0.0)
        )
      )
    )
    val r2 = ScenarioResult(
      ScenarioConfig("cheap", Map.empty),
      SimulationResults.empty.copy(simulationEndTime = SimTime(20.0)),
      MultiObjectiveResult(
        "cheap",
        Vector(
          ObjectiveValue("cost", 10.0),
          ObjectiveValue("latency", 10.0),
          ObjectiveValue("energy", 50.0),
          ObjectiveValue("failures", 0.0)
        )
      )
    )
    val r3 = ScenarioResult(
      ScenarioConfig("dominated", Map.empty),
      SimulationResults.empty.copy(simulationEndTime = SimTime(30.0)),
      MultiObjectiveResult(
        "dominated",
        Vector(
          ObjectiveValue("cost", 100.0),
          ObjectiveValue("latency", 10.0),
          ObjectiveValue("energy", 50.0),
          ObjectiveValue("failures", 0.0)
        )
      )
    )
    val pareto = ScenarioRunner.paretoAnalysis(Vector(r1, r2, r3))
    pareto.map(_.config.label).toSet shouldBe Set("fast", "cheap")
  }

  // ─── bestBy ───────────────────────────────────────────────────────

  "ScenarioRunner.bestBy" should "find scenario with lowest objective value" in {
    val r1 = ScenarioResult(
      ScenarioConfig("expensive", Map.empty),
      SimulationResults.empty,
      MultiObjectiveResult("expensive", Vector(ObjectiveValue("cost", 100.0)))
    )
    val r2 = ScenarioResult(
      ScenarioConfig("cheap", Map.empty),
      SimulationResults.empty,
      MultiObjectiveResult("cheap", Vector(ObjectiveValue("cost", 10.0)))
    )
    ScenarioRunner.bestBy(Vector(r1, r2), "cost").map(_.config.label) shouldBe Some("cheap")
  }

  it should "return None for empty results" in {
    ScenarioRunner.bestBy(Vector.empty, "cost") shouldBe None
  }
