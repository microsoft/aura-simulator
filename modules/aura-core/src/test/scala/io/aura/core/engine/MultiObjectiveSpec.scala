// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MultiObjectiveSpec extends AnyFlatSpec with Matchers:

  private def result(label: String, cost: Double, latency: Double, energy: Double) =
    MultiObjectiveResult(
      label,
      Vector(
        ObjectiveValue("cost", cost),
        ObjectiveValue("latency", latency),
        ObjectiveValue("energy", energy)
      )
    )

  // ─── dominates ──────────────────────────────────────────────────────

  "MultiObjectiveAnalysis.dominates" should "detect domination" in {
    val a = result("a", cost = 1.0, latency = 2.0, energy = 3.0)
    val b = result("b", cost = 2.0, latency = 3.0, energy = 4.0)
    MultiObjectiveAnalysis.dominates(a, b) shouldBe true
    MultiObjectiveAnalysis.dominates(b, a) shouldBe false
  }

  it should "not dominate when equal" in {
    val a = result("a", cost = 1.0, latency = 2.0, energy = 3.0)
    val b = result("b", cost = 1.0, latency = 2.0, energy = 3.0)
    MultiObjectiveAnalysis.dominates(a, b) shouldBe false
  }

  it should "not dominate when trade-offs exist" in {
    val a = result("a", cost = 1.0, latency = 5.0, energy = 3.0) // cheaper but slower
    val b = result("b", cost = 3.0, latency = 2.0, energy = 3.0) // more expensive but faster
    MultiObjectiveAnalysis.dominates(a, b) shouldBe false
    MultiObjectiveAnalysis.dominates(b, a) shouldBe false
  }

  // ─── paretoFront ────────────────────────────────────────────────────

  "MultiObjectiveAnalysis.paretoFront" should "find non-dominated solutions" in {
    val results = Vector(
      result("cheap", cost = 1.0, latency = 5.0, energy = 3.0),
      result("fast", cost = 5.0, latency = 1.0, energy = 3.0),
      result("dominated", cost = 5.0, latency = 5.0, energy = 5.0),
      result("green", cost = 3.0, latency = 3.0, energy = 1.0)
    )
    val front = MultiObjectiveAnalysis.paretoFront(results)
    front.map(_.label).toSet shouldBe Set("cheap", "fast", "green")
  }

  it should "return all results when none dominate" in {
    val results = Vector(
      result("a", cost = 1.0, latency = 3.0, energy = 2.0),
      result("b", cost = 3.0, latency = 1.0, energy = 2.0),
      result("c", cost = 2.0, latency = 2.0, energy = 1.0)
    )
    val front = MultiObjectiveAnalysis.paretoFront(results)
    front should have size 3
  }

  // ─── normalize ──────────────────────────────────────────────────────

  "MultiObjectiveAnalysis.normalize" should "scale values to [0, 1]" in {
    val results = Vector(
      result("low", cost = 10.0, latency = 20.0, energy = 30.0),
      result("high", cost = 50.0, latency = 60.0, energy = 70.0)
    )
    val normalized = MultiObjectiveAnalysis.normalize(results)
    // For minimize: 0 = best (lowest), 1 = worst (highest)
    normalized(0).objectives(0).value shouldBe 0.0 // cost 10 is min
    normalized(1).objectives(0).value shouldBe 1.0 // cost 50 is max
  }

  // ─── rankedByWeightedSum ────────────────────────────────────────────

  "MultiObjectiveAnalysis.rankedByWeightedSum" should "rank by weighted score" in {
    val results = Vector(
      result("a", cost = 1.0, latency = 10.0, energy = 5.0),
      result("b", cost = 5.0, latency = 1.0, energy = 5.0)
    )
    // With equal weights, a scores 1+10+5=16, b scores 5+1+5=11
    val ranked =
      MultiObjectiveAnalysis.rankedByWeightedSum(results, Map("cost" -> 1.0, "latency" -> 1.0, "energy" -> 1.0))
    ranked.head._1.label shouldBe "b" // lower total = better
  }

  it should "respect custom weights" in {
    val results = Vector(
      result("cheap", cost = 1.0, latency = 10.0, energy = 5.0),
      result("fast", cost = 10.0, latency = 1.0, energy = 5.0)
    )
    // With cost weight 10x: cheap = 1*10 + 10*1 + 5*1 = 25, fast = 10*10 + 1*1 + 5*1 = 106
    val ranked =
      MultiObjectiveAnalysis.rankedByWeightedSum(results, Map("cost" -> 10.0, "latency" -> 1.0, "energy" -> 1.0))
    ranked.head._1.label shouldBe "cheap"
  }

  // ─── fromResults ────────────────────────────────────────────────────

  "MultiObjectiveAnalysis.fromResults" should "extract objectives from SimulationResults" in {
    val results = SimulationResults.empty
    val mo      = MultiObjectiveAnalysis.fromResults("test", results)
    mo.objectives should have size 4
    mo.objectiveByName("cost") shouldBe defined
    mo.objectiveByName("latency") shouldBe defined
    mo.objectiveByName("energy") shouldBe defined
    mo.objectiveByName("failures") shouldBe defined
  }

  // ─── formatReport ──────────────────────────────────────────────────

  "MultiObjectiveAnalysis.formatReport" should "produce a readable report" in {
    val results = Vector(
      result("cheap", cost = 1.0, latency = 5.0, energy = 3.0),
      result("fast", cost = 5.0, latency = 1.0, energy = 3.0),
      result("dominated", cost = 5.0, latency = 5.0, energy = 5.0)
    )
    val report = MultiObjectiveAnalysis.formatReport(results)
    report should include("cheap")
    report should include("fast")
    report should include("Pareto-optimal")
  }
