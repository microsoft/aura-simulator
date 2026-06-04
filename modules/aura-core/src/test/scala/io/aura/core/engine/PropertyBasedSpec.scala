// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import io.aura.core.types.*

class PropertyBasedSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  // ─── Pareto Front Properties ─────────────────────────────────────────

  private val objectiveGen = Gen.chooseNum(0.0, 100.0)
  private val resultGen = for
    label   <- Gen.alphaNumStr.suchThat(_.nonEmpty)
    cost    <- objectiveGen
    latency <- objectiveGen
    energy  <- objectiveGen
  yield MultiObjectiveResult(
    label,
    Vector(
      ObjectiveValue("cost", cost),
      ObjectiveValue("latency", latency),
      ObjectiveValue("energy", energy)
    )
  )

  "Pareto front" should "never contain dominated solutions" in {
    forAll(Gen.listOfN(10, resultGen).suchThat(_.size >= 2)) { results =>
      val uniqueResults = results.zipWithIndex.map((r, i) => r.copy(label = s"r$i")).toVector
      val front         = MultiObjectiveAnalysis.paretoFront(uniqueResults)
      for
        a <- front
        b <- front
        if a.label != b.label
      do MultiObjectiveAnalysis.dominates(a, b) shouldBe false
    }
  }

  it should "be a subset of the input" in {
    forAll(Gen.listOfN(8, resultGen).suchThat(_.nonEmpty)) { results =>
      val uniqueResults = results.zipWithIndex.map((r, i) => r.copy(label = s"r$i")).toVector
      val front         = MultiObjectiveAnalysis.paretoFront(uniqueResults)
      front.size should be <= uniqueResults.size
      front.foreach { f =>
        uniqueResults.map(_.label) should contain(f.label)
      }
    }
  }

  it should "contain at least one solution for non-empty input" in {
    forAll(Gen.listOfN(5, resultGen).suchThat(_.nonEmpty)) { results =>
      val uniqueResults = results.zipWithIndex.map((r, i) => r.copy(label = s"r$i")).toVector
      val front         = MultiObjectiveAnalysis.paretoFront(uniqueResults)
      front should not be empty
    }
  }

  // ─── Dominance Properties ──────────────────────────────────────────

  "dominates" should "be irreflexive (nothing dominates itself)" in {
    forAll(resultGen) { r =>
      MultiObjectiveAnalysis.dominates(r, r) shouldBe false
    }
  }

  it should "be asymmetric (if a dominates b, b cannot dominate a)" in {
    forAll(resultGen, resultGen) { (a, b) =>
      if MultiObjectiveAnalysis.dominates(a, b) then MultiObjectiveAnalysis.dominates(b, a) shouldBe false
    }
  }

  // ─── Normalize Properties ──────────────────────────────────────────

  "normalize" should "produce values in [0, 1]" in {
    forAll(Gen.listOfN(5, resultGen).suchThat(_.size >= 2)) { results =>
      val uniqueResults = results.zipWithIndex.map((r, i) => r.copy(label = s"r$i")).toVector
      val normalized    = MultiObjectiveAnalysis.normalize(uniqueResults)
      normalized.foreach { r =>
        r.objectives.foreach { obj =>
          obj.value should be >= 0.0
          obj.value should be <= 1.0
        }
      }
    }
  }

  it should "preserve relative ordering" in {
    forAll(Gen.listOfN(3, resultGen).suchThat(_.size >= 2)) { results =>
      val uniqueResults = results.zipWithIndex.map((r, i) => r.copy(label = s"r$i")).toVector
      val normalized    = MultiObjectiveAnalysis.normalize(uniqueResults)
      // For minimize objectives, lower original value => lower normalized value
      for i <- uniqueResults.head.objectives.indices do
        val originalValues   = uniqueResults.map(_.objectives(i).value)
        val normalizedValues = normalized.map(_.objectives(i).value)
        val origPairs        = originalValues.zip(normalizedValues)
        for
          (ov1, nv1) <- origPairs
          (ov2, nv2) <- origPairs
        do if ov1 < ov2 then nv1 should be <= nv2
    }
  }

  // ─── Weighted Sum Properties ───────────────────────────────────────

  "rankedByWeightedSum" should "return all input results" in {
    forAll(Gen.listOfN(5, resultGen).suchThat(_.nonEmpty)) { results =>
      val uniqueResults = results.zipWithIndex.map((r, i) => r.copy(label = s"r$i")).toVector
      val weights       = Map("cost" -> 1.0, "latency" -> 1.0, "energy" -> 1.0)
      val ranked        = MultiObjectiveAnalysis.rankedByWeightedSum(uniqueResults, weights)
      ranked.size shouldBe uniqueResults.size
    }
  }

  it should "produce scores in ascending order" in {
    forAll(Gen.listOfN(5, resultGen).suchThat(_.size >= 2)) { results =>
      val uniqueResults = results.zipWithIndex.map((r, i) => r.copy(label = s"r$i")).toVector
      val weights       = Map("cost" -> 1.0, "latency" -> 1.0, "energy" -> 1.0)
      val ranked        = MultiObjectiveAnalysis.rankedByWeightedSum(uniqueResults, weights)
      val scores        = ranked.map(_._2)
      scores shouldBe sorted
    }
  }

  // ─── SimTime Arithmetic Properties ─────────────────────────────────

  "SimTime" should "satisfy additive identity" in {
    forAll(Gen.chooseNum(0.0, 1e12)) { v =>
      val t = SimTime(v)
      (t + SimTime.Zero).value shouldBe v
    }
  }

  it should "satisfy commutativity of addition" in {
    forAll(Gen.chooseNum(0.0, 1e6), Gen.chooseNum(0.0, 1e6)) { (a, b) =>
      val ta = SimTime(a)
      val tb = SimTime(b)
      (ta + tb).value shouldBe (tb + ta).value
    }
  }

  // ─── MI Execution Time Properties ───────────────────────────────────

  "MI.executionTime" should "be inversely proportional to MIPS" in {
    forAll(Gen.chooseNum(1.0, 1e6), Gen.chooseNum(1.0, 1e6)) { (mi, mips) =>
      val time = MI(mi).executionTime(MIPS(mips))
      time.value shouldBe (mi / mips) +- 0.001
    }
  }

  it should "return MaxValue for zero MIPS" in {
    forAll(Gen.chooseNum(1.0, 1e6)) { mi =>
      MI(mi).executionTime(MIPS.Zero).value shouldBe Double.MaxValue
    }
  }
