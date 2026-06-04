// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.engine

import io.aura.core.types.*

/** A named objective measurement from a simulation run. */
final case class ObjectiveValue(
    name: String,
    value: Double,
    minimize: Boolean = true
)

/** A point in multi-objective space representing one simulation configuration. */
final case class MultiObjectiveResult(
    label: String,
    objectives: Vector[ObjectiveValue]
):
  def objectiveByName(name: String): Option[ObjectiveValue] =
    objectives.find(_.name == name)

  /** Weighted-sum score (lower is better for minimize objectives). */
  def weightedScore(weights: Map[String, Double]): Double =
    objectives.map { obj =>
      val w          = weights.getOrElse(obj.name, 1.0)
      val normalized = if obj.minimize then obj.value else -obj.value
      w * normalized
    }.sum

/** Utilities for multi-objective optimization and Pareto analysis. */
object MultiObjectiveAnalysis:

  /** Check if result A dominates result B. A dominates B iff A is at least as good in all objectives and strictly
    * better in at least one.
    */
  def dominates(a: MultiObjectiveResult, b: MultiObjectiveResult): Boolean =
    val paired = a.objectives.zip(b.objectives)
    val allBetterOrEqual = paired.forall { (oa, ob) =>
      if oa.minimize then oa.value <= ob.value
      else oa.value >= ob.value
    }
    val strictlyBetter = paired.exists { (oa, ob) =>
      if oa.minimize then oa.value < ob.value
      else oa.value > ob.value
    }
    allBetterOrEqual && strictlyBetter

  /** Compute the Pareto front from a set of results. Returns only non-dominated solutions.
    */
  def paretoFront(results: Vector[MultiObjectiveResult]): Vector[MultiObjectiveResult] =
    results.filter { candidate =>
      !results.exists(other => other.label != candidate.label && dominates(other, candidate))
    }

  /** Rank results by weighted-sum scoring (lower score = better). */
  def rankedByWeightedSum(
      results: Vector[MultiObjectiveResult],
      weights: Map[String, Double]
  ): Vector[(MultiObjectiveResult, Double)] =
    results.map(r => (r, r.weightedScore(weights))).sortBy(_._2)

  /** Normalize objective values to [0, 1] range across all results. For minimize objectives: 0 = best (lowest), 1 =
    * worst (highest). For maximize objectives: 0 = worst (lowest), 1 = best (highest).
    */
  def normalize(results: Vector[MultiObjectiveResult]): Vector[MultiObjectiveResult] =
    if results.isEmpty then results
    else
      val objectiveCount = results.head.objectives.size
      val ranges = (0 until objectiveCount).map { i =>
        val values = results.map(_.objectives(i).value)
        (values.min, values.max)
      }

      results.map { r =>
        val normalized = r.objectives.zipWithIndex.map { (obj, i) =>
          val (minVal, maxVal) = ranges(i)
          val range            = maxVal - minVal
          val norm             = if range > 0 then (obj.value - minVal) / range else 0.0
          obj.copy(value = if obj.minimize then norm else 1.0 - norm)
        }
        r.copy(objectives = normalized)
      }

  /** Extract standard objectives from SimulationResults. */
  def fromResults(label: String, results: SimulationResults): MultiObjectiveResult =
    MultiObjectiveResult(
      label = label,
      objectives = Vector(
        ObjectiveValue("cost", results.totalCost.value, minimize = true),
        ObjectiveValue("latency", results.avgCompletionTime.value, minimize = true),
        ObjectiveValue("energy", results.totalEnergyWh.value, minimize = true),
        ObjectiveValue("failures", results.failedWorkloads.size.toDouble, minimize = true)
      )
    )

  /** Format a Pareto analysis report. */
  def formatReport(results: Vector[MultiObjectiveResult]): String =
    if results.isEmpty then "No results to analyze."
    else
      val front        = paretoFront(results)
      val normalized   = normalize(results)
      val equalWeights = results.head.objectives.map(o => o.name -> 1.0).toMap
      val ranked       = rankedByWeightedSum(normalized, equalWeights)

      val objectiveNames = results.head.objectives.map(_.name)
      val headerCols     = "Label" +: objectiveNames :+ "Score"
      val header         = headerCols.map(c => f"$c%-16s").mkString
      val separator      = "-" * header.length

      val rows = ranked.map { case (r, score) =>
        val original        = results.find(_.label == r.label).get
        val isParetoOptimal = front.exists(_.label == r.label)
        val marker          = if isParetoOptimal then " *" else ""
        val cols            = original.label +: original.objectives.map(o => f"${o.value}%.4f") :+ f"$score%.4f$marker"
        cols.map(c => f"$c%-16s").mkString
      }

      val legend = "\n* = Pareto-optimal solution"
      (header +: separator +: rows :+ legend).mkString("\n")
