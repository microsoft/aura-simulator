// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.engine

import io.aura.core.types.*

/** A single scenario parameter with a name and set of values to sweep. */
final case class ScenarioParam[A](
    name: String,
    values: Vector[A]
)

/** A scenario configuration: a labeled set of parameter assignments. */
final case class ScenarioConfig(
    label: String,
    params: Map[String, Any]
):
  def get[A](name: String): Option[A] =
    params.get(name).map(_.asInstanceOf[A])

  def getOrElse[A](name: String, default: A): A =
    params.get(name).map(_.asInstanceOf[A]).getOrElse(default)

/** Result of running one scenario. */
final case class ScenarioResult(
    config: ScenarioConfig,
    results: SimulationResults,
    multiObjective: MultiObjectiveResult
)

/** Framework for running parameter sweep experiments.
  *
  * Generates all combinations of parameter values and runs simulations for each, collecting results and multi-objective
  * metrics.
  */
object ScenarioRunner:

  /** Generate all combinations of scenario parameters.
    *
    * For N parameters with k1, k2, ..., kN values each, produces k1 * k2 * ... * kN ScenarioConfigs.
    */
  def generateConfigs(params: Vector[ScenarioParam[?]]): Vector[ScenarioConfig] =
    if params.isEmpty then Vector(ScenarioConfig("empty", Map.empty))
    else
      def combine(remaining: Vector[ScenarioParam[?]], current: Vector[Map[String, Any]]): Vector[Map[String, Any]] =
        remaining match
          case head +: tail =>
            val expanded = for
              existing <- current
              value    <- head.values
            yield existing + (head.name -> value)
            combine(tail, expanded)
          case _ => current

      val allCombinations = combine(params, Vector(Map.empty))
      allCombinations.zipWithIndex.map { (paramMap, _) =>
        val label = paramMap.map((k, v) => s"$k=$v").mkString(", ")
        ScenarioConfig(label, paramMap)
      }

  /** Run a sweep: generate configs and execute each one.
    *
    * @param params
    *   Parameters to sweep
    * @param runner
    *   Function that takes a ScenarioConfig and runs a simulation
    * @return
    *   Vector of results, one per config
    */
  def sweep(
      params: Vector[ScenarioParam[?]],
      runner: ScenarioConfig => Either[SimulationError, SimulationResults]
  ): Vector[Either[SimulationError, ScenarioResult]] =
    val configs = generateConfigs(params)
    configs.map { config =>
      runner(config).map { results =>
        val mo = MultiObjectiveAnalysis.fromResults(config.label, results)
        ScenarioResult(config, results, mo)
      }
    }

  /** Run a sweep and return only successful results. */
  def sweepSuccessful(
      params: Vector[ScenarioParam[?]],
      runner: ScenarioConfig => Either[SimulationError, SimulationResults]
  ): Vector[ScenarioResult] =
    sweep(params, runner).collect { case Right(r) => r }

  /** Format a comparison table from scenario results. */
  def formatComparison(results: Vector[ScenarioResult]): String =
    if results.isEmpty then "No scenario results to compare."
    else
      val header =
        f"${"Scenario"}%-40s ${"Workloads"}%-12s ${"Avg Time"}%-12s ${"Energy (Wh)"}%-14s ${"Cost"}%-12s ${"Failed"}%-8s"
      val separator = "-" * header.length
      val rows = results.map { sr =>
        val r       = sr.results
        val avgTime = if r.workloadResults.nonEmpty then r.avgCompletionTime.value else 0.0
        val label   = if sr.config.label.length > 38 then sr.config.label.take(38) + ".." else sr.config.label
        f"$label%-40s ${r.workloadResults.size}%-12d $avgTime%-12.2f ${r.totalEnergyWh.value}%-14.4f ${r.totalCost.value}%-12.4f ${r.failedWorkloads.size}%-8d"
      }
      (header +: separator +: rows).mkString("\n")

  /** Perform Pareto analysis on scenario results. */
  def paretoAnalysis(results: Vector[ScenarioResult]): Vector[ScenarioResult] =
    val moResults   = results.map(_.multiObjective)
    val front       = MultiObjectiveAnalysis.paretoFront(moResults)
    val frontLabels = front.map(_.label).toSet
    results.filter(r => frontLabels.contains(r.config.label))

  /** Find the best scenario by a single objective. */
  def bestBy(results: Vector[ScenarioResult], objectiveName: String): Option[ScenarioResult] =
    results.minByOption { sr =>
      sr.multiObjective.objectiveByName(objectiveName).map(_.value).getOrElse(Double.MaxValue)
    }
