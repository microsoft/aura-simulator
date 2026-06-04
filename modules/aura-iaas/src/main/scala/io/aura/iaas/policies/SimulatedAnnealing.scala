// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import io.aura.core.types.*

/** Simulated Annealing configuration. */
final case class SAConfig(
    initialTemperature: Double = 1000.0,
    coolingRate: Double = 0.003,
    coldTemperature: Double = 0.001,
    searchesPerIteration: Int = 10,
    seed: Long = 42L
)

/** Generic Simulated Annealing solver.
  *
  * Uses a single generic function parameterized by cost and neighbor functions.
  */
object SimulatedAnnealing:
  def solve[T](
      config: SAConfig,
      initialSolution: T,
      costFunction: T => Double,
      neighborFunction: (T, java.util.Random) => T
  ): (T, Double, Int) =
    val rng = new java.util.Random(config.seed)

    @annotation.tailrec
    def anneal(
        current: T,
        currentCost: Double,
        best: T,
        bestCost: Double,
        temperature: Double,
        iterations: Int
    ): (T, Double, Int) =
      if temperature <= config.coldTemperature then (best, bestCost, iterations)
      else
        val (newCurrent, newCurrentCost, newBest, newBestCost) =
          (0 until config.searchesPerIteration).foldLeft((current, currentCost, best, bestCost)) {
            case ((cur, curCost, b, bCost), _) =>
              val neighbor     = neighborFunction(cur, rng)
              val neighborCost = costFunction(neighbor)
              val acceptanceProbability =
                if neighborCost < curCost then 1.0
                else math.exp((curCost - neighborCost) / temperature)
              if rng.nextDouble() < acceptanceProbability then
                val newBest     = if neighborCost < bCost then neighbor else b
                val newBestCost = math.min(neighborCost, bCost)
                (neighbor, neighborCost, newBest, newBestCost)
              else (cur, curCost, b, bCost)
          }
        anneal(newCurrent, newCurrentCost, newBest, newBestCost, temperature * (1 - config.coolingRate), iterations + 1)

    val initialCost = costFunction(initialSolution)
    anneal(initialSolution, initialCost, initialSolution, initialCost, config.initialTemperature, 0)

/** Workload-to-VM mapping using Simulated Annealing. */
object WorkloadVmMapping:
  type Mapping = Map[WorkloadId, VmId]

  def optimize(
      workloadIds: Vector[WorkloadId],
      vmIds: Vector[VmId],
      config: SAConfig = SAConfig()
  ): Mapping =
    if workloadIds.isEmpty || vmIds.isEmpty then Map.empty
    else
      val initial: Mapping = workloadIds.map(w => w -> vmIds(0)).toMap
      val costFn: Mapping => Double = mapping =>
        val vmLoads = mapping.values.groupBy(identity).map((_, wIds) => wIds.size)
        vmLoads.map(load => math.max(0, load - 2).toDouble).sum
      val neighborFn: (Mapping, java.util.Random) => Mapping = (m, rng) =>
        val keys = m.keys.toVector
        val k    = keys(rng.nextInt(keys.size))
        m.updated(k, vmIds(rng.nextInt(vmIds.size)))
      val (best, _, _) = SimulatedAnnealing.solve(config, initial, costFn, neighborFn)
      best
