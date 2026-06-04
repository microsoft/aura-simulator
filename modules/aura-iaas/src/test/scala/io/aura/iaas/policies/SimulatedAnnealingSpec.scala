// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.types.*

class SimulatedAnnealingSpec extends AnyFlatSpec with Matchers:

  "SimulatedAnnealing.solve" should "converge to a better solution than initial" in {
    // Simple optimization: find x that minimizes (x - 5)^2
    val config = SAConfig(
      initialTemperature = 100.0,
      coolingRate = 0.01,
      coldTemperature = 0.001,
      searchesPerIteration = 5,
      seed = 42L
    )

    val (best, bestCost, iterations) = SimulatedAnnealing.solve[Double](
      config,
      initialSolution = 0.0,
      costFunction = x => (x - 5.0) * (x - 5.0),
      neighborFunction = (x, rng) => x + (rng.nextDouble() - 0.5) * 2.0
    )

    best shouldBe 5.0 +- 1.0
    bestCost should be < 25.0 // Should be better than initial cost of 25
    iterations should be > 0
  }

  it should "be reproducible from the same seed" in {
    val config = SAConfig(seed = 42L)

    val (best1, cost1, _) = SimulatedAnnealing.solve[Double](
      config,
      initialSolution = 0.0,
      costFunction = x => (x - 5.0) * (x - 5.0),
      neighborFunction = (x, rng) => x + (rng.nextDouble() - 0.5) * 2.0
    )

    val (best2, cost2, _) = SimulatedAnnealing.solve[Double](
      config,
      initialSolution = 0.0,
      costFunction = x => (x - 5.0) * (x - 5.0),
      neighborFunction = (x, rng) => x + (rng.nextDouble() - 0.5) * 2.0
    )

    best1 shouldBe best2
    cost1 shouldBe cost2
  }

  it should "handle integer optimization" in {
    // Find integer closest to 10
    val config = SAConfig(seed = 123L)

    val (best, bestCost, _) = SimulatedAnnealing.solve[Int](
      config,
      initialSolution = 0,
      costFunction = x => math.abs(x - 10).toDouble,
      neighborFunction = (x, rng) => x + rng.nextInt(5) - 2
    )

    math.abs(best - 10) should be <= 2
    bestCost should be < 10.0
  }

  "WorkloadVmMapping.optimize" should "distribute workloads across VMs" in {
    val workloads = (0 until 10).map(i => WorkloadId(i.toLong)).toVector
    val vms       = (0 until 5).map(i => VmId(i.toLong)).toVector

    val mapping = WorkloadVmMapping.optimize(workloads, vms)

    // All workloads should be mapped
    mapping.size shouldBe 10
    mapping.keys.toSet shouldBe workloads.toSet

    // All mapped VMs should be from the available VMs
    mapping.values.toSet.subsetOf(vms.toSet) shouldBe true

    // Check reasonable distribution (no VM should have all workloads)
    val vmLoads = mapping.values.groupBy(identity).map((_, wIds) => wIds.size)
    vmLoads.max should be <= 4 // With 10 workloads and 5 VMs, max ~2-3
  }

  it should "handle empty inputs" in {
    WorkloadVmMapping.optimize(Vector.empty, Vector(VmId(0))) shouldBe empty
    WorkloadVmMapping.optimize(Vector(WorkloadId(0)), Vector.empty) shouldBe empty
  }
