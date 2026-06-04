// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.types.*

class CostModelSpec extends AnyFlatSpec with Matchers:

  "CostRates.awsM5" should "compute expected costs for a VM" in {
    val spec       = ResourceSpec(PEs(2), MIPS(10000.0), MegaBytes(4096.0), Mbps(1000.0), MegaBytes(10000.0))
    val duration   = SimTime(3600.0) // 1 hour
    val calculator = VmCostCalculator.fromRates(CostRates.awsM5)
    val breakdown  = calculator(spec, duration)

    // CPU: 0.048 * 2 PEs * 1 hour = 0.096
    breakdown.cpuCost.value shouldBe 0.096 +- 0.001

    // RAM: 0.000006 * 4096 MB * 1 hour = 0.024576
    breakdown.ramCost.value shouldBe 0.024576 +- 0.001

    // BW: 0.0 * anything = 0.0
    breakdown.bwCost.value shouldBe 0.0

    // Storage: 0.0000001 * 10000 MB * 1 hour = 0.001
    breakdown.storageCost.value shouldBe 0.001 +- 0.0001

    // Total should be sum
    breakdown.total.value shouldBe (breakdown.cpuCost.value + breakdown.ramCost.value + breakdown.bwCost.value + breakdown.storageCost.value) +- 0.0001
  }

  "CostRates.zero" should "produce zero cost" in {
    val spec       = ResourceSpec(PEs(4), MIPS(20000.0), MegaBytes(8192.0), Mbps(10000.0), MegaBytes(100000.0))
    val duration   = SimTime(7200.0) // 2 hours
    val calculator = VmCostCalculator.fromRates(CostRates.zero)
    val breakdown  = calculator(spec, duration)

    breakdown.cpuCost.value shouldBe 0.0
    breakdown.ramCost.value shouldBe 0.0
    breakdown.bwCost.value shouldBe 0.0
    breakdown.storageCost.value shouldBe 0.0
    breakdown.total.value shouldBe 0.0
  }

  "VmCostCalculator" should "scale with duration" in {
    val spec       = ResourceSpec(PEs(2), MIPS(10000.0), MegaBytes(4096.0), Mbps(1000.0), MegaBytes(10000.0))
    val calculator = VmCostCalculator.fromRates(CostRates.awsM5)

    val cost1h = calculator(spec, SimTime(3600.0))
    val cost2h = calculator(spec, SimTime(7200.0))

    cost2h.total.value shouldBe cost1h.total.value * 2 +- 0.0001
  }

  it should "handle zero duration" in {
    val spec       = ResourceSpec(PEs(2), MIPS(10000.0), MegaBytes(4096.0), Mbps(1000.0), MegaBytes(10000.0))
    val calculator = VmCostCalculator.fromRates(CostRates.awsM5)
    val breakdown  = calculator(spec, SimTime.Zero)

    breakdown.total.value shouldBe 0.0
  }

  "Cost opaque type" should "support arithmetic" in {
    val c1 = Cost(10.0)
    val c2 = Cost(5.0)

    (c1 + c2).value shouldBe 15.0
    (c1 * 3.0).value shouldBe 30.0
    (c1 > c2) shouldBe true
    (c2 < c1) shouldBe true
  }

  it should "be orderable" in {
    val costs = Vector(Cost(5.0), Cost(1.0), Cost(3.0))
    costs.sorted.map(_.value) shouldBe Vector(1.0, 3.0, 5.0)
  }
