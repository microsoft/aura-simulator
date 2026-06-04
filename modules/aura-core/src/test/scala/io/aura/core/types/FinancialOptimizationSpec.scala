// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FinancialOptimizationSpec extends AnyFlatSpec with Matchers:

  private val vmSpec       = ResourceSpec(PEs(4), MIPS(2000.0), MegaBytes(8192.0), Mbps(1000.0), MegaBytes(50000.0))
  private val onDemandRate = Cost(0.50) // $0.50/hr

  // ── PricingTier discounts ───────────────────────────────────────────────

  "PricingTier.discountRate" should "return 0 for on-demand" in {
    PricingTier.discountRate(PricingTier.OnDemand) shouldBe 0.0
  }

  it should "give deeper discount for longer RI term" in {
    val ri1yr = PricingTier.discountRate(PricingTier.Reserved(12))
    val ri3yr = PricingTier.discountRate(PricingTier.Reserved(36))
    ri3yr should be > ri1yr
  }

  it should "give extra discount for upfront payment" in {
    val noUpfront  = PricingTier.discountRate(PricingTier.Reserved(12, upfrontPct = 0.0))
    val allUpfront = PricingTier.discountRate(PricingTier.Reserved(12, upfrontPct = 1.0))
    allUpfront should be > noUpfront
  }

  it should "give ~70% discount for spot" in {
    PricingTier.discountRate(PricingTier.Spot()) shouldBe 0.70
  }

  // ── CostBreakdown ──────────────────────────────────────────────────────

  "CostBreakdown" should "compute total" in {
    val bd = CostBreakdown(Cost(10.0), Cost(5.0), Cost(3.0), Cost(2.0))
    bd.total.value shouldBe 20.0
  }

  it should "add breakdowns" in {
    val a   = CostBreakdown(Cost(10.0), Cost(5.0), Cost(3.0))
    val b   = CostBreakdown(Cost(20.0), Cost(10.0), Cost(7.0))
    val sum = a + b
    sum.compute.value shouldBe 30.0
    sum.storage.value shouldBe 15.0
  }

  it should "format nicely" in {
    val bd = CostBreakdown(Cost(100.0), Cost(50.0), Cost(25.0))
    bd.format should include("Total: $175.00")
  }

  // ── ReservedInstance ────────────────────────────────────────────────────

  "ReservedInstance" should "compute effective hourly rate" in {
    val ri = ReservedInstance(
      tier = PricingTier.Reserved(12),
      spec = vmSpec,
      count = 1,
      upfrontCost = Cost(1000.0),
      hourlyRate = Cost(0.10)
    )
    val effective = ri.effectiveHourlyRate
    effective.value should be > 0.10 // Includes amortized upfront
  }

  "RiUtilization" should "compute utilization percentage" in {
    val util = RiUtilization(totalRiHours = 730.0, usedRiHours = 600.0)
    util.utilizationPct shouldBe 82.19 +- 0.1
  }

  it should "compute wasted cost" in {
    val util   = RiUtilization(totalRiHours = 100.0, usedRiHours = 70.0)
    val wasted = util.wastedCost(Cost(0.10))
    wasted.value shouldBe 3.0 +- 0.01
  }

  // ── FinancialOptimizer.computeHourlyCost ────────────────────────────────

  "FinancialOptimizer.computeHourlyCost" should "apply discount for RI" in {
    val od = FinancialOptimizer.computeHourlyCost(vmSpec, onDemandRate, PricingTier.OnDemand)
    val ri = FinancialOptimizer.computeHourlyCost(vmSpec, onDemandRate, PricingTier.Reserved(12))
    ri.value should be < od.value
  }

  it should "apply spot discount" in {
    val spot = FinancialOptimizer.computeHourlyCost(vmSpec, onDemandRate, PricingTier.Spot())
    spot.value shouldBe 0.15 +- 0.01 // 70% off $0.50
  }

  // ── riSavings / spotSavings ─────────────────────────────────────────────

  "FinancialOptimizer.riSavings" should "compute RI savings percentage" in {
    val savings = FinancialOptimizer.riSavings(Cost(1000.0), Cost(600.0), 1.0)
    savings shouldBe 40.0 +- 0.1
  }

  it should "account for low RI utilization" in {
    val savings = FinancialOptimizer.riSavings(Cost(1000.0), Cost(600.0), 0.5)
    savings should be < 0.0 // Wasting money at 50% util with RI
  }

  "FinancialOptimizer.spotSavings" should "compute spot savings" in {
    FinancialOptimizer.spotSavings(Cost(1.0), Cost(0.30)) shouldBe 70.0 +- 0.1
  }

  // ── optimalMix ──────────────────────────────────────────────────────────

  "FinancialOptimizer.optimalMix" should "assign steady workloads to RI" in {
    val workloads = Vector(
      (vmSpec, 720.0, false), // Steady, full month → RI
      (vmSpec, 100.0, false), // Low usage → On-demand
      (vmSpec, 500.0, true)   // Flexible → Spot
    )
    val mix = FinancialOptimizer.optimalMix(workloads, onDemandRate)
    mix.values.sum shouldBe 3
    mix.getOrElse(PricingTier.Spot(), 0) shouldBe 1
  }

  // ── budgetStatus ────────────────────────────────────────────────────────

  "FinancialOptimizer.budgetStatus" should "return Normal for healthy spend" in {
    val status = FinancialOptimizer.budgetStatus(Cost(300.0), Cost(1000.0), 10, 30)
    status.level shouldBe BudgetAlertLevel.Normal
  }

  it should "return Exceeded when over budget" in {
    val status = FinancialOptimizer.budgetStatus(Cost(1100.0), Cost(1000.0), 30, 30)
    status.level shouldBe BudgetAlertLevel.Exceeded
  }

  it should "return Warning when approaching budget" in {
    val status = FinancialOptimizer.budgetStatus(Cost(850.0), Cost(1000.0), 25, 30)
    status.level shouldBe BudgetAlertLevel.Warning
  }

  // ── compareCosts ────────────────────────────────────────────────────────

  "FinancialOptimizer.compareCosts" should "compute savings amount and percentage" in {
    val (saved, pct) = FinancialOptimizer.compareCosts(Cost(1000.0), Cost(700.0))
    saved.value shouldBe 300.0
    pct shouldBe 30.0 +- 0.1
  }

  // ── wastedResourcesCost ─────────────────────────────────────────────────

  "FinancialOptimizer.wastedResourcesCost" should "compute cost of idle resources" in {
    val wasted = FinancialOptimizer.wastedResourcesCost(Cost(1000.0), 0.6)
    wasted.value shouldBe 400.0
  }
