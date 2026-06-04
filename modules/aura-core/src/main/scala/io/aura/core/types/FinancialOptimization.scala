// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

/** Financial optimization: pricing tiers, reserved instance tracking, budget alerting, cost projection, and tier-mix
  * optimization — modeled as pure functions and immutable state.
  */

/** Pricing tier for cloud resource billing. */
enum PricingTier:
  case OnDemand
  case Reserved(termMonths: Int, upfrontPct: Double = 0.0)
  case Spot(maxBidPct: Double = 0.7)
  case SavingsPlan(commitmentPerHour: Cost, termMonths: Int)

/** Discount rate for a pricing tier relative to on-demand. */
object PricingTier:
  def discountRate(tier: PricingTier): Double = tier match
    case PricingTier.OnDemand => 0.0
    case PricingTier.Reserved(term, upfrontPct) =>
      val baseDiscount = if term >= 36 then 0.60 else if term >= 12 then 0.40 else 0.20
      baseDiscount + upfrontPct * 0.10 // Extra discount for upfront payment
    case PricingTier.Spot(_) => 0.70
    case PricingTier.SavingsPlan(_, term) =>
      if term >= 36 then 0.55 else 0.35

/** Cost breakdown by category. */
final case class CostBreakdown(
    compute: Cost,
    storage: Cost,
    network: Cost,
    licensing: Cost = Cost.Zero
):
  def total: Cost = Cost(compute.value + storage.value + network.value + licensing.value)

  def +(other: CostBreakdown): CostBreakdown = CostBreakdown(
    compute = Cost(compute.value + other.compute.value),
    storage = Cost(storage.value + other.storage.value),
    network = Cost(network.value + other.network.value),
    licensing = Cost(licensing.value + other.licensing.value)
  )

  def format: String =
    f"Compute: $$${compute.value}%.2f | Storage: $$${storage.value}%.2f | " +
      f"Network: $$${network.value}%.2f | License: $$${licensing.value}%.2f | " +
      f"Total: $$${total.value}%.2f"

object CostBreakdown:
  val zero: CostBreakdown = CostBreakdown(Cost.Zero, Cost.Zero, Cost.Zero)

/** Reserved instance coverage tracking. */
final case class ReservedInstance(
    tier: PricingTier.Reserved,
    spec: ResourceSpec,
    count: Int,
    upfrontCost: Cost,
    hourlyRate: Cost
):
  /** Total cost over the reservation term. */
  def totalCost: Cost =
    Cost(upfrontCost.value + hourlyRate.value * tier.termMonths * 730.0) // ~730 hrs/month

  /** Effective hourly rate including amortized upfront. */
  def effectiveHourlyRate: Cost =
    val totalHours = tier.termMonths * 730.0
    if totalHours <= 0 then Cost.Zero
    else Cost((upfrontCost.value / totalHours) + hourlyRate.value)

/** RI utilization over a time period. */
final case class RiUtilization(
    totalRiHours: Double,
    usedRiHours: Double
):
  def utilizationPct: Double =
    if totalRiHours <= 0 then 0.0
    else usedRiHours / totalRiHours * 100.0

  def wastedCost(hourlyRate: Cost): Cost =
    Cost((totalRiHours - usedRiHours) * hourlyRate.value)

/** Budget alert levels. */
enum BudgetAlertLevel:
  case Normal, Warning, Critical, Exceeded

/** Budget status with alert. */
final case class BudgetStatus(
    spent: Cost,
    budget: Cost,
    level: BudgetAlertLevel,
    utilizationPct: Double,
    projectedOverrun: Cost
):
  def format: String =
    val status = level match
      case BudgetAlertLevel.Normal   => "OK"
      case BudgetAlertLevel.Warning  => "WARNING"
      case BudgetAlertLevel.Critical => "CRITICAL"
      case BudgetAlertLevel.Exceeded => "EXCEEDED"
    f"Budget: $status (${utilizationPct}%.1f%%) - Spent $$${spent.value}%.2f / $$${budget.value}%.2f"

/** Pure-function financial optimizer. */
object FinancialOptimizer:

  /** Compute hourly cost for a resource spec under a given pricing tier. */
  def computeHourlyCost(
      spec: ResourceSpec,
      onDemandRate: Cost,
      tier: PricingTier
  ): Cost =
    val discount = PricingTier.discountRate(tier)
    Cost(onDemandRate.value * (1.0 - discount))

  /** Compute total cost over a duration. */
  def computeTotalCost(
      spec: ResourceSpec,
      onDemandHourlyRate: Cost,
      tier: PricingTier,
      durationHours: Double
  ): Cost =
    val hourly = computeHourlyCost(spec, onDemandHourlyRate, tier)
    Cost(hourly.value * durationHours)

  /** Calculate savings from reserved instances vs on-demand. */
  def riSavings(
      onDemandCost: Cost,
      riTotalCost: Cost,
      riUtilization: Double
  ): Double =
    if onDemandCost.value <= 0 then 0.0
    else
      val effectiveRiCost = riTotalCost.value / math.max(0.01, riUtilization)
      (1.0 - effectiveRiCost / onDemandCost.value) * 100.0

  /** Calculate savings from spot pricing. */
  def spotSavings(onDemandCost: Cost, spotCost: Cost): Double =
    if onDemandCost.value <= 0 then 0.0
    else (1.0 - spotCost.value / onDemandCost.value) * 100.0

  /** Recommend optimal tier mix for a set of workloads and budget. Steady workloads → Reserved, Flexible → Spot, Rest →
    * OnDemand.
    * @param workloads
    *   (spec, hoursPerMonth, isFlexible)
    */
  def optimalMix(
      workloads: Vector[(ResourceSpec, Double, Boolean)],
      onDemandHourlyRate: Cost
  ): Map[PricingTier, Int] =
    val steady    = workloads.count((_, hours, flex) => hours >= 500 && !flex)
    val flexible  = workloads.count((_, _, flex) => flex)
    val remaining = workloads.size - steady - flexible
    Map(
      PricingTier.Reserved(12) -> steady,
      PricingTier.Spot()       -> flexible,
      PricingTier.OnDemand     -> remaining
    ).filter(_._2 > 0)

  /** Project monthly cost from current usage and growth rate. */
  def projectedMonthlyCost(
      currentDailyRate: Cost,
      daysRemaining: Int,
      growthRatePct: Double = 0.0
  ): Cost =
    val dailyGrowth = 1.0 + growthRatePct / 100.0
    val projected = (0 until daysRemaining).map { day =>
      currentDailyRate.value * math.pow(dailyGrowth, day.toDouble)
    }.sum
    Cost(projected)

  /** Evaluate budget status and generate alert. */
  def budgetStatus(
      spent: Cost,
      budget: Cost,
      daysElapsed: Int,
      totalDays: Int
  ): BudgetStatus =
    val utilization    = if budget.value <= 0 then 100.0 else spent.value / budget.value * 100.0
    val expectedPct    = if totalDays <= 0 then 100.0 else daysElapsed.toDouble / totalDays * 100.0
    val burnRate       = if daysElapsed <= 0 then 0.0 else spent.value / daysElapsed
    val projectedTotal = Cost(burnRate * totalDays)
    val overrun        = Cost(math.max(0.0, projectedTotal.value - budget.value))

    val level =
      if utilization >= 100.0 then BudgetAlertLevel.Exceeded
      else if utilization >= 95.0 || (utilization > expectedPct * 1.2) then BudgetAlertLevel.Critical
      else if utilization >= 80.0 || (utilization > expectedPct * 1.1) then BudgetAlertLevel.Warning
      else BudgetAlertLevel.Normal

    BudgetStatus(spent, budget, level, utilization, overrun)

  /** Calculate cost of wasted resources. */
  def wastedResourcesCost(
      totalProvisionedCost: Cost,
      avgUtilization: Double
  ): Cost =
    Cost(totalProvisionedCost.value * (1.0 - avgUtilization))

  /** Compare two cost scenarios and return savings. */
  def compareCosts(
      baselineCost: Cost,
      optimizedCost: Cost
  ): (Cost, Double) =
    val saved = Cost(baselineCost.value - optimizedCost.value)
    val pct   = if baselineCost.value <= 0 then 0.0 else saved.value / baselineCost.value * 100.0
    (saved, pct)
