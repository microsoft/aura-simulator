// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import io.aura.core.types.*

/** Per-resource cost rates ($/hour). */
final case class CostRates(
    perCpuPerHour: Cost = Cost(0.0),
    perRamMBPerHour: Cost = Cost(0.0),
    perBwMbpsPerHour: Cost = Cost(0.0),
    perStorageMBPerHour: Cost = Cost(0.0),
    perTransferMB: Cost = Cost(0.0),
    billingGranularity: SimTime = SimTime(3600.0)
)

object CostRates:
  /** AWS m5.large-like pricing (approximate). */
  val awsM5: CostRates = CostRates(
    perCpuPerHour = Cost(0.048),
    perRamMBPerHour = Cost(0.000006),
    perBwMbpsPerHour = Cost(0.0),
    perStorageMBPerHour = Cost(0.0000001)
  )

  /** AWS Spot pricing (~70% discount on CPU vs on-demand). */
  val awsSpot: CostRates = CostRates(
    perCpuPerHour = Cost(0.0144),
    perRamMBPerHour = Cost(0.000006),
    perBwMbpsPerHour = Cost(0.0),
    perStorageMBPerHour = Cost(0.0000001)
  )

  /** AWS Reserved pricing (~37% discount on CPU vs on-demand). */
  val awsReserved: CostRates = CostRates(
    perCpuPerHour = Cost(0.030),
    perRamMBPerHour = Cost(0.000006),
    perBwMbpsPerHour = Cost(0.0),
    perStorageMBPerHour = Cost(0.0000001)
  )

  val zero: CostRates = CostRates()

/** VM cost calculation as a pure function. */
type VmCostCalculator = (ResourceSpec, SimTime) => VmCostBreakdown

final case class VmCostBreakdown(
    cpuCost: Cost,
    ramCost: Cost,
    bwCost: Cost,
    storageCost: Cost
):
  def total: Cost = cpuCost + ramCost + bwCost + storageCost

object VmCostCalculator:
  def fromRates(rates: CostRates): VmCostCalculator =
    (spec, duration) =>
      val granularity = rates.billingGranularity.value
      val billedSeconds =
        if granularity > 0 then math.ceil(duration.value / granularity) * granularity
        else duration.value
      val hours = billedSeconds / 3600.0
      VmCostBreakdown(
        cpuCost = Cost(rates.perCpuPerHour.value * spec.pes.value * hours),
        ramCost = Cost(rates.perRamMBPerHour.value * spec.ram.value * hours),
        bwCost = Cost(rates.perBwMbpsPerHour.value * spec.bw.value * hours),
        storageCost = Cost(rates.perStorageMBPerHour.value * spec.storage.value * hours)
      )

/** Migration cost calculator: (dataTransferred, rates) => cost */
type MigrationCostCalculator = (MegaBytes, CostRates) => Cost

object MigrationCostCalculator:
  val fromRates: MigrationCostCalculator = (dataTransferred, rates) =>
    Cost(dataTransferred.value * rates.perTransferMB.value)
