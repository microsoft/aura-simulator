// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.types.*

class CostModelPhase8Spec extends AnyFlatSpec with Matchers:

  "CostRates.awsSpot" should "produce lower CPU cost than awsM5" in {
    val spec     = ResourceSpec(PEs(2), MIPS(10000.0), MegaBytes(4096.0), Mbps(1000.0), MegaBytes(10000.0))
    val duration = SimTime(3600.0)
    val onDemand = VmCostCalculator.fromRates(CostRates.awsM5)(spec, duration)
    val spot     = VmCostCalculator.fromRates(CostRates.awsSpot)(spec, duration)

    spot.cpuCost.value should be < onDemand.cpuCost.value
    spot.total.value should be < onDemand.total.value
  }

  "CostRates.awsReserved" should "be between spot and on-demand" in {
    val spec     = ResourceSpec(PEs(2), MIPS(10000.0), MegaBytes(4096.0), Mbps(1000.0), MegaBytes(10000.0))
    val duration = SimTime(3600.0)
    val onDemand = VmCostCalculator.fromRates(CostRates.awsM5)(spec, duration)
    val reserved = VmCostCalculator.fromRates(CostRates.awsReserved)(spec, duration)
    val spot     = VmCostCalculator.fromRates(CostRates.awsSpot)(spec, duration)

    reserved.cpuCost.value should be < onDemand.cpuCost.value
    reserved.cpuCost.value should be > spot.cpuCost.value
  }

  "Per-second billing granularity" should "differ from hourly billing for short-lived VMs" in {
    val spec     = ResourceSpec(PEs(2), MIPS(10000.0), MegaBytes(4096.0), Mbps(1000.0), MegaBytes(10000.0))
    val duration = SimTime(60.0) // 1 minute

    val hourly = VmCostCalculator.fromRates(CostRates.awsM5)(spec, duration)
    val perSecond = VmCostCalculator.fromRates(
      CostRates.awsM5.copy(billingGranularity = SimTime(1.0))
    )(spec, duration)

    // Hourly billing rounds up to 1 hour; per-second bills exactly 60s
    hourly.total.value should be > perSecond.total.value
    // Hourly cost should be ~60x per-second cost for 60s of usage
    hourly.total.value shouldBe perSecond.total.value * 60.0 +- 0.001
  }

  "MigrationCostCalculator" should "compute transfer cost from data and rates" in {
    val rates = CostRates.awsM5.copy(perTransferMB = Cost(0.01))
    val data  = MegaBytes(1024.0)
    val cost  = MigrationCostCalculator.fromRates(data, rates)

    cost.value shouldBe 10.24 +- 0.001
  }
