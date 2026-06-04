// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.dsl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.core.engine.SimulationResults
import io.aura.dsl.SimulationDsl.*
import io.aura.iaas.policies.{CostRates, MigrationModel, VmAllocationPolicy, VmCostCalculator, WorkloadScheduler}

class Phase8IntegrationSpec extends AnyFlatSpec with Matchers:

  private def baseConfig(rates: Option[CostRates] = None) =
    simulation("cost-integration", endTime = SimTime(200.0)) {
      datacenter("dc") {
        allocationPolicy(VmAllocationPolicy.firstFit)
        scheduler(WorkloadScheduler.timeShared)
        host(
          pes = PEs(4),
          mips = MIPS(10000.0),
          ram = MegaBytes(16384.0),
          bw = Mbps(10000.0),
          storage = MegaBytes(1000000.0)
        )
      }
      broker("b") {
        rates.foreach(r => costRates(r))
        vm(pes = PEs(2), mips = MIPS(10000.0), ram = MegaBytes(4096.0))
        workload(length = MI(5000.0))
      }
    }

  "Cost tracking" should "produce non-empty cost records when costRates configured" in {
    val config = baseConfig(Some(CostRates.awsM5))
    val result = config.run()
    result.isRight shouldBe true
    result.foreach { r =>
      r.costRecords should not be empty
      r.totalCost.value should be > 0.0
    }
  }

  it should "produce empty cost records without costRates" in {
    val config = baseConfig(None)
    val result = config.run()
    result.isRight shouldBe true
    result.foreach { r =>
      r.costRecords shouldBe empty
      r.totalCost.value shouldBe 0.0
    }
  }

  "Per-second billing" should "differ from hourly billing for short-lived VMs" in {
    val hourlyRates    = CostRates.awsM5 // default granularity = 3600s
    val perSecondRates = CostRates.awsM5.copy(billingGranularity = SimTime(1.0))

    val spec          = ResourceSpec(PEs(2), MIPS(10000.0), MegaBytes(4096.0), Mbps(1000.0), MegaBytes(10000.0))
    val shortDuration = SimTime(120.0) // 2 minutes

    val hourlyCost    = VmCostCalculator.fromRates(hourlyRates)(spec, shortDuration)
    val perSecondCost = VmCostCalculator.fromRates(perSecondRates)(spec, shortDuration)

    // Hourly billing rounds 120s up to 3600s, per-second bills exactly 120s
    hourlyCost.total.value should be > perSecondCost.total.value
  }

  "Spot pricing" should "produce lower total cost than on-demand" in {
    val spec     = ResourceSpec(PEs(4), MIPS(10000.0), MegaBytes(8192.0), Mbps(1000.0), MegaBytes(50000.0))
    val duration = SimTime(7200.0) // 2 hours

    val onDemandCost = VmCostCalculator.fromRates(CostRates.awsM5)(spec, duration)
    val spotCost     = VmCostCalculator.fromRates(CostRates.awsSpot)(spec, duration)

    spotCost.total.value should be < onDemandCost.total.value
  }

  "PostCopy migration" should "produce lower downtime than preCopy" in {
    val params       = io.aura.iaas.policies.MigrationParams(MegaBytes(4096.0), Mbps(10000.0))
    val preCopyPlan  = MigrationModel.preCopy(params)
    val postCopyPlan = MigrationModel.postCopy()(params)

    postCopyPlan.downtime.value should be < preCopyPlan.downtime.value
    postCopyPlan.totalDataTransferred.value shouldBe preCopyPlan.totalDataTransferred.value +- 0.01
  }

  "NonLive migration" should "produce downtime equal to total time" in {
    val params = io.aura.iaas.policies.MigrationParams(MegaBytes(2048.0), Mbps(10000.0))
    val plan   = MigrationModel.nonLive()(params)

    plan.downtime.value shouldBe plan.totalTime.value +- 0.001
    plan.totalTime.value should be > 0.0
  }

  "formatUnifiedCostReport" should "aggregate VM + migration costs" in {
    val results = SimulationResults.empty.copy(
      costRecords = Vector(
        io.aura.core.engine.VmCostRecord(
          VmId(0),
          Cost(1.0),
          Cost(0.5),
          Cost(0.0),
          Cost(0.1),
          Cost(1.6),
          SimTime(100.0)
        )
      ),
      migrationRecords = Vector(
        io.aura.core.engine.MigrationRecord(
          VmId(0),
          HostId(0),
          HostId(1),
          SimTime(50.0),
          SimTime(1.0),
          MegaBytes(1024.0)
        )
      )
    )

    val report = results.formatUnifiedCostReport(
      perTransferMB = Cost(0.01),
      faasRatePerGBs = Cost.Zero
    )

    report should include("VM Resource Costs:")
    report should include("Migration BW Costs:")
    report should include("Grand Total:")
    // VM cost = 1.6, migration = 1024 * 0.01 = 10.24, total = 11.84
    report should include("11.84")
  }

  "Network-aware migration" should "use topology bandwidth when configured" in {
    val config = simulation("network-migration", endTime = SimTime(200.0)) {
      datacenter("dc-1") {
        allocationPolicy(VmAllocationPolicy.firstFit)
        scheduler(WorkloadScheduler.timeShared)
        hosts(
          count = 2,
          pes = PEs(4),
          mips = MIPS(10000.0),
          ram = MegaBytes(16384.0),
          bw = Mbps(10000.0),
          storage = MegaBytes(1000000.0)
        )
      }
      datacenter("dc-2") {
        host(
          pes = PEs(4),
          mips = MIPS(10000.0),
          ram = MegaBytes(16384.0),
          bw = Mbps(10000.0),
          storage = MegaBytes(1000000.0)
        )
      }
      network {
        link("dc-1", "dc-2", latency = SimTime(0.050), bandwidth = Mbps(10000.0))
      }
      broker("b") {
        vm(pes = PEs(2), mips = MIPS(10000.0), ram = MegaBytes(4096.0))
        workload(length = MI(5000.0))
      }
    }

    // Verify the topology is set
    config.networkTopology.links should have size 1
    config.networkTopology.bandwidth(DatacenterId(0), DatacenterId(1)).value shouldBe 10000.0

    // Run should succeed (backward compat)
    val result = config.run()
    result.isRight shouldBe true
  }
