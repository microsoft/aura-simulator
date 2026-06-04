// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.dsl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.iaas.policies.{VmAllocationPolicy, WorkloadScheduler}
import io.aura.iaas.policies.{
  OverloadDetector as ConsOverload,
  UnderloadDetector as ConsUnderload,
  VmSelectionPolicy as ConsVmSelect
}
import io.aura.power.PowerModel

class SimulationDslPhase2Spec extends AnyFlatSpec with Matchers:

  "DSL" should "support migration policy configuration" in {
    val config = simulation("migration-test", endTime = SimTime(500.0)) {
      datacenter("dc-1") {
        allocationPolicy(VmAllocationPolicy.bestFit)
        scheduler(WorkloadScheduler.timeShared)
        consolidation(
          overloadDetector = ConsOverload.staticThreshold(0.8),
          underloadDetector = ConsUnderload.staticThreshold(0.2),
          vmSelector = ConsVmSelect.minimumMigrationTime
        )
        hosts(count = 2, pes = PEs(4), mips = MIPS(10000.0))
      }
      broker("b") {
        vm()
        workload()
      }
    }

    config.datacenters.head.consolidation shouldBe defined
    config.datacenters.head.migrationModel shouldBe defined
  }

  it should "support network topology configuration" in {
    val config = simulation("network-test", endTime = SimTime(500.0)) {
      datacenter("us-east") {
        host(pes = PEs(4), mips = MIPS(10000.0))
      }
      datacenter("eu-west") {
        host(pes = PEs(4), mips = MIPS(10000.0))
      }
      network {
        link("us-east", "eu-west", latency = SimTime(0.050), bandwidth = Mbps(10000.0))
      }
      broker("b") {
        vm()
        workload()
      }
    }

    config.networkTopology.links should have size 1
    config.networkTopology.delay(DatacenterId(0L), DatacenterId(1L)).value shouldBe 0.050 +- 0.001
  }

  it should "support CFS scheduler with weighted workloads" in {
    val config = simulation("cfs-test", endTime = SimTime(500.0)) {
      datacenter("dc-1") {
        scheduler(WorkloadScheduler.cfs)
        host(pes = PEs(4), mips = MIPS(10000.0))
      }
      broker("b") {
        vm()
        workload(length = MI(10000.0), weight = 2048)
        workload(length = MI(10000.0), weight = 512)
      }
    }

    val weights = config.brokers.head.workloads.map(_.weight)
    weights should contain(2048)
    weights should contain(512)
  }

  it should "support .run() convenience method" in {
    val config = simulation("run-test", endTime = SimTime(100.0)) {
      datacenter("dc") {
        host(
          pes = PEs(4),
          mips = MIPS(10000.0),
          ram = MegaBytes(16384.0),
          bw = Mbps(10000.0),
          storage = MegaBytes(1000000.0)
        )
      }
      broker("b") {
        vm(pes = PEs(2), mips = MIPS(10000.0), ram = MegaBytes(4096.0))
        workload(length = MI(5000.0))
      }
    }

    val result = config.run()
    result.isRight shouldBe true
    result.foreach { r =>
      r.workloadResults should have size 1
    }
  }

  it should "produce energy data in results" in {
    // Workload must run longer than the default energy sampling interval (10s)
    // so that at least one energy sample fires before the simulation ends.
    val config = simulation("energy-test", endTime = SimTime(200.0)) {
      datacenter("dc") {
        scheduler(WorkloadScheduler.timeShared)
        host(
          pes = PEs(4),
          mips = MIPS(10000.0),
          ram = MegaBytes(16384.0),
          bw = Mbps(10000.0),
          storage = MegaBytes(1000000.0),
          powerModel = Some(PowerModel.linear(Watts(120.0), Watts(70.0)))
        )
      }
      broker("b") {
        vm(pes = PEs(2), mips = MIPS(10000.0), ram = MegaBytes(4096.0))
        workload(length = MI(200000.0))
      }
    }

    val result = config.run()
    result.isRight shouldBe true
    result.foreach { r =>
      r.totalEnergyWh.value should be > 0.0
      r.energyRecords should not be empty
    }
  }

  it should "format summary correctly" in {
    val config = simulation("summary-test", endTime = SimTime(100.0)) {
      datacenter("dc") {
        host(
          pes = PEs(4),
          mips = MIPS(10000.0),
          ram = MegaBytes(16384.0),
          bw = Mbps(10000.0),
          storage = MegaBytes(1000000.0)
        )
      }
      broker("b") {
        vm(pes = PEs(2), mips = MIPS(10000.0), ram = MegaBytes(4096.0))
        workload(length = MI(10000.0))
      }
    }

    val result = config.run()
    result.foreach { r =>
      r.formatSummary should include("Completed: 1 workloads")
      r.formatSummary should include("Energy:")
      r.formatSummary should include("Migrations:")
    }
  }

  it should "produce valid JSON output" in {
    val config = simulation("json-test", endTime = SimTime(100.0)) {
      datacenter("dc") {
        host(
          pes = PEs(4),
          mips = MIPS(10000.0),
          ram = MegaBytes(16384.0),
          bw = Mbps(10000.0),
          storage = MegaBytes(1000000.0)
        )
      }
      broker("b") {
        vm(pes = PEs(2), mips = MIPS(10000.0), ram = MegaBytes(4096.0))
        workload(length = MI(5000.0))
      }
    }

    val result = config.run()
    result.foreach { r =>
      val json = r.toJSON
      json should include("simulationEndTime")
      json should include("workloadResults")
      json should include("energyRecords")
      json should include("migrationRecords")
    }
  }

  it should "maintain backward compatibility with existing DSL" in {
    val config = simulation("compat", endTime = SimTime(100.0)) {
      datacenter("dc-1") {
        allocationPolicy(VmAllocationPolicy.firstFit)
        scheduler(WorkloadScheduler.timeShared)
        hosts(
          count = 2,
          pes = PEs(4),
          mips = MIPS(10000.0),
          ram = MegaBytes(8192.0),
          bw = Mbps(1000.0),
          storage = MegaBytes(100000.0)
        )
      }
      broker("broker-1") {
        vm(pes = PEs(2), mips = MIPS(5000.0), ram = MegaBytes(2048.0))
        workload(length = MI(10000.0), pes = PEs(1))
      }
    }

    config.endTime.value shouldBe 100.0
    config.datacenters should have size 1
    config.datacenters.head.hosts should have size 2
    config.brokers should have size 1
    config.networkTopology shouldBe NetworkTopology.empty
  }
