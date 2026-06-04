// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.dsl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.iaas.policies.{VmAllocationPolicy, WorkloadScheduler}
import io.aura.power.PowerModel

class SimulationDslSpec extends AnyFlatSpec with Matchers:

  "SimulationDsl" should "create a valid configuration with the DSL" in {
    val config = simulation("test-sim", endTime = SimTime(500.0)) {
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

    config.endTime.value shouldBe 500.0
    config.datacenters should have size 1
    config.datacenters.head.hosts should have size 2
    config.brokers should have size 1
    config.brokers.head.vmRequests should have size 1
    config.brokers.head.workloads should have size 1
  }

  it should "support multiple datacenters and brokers" in {
    val config = simulation("multi", endTime = SimTime(1000.0)) {
      datacenter("dc-1") {
        host(pes = PEs(4), mips = MIPS(10000.0))
      }

      datacenter("dc-2") {
        hosts(count = 3, pes = PEs(8), mips = MIPS(20000.0))
      }

      broker("broker-1") {
        vm()
        workload()
      }

      broker("broker-2") {
        vms(count = 2)
        workloads(count = 4, length = MI(50000.0))
      }
    }

    config.datacenters should have size 2
    config.datacenters(0).hosts should have size 1
    config.datacenters(1).hosts should have size 3
    config.brokers should have size 2
    config.brokers(0).vmRequests should have size 1
    config.brokers(0).workloads should have size 1
    config.brokers(1).vmRequests should have size 2
    config.brokers(1).workloads should have size 4
  }

  it should "assign unique host IDs within a datacenter" in {
    val config = simulation("ids", endTime = SimTime(100.0)) {
      datacenter("dc-1") {
        hosts(count = 3, pes = PEs(4))
      }
    }

    val hostIds = config.datacenters.head.hosts.map(_.hostId.value)
    hostIds should have size 3
    hostIds.distinct should have size 3 // all unique
  }

  it should "assign unique workload IDs" in {
    val config = simulation("wl-ids", endTime = SimTime(100.0)) {
      datacenter("dc-1") {
        host()
      }
      broker("b") {
        vm()
        workloads(count = 5)
      }
    }

    val wlIds = config.brokers.head.workloads.map(_.id.value)
    wlIds should have size 5
    wlIds.distinct should have size 5
  }

  it should "support custom power models" in {
    val config = simulation("power", endTime = SimTime(100.0)) {
      datacenter("dc-1") {
        host(
          pes = PEs(4),
          powerModel = Some(PowerModel.hpProLiantG5)
        )
      }
      broker("b") {
        vm()
        workload()
      }
    }

    config.datacenters.head.hosts should have size 1
  }

  it should "implement SimulationConfig trait" in {
    val config: io.aura.core.engine.SimulationConfig = simulation("test", endTime = SimTime(100.0)) {
      datacenter("dc")(host())
      broker("b") { vm(); workload() }
    }

    config.endTime.value shouldBe 100.0
  }
