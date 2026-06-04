// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.dsl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.iaas.policies.{CostRates, MigrationModel}

class Phase8DslSpec extends AnyFlatSpec with Matchers:

  "DSL with costRates" should "pass costRates through to BrokerConfig" in {
    val config = simulation("cost-test", endTime = SimTime(100.0)) {
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
        costRates(CostRates.awsM5)
        vm(pes = PEs(2), mips = MIPS(10000.0), ram = MegaBytes(4096.0))
        workload(length = MI(5000.0))
      }
    }

    config.brokers.head.costRates shouldBe defined
    config.brokers.head.costRates.get shouldBe CostRates.awsM5
  }

  "DSL with BRITE topology" should "parse and integrate network topology" in {
    val briteContent =
      """Nodes: (2)
        |0	1.0	2.0
        |1	5.0	6.0
        |
        |Edges: (1)
        |0	0	1	0.050	10000.0
        |""".stripMargin

    val config = simulation("brite-test", endTime = SimTime(100.0)) {
      briteTopology(briteContent)
      datacenter("dc-1") {
        host(pes = PEs(4), mips = MIPS(10000.0), ram = MegaBytes(16384.0))
      }
      broker("b") {
        vm()
        workload()
      }
    }

    config.networkTopology.links should have size 1
    config.networkTopology.delay(DatacenterId(0), DatacenterId(1)).value shouldBe 0.050 +- 0.001
  }

  "DSL with postCopy migration" should "accept new migration model" in {
    val config = simulation("postcopy-test", endTime = SimTime(100.0)) {
      datacenter("dc") {
        migrationPolicy(
          overloadDetector = (_, _) => false,
          vmSelector = _ => None,
          migrationModel = MigrationModel.postCopy()
        )
        hosts(count = 2, pes = PEs(4), mips = MIPS(10000.0), ram = MegaBytes(16384.0))
      }
      broker("b") {
        vm()
        workload()
      }
    }

    config.datacenters.head.migrationModel shouldBe defined
  }

  "DSL with nonLive migration" should "accept cold migration model" in {
    val config = simulation("nonlive-test", endTime = SimTime(100.0)) {
      datacenter("dc") {
        migrationPolicy(
          overloadDetector = (_, _) => false,
          vmSelector = _ => None,
          migrationModel = MigrationModel.nonLive()
        )
        hosts(count = 2, pes = PEs(4), mips = MIPS(10000.0), ram = MegaBytes(16384.0))
      }
      broker("b") {
        vm()
        workload()
      }
    }

    config.datacenters.head.migrationModel shouldBe defined
  }

  "DSL with awsSpot rates" should "produce lower cost than awsM5" in {
    val spec     = ResourceSpec(PEs(2), MIPS(10000.0), MegaBytes(4096.0), Mbps(1000.0), MegaBytes(10000.0))
    val duration = SimTime(3600.0)

    val onDemand = io.aura.iaas.policies.VmCostCalculator.fromRates(CostRates.awsM5)(spec, duration)
    val spot     = io.aura.iaas.policies.VmCostCalculator.fromRates(CostRates.awsSpot)(spec, duration)

    spot.total.value should be < onDemand.total.value
  }
