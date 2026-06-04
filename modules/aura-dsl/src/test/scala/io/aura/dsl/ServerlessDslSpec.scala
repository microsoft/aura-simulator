// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.dsl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.serverless.*

class ServerlessDslSpec extends AnyFlatSpec with Matchers:

  "Serverless DSL" should "create a valid serverless configuration" in {
    val config = simulation("serverless-test", endTime = SimTime(300.0)) {
      faasPlatform("lambda") {
        coldStartModel(ColdStartModel.byRuntime)
        billingModel(BillingModel.awsLambda)
        containerTtl(SimTime(600.0))

        function("image-resize") {
          runtime(Runtime.Python)
          memory(MegaBytes(512.0))
          timeout(SimTime(30.0))
          concurrencyLimit(100)
        }

        function("api-handler") {
          runtime(Runtime.Java)
          memory(MegaBytes(256.0))
          timeout(SimTime(10.0))
        }
      }

      serverlessBroker("client") {
        invocations(
          "image-resize",
          count = 50,
          executionLength = MI(5000.0),
          arrivalPattern = ArrivalPattern.poisson(rate = 5.0)
        )
        invocations(
          "api-handler",
          count = 100,
          executionLength = MI(1000.0),
          arrivalPattern = ArrivalPattern.burst(batchSize = 20, interval = SimTime(5.0))
        )
      }
    }

    config.faasPlatforms should have size 1
    config.faasPlatforms.head.functions should have size 2
    config.serverlessBrokers should have size 1
    config.serverlessBrokers.head.batches should have size 2
  }

  it should "assign globally unique function IDs" in {
    val config = simulation("multi-platform", endTime = SimTime(100.0)) {
      faasPlatform("platform-1") {
        function("func-a")(runtime(Runtime.Python))
        function("func-b")(runtime(Runtime.Java))
      }
      faasPlatform("platform-2") {
        function("func-c")(runtime(Runtime.Go))
      }
      serverlessBroker("b") {
        invocations("func-a", count = 1)
      }
    }

    val allFuncIds = config.faasPlatforms.flatMap(_.functions.map(_.functionId.value))
    allFuncIds should have size 3
    allFuncIds.distinct should have size 3 // all unique
  }

  it should "include serverless brokers in brokerCount" in {
    val config = simulation("mixed", endTime = SimTime(100.0)) {
      faasPlatform("lambda") {
        function("f1")(runtime(Runtime.Python))
      }
      serverlessBroker("sb-1") {
        invocations("f1", count = 10)
      }
      serverlessBroker("sb-2") {
        invocations("f1", count = 5)
      }
    }

    config.brokerCount shouldBe 2 // 0 IaaS + 2 serverless
  }

  it should "support mixed IaaS and serverless" in {
    val config = simulation("mixed-workloads", endTime = SimTime(1000.0)) {
      datacenter("dc-1") {
        host(pes = PEs(4), mips = MIPS(10000.0))
      }
      broker("iaas-broker") {
        vm()
        workload()
      }
      faasPlatform("lambda") {
        function("api")(runtime(Runtime.Python))
      }
      serverlessBroker("faas-client") {
        invocations("api", count = 10)
      }
    }

    config.datacenters should have size 1
    config.brokers should have size 1
    config.faasPlatforms should have size 1
    config.serverlessBrokers should have size 1
    config.brokerCount shouldBe 2 // 1 IaaS + 1 serverless
  }

  it should "use default values for function configuration" in {
    val config = simulation("defaults", endTime = SimTime(100.0)) {
      faasPlatform("p") {
        function("f") {}
      }
      serverlessBroker("b") {
        invocations("f", count = 1)
      }
    }

    val func = config.faasPlatforms.head.functions.head
    func.runtime shouldBe Runtime.Python // default
    func.memoryMB.value shouldBe 256.0   // default
    func.timeout.value shouldBe 30.0     // default
    func.concurrencyLimit shouldBe 1000  // default
  }
