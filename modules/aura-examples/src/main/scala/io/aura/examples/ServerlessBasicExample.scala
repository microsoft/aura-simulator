// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.examples

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.serverless.*

/** Basic serverless simulation demonstrating Aura FaaS support.
  *
  * Two functions (Python + Java) with Poisson arrivals. Shows cold/warm distribution, billing, and concurrency.
  */
object ServerlessBasicExample:
  def main(args: Array[String]): Unit =

    println("=" * 70)
    println("Aura Cloud Simulator - Serverless Basic Example")
    println("=" * 70)

    val config = simulation("serverless-basic", endTime = SimTime(300.0)) {
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
          arrivalPattern = ArrivalPattern.poisson(rate = 10.0)
        )
      }
    }

    println(s"\nPlatform: lambda")
    println(s"Functions: ${config.faasPlatforms.head.functions.map(_.name).mkString(", ")}")
    println(s"Total invocations: ${config.serverlessBrokers.head.batches.map(_.count).sum}")
    println(s"Broker count: ${config.brokerCount}")

    println("\nStarting simulation...")
    config.run() match
      case Right(results) =>
        println("\nSimulation completed!")
        println(s"End time: ${results.simulationEndTime.value}")
        println(s"Total events: ${results.totalEventsProcessed}")
        println()
        println("=== Invocation Summary ===")
        println(results.formatInvocationSummary)
        println()
        println("=== Cold Start Report ===")
        println(results.formatColdStartReport)
        println()
        println("=== Billing Report ===")
        println(results.formatBillingReport)
        println()
        println("=== Concurrency Report ===")
        println(results.formatConcurrencyReport)
      case Left(error) =>
        println(s"\nSimulation failed: $error")
