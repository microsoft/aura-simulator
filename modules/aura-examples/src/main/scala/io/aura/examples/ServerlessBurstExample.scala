// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.examples

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.serverless.*

/** Serverless burst traffic example.
  *
  * Tests concurrency limits with burst arrival patterns. Shows throttling during traffic spikes and cold start
  * clustering.
  */
object ServerlessBurstExample:
  def main(args: Array[String]): Unit =

    println("=" * 70)
    println("Aura Cloud Simulator - Serverless Burst Traffic Example")
    println("=" * 70)

    val config = simulation("serverless-burst", endTime = SimTime(300.0)) {
      faasPlatform("lambda") {
        coldStartModel(ColdStartModel.byRuntime)
        billingModel(BillingModel.awsLambda)
        containerTtl(SimTime(600.0))

        function("api-handler") {
          runtime(Runtime.Python)
          memory(MegaBytes(256.0))
          timeout(SimTime(30.0))
          concurrencyLimit(10) // Low limit to demonstrate throttling
        }
      }

      serverlessBroker("burst-client") {
        invocations(
          "api-handler",
          count = 100,
          executionLength = MI(2000.0),
          arrivalPattern = ArrivalPattern.burst(batchSize = 20, interval = SimTime(5.0))
        )
      }
    }

    println(s"\nPlatform: lambda with concurrency limit = 10")
    println(s"Arrival pattern: bursts of 20 every 5s (100 total)")
    println(s"Expected: ~50% throttled due to limit 10 < batch size 20")

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
        println()
        println(s"Throttled invocations: ${results.throttledInvocations.size}")
        println(s"Timed out invocations: ${results.timedOutInvocations.size}")
      case Left(error) =>
        println(s"\nSimulation failed: $error")
