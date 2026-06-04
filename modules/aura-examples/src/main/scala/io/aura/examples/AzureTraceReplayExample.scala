// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.examples

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.serverless.*
import io.aura.traces.AzureFunctionsTrace
import io.aura.traces.AzureFunctionsTrace.{FunctionInvocationProfile, TriggerType}

/** Trace-driven serverless validation using the Azure Functions 2019 dataset.
  *
  * Replays 15 representative HTTP-triggered functions (5 high / 5 mid / 5 low frequency) from real production traces
  * through the Aura FaaS simulator with cold start modeling.
  *
  * Usage: AzureTraceReplayExample <invocations.csv> [durations.csv]
  */
object AzureTraceReplayExample:

  def main(args: Array[String]): Unit =
    if args == null || args.isEmpty then
      println("Usage: AzureTraceReplayExample <invocations.csv> [durations.csv]")
      sys.exit(1)

    run(args(0), if args.length > 1 then Some(args(1)) else None)

  def run(invocationPath: String, durationPath: Option[String]): Unit =
    println("=" * 70)
    println("Aura Cloud Simulator - Azure Functions Trace Replay")
    println("=" * 70)

    // Parse invocation profiles
    println(s"\nReading invocation profiles from: $invocationPath")
    val profiles = AzureFunctionsTrace.readInvocationProfiles(invocationPath) match
      case Right(ps) => ps
      case Left(err) =>
        println(s"Error reading invocations: $err")
        sys.exit(1)

    // Filter HTTP-triggered functions with non-zero invocations
    val httpProfiles = profiles
      .filter(_.trigger == TriggerType.Http)
      .filter(_.totalInvocations > 0)
      .sortBy(-_.totalInvocations)

    println(s"Total profiles: ${profiles.size}")
    println(s"HTTP-triggered with invocations: ${httpProfiles.size}")

    // Select 15 representative functions: 5 high / 5 mid / 5 low frequency
    // Use invocation-count bands to avoid extreme outliers (top functions have 80M+
    // invocations/day which would overwhelm single-JVM simulation).
    val highBand = httpProfiles.filter(p => p.totalInvocations >= 10000 && p.totalInvocations <= 50000)
    val midBand  = httpProfiles.filter(p => p.totalInvocations >= 100 && p.totalInvocations < 10000)
    val lowBand  = httpProfiles.filter(p => p.totalInvocations >= 1 && p.totalInvocations < 100)

    val selected: Vector[FunctionInvocationProfile] =
      highBand.take(5) ++ midBand.take(5) ++ lowBand.take(5)

    println(s"\nSelected ${selected.size} functions for replay:")
    selected.zipWithIndex.foreach { case (p, i) =>
      val tier = if i < 5 then "HIGH" else if i < 10 then "MID " else "LOW "
      println(f"  [$tier] ${p.functionKey}%-60s invocations=${p.totalInvocations}%8d active_min=${p.activeMinutes}%5d")
    }

    // Convert to arrival times
    val arrivalsByFunction: Vector[(String, Vector[SimTime])] = selected.zipWithIndex.map { case (p, i) =>
      val arrivals = AzureFunctionsTrace.toArrivalTimes(p, seed = 42L + i)
      (s"func-$i", arrivals)
    }

    // Build simulation: 24-hour replay with .NET runtime, 600s TTL
    val config = simulation("azure-trace-replay", endTime = SimTime(86400.0)) {
      faasPlatform("azure-functions") {
        coldStartModel(ColdStartModel.byRuntime)
        billingModel(BillingModel.awsLambda)
        containerTtl(SimTime(600.0))

        arrivalsByFunction.foreach { case (name, _) =>
          function(name) {
            runtime(Runtime.DotNet)
            memory(MegaBytes(256.0))
            timeout(SimTime(30.0))
          }
        }
      }

      serverlessBroker("trace-replay") {
        arrivalsByFunction.foreach { case (name, arrivals) =>
          invocations(
            name,
            count = arrivals.size,
            executionLength = MI(1000.0),
            arrivalPattern = ArrivalPattern.trace(arrivals)
          )
        }
      }
    }

    val totalInvocations = arrivalsByFunction.map(_._2.size).sum
    println(s"\nSimulation: 24h replay, ${selected.size} functions, $totalInvocations total invocations")
    println("Starting simulation...")

    config.run() match
      case Right(results) =>
        println("\nSimulation completed!")
        println(s"End time: ${results.simulationEndTime.value}s")
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
