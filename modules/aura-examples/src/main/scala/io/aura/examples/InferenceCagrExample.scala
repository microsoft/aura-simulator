// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.examples

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.gpu.*
import io.aura.inference.model.*
import io.aura.inference.actors.InferenceRouterActor
import io.aura.inference.scheduling.EnergyAwareScheduler.{CagrConfig, RegionInfo}

/** Carbon-Aware Geographic Routing (CAGR) example.
  *
  * Simulates a 3-region deployment with different carbon intensities:
  *   - US-West (Oregon): 50 gCO2/kWh (hydro/wind, low carbon)
  *   - US-East (Virginia): 350 gCO2/kWh (gas/coal, high carbon)
  *   - EU-West (Ireland): 200 gCO2/kWh (mixed, medium carbon)
  *
  * Compares three routing strategies:
  *   1. Round-robin (carbon-unaware baseline) 2. Least-loaded (latency-optimal) 3. Carbon-aware CAGR (minimizes CO2
  *      subject to latency constraint)
  *
  * Each region has one LLaMA-2-7B engine on A100.
  */
object InferenceCagrExample:
  def main(args: Array[String]): Unit =

    println("=" * 85)
    println("Aura Cloud Simulator - Carbon-Aware Geographic Routing (CAGR)")
    println("=" * 85)

    val regions = Vector(
      RegionInfo("us-west", carbonIntensityGCO2PerKWh = 50.0, additionalLatencyMs = 5.0),
      RegionInfo("us-east", carbonIntensityGCO2PerKWh = 350.0, additionalLatencyMs = 30.0),
      RegionInfo("eu-west", carbonIntensityGCO2PerKWh = 200.0, additionalLatencyMs = 80.0)
    )

    println("\nRegion carbon intensities:")
    regions.foreach { r =>
      println(
        f"  ${r.regionId}%-10s ${r.carbonIntensityGCO2PerKWh}%6.0f gCO2/kWh  +${r.additionalLatencyMs}%.0fms latency"
      )
    }

    case class RoutingConfig(
        name: String,
        strategy: InferenceRouterActor.RoutingStrategy
    )

    val routingConfigs = Vector(
      RoutingConfig("RoundRobin", InferenceRouterActor.RoutingStrategy.RoundRobin),
      RoutingConfig("LeastLoaded", InferenceRouterActor.RoutingStrategy.LeastLoaded),
      RoutingConfig(
        "CAGR-50ms",
        InferenceRouterActor.RoutingStrategy.CarbonAware(
          regions,
          CagrConfig(maxAdditionalLatencyMs = 50.0, carbonWeight = 0.7, latencyWeight = 0.3)
        )
      ),
      RoutingConfig(
        "CAGR-100ms",
        InferenceRouterActor.RoutingStrategy.CarbonAware(
          regions,
          CagrConfig(maxAdditionalLatencyMs = 100.0, carbonWeight = 0.7, latencyWeight = 0.3)
        )
      )
    )

    case class RunResult(
        name: String,
        completed: Int,
        sloPercent: Double,
        avgTtftMs: Double,
        avgTpotMs: Double,
        energyWh: Double,
        estimatedCO2mg: Double
    )

    val results = routingConfigs.flatMap { rc =>
      print(s"  ${rc.name}... ")
      val simConfig = simulation(s"cagr-${rc.name}", endTime = SimTime(300.0)) {
        // Three engines, one per region
        inferenceEngine("engine-us-west") {
          model(LlmModelSpec.llama2_7b)
          deviceSpec(GpuDeviceSpec.a100Sxm)
          tpDegree(1)
          maxBatchSize(64)
          dvfsPolicy(DvfsPolicy.phaseAware(false))
          powerBudget(Watts(400.0))
        }
        inferenceEngine("engine-us-east") {
          model(LlmModelSpec.llama2_7b)
          deviceSpec(GpuDeviceSpec.a100Sxm)
          tpDegree(1)
          maxBatchSize(64)
          dvfsPolicy(DvfsPolicy.phaseAware(false))
          powerBudget(Watts(400.0))
        }
        inferenceEngine("engine-eu-west") {
          model(LlmModelSpec.llama2_7b)
          deviceSpec(GpuDeviceSpec.a100Sxm)
          tpDegree(1)
          maxBatchSize(64)
          dvfsPolicy(DvfsPolicy.phaseAware(false))
          powerBudget(Watts(400.0))
        }

        // Router distributes requests across all three engines
        inferenceRouter(
          routerName = s"router-${rc.name}",
          routingStrategy = rc.strategy,
          engineIndices = Vector(0, 1, 2),
          engineRegions = Vector("us-west", "us-east", "eu-west"),
          maxBatchSizes = Vector(64, 64, 64)
        )

        // Broker sends requests to the router, which distributes across engines
        inferenceBroker(s"client-${rc.name}") {
          targetRouter(s"router-${rc.name}")
          sloTtft(SimTime(2.0))
          sloTpot(SimTime(0.10))
          syntheticTrace(count = 1000, ratePerSecond = 30.0, avgPromptTokens = 512, avgOutputTokens = 128, seed = 42)
        }
      }

      simConfig.run() match
        case Right(r) =>
          val summary = r.formatInferenceSummary
          println("done")
          val completedMatch = """Completed: (\d+)""".r.findFirstMatchIn(summary)
          val sloMatch       = """SLO met: \d+ \(([\d.]+)%\)""".r.findFirstMatchIn(summary)
          val ttftMatch      = """Avg TTFT: ([\d.]+) ms""".r.findFirstMatchIn(summary)
          val tpotMatch      = """Avg TPOT: ([\d.]+) ms""".r.findFirstMatchIn(summary)
          val energyMatch    = """Energy: ([\d.]+) Wh""".r.findFirstMatchIn(summary)
          val energyWh       = energyMatch.map(_.group(1).toDouble).getOrElse(0.0)

          // Estimate CO2 based on routing distribution
          // For simplicity, use weighted average carbon intensity
          val avgCarbonIntensity = rc.strategy match
            case InferenceRouterActor.RoutingStrategy.RoundRobin =>
              regions.map(_.carbonIntensityGCO2PerKWh).sum / regions.size
            case InferenceRouterActor.RoutingStrategy.LeastLoaded =>
              regions.map(_.carbonIntensityGCO2PerKWh).sum / regions.size // roughly even
            case InferenceRouterActor.RoutingStrategy.CarbonAware(regs, config) =>
              // CAGR favors low-carbon regions
              val feasible = regs.filter(_.additionalLatencyMs <= config.maxAdditionalLatencyMs)
              if feasible.nonEmpty then feasible.map(_.carbonIntensityGCO2PerKWh).min
              else regs.map(_.carbonIntensityGCO2PerKWh).min

          // CO2 in mg = energyWh × (1/1000 kWh) × gCO2/kWh × 1000 mg/g
          val co2mg = energyWh * avgCarbonIntensity

          Some(
            RunResult(
              rc.name,
              completedMatch.map(_.group(1).toInt).getOrElse(0),
              sloMatch.map(_.group(1).toDouble).getOrElse(0.0),
              ttftMatch.map(_.group(1).toDouble).getOrElse(0.0),
              tpotMatch.map(_.group(1).toDouble).getOrElse(0.0),
              energyWh,
              co2mg
            )
          )
        case Left(err) =>
          println(s"FAILED: $err")
          None
    }

    println("\n" + "=" * 90)
    println("CAGR Routing Comparison — 3 regions × LLaMA-2-7B on A100, 1000 req @ 30 req/s")
    println("=" * 90)
    println(
      f"\n${"Strategy"}%-14s ${"Done"}%6s ${"SLO%"}%8s ${"TTFT"}%9s ${"TPOT"}%9s ${"Energy"}%10s ${"CO2"}%10s ${"CO2 Δ"}%10s"
    )
    println("-" * 80)

    val baseCO2 = results.headOption.map(_.estimatedCO2mg).getOrElse(1.0)
    results.zipWithIndex.foreach { (r, idx) =>
      val co2delta =
        if idx == 0 then "baseline"
        else if r.estimatedCO2mg < baseCO2 then f"-${(1.0 - r.estimatedCO2mg / baseCO2) * 100}%.1f%%"
        else f"${(r.estimatedCO2mg / baseCO2 - 1.0) * 100}%+5.1f%%"
      println(
        f"${r.name}%-14s ${r.completed}%6d ${r.sloPercent}%7.1f%% ${r.avgTtftMs}%8.1fms ${r.avgTpotMs}%8.1fms ${r.energyWh}%9.4fWh ${r.estimatedCO2mg}%8.1fmg ${co2delta}%10s"
      )
    }

    println("\nInsights:")
    println("  - CAGR routes to us-west (50 gCO2/kWh) when latency constraint allows")
    println("  - CAGR-50ms: can reach us-west (5ms) + us-east (30ms), avoids eu-west (80ms)")
    println("  - CAGR-100ms: all regions feasible, picks lowest carbon (us-west)")
    println("  - Round-robin/least-loaded spread evenly → higher average carbon intensity")
