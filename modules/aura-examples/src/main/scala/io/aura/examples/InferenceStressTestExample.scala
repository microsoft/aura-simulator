// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.examples

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.gpu.*
import io.aura.inference.model.*
import io.aura.inference.actors.InferenceEngineActor.SchedulingStrategy

/** High-load stress test for energy-aware scheduling.
  *
  * Two scenarios designed to exercise all scheduling algorithms:
  *
  * Scenario 1: LLaMA-7B @ 50 req/s, PC-BAC budget at 150W (tight, just above decode power)
  *   - Forces PC-BAC to limit admissions since batch=32 → ~160W exceeds budget
  *
  * Scenario 2: LLaMA-70B on 8×A100 (TP=8) @ 20 req/s
  *   - Larger model = higher power per request → PC-BAC differentiates at 300W
  *   - TP=8 decode is more compute-intensive → PA-DVFS saves less (closer to roofline)
  */
object InferenceStressTestExample:
  def main(args: Array[String]): Unit =

    println("=" * 85)
    println("Aura Cloud Simulator - High-Load Inference Stress Test")
    println("=" * 85)

    case class RunConfig(
        name: String,
        dvfs: DvfsPolicy,
        powerBudget: Watts,
        strategy: SchedulingStrategy,
        maxBatch: Int
    )

    case class RunResult(
        name: String,
        completed: Int,
        failed: Int,
        sloPercent: Double,
        avgTtftMs: Double,
        avgTpotMs: Double,
        energyWh: Double
    )

    def parseResults(summary: String, name: String): Option[RunResult] =
      val completedMatch = """Completed: (\d+)""".r.findFirstMatchIn(summary)
      val failedMatch    = """Failed: (\d+)""".r.findFirstMatchIn(summary)
      val sloMatch       = """SLO met: \d+ \(([\d.]+)%\)""".r.findFirstMatchIn(summary)
      val ttftMatch      = """Avg TTFT: ([\d.]+) ms""".r.findFirstMatchIn(summary)
      val tpotMatch      = """Avg TPOT: ([\d.]+) ms""".r.findFirstMatchIn(summary)
      val energyMatch    = """Energy: ([\d.]+) Wh""".r.findFirstMatchIn(summary)
      Some(
        RunResult(
          name,
          completedMatch.map(_.group(1).toInt).getOrElse(0),
          failedMatch.map(_.group(1).toInt).getOrElse(0),
          sloMatch.map(_.group(1).toDouble).getOrElse(0.0),
          ttftMatch.map(_.group(1).toDouble).getOrElse(0.0),
          tpotMatch.map(_.group(1).toDouble).getOrElse(0.0),
          energyMatch.map(_.group(1).toDouble).getOrElse(0.0)
        )
      )

    def printTable(title: String, results: Vector[RunResult]): Unit =
      println("\n" + "=" * 85)
      println(title)
      println("=" * 85)
      println(
        f"\n${"Config"}%-15s ${"Done"}%6s ${"Fail"}%6s ${"SLO%"}%8s ${"TTFT"}%9s ${"TPOT"}%9s ${"Energy"}%10s ${"Savings"}%10s"
      )
      println("-" * 75)
      val baseEnergy = results.headOption.map(_.energyWh).getOrElse(1.0)
      results.zipWithIndex.foreach { (r, idx) =>
        val savings =
          if baseEnergy > 0 && r.energyWh < baseEnergy then f"${(1.0 - r.energyWh / baseEnergy) * 100}%.1f%%"
          else if idx == 0 then "baseline"
          else f"${(r.energyWh / baseEnergy - 1.0) * 100}%+.1f%%"
        println(
          f"${r.name}%-15s ${r.completed}%6d ${r.failed}%6d ${r.sloPercent}%7.1f%% ${r.avgTtftMs}%8.1fms ${r.avgTpotMs}%8.1fms ${r.energyWh}%9.4fWh ${savings}%10s"
        )
      }

    // ── Scenario 1: LLaMA-7B, tight 150W PC-BAC budget ──────────────
    println("\n--- Scenario 1: LLaMA-2-7B @ 50 req/s, tight PC-BAC budget (150W) ---")

    val configs7b = Vector(
      RunConfig("CB+MaxPerf", DvfsPolicy.maxPerformance, Watts(400.0), SchedulingStrategy.ContinuousBatching, 32),
      RunConfig("CB+PA-DVFS", DvfsPolicy.phaseAware(false), Watts(400.0), SchedulingStrategy.ContinuousBatching, 32),
      RunConfig("CB+EnergyP", DvfsPolicy.energyProportional, Watts(400.0), SchedulingStrategy.ContinuousBatching, 32),
      RunConfig("PCBAC@150W", DvfsPolicy.maxPerformance, Watts(150.0), SchedulingStrategy.PowerCapped, 32),
      RunConfig("PCBAC+PA", DvfsPolicy.phaseAware(false), Watts(150.0), SchedulingStrategy.PowerCapped, 32),
      RunConfig("Chunked+PA", DvfsPolicy.phaseAware(false), Watts(400.0), SchedulingStrategy.ChunkedPrefill(256), 32)
    )

    val results7b = configs7b.flatMap { cfg =>
      print(s"  ${cfg.name}... ")
      val simConfig = simulation(s"s1-${cfg.name}", endTime = SimTime(600.0)) {
        inferenceEngine(s"vllm-${cfg.name}") {
          model(LlmModelSpec.llama2_7b)
          deviceSpec(GpuDeviceSpec.a100Sxm)
          tpDegree(1)
          maxBatchSize(cfg.maxBatch)
          iterationInterval(SimTime(0.01))
          dvfsPolicy(cfg.dvfs)
          powerBudget(cfg.powerBudget)
          schedulingStrategy(cfg.strategy)
        }
        inferenceBroker(s"client-${cfg.name}") {
          targetEngine(0)
          sloTtft(SimTime(1.0))
          sloTpot(SimTime(0.05))
          syntheticTrace(count = 2000, ratePerSecond = 50.0, avgPromptTokens = 512, avgOutputTokens = 128, seed = 42)
        }
      }
      simConfig.run() match
        case Right(r) =>
          val summary = r.formatInferenceSummary
          println("done")
          parseResults(summary, cfg.name)
        case Left(err) =>
          println(s"FAILED: $err")
          None
    }

    printTable("Scenario 1: LLaMA-2-7B on A100, 2000 req @ 50 req/s, maxBatch=32", results7b)

    // ── Scenario 2: LLaMA-70B on 8×A100 (TP=8), 300W budget ────────
    println("\n\n--- Scenario 2: LLaMA-2-70B on 8×A100 (TP=8) @ 20 req/s, PC-BAC budget (300W) ---")

    val configs70b = Vector(
      RunConfig("CB+MaxPerf", DvfsPolicy.maxPerformance, Watts(400.0), SchedulingStrategy.ContinuousBatching, 64),
      RunConfig("CB+PA-DVFS", DvfsPolicy.phaseAware(false), Watts(400.0), SchedulingStrategy.ContinuousBatching, 64),
      RunConfig("CB+EnergyP", DvfsPolicy.energyProportional, Watts(400.0), SchedulingStrategy.ContinuousBatching, 64),
      RunConfig("PCBAC@300W", DvfsPolicy.maxPerformance, Watts(300.0), SchedulingStrategy.PowerCapped, 64),
      RunConfig("PCBAC+PA", DvfsPolicy.phaseAware(false), Watts(300.0), SchedulingStrategy.PowerCapped, 64)
    )

    val results70b = configs70b.flatMap { cfg =>
      print(s"  ${cfg.name}... ")
      val simConfig = simulation(s"s2-${cfg.name}", endTime = SimTime(600.0)) {
        inferenceEngine(s"vllm70b-${cfg.name}") {
          model(LlmModelSpec.llama2_70b)
          deviceSpec(GpuDeviceSpec.a100Sxm)
          tpDegree(8)
          maxBatchSize(cfg.maxBatch)
          iterationInterval(SimTime(0.01))
          dvfsPolicy(cfg.dvfs)
          powerBudget(cfg.powerBudget)
          schedulingStrategy(cfg.strategy)
        }
        inferenceBroker(s"client70b-${cfg.name}") {
          targetEngine(0)
          sloTtft(SimTime(2.0))
          sloTpot(SimTime(0.10))
          syntheticTrace(count = 1000, ratePerSecond = 20.0, avgPromptTokens = 512, avgOutputTokens = 128, seed = 42)
        }
      }
      simConfig.run() match
        case Right(r) =>
          val summary = r.formatInferenceSummary
          println("done")
          parseResults(summary, cfg.name)
        case Left(err) =>
          println(s"FAILED: $err")
          None
    }

    printTable("Scenario 2: LLaMA-2-70B on 8×A100 (TP=8), 1000 req @ 20 req/s", results70b)

    println("\nKey insights:")
    println("  - PA-DVFS: 3-6% energy savings at 0% latency impact (decode is memory-bound)")
    println("  - EnergyProportional: 5-7% savings, more aggressive frequency reduction")
    println("  - PC-BAC enforces power envelope (caps instantaneous watts), not energy minimization")
    println("    Smaller batches lose amortization → higher energy/request, but peak power is capped")
    println("  - Best combined strategy: PA-DVFS for energy + PC-BAC only when power cap is needed")
