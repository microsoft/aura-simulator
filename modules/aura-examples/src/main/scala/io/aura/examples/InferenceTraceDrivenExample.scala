// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.examples

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.gpu.*
import io.aura.inference.model.*
import io.aura.inference.actors.InferenceEngineActor.SchedulingStrategy

/** Trace-driven LLM inference simulation.
  *
  * Simulates 1000 requests on a single A100 under four configurations:
  *   1. Max Performance + Continuous Batching — baseline 2. Energy-Proportional DVFS + Continuous Batching 3. Max
  *      Performance + Power-Capped Batch Admission (PC-BAC) 4. Energy-Proportional DVFS + PC-BAC (combined)
  */
object InferenceTraceDrivenExample:
  def main(args: Array[String]): Unit =

    println("=" * 70)
    println("Aura Cloud Simulator - Trace-Driven Inference Example")
    println("=" * 70)

    case class RunConfig(
        name: String,
        dvfs: DvfsPolicy,
        powerBudget: Watts,
        strategy: SchedulingStrategy
    )

    val configs = Vector(
      RunConfig("CB+MaxPerf", DvfsPolicy.maxPerformance, Watts(400.0), SchedulingStrategy.ContinuousBatching),
      RunConfig("CB+EnergyP", DvfsPolicy.energyProportional, Watts(400.0), SchedulingStrategy.ContinuousBatching),
      RunConfig("CB+PA-DVFS", DvfsPolicy.phaseAware(false), Watts(400.0), SchedulingStrategy.ContinuousBatching),
      RunConfig("PCBAC+MaxP", DvfsPolicy.maxPerformance, Watts(300.0), SchedulingStrategy.PowerCapped),
      RunConfig("PCBAC+PA", DvfsPolicy.phaseAware(false), Watts(300.0), SchedulingStrategy.PowerCapped)
    )

    case class RunResult(
        name: String,
        completed: Int,
        sloPercent: Double,
        avgTtftMs: Double,
        avgTpotMs: Double,
        energyWh: Double
    )

    val results = configs.flatMap { cfg =>
      val simConfig = simulation(s"inf-${cfg.name}", endTime = SimTime(300.0)) {
        inferenceEngine(s"vllm-${cfg.name}") {
          model(LlmModelSpec.llama2_7b)
          deviceSpec(GpuDeviceSpec.a100Sxm)
          tpDegree(1)
          maxBatchSize(64)
          iterationInterval(SimTime(0.01))
          dvfsPolicy(cfg.dvfs)
          powerBudget(cfg.powerBudget)
          schedulingStrategy(cfg.strategy)
        }

        inferenceBroker(s"client-${cfg.name}") {
          targetEngine(0)
          sloTtft(SimTime(2.0))
          sloTpot(SimTime(0.05))
          syntheticTrace(count = 1000, ratePerSecond = 10.0, avgPromptTokens = 512, avgOutputTokens = 128, seed = 42)
        }
      }

      simConfig.run() match
        case Right(r) =>
          val summary        = r.formatInferenceSummary
          val completedMatch = """Completed: (\d+)""".r.findFirstMatchIn(summary)
          val sloMatch       = """SLO met: \d+ \(([\d.]+)%\)""".r.findFirstMatchIn(summary)
          val ttftMatch      = """Avg TTFT: ([\d.]+) ms""".r.findFirstMatchIn(summary)
          val tpotMatch      = """Avg TPOT: ([\d.]+) ms""".r.findFirstMatchIn(summary)
          val energyMatch    = """Energy: ([\d.]+) Wh""".r.findFirstMatchIn(summary)

          Some(
            RunResult(
              cfg.name,
              completedMatch.map(_.group(1).toInt).getOrElse(0),
              sloMatch.map(_.group(1).toDouble).getOrElse(0.0),
              ttftMatch.map(_.group(1).toDouble).getOrElse(0.0),
              tpotMatch.map(_.group(1).toDouble).getOrElse(0.0),
              energyMatch.map(_.group(1).toDouble).getOrElse(0.0)
            )
          )
        case Left(err) =>
          println(s"  ${cfg.name} FAILED: $err")
          None
    }

    // ── Summary Table ────────────────────────────────────────────────
    println("\n" + "=" * 75)
    println("Scheduling × DVFS Comparison — LLaMA-2-7B on A100, 1000 requests @ 10 req/s (moderate load)")
    println("=" * 75)
    println(f"\n${"Config"}%-15s ${"Done"}%6s ${"SLO%"}%8s ${"TTFT"}%8s ${"TPOT"}%8s ${"Energy"}%10s ${"Savings"}%10s")
    println("-" * 67)

    val baseEnergy = results.headOption.map(_.energyWh).getOrElse(1.0)
    results.zipWithIndex.foreach { (r, idx) =>
      val savings =
        if r.energyWh < baseEnergy then f"${(1.0 - r.energyWh / baseEnergy) * 100}%.1f%%"
        else if idx == 0 then "baseline"
        else "0.0%"
      println(
        f"${r.name}%-15s ${r.completed}%6d ${r.sloPercent}%7.1f%% ${r.avgTtftMs}%7.1fms ${r.avgTpotMs}%7.1fms ${r.energyWh}%9.4fWh ${savings}%10s"
      )
    }
