// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.examples

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.gpu.*
import io.aura.inference.model.*

/** Parallelism strategies: TP, PP, EP, and hybrid configurations.
  *
  * Exercises the four key model configurations from the evaluation plan:
  *   1. LLaMA-2-7B (TP=1) — single GPU baseline 2. LLaMA-2-70B (TP=8) — tensor parallel across 8 GPUs 3. Mixtral-8x7B
  *      (EP=8) — expert parallel MoE 4. LLaMA-3-405B (PP=4,TP=8) — hybrid pipeline + tensor parallel
  */
object InferenceParallelismExample:
  def main(args: Array[String]): Unit =

    println("=" * 85)
    println("Aura Cloud Simulator - Parallelism Strategy Comparison")
    println("=" * 85)

    case class ModelConfig(
        name: String,
        model: LlmModelSpec,
        tp: Int,
        pp: Int,
        ep: Int,
        gpuCount: Int,
        maxBatch: Int
    )

    val configs = Vector(
      ModelConfig("7B-TP1", LlmModelSpec.llama2_7b, tp = 1, pp = 1, ep = 1, gpuCount = 1, maxBatch = 64),
      ModelConfig("7B-TP4", LlmModelSpec.llama2_7b, tp = 4, pp = 1, ep = 1, gpuCount = 4, maxBatch = 64),
      ModelConfig("70B-TP8", LlmModelSpec.llama2_70b, tp = 8, pp = 1, ep = 1, gpuCount = 8, maxBatch = 32),
      ModelConfig("70B-PP4TP2", LlmModelSpec.llama2_70b, tp = 2, pp = 4, ep = 1, gpuCount = 8, maxBatch = 32),
      ModelConfig("Mixtral-EP8", LlmModelSpec.mixtral_8x7b, tp = 1, pp = 1, ep = 8, gpuCount = 8, maxBatch = 64),
      ModelConfig("Mixtral-TP8", LlmModelSpec.mixtral_8x7b, tp = 8, pp = 1, ep = 1, gpuCount = 8, maxBatch = 64),
      ModelConfig("405B-PP4TP8", LlmModelSpec.llama3_405b, tp = 8, pp = 4, ep = 1, gpuCount = 32, maxBatch = 16)
    )

    case class RunResult(
        name: String,
        modelName: String,
        gpuCount: Int,
        completed: Int,
        sloPercent: Double,
        avgTtftMs: Double,
        avgTpotMs: Double,
        energyWh: Double
    )

    val results = configs.flatMap { cfg =>
      print(s"  ${cfg.name} (${cfg.model.name}, ${cfg.gpuCount} GPUs)... ")
      val simConfig = simulation(s"par-${cfg.name}", endTime = SimTime(300.0)) {
        inferenceEngine(s"engine-${cfg.name}") {
          model(cfg.model)
          deviceSpec(GpuDeviceSpec.a100Sxm)
          tpDegree(cfg.tp)
          ppStages(cfg.pp)
          epDegree(cfg.ep)
          maxBatchSize(cfg.maxBatch)
          iterationInterval(SimTime(0.01))
          dvfsPolicy(DvfsPolicy.phaseAware(false))
          powerBudget(Watts(400.0))
        }
        inferenceBroker(s"client-${cfg.name}") {
          targetEngine(0)
          sloTtft(SimTime(2.0))
          sloTpot(SimTime(0.10))
          syntheticTrace(count = 500, ratePerSecond = 10.0, avgPromptTokens = 512, avgOutputTokens = 128, seed = 42)
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
          Some(
            RunResult(
              cfg.name,
              cfg.model.name,
              cfg.gpuCount,
              completedMatch.map(_.group(1).toInt).getOrElse(0),
              sloMatch.map(_.group(1).toDouble).getOrElse(0.0),
              ttftMatch.map(_.group(1).toDouble).getOrElse(0.0),
              tpotMatch.map(_.group(1).toDouble).getOrElse(0.0),
              energyMatch.map(_.group(1).toDouble).getOrElse(0.0)
            )
          )
        case Left(err) =>
          println(s"FAILED: $err")
          None
    }

    println("\n" + "=" * 95)
    println("Parallelism Comparison — 500 requests @ 10 req/s, PA-DVFS, A100 GPUs")
    println("=" * 95)
    println(
      f"\n${"Config"}%-15s ${"Model"}%-15s ${"GPUs"}%5s ${"Done"}%6s ${"SLO%"}%8s ${"TTFT"}%9s ${"TPOT"}%9s ${"Energy"}%10s"
    )
    println("-" * 80)
    results.foreach { r =>
      println(
        f"${r.name}%-15s ${r.modelName}%-15s ${r.gpuCount}%5d ${r.completed}%6d ${r.sloPercent}%7.1f%% ${r.avgTtftMs}%8.1fms ${r.avgTpotMs}%8.1fms ${r.energyWh}%9.4fWh"
      )
    }

    // TP speedup analysis
    println("\n--- TP Speedup Analysis ---")
    val tp1_7b = results.find(_.name == "7B-TP1")
    val tp4_7b = results.find(_.name == "7B-TP4")
    (tp1_7b, tp4_7b) match
      case (Some(base), Some(tp)) =>
        val tpotSpeedup = base.avgTpotMs / tp.avgTpotMs
        println(f"  7B TP=1→4: TPOT ${base.avgTpotMs}%.1fms → ${tp.avgTpotMs}%.1fms (${tpotSpeedup}%.2fx speedup)")
      case _ => ()

    // PP vs TP comparison for 70B
    println("\n--- 70B: TP=8 vs PP=4×TP=2 ---")
    val tp8_70b    = results.find(_.name == "70B-TP8")
    val pp4tp2_70b = results.find(_.name == "70B-PP4TP2")
    (tp8_70b, pp4tp2_70b) match
      case (Some(tp), Some(pp)) =>
        println(f"  TP=8:      TTFT=${tp.avgTtftMs}%.1fms  TPOT=${tp.avgTpotMs}%.1fms  Energy=${tp.energyWh}%.4fWh")
        println(f"  PP=4×TP=2: TTFT=${pp.avgTtftMs}%.1fms  TPOT=${pp.avgTpotMs}%.1fms  Energy=${pp.energyWh}%.4fWh")
      case _ => ()

    // MoE comparison
    println("\n--- Mixtral: EP=8 vs TP=8 ---")
    val ep8     = results.find(_.name == "Mixtral-EP8")
    val tp8_mix = results.find(_.name == "Mixtral-TP8")
    (ep8, tp8_mix) match
      case (Some(ep), Some(tp)) =>
        println(f"  EP=8: TTFT=${ep.avgTtftMs}%.1fms  TPOT=${ep.avgTpotMs}%.1fms  Energy=${ep.energyWh}%.4fWh")
        println(f"  TP=8: TTFT=${tp.avgTtftMs}%.1fms  TPOT=${tp.avgTpotMs}%.1fms  Energy=${tp.energyWh}%.4fWh")
      case _ => ()
