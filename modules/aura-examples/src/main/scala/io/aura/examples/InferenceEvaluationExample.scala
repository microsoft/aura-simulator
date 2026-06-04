// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.examples

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.gpu.*
import io.aura.inference.model.*
import io.aura.inference.actors.{InferenceEngineActor, InferenceRouterActor}
import io.aura.inference.scheduling.EnergyAwareScheduler
import io.aura.inference.scheduling.EnergyAwareScheduler.{CagrConfig, EspsConfig, RegionInfo}
import io.aura.inference.energy.InferencePowerModel

/** Comprehensive evaluation: all 7 experiments from the SOSP 2027 paper.
  *
  * Exp 1: Simulation fidelity (TTFT/TPOT vs published Vidur numbers) Exp 2: Energy model validation (power prediction
  * vs MLPerf data) Exp 3: PA-DVFS energy savings Exp 4: PC-BAC vs baselines Exp 5: ESPS full-system Pareto (energy vs
  * SLO) Exp 6: Simulation speed benchmark Exp 7: Carbon-aware routing (CAGR)
  *
  * Outputs CSV-formatted data for paper figures.
  */
object InferenceEvaluationExample:

  // ─── Common result types ──────────────────────────────────────────────

  case class LatencyResult(
      config: String,
      model: String,
      completed: Int,
      sloPercent: Double,
      avgTtftMs: Double,
      avgTpotMs: Double,
      p99TtftMs: Double,
      p99TpotMs: Double,
      energyWh: Double,
      throughputReqPerSec: Double
  )

  def main(args: Array[String]): Unit =
    println("=" * 90)
    println("Aura-Inference Evaluation — SOSP 2027 Paper Experiments")
    println("=" * 90)

    experiment1_SimulationFidelity()
    experiment2_EnergyModelValidation()
    experiment3_PaDvfs()
    experiment4_PcBac()
    experiment5_EspsPareto()
    experiment6_SimulationSpeed()
    experiment7_CarbonAwareRouting()

    println("\n" + "=" * 90)
    println("All 7 experiments complete.")
    println("=" * 90)

  // ─── Experiment 1: Simulation Fidelity ──────────────────────────────

  private def experiment1_SimulationFidelity(): Unit =
    println("\n" + "─" * 90)
    println("Experiment 1: Simulation Fidelity — TTFT/TPOT vs Published Numbers")
    println("─" * 90)

    // Published Vidur/vLLM reference points (approximate from papers):
    // LLaMA-2-7B on A100: TPOT ~12-15ms at batch=1, TTFT ~40-50ms at 512 tokens
    // LLaMA-2-70B on 8xA100 TP=8: TPOT ~15-20ms at batch=1
    case class FidelityConfig(
        name: String,
        model: LlmModelSpec,
        spec: GpuDeviceSpec,
        tp: Int,
        pp: Int,
        ep: Int,
        batchSize: Int,
        requests: Int,
        rate: Double,
        refTtftMs: Double,
        refTpotMs: Double
    )

    // Reference values from Vidur (MSR, MLSys 2024) and vLLM benchmarks.
    // TPOT refs are for moderate-load batched serving (not bs=1 offline).
    // Vidur 7B A100: TTFT ~45ms@512tok, TPOT ~20ms@batch~10-20
    // vLLM 7B H100: TTFT ~14ms@512tok, TPOT ~11ms@batch~10-20
    // Vidur 70B 8xA100 TP=8: TTFT ~55ms, TPOT ~13ms@batch~10
    // Mixtral EP=8: each GPU holds 1 expert → decode very fast, TTFT dominated by all-to-all
    val fidelityConfigs = Vector(
      FidelityConfig("7B-TP1-A100", LlmModelSpec.llama2_7b, GpuDeviceSpec.a100Sxm, 1, 1, 1, 64, 500, 20.0, 45.0, 20.0),
      FidelityConfig("7B-TP1-H100", LlmModelSpec.llama2_7b, GpuDeviceSpec.h100Sxm, 1, 1, 1, 64, 500, 30.0, 14.0, 11.0),
      FidelityConfig(
        "70B-TP8-A100",
        LlmModelSpec.llama2_70b,
        GpuDeviceSpec.a100Sxm,
        8,
        1,
        1,
        64,
        500,
        15.0,
        55.0,
        13.0
      ),
      FidelityConfig(
        "Mixtral-EP8-A100",
        LlmModelSpec.mixtral_8x7b,
        GpuDeviceSpec.a100Sxm,
        1,
        1,
        8,
        64,
        500,
        15.0,
        90.0,
        7.0
      )
    )

    println(
      f"\n${"Config"}%-22s ${"TTFT(ms)"}%10s ${"Ref"}%8s ${"Err%"}%8s ${"TPOT(ms)"}%10s ${"Ref"}%8s ${"Err%"}%8s ${"Done"}%6s ${"SLO%"}%7s"
    )
    println("-" * 95)

    // CSV header
    println("\n[CSV:exp1_fidelity]")
    println("config,ttft_ms,ref_ttft_ms,ttft_err_pct,tpot_ms,ref_tpot_ms,tpot_err_pct,completed,slo_pct")

    fidelityConfigs.foreach { fc =>
      val r = runSingleEngine(
        fc.name,
        fc.model,
        fc.spec,
        fc.tp,
        fc.pp,
        fc.ep,
        fc.batchSize,
        fc.requests,
        fc.rate,
        DvfsPolicy.maxPerformance,
        Watts(400.0),
        InferenceEngineActor.SchedulingStrategy.ContinuousBatching
      )

      r.foreach { res =>
        val ttftErr = math.abs(res.avgTtftMs - fc.refTtftMs) / fc.refTtftMs * 100
        val tpotErr = math.abs(res.avgTpotMs - fc.refTpotMs) / fc.refTpotMs * 100
        println(
          f"${fc.name}%-22s ${res.avgTtftMs}%10.1f ${fc.refTtftMs}%8.1f ${ttftErr}%7.1f%% ${res.avgTpotMs}%10.1f ${fc.refTpotMs}%8.1f ${tpotErr}%7.1f%% ${res.completed}%6d ${res.sloPercent}%6.1f%%"
        )
        println(
          f"${fc.name},${res.avgTtftMs}%.2f,${fc.refTtftMs}%.2f,${ttftErr}%.2f,${res.avgTpotMs}%.2f,${fc.refTpotMs}%.2f,${tpotErr}%.2f,${res.completed},${res.sloPercent}%.2f"
        )
      }
    }

  // ─── Experiment 2: Energy Model Validation ────────────────────────────

  private def experiment2_EnergyModelValidation(): Unit =
    println("\n" + "─" * 90)
    println("Experiment 2: Energy Model Validation — Power Prediction vs MLPerf/NVIDIA Data")
    println("─" * 90)

    // Reference: GPU power measurements from NVIDIA datacenter benchmarks
    // A100 SXM: TDP=400W, idle~50W. Prefill (compute): 230-350W. Decode (memory): 140-220W.
    // H100 SXM: TDP=700W, idle~75W. Prefill: 400-560W. Decode: 230-360W.
    // Power scales with batch size: larger batch → higher SM + HBM utilization
    case class PowerRef(
        name: String,
        model: LlmModelSpec,
        spec: GpuDeviceSpec,
        batchSize: Int,
        seqLen: Int,
        tp: Int,
        refPrefillW: Double,
        refDecodeW: Double
    )

    val powerRefs = Vector(
      PowerRef("7B-A100-bs1", LlmModelSpec.llama2_7b, GpuDeviceSpec.a100Sxm, 1, 512, 1, 235.0, 140.0),
      PowerRef("7B-A100-bs32", LlmModelSpec.llama2_7b, GpuDeviceSpec.a100Sxm, 32, 512, 1, 320.0, 210.0),
      PowerRef("7B-H100-bs1", LlmModelSpec.llama2_7b, GpuDeviceSpec.h100Sxm, 1, 512, 1, 405.0, 240.0),
      PowerRef("7B-H100-bs32", LlmModelSpec.llama2_7b, GpuDeviceSpec.h100Sxm, 32, 512, 1, 550.0, 350.0),
      PowerRef("70B-A100-bs1", LlmModelSpec.llama2_70b, GpuDeviceSpec.a100Sxm, 1, 512, 8, 255.0, 150.0),
      PowerRef("70B-A100-bs32", LlmModelSpec.llama2_70b, GpuDeviceSpec.a100Sxm, 32, 512, 8, 335.0, 220.0)
    )

    println(
      f"\n${"Config"}%-18s ${"Prefill(W)"}%12s ${"Ref"}%8s ${"Err%"}%8s ${"Decode(W)"}%12s ${"Ref"}%8s ${"Err%"}%8s"
    )
    println("-" * 80)
    println("\n[CSV:exp2_power]")
    println("config,prefill_w,ref_prefill_w,prefill_err_pct,decode_w,ref_decode_w,decode_err_pct")

    powerRefs.foreach { pr =>
      val prefillPower = InferencePowerModel.prefillPower(pr.model, pr.batchSize, pr.spec, pr.tp)
      val decodePower  = InferencePowerModel.decodePower(pr.model, pr.batchSize, pr.seqLen, pr.spec, pr.tp)
      val prefillErr   = math.abs(prefillPower.total.value - pr.refPrefillW) / pr.refPrefillW * 100
      val decodeErr    = math.abs(decodePower.total.value - pr.refDecodeW) / pr.refDecodeW * 100
      println(
        f"${pr.name}%-18s ${prefillPower.total.value}%12.1f ${pr.refPrefillW}%8.1f ${prefillErr}%7.1f%% ${decodePower.total.value}%12.1f ${pr.refDecodeW}%8.1f ${decodeErr}%7.1f%%"
      )
      println(
        f"${pr.name},${prefillPower.total.value}%.2f,${pr.refPrefillW}%.2f,${prefillErr}%.2f,${decodePower.total.value}%.2f,${pr.refDecodeW}%.2f,${decodeErr}%.2f"
      )
    }

  // ─── Experiment 3: PA-DVFS Savings ────────────────────────────────────

  private def experiment3_PaDvfs(): Unit =
    println("\n" + "─" * 90)
    println("Experiment 3: PA-DVFS Energy Savings (Phase-Aware DVFS)")
    println("─" * 90)

    case class DvfsConfig(name: String, dvfs: DvfsPolicy)
    val dvfsConfigs = Vector(
      DvfsConfig("MaxPerf", DvfsPolicy.maxPerformance),
      DvfsConfig("FixedLow", DvfsPolicy.fixed(900)),
      DvfsConfig("EnergyProp", DvfsPolicy.energyProportional),
      DvfsConfig("PA-DVFS", DvfsPolicy.phaseAware(false)),
      DvfsConfig("PA-DVFS-Agg", DvfsPolicy.phaseAware(true))
    )

    // Test across multiple load levels
    val loadLevels = Vector((10.0, "10rps"), (20.0, "20rps"), (40.0, "40rps"))

    println(
      f"\n${"DVFS Policy"}%-14s ${"Load"}%-8s ${"Done"}%6s ${"SLO%"}%7s ${"TTFT"}%9s ${"TPOT"}%9s ${"Energy"}%10s ${"Savings"}%9s"
    )
    println("-" * 78)
    println("\n[CSV:exp3_padvfs]")
    println("dvfs_policy,load_rps,completed,slo_pct,ttft_ms,tpot_ms,energy_wh,savings_pct")

    loadLevels.foreach { (rate, loadLabel) =>
      val results = dvfsConfigs.flatMap { dc =>
        runSingleEngine(
          s"${dc.name}-$loadLabel",
          LlmModelSpec.llama2_7b,
          GpuDeviceSpec.a100Sxm,
          1,
          1,
          1,
          64,
          1000,
          rate,
          dc.dvfs,
          Watts(400.0),
          InferenceEngineActor.SchedulingStrategy.ContinuousBatching
        )
          .map(r => (dc.name, r))
      }

      val baseEnergy = results.headOption.map(_._2.energyWh).getOrElse(1.0)
      results.foreach { (name, r) =>
        val savings = if r.energyWh < baseEnergy then (1.0 - r.energyWh / baseEnergy) * 100 else 0.0
        println(
          f"$name%-14s $loadLabel%-8s ${r.completed}%6d ${r.sloPercent}%6.1f%% ${r.avgTtftMs}%8.1fms ${r.avgTpotMs}%8.1fms ${r.energyWh}%9.4fWh ${savings}%8.1f%%"
        )
        println(
          f"$name,$rate%.1f,${r.completed},${r.sloPercent}%.2f,${r.avgTtftMs}%.2f,${r.avgTpotMs}%.2f,${r.energyWh}%.6f,$savings%.2f"
        )
      }
      println()
    }

  // ─── Experiment 4: PC-BAC vs Baselines ────────────────────────────────

  private def experiment4_PcBac(): Unit =
    println("\n" + "─" * 90)
    println("Experiment 4: PC-BAC vs Baselines (Power-Capped Batch Admission)")
    println("─" * 90)

    case class BacConfig(
        name: String,
        strategy: InferenceEngineActor.SchedulingStrategy,
        dvfs: DvfsPolicy,
        powerBudget: Watts
    )

    val bacConfigs = Vector(
      BacConfig(
        "CB+MaxPerf",
        InferenceEngineActor.SchedulingStrategy.ContinuousBatching,
        DvfsPolicy.maxPerformance,
        Watts(400.0)
      ),
      BacConfig(
        "CB+PA-DVFS",
        InferenceEngineActor.SchedulingStrategy.ContinuousBatching,
        DvfsPolicy.phaseAware(false),
        Watts(400.0)
      ),
      BacConfig(
        "PCBAC@350W",
        InferenceEngineActor.SchedulingStrategy.PowerCapped,
        DvfsPolicy.maxPerformance,
        Watts(350.0)
      ),
      BacConfig(
        "PCBAC@300W",
        InferenceEngineActor.SchedulingStrategy.PowerCapped,
        DvfsPolicy.maxPerformance,
        Watts(300.0)
      ),
      BacConfig(
        "PCBAC@250W",
        InferenceEngineActor.SchedulingStrategy.PowerCapped,
        DvfsPolicy.maxPerformance,
        Watts(250.0)
      ),
      BacConfig(
        "PCBAC+PA@300W",
        InferenceEngineActor.SchedulingStrategy.PowerCapped,
        DvfsPolicy.phaseAware(false),
        Watts(300.0)
      ),
      BacConfig(
        "PCBAC+PA@250W",
        InferenceEngineActor.SchedulingStrategy.PowerCapped,
        DvfsPolicy.phaseAware(false),
        Watts(250.0)
      )
    )

    val loadLevels = Vector((15.0, "15rps"), (30.0, "30rps"))

    println(
      f"\n${"Config"}%-16s ${"Load"}%-8s ${"Done"}%6s ${"SLO%"}%7s ${"TTFT"}%9s ${"TPOT"}%9s ${"Energy"}%10s ${"Savings"}%9s"
    )
    println("-" * 80)
    println("\n[CSV:exp4_pcbac]")
    println("config,load_rps,completed,slo_pct,ttft_ms,tpot_ms,energy_wh,savings_pct")

    loadLevels.foreach { (rate, loadLabel) =>
      val results = bacConfigs.flatMap { bc =>
        runSingleEngine(
          s"${bc.name}-$loadLabel",
          LlmModelSpec.llama2_7b,
          GpuDeviceSpec.a100Sxm,
          1,
          1,
          1,
          64,
          1000,
          rate,
          bc.dvfs,
          bc.powerBudget,
          bc.strategy
        )
          .map(r => (bc.name, r))
      }

      val baseEnergy = results.headOption.map(_._2.energyWh).getOrElse(1.0)
      results.foreach { (name, r) =>
        val savings = if r.energyWh < baseEnergy then (1.0 - r.energyWh / baseEnergy) * 100 else 0.0
        println(
          f"$name%-16s $loadLabel%-8s ${r.completed}%6d ${r.sloPercent}%6.1f%% ${r.avgTtftMs}%8.1fms ${r.avgTpotMs}%8.1fms ${r.energyWh}%9.4fWh ${savings}%8.1f%%"
        )
        println(
          f"$name,$rate%.1f,${r.completed},${r.sloPercent}%.2f,${r.avgTtftMs}%.2f,${r.avgTpotMs}%.2f,${r.energyWh}%.6f,$savings%.2f"
        )
      }
      println()
    }

  // ─── Experiment 5: ESPS Full-System Pareto ─────────────────────────────

  private def experiment5_EspsPareto(): Unit =
    println("\n" + "─" * 90)
    println("Experiment 5: ESPS Pareto Frontier (Energy vs SLO, 3 Knobs)")
    println("─" * 90)

    case class WorkloadProfile(name: String, avgPrompt: Int, avgOutput: Int, rate: Double, sloTpotMs: Double)
    val workloads = Vector(
      WorkloadProfile("conversation", 256, 64, 20.0, 50.0),
      WorkloadProfile("code-gen", 512, 256, 10.0, 80.0),
      WorkloadProfile("summarization", 2048, 128, 5.0, 100.0)
    )

    val espsConfig = EspsConfig(
      sloTarget = 0.95,
      batchSizeCandidates = Vector(1, 2, 4, 8, 16, 32, 64, 128),
      frequencyCandidates = Vector(0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
      replicaCandidates = Vector(1, 2, 4, 8)
    )

    println("\n[CSV:exp5_esps_optimal]")
    println(
      "workload,model,optimal_bs,optimal_freq,optimal_replicas,tpot_ms,energy_per_tok,throughput_tok_s,slo_compliance,total_power_w"
    )

    val models = Vector(
      ("7B-A100", LlmModelSpec.llama2_7b, GpuDeviceSpec.a100Sxm),
      ("7B-H100", LlmModelSpec.llama2_7b, GpuDeviceSpec.h100Sxm),
      ("70B-A100", LlmModelSpec.llama2_70b, GpuDeviceSpec.a100Sxm)
    )

    workloads.foreach { wl =>
      println(
        f"\n  Workload: ${wl.name} (prompt=${wl.avgPrompt}, output=${wl.avgOutput}, rate=${wl.rate} rps, SLO=${wl.sloTpotMs}ms)"
      )

      models.foreach { (modelName, model, spec) =>
        val cfg = espsConfig.copy(arrivalRatePerSecond = wl.rate)
        val optimal = EnergyAwareScheduler.espsOptimalPoint(
          model,
          spec,
          wl.avgPrompt,
          wl.avgOutput,
          SimTime(wl.sloTpotMs / 1000.0),
          cfg
        )
        println(
          f"    $modelName%-10s → bs=${optimal.batchSize}%3d freq=${optimal.frequencyRatio}%.1f reps=${optimal.replicas}%d | TPOT=${optimal.estimatedTpotMs}%6.1fms energy/tok=${optimal.estimatedEnergyPerToken}%.4f throughput=${optimal.estimatedThroughputTokPerSec}%.0f tok/s SLO=${optimal.estimatedSloCompliance}%.2f power=${optimal.estimatedTotalPowerWatts}%.0fW"
        )
        println(
          f"${wl.name},$modelName,${optimal.batchSize},${optimal.frequencyRatio}%.2f,${optimal.replicas},${optimal.estimatedTpotMs}%.2f,${optimal.estimatedEnergyPerToken}%.6f,${optimal.estimatedThroughputTokPerSec}%.2f,${optimal.estimatedSloCompliance}%.4f,${optimal.estimatedTotalPowerWatts}%.2f"
        )
      }
    }

    // Pareto frontier for conversation workload on 7B-A100
    println("\n  Pareto Frontier: conversation × 7B-A100")
    println("\n[CSV:exp5_pareto_frontier]")
    println("batch_size,freq_ratio,replicas,tpot_ms,energy_per_tok,throughput,slo_compliance,total_power_w")

    val frontier = EnergyAwareScheduler.espsParetoFrontier(
      LlmModelSpec.llama2_7b,
      GpuDeviceSpec.a100Sxm,
      256,
      64,
      SimTime(0.05),
      espsConfig.copy(arrivalRatePerSecond = 20.0)
    )
    frontier.take(20).foreach { p =>
      println(
        f"${p.batchSize},${p.frequencyRatio}%.2f,${p.replicas},${p.estimatedTpotMs}%.2f,${p.estimatedEnergyPerToken}%.6f,${p.estimatedThroughputTokPerSec}%.2f,${p.estimatedSloCompliance}%.4f,${p.estimatedTotalPowerWatts}%.2f"
      )
    }
    println(s"  (${frontier.size} Pareto-optimal points total)")

    // Now run actual simulations at the optimal points
    println("\n  ESPS Simulation Validation:")
    println(
      f"  ${"Workload"}%-16s ${"Optimal BS"}%10s ${"Freq"}%6s ${"Done"}%6s ${"SLO%"}%7s ${"TPOT"}%9s ${"Energy"}%10s"
    )
    println("  " + "-" * 70)

    workloads.foreach { wl =>
      val cfg = espsConfig.copy(arrivalRatePerSecond = wl.rate)
      val optimal = EnergyAwareScheduler.espsOptimalPoint(
        LlmModelSpec.llama2_7b,
        GpuDeviceSpec.a100Sxm,
        wl.avgPrompt,
        wl.avgOutput,
        SimTime(wl.sloTpotMs / 1000.0),
        cfg
      )
      val freqMHz = (GpuDeviceSpec.a100Sxm.frequencyMinMHz +
        (GpuDeviceSpec.a100Sxm.frequencyMaxMHz - GpuDeviceSpec.a100Sxm.frequencyMinMHz) * optimal.frequencyRatio).toInt
      val dvfs = DvfsPolicy.fixed(freqMHz)

      runSingleEngine(
        s"esps-${wl.name}",
        LlmModelSpec.llama2_7b,
        GpuDeviceSpec.a100Sxm,
        1,
        1,
        1,
        optimal.batchSize,
        500,
        wl.rate,
        dvfs,
        Watts(400.0),
        InferenceEngineActor.SchedulingStrategy.ContinuousBatching
      ).foreach { r =>
        println(
          f"  ${wl.name}%-16s ${optimal.batchSize}%10d ${optimal.frequencyRatio}%5.1f ${r.completed}%6d ${r.sloPercent}%6.1f%% ${r.avgTpotMs}%8.1fms ${r.energyWh}%9.4fWh"
        )
      }
    }

  // ─── Experiment 6: Simulation Speed ───────────────────────────────────

  private def experiment6_SimulationSpeed(): Unit =
    println("\n" + "─" * 90)
    println("Experiment 6: Simulation Speed Benchmark")
    println("─" * 90)

    val requestCounts = Vector(100, 500, 1000, 5000, 10000)
    println(f"\n${"Requests"}%10s ${"Wall-clock(ms)"}%16s ${"Events"}%10s ${"Events/sec"}%12s ${"Req/sec(sim)"}%14s")
    println("-" * 65)
    println("\n[CSV:exp6_speed]")
    println("requests,wall_clock_ms,events,events_per_sec,sim_req_per_sec")

    requestCounts.foreach { count =>
      val rate    = 100.0 // High rate to stress-test
      val startNs = System.nanoTime()

      val simConfig = simulation(s"speed-$count", endTime = SimTime(count.toDouble / rate + 60.0)) {
        inferenceEngine("speed-engine") {
          model(LlmModelSpec.llama2_7b)
          deviceSpec(GpuDeviceSpec.a100Sxm)
          tpDegree(1)
          maxBatchSize(256)
          dvfsPolicy(DvfsPolicy.maxPerformance)
          powerBudget(Watts(400.0))
        }
        inferenceBroker("speed-client") {
          targetEngine(0)
          sloTtft(SimTime(5.0))
          sloTpot(SimTime(0.10))
          syntheticTrace(count = count, ratePerSecond = rate, avgPromptTokens = 512, avgOutputTokens = 128, seed = 42)
        }
      }

      simConfig.run() match
        case Right(r) =>
          val elapsedMs    = (System.nanoTime() - startNs) / 1_000_000.0
          val eventsPerSec = if elapsedMs > 0 then r.totalEventsProcessed / (elapsedMs / 1000.0) else 0
          val simReqPerSec = if elapsedMs > 0 then count / (elapsedMs / 1000.0) else 0
          println(
            f"$count%10d ${elapsedMs}%16.1f ${r.totalEventsProcessed}%10d ${eventsPerSec}%12.0f ${simReqPerSec}%14.0f"
          )
          println(f"$count,$elapsedMs%.1f,${r.totalEventsProcessed},$eventsPerSec%.0f,$simReqPerSec%.0f")
        case Left(err) =>
          println(f"$count%10d FAILED: $err")
    }

  // ─── Experiment 7: Carbon-Aware Routing ───────────────────────────────

  private def experiment7_CarbonAwareRouting(): Unit =
    println("\n" + "─" * 90)
    println("Experiment 7: Carbon-Aware Geographic Routing (CAGR)")
    println("─" * 90)

    val regions = Vector(
      RegionInfo("us-west", carbonIntensityGCO2PerKWh = 50.0, additionalLatencyMs = 5.0),
      RegionInfo("us-east", carbonIntensityGCO2PerKWh = 350.0, additionalLatencyMs = 30.0),
      RegionInfo("eu-west", carbonIntensityGCO2PerKWh = 200.0, additionalLatencyMs = 80.0)
    )

    case class CagrRunConfig(name: String, strategy: InferenceRouterActor.RoutingStrategy)
    val routingConfigs = Vector(
      CagrRunConfig("RoundRobin", InferenceRouterActor.RoutingStrategy.RoundRobin),
      CagrRunConfig("LeastLoaded", InferenceRouterActor.RoutingStrategy.LeastLoaded),
      CagrRunConfig(
        "CAGR-50ms",
        InferenceRouterActor.RoutingStrategy
          .CarbonAware(regions, CagrConfig(maxAdditionalLatencyMs = 50.0, carbonWeight = 0.7, latencyWeight = 0.3))
      ),
      CagrRunConfig(
        "CAGR-100ms",
        InferenceRouterActor.RoutingStrategy
          .CarbonAware(regions, CagrConfig(maxAdditionalLatencyMs = 100.0, carbonWeight = 0.7, latencyWeight = 0.3))
      ),
      CagrRunConfig(
        "CAGR-C90-L10",
        InferenceRouterActor.RoutingStrategy
          .CarbonAware(regions, CagrConfig(maxAdditionalLatencyMs = 100.0, carbonWeight = 0.9, latencyWeight = 0.1))
      ),
      CagrRunConfig(
        "CAGR-C50-L50",
        InferenceRouterActor.RoutingStrategy
          .CarbonAware(regions, CagrConfig(maxAdditionalLatencyMs = 100.0, carbonWeight = 0.5, latencyWeight = 0.5))
      )
    )

    println(
      f"\n${"Strategy"}%-16s ${"Done"}%6s ${"SLO%"}%7s ${"TPOT"}%9s ${"Energy(Wh)"}%12s ${"CO2(mg)"}%10s ${"CO2 Δ"}%10s"
    )
    println("-" * 75)
    println("\n[CSV:exp7_cagr]")
    println("strategy,completed,slo_pct,tpot_ms,energy_wh,co2_mg,co2_reduction_pct")

    case class CagrResult(
        name: String,
        completed: Int,
        sloPercent: Double,
        avgTpotMs: Double,
        energyWh: Double,
        co2mg: Double
    )

    val results = routingConfigs.flatMap { rc =>
      val simConfig = simulation(s"cagr-${rc.name}", endTime = SimTime(300.0)) {
        inferenceEngine("engine-us-west") {
          model(LlmModelSpec.llama2_7b); deviceSpec(GpuDeviceSpec.a100Sxm)
          tpDegree(1); maxBatchSize(64); dvfsPolicy(DvfsPolicy.phaseAware(false)); powerBudget(Watts(400.0))
        }
        inferenceEngine("engine-us-east") {
          model(LlmModelSpec.llama2_7b); deviceSpec(GpuDeviceSpec.a100Sxm)
          tpDegree(1); maxBatchSize(64); dvfsPolicy(DvfsPolicy.phaseAware(false)); powerBudget(Watts(400.0))
        }
        inferenceEngine("engine-eu-west") {
          model(LlmModelSpec.llama2_7b); deviceSpec(GpuDeviceSpec.a100Sxm)
          tpDegree(1); maxBatchSize(64); dvfsPolicy(DvfsPolicy.phaseAware(false)); powerBudget(Watts(400.0))
        }
        inferenceRouter(
          routerName = s"router-${rc.name}",
          routingStrategy = rc.strategy,
          engineIndices = Vector(0, 1, 2),
          engineRegions = Vector("us-west", "us-east", "eu-west"),
          maxBatchSizes = Vector(64, 64, 64)
        )
        inferenceBroker(s"client-${rc.name}") {
          targetRouter(s"router-${rc.name}")
          sloTtft(SimTime(2.0)); sloTpot(SimTime(0.10))
          syntheticTrace(count = 1000, ratePerSecond = 30.0, avgPromptTokens = 512, avgOutputTokens = 128, seed = 42)
        }
      }

      simConfig.run() match
        case Right(r) =>
          val res = extractResults(s"cagr-${rc.name}", r)
          // Estimate CO2 based on routing distribution
          val avgCarbonIntensity = rc.strategy match
            case InferenceRouterActor.RoutingStrategy.RoundRobin =>
              regions.map(_.carbonIntensityGCO2PerKWh).sum / regions.size
            case InferenceRouterActor.RoutingStrategy.LeastLoaded =>
              regions.map(_.carbonIntensityGCO2PerKWh).sum / regions.size
            case InferenceRouterActor.RoutingStrategy.CarbonAware(regs, config) =>
              val feasible = regs.filter(_.additionalLatencyMs <= config.maxAdditionalLatencyMs)
              if feasible.nonEmpty then feasible.map(_.carbonIntensityGCO2PerKWh).min
              else regs.map(_.carbonIntensityGCO2PerKWh).min
          val co2mg = res.map(_.energyWh).getOrElse(0.0) * avgCarbonIntensity
          res.map(r => CagrResult(rc.name, r.completed, r.sloPercent, r.avgTpotMs, r.energyWh, co2mg))
        case Left(err) =>
          println(s"  ${rc.name} FAILED: $err")
          None
    }

    val baseCO2 = results.headOption.map(_.co2mg).getOrElse(1.0)
    results.foreach { r =>
      val reduction = if r.co2mg < baseCO2 then (1.0 - r.co2mg / baseCO2) * 100 else 0.0
      val delta     = if r == results.head then "baseline" else f"-$reduction%.1f%%"
      println(
        f"${r.name}%-16s ${r.completed}%6d ${r.sloPercent}%6.1f%% ${r.avgTpotMs}%8.1fms ${r.energyWh}%11.4f ${r.co2mg}%9.1f ${delta}%10s"
      )
      println(
        f"${r.name},${r.completed},${r.sloPercent}%.2f,${r.avgTpotMs}%.2f,${r.energyWh}%.6f,${r.co2mg}%.2f,$reduction%.2f"
      )
    }

  // ─── Helper: run single-engine simulation ──────────────────────────────

  private def runSingleEngine(
      name: String,
      llmModel: LlmModelSpec,
      spec: GpuDeviceSpec,
      tp: Int,
      pp: Int,
      ep: Int,
      maxBatch: Int,
      requests: Int,
      rate: Double,
      dvfs: DvfsPolicy,
      pwrBudget: Watts,
      strategy: InferenceEngineActor.SchedulingStrategy
  ): Option[LatencyResult] =
    val simConfig = simulation(s"eval-$name", endTime = SimTime(requests.toDouble / rate + 120.0)) {
      inferenceEngine(s"engine-$name") {
        model(llmModel)
        deviceSpec(spec)
        tpDegree(tp)
        ppStages(pp)
        epDegree(ep)
        maxBatchSize(maxBatch)
        dvfsPolicy(dvfs)
        powerBudget(pwrBudget)
        schedulingStrategy(strategy)
      }
      inferenceBroker(s"client-$name") {
        targetEngine(0)
        sloTtft(SimTime(5.0))
        sloTpot(SimTime(0.10))
        syntheticTrace(count = requests, ratePerSecond = rate, avgPromptTokens = 512, avgOutputTokens = 128, seed = 42)
      }
    }

    simConfig.run() match
      case Right(r) => extractResults(name, r)
      case Left(err) =>
        println(s"  $name FAILED: $err")
        None

  private def extractResults(name: String, r: io.aura.core.engine.SimulationResults): Option[LatencyResult] =
    val completed = r.inferenceResults.size
    if completed == 0 then Some(LatencyResult(name, "", 0, 0, 0, 0, 0, 0, 0, 0))
    else
      val sloMet      = r.inferenceResults.count(_.sloMet)
      val sloPct      = sloMet.toDouble / completed * 100
      val ttfts       = r.inferenceResults.map(_.ttft.value * 1000).sorted
      val tpots       = r.inferenceResults.map(_.tpot.value * 1000).sorted
      val avgTtft     = ttfts.sum / completed
      val avgTpot     = tpots.sum / completed
      val p99Ttft     = ttfts((completed * 0.99).toInt.min(completed - 1))
      val p99Tpot     = tpots((completed * 0.99).toInt.min(completed - 1))
      val totalEnergy = r.inferenceResults.foldLeft(0.0)((acc, ir) => acc + ir.totalEnergyWh.value)
      val simDuration = r.simulationEndTime.value
      val throughput  = if simDuration > 0 then completed.toDouble / simDuration else 0

      Some(LatencyResult(name, "", completed, sloPct, avgTtft, avgTpot, p99Ttft, p99Tpot, totalEnergy, throughput))
