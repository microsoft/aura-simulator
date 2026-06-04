// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.examples

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.gpu.*
import io.aura.inference.model.*
import io.aura.inference.actors.InferenceEngineActor
import io.aura.inference.scheduling.EnergyAwareScheduler
import io.aura.inference.scheduling.EnergyAwareScheduler.{EspsConfig, RegionInfo}
import io.aura.inference.energy.InferencePowerModel

/** Generates publication-ready CSV data for all paper figures.
  *
  * Each figure outputs a separate CSV block prefixed with [CSV:figure_N]. Post-process with matplotlib/pgfplots to
  * render final figures.
  *
  * Figures:
  *   1. GPU power breakdown by phase (prefill vs decode) — bar chart 2. PA-DVFS frequency trace over time — line plot
  *      3. Energy savings vs SLO violation rate — scatter plot 4. ESPS Pareto frontier — 2D frontier plot 5. Throughput
  *      vs energy per token at varying batch sizes — line plot 6. CAGR CO2 reduction across carbon intensity scenarios
  *      — grouped bar 7. Simulation speed scaling — log-log plot 8. Latency CDF (TTFT and TPOT) — CDF plot
  */
object InferencePaperFigures:
  def main(args: Array[String]): Unit =
    println("=" * 90)
    println("Aura-Inference Paper Figure Data Generator")
    println("=" * 90)

    figure1_PowerBreakdown()
    figure2_DvfsFrequencyProfile()
    figure3_EnergySloTradeoff()
    figure4_EspsParetoFrontier()
    figure5_BatchSizeScaling()
    figure6_CagrCarbonScenarios()
    figure7_SimulationSpeedScaling()
    figure8_LatencyCdf()

    println("\n" + "=" * 90)
    println("All figure data generated. Parse [CSV:figure_N] blocks for plotting.")
    println("=" * 90)

  // ─── Figure 1: GPU Power Breakdown ─────────────────────────────────────

  private def figure1_PowerBreakdown(): Unit =
    println("\n[CSV:figure_1_power_breakdown]")
    println("model,gpu,batch_size,phase,compute_w,hbm_w,nvlink_w,pcie_w,idle_w,total_w")

    val configs = Vector(
      ("7B", LlmModelSpec.llama2_7b, GpuDeviceSpec.a100Sxm, 1),
      ("7B", LlmModelSpec.llama2_7b, GpuDeviceSpec.a100Sxm, 8),
      ("7B", LlmModelSpec.llama2_7b, GpuDeviceSpec.a100Sxm, 32),
      ("7B", LlmModelSpec.llama2_7b, GpuDeviceSpec.h100Sxm, 1),
      ("7B", LlmModelSpec.llama2_7b, GpuDeviceSpec.h100Sxm, 32),
      ("70B", LlmModelSpec.llama2_70b, GpuDeviceSpec.a100Sxm, 1),
      ("70B", LlmModelSpec.llama2_70b, GpuDeviceSpec.a100Sxm, 32)
    )

    configs.foreach { (modelName, model, spec, bs) =>
      val gpuName = if spec == GpuDeviceSpec.a100Sxm then "A100" else "H100"
      val prefill = InferencePowerModel.prefillPower(model, bs, spec)
      val decode  = InferencePowerModel.decodePower(model, bs, 512, spec)
      println(
        f"$modelName,$gpuName,$bs,prefill,${prefill.computeWatts.value}%.2f,${prefill.hbmWatts.value}%.2f,${prefill.nvlinkWatts.value}%.2f,${prefill.pcieWatts.value}%.2f,${prefill.idleWatts.value}%.2f,${prefill.total.value}%.2f"
      )
      println(
        f"$modelName,$gpuName,$bs,decode,${decode.computeWatts.value}%.2f,${decode.hbmWatts.value}%.2f,${decode.nvlinkWatts.value}%.2f,${decode.pcieWatts.value}%.2f,${decode.idleWatts.value}%.2f,${decode.total.value}%.2f"
      )
    }

  // ─── Figure 2: DVFS Frequency Profile ──────────────────────────────────

  private def figure2_DvfsFrequencyProfile(): Unit =
    println("\n[CSV:figure_2_dvfs_profile]")
    println("batch_size,sm_util,hbm_util_gbps,max_perf_mhz,energy_prop_mhz,pa_dvfs_prefill_mhz,pa_dvfs_decode_mhz")

    val spec       = GpuDeviceSpec.a100Sxm
    val model      = LlmModelSpec.llama2_7b
    val batchSizes = Vector(1, 2, 4, 8, 16, 32, 64, 128)

    batchSizes.foreach { bs =>
      val prefillAct = InferenceProfile.prefillActivity(model, bs, spec)
      val decodeAct  = InferenceProfile.decodeActivity(model, bs, 512, spec)

      val maxPerf       = spec.frequencyMaxMHz
      val energyProp    = DvfsPolicy.energyProportional(spec, decodeAct, Watts(400.0))
      val paDvfsPrefill = EnergyAwareScheduler.paDvfsFrequency(spec, prefillAct, isPrefill = true)
      val paDvfsDecode  = EnergyAwareScheduler.paDvfsFrequency(spec, decodeAct, isPrefill = false)

      println(
        f"$bs,${decodeAct.smUtilization.value}%.4f,${decodeAct.hbmTotalBWGBps}%.2f,$maxPerf,$energyProp,$paDvfsPrefill,$paDvfsDecode"
      )
    }

  // ─── Figure 3: Energy-SLO Tradeoff ────────────────────────────────────

  private def figure3_EnergySloTradeoff(): Unit =
    println("\n[CSV:figure_3_energy_slo_tradeoff]")
    println("config,load_rps,energy_wh,slo_violation_pct,completed")

    case class Config(
        name: String,
        dvfs: DvfsPolicy,
        strategy: InferenceEngineActor.SchedulingStrategy,
        powerBudget: Watts
    )
    val configs = Vector(
      Config(
        "MaxPerf",
        DvfsPolicy.maxPerformance,
        InferenceEngineActor.SchedulingStrategy.ContinuousBatching,
        Watts(400.0)
      ),
      Config(
        "PA-DVFS",
        DvfsPolicy.phaseAware(false),
        InferenceEngineActor.SchedulingStrategy.ContinuousBatching,
        Watts(400.0)
      ),
      Config(
        "PCBAC@300W",
        DvfsPolicy.maxPerformance,
        InferenceEngineActor.SchedulingStrategy.PowerCapped,
        Watts(300.0)
      ),
      Config(
        "PCBAC+PA@300W",
        DvfsPolicy.phaseAware(false),
        InferenceEngineActor.SchedulingStrategy.PowerCapped,
        Watts(300.0)
      ),
      Config(
        "EnergyProp",
        DvfsPolicy.energyProportional,
        InferenceEngineActor.SchedulingStrategy.ContinuousBatching,
        Watts(400.0)
      )
    )

    val rates = Vector(5.0, 10.0, 15.0, 20.0, 30.0, 40.0)

    rates.foreach { rate =>
      configs.foreach { cfg =>
        val simConfig = simulation(s"fig3-${cfg.name}-${rate.toInt}", endTime = SimTime(500.0 / rate + 120.0)) {
          inferenceEngine("fig3-engine") {
            model(LlmModelSpec.llama2_7b); deviceSpec(GpuDeviceSpec.a100Sxm)
            tpDegree(1); maxBatchSize(64)
            dvfsPolicy(cfg.dvfs); powerBudget(cfg.powerBudget)
            schedulingStrategy(cfg.strategy)
          }
          inferenceBroker("fig3-client") {
            targetEngine(0); sloTtft(SimTime(2.0)); sloTpot(SimTime(0.05))
            syntheticTrace(count = 500, ratePerSecond = rate, avgPromptTokens = 512, avgOutputTokens = 128, seed = 42)
          }
        }

        simConfig.run() match
          case Right(r) =>
            val completed = r.inferenceResults.size
            val sloViol =
              if completed > 0 then (1.0 - r.inferenceResults.count(_.sloMet).toDouble / completed) * 100 else 100.0
            val energy = r.inferenceResults.foldLeft(0.0)(_ + _.totalEnergyWh.value)
            println(f"${cfg.name},$rate%.1f,$energy%.6f,$sloViol%.2f,$completed")
          case Left(_) =>
            println(f"${cfg.name},$rate%.1f,0,100,0")
      }
    }

  // ─── Figure 4: ESPS Pareto Frontier ───────────────────────────────────

  private def figure4_EspsParetoFrontier(): Unit =
    println("\n[CSV:figure_4_esps_pareto]")
    println("workload,batch_size,freq_ratio,replicas,energy_per_tok,slo_compliance,tpot_ms,total_power_w")

    case class Workload(name: String, prompt: Int, output: Int, rate: Double, sloMs: Double)
    val workloads = Vector(
      Workload("conversation", 256, 64, 20.0, 50.0),
      Workload("code-gen", 512, 256, 10.0, 80.0),
      Workload("summarization", 2048, 128, 5.0, 100.0)
    )

    val espsConfig = EspsConfig(
      batchSizeCandidates = Vector(1, 2, 4, 8, 16, 32, 64, 128),
      frequencyCandidates = Vector(0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
      replicaCandidates = Vector(1, 2, 4, 8)
    )

    workloads.foreach { wl =>
      val frontier = EnergyAwareScheduler.espsParetoFrontier(
        LlmModelSpec.llama2_7b,
        GpuDeviceSpec.a100Sxm,
        wl.prompt,
        wl.output,
        SimTime(wl.sloMs / 1000.0),
        espsConfig.copy(arrivalRatePerSecond = wl.rate)
      )
      frontier.foreach { p =>
        println(
          f"${wl.name},${p.batchSize},${p.frequencyRatio}%.2f,${p.replicas},${p.estimatedEnergyPerToken}%.6f,${p.estimatedSloCompliance}%.4f,${p.estimatedTpotMs}%.2f,${p.estimatedTotalPowerWatts}%.2f"
        )
      }
    }

  // ─── Figure 5: Batch Size Scaling ──────────────────────────────────────

  private def figure5_BatchSizeScaling(): Unit =
    println("\n[CSV:figure_5_batch_scaling]")
    println("model,gpu,batch_size,tpot_ms,throughput_tok_s,power_w,energy_per_tok")

    val configs = Vector(
      ("7B-A100", LlmModelSpec.llama2_7b, GpuDeviceSpec.a100Sxm),
      ("7B-H100", LlmModelSpec.llama2_7b, GpuDeviceSpec.h100Sxm),
      ("70B-A100-TP8", LlmModelSpec.llama2_70b, GpuDeviceSpec.a100Sxm)
    )
    val batchSizes = Vector(1, 2, 4, 8, 16, 32, 64, 128, 256)

    configs.foreach { (name, model, spec) =>
      val tp = if name.contains("70B") then 8 else 1
      batchSizes.foreach { bs =>
        val tpot         = InferenceProfile.decodeLatencyPerToken(model, bs, 512, spec, tp)
        val power        = InferencePowerModel.decodePower(model, bs, 512, spec, tp)
        val throughput   = if tpot.value > 0 then bs.toDouble / tpot.value else 0.0
        val energyPerTok = if bs > 0 then power.total.value * tpot.value / bs else 0.0
        println(f"$name,$name,$bs,${tpot.value * 1000}%.4f,$throughput%.2f,${power.total.value}%.2f,$energyPerTok%.6f")
      }
    }

  // ─── Figure 6: CAGR Carbon Scenarios ───────────────────────────────────

  private def figure6_CagrCarbonScenarios(): Unit =
    println("\n[CSV:figure_6_cagr_scenarios]")
    println("scenario,region,carbon_intensity,latency_ms,routing,region_share,co2_per_kwh_weighted")

    // Different time-of-day carbon scenarios
    case class Scenario(name: String, regions: Vector[RegionInfo])
    val scenarios = Vector(
      Scenario(
        "daytime-US",
        Vector(
          RegionInfo("us-west", 50.0, 5.0),
          RegionInfo("us-east", 350.0, 30.0),
          RegionInfo("eu-west", 200.0, 80.0)
        )
      ),
      Scenario(
        "nighttime-US",
        Vector(
          RegionInfo("us-west", 120.0, 5.0),
          RegionInfo("us-east", 180.0, 30.0),
          RegionInfo("eu-west", 100.0, 80.0)
        )
      ),
      Scenario(
        "renewables-high",
        Vector(
          RegionInfo("us-west", 20.0, 5.0),
          RegionInfo("us-east", 400.0, 30.0),
          RegionInfo("eu-west", 50.0, 80.0)
        )
      ),
      Scenario(
        "all-dirty",
        Vector(
          RegionInfo("us-west", 300.0, 5.0),
          RegionInfo("us-east", 400.0, 30.0),
          RegionInfo("eu-west", 350.0, 80.0)
        )
      )
    )

    val routingStrategies = Vector("round-robin", "cagr-50ms", "cagr-100ms")

    scenarios.foreach { scenario =>
      routingStrategies.foreach { routing =>
        val distribution = routing match
          case "round-robin" =>
            scenario.regions.map(r => (r, 1.0 / scenario.regions.size))
          case "cagr-50ms" =>
            val feasible = scenario.regions.filter(_.additionalLatencyMs <= 50.0)
            val best =
              if feasible.nonEmpty then feasible.minBy(_.carbonIntensityGCO2PerKWh)
              else scenario.regions.minBy(_.carbonIntensityGCO2PerKWh)
            scenario.regions.map(r => (r, if r.regionId == best.regionId then 1.0 else 0.0))
          case "cagr-100ms" =>
            val feasible = scenario.regions.filter(_.additionalLatencyMs <= 100.0)
            val best =
              if feasible.nonEmpty then feasible.minBy(_.carbonIntensityGCO2PerKWh)
              else scenario.regions.minBy(_.carbonIntensityGCO2PerKWh)
            scenario.regions.map(r => (r, if r.regionId == best.regionId then 1.0 else 0.0))
          case _ => scenario.regions.map(r => (r, 1.0 / scenario.regions.size))

        val weightedCarbon = distribution.map((r, share) => r.carbonIntensityGCO2PerKWh * share).sum
        distribution.foreach { (r, share) =>
          println(
            f"${scenario.name},${r.regionId},${r.carbonIntensityGCO2PerKWh}%.0f,${r.additionalLatencyMs}%.0f,$routing,$share%.2f,$weightedCarbon%.1f"
          )
        }
      }
    }

  // ─── Figure 7: Simulation Speed Scaling ────────────────────────────────

  private def figure7_SimulationSpeedScaling(): Unit =
    println("\n[CSV:figure_7_sim_speed]")
    println("requests,wall_sec,sim_req_per_sec,events,events_per_sec")

    val counts = Vector(100, 500, 1000, 5000, 10000)

    counts.foreach { count =>
      val rate    = 100.0
      val startNs = System.nanoTime()

      val simConfig = simulation(s"fig7-$count", endTime = SimTime(count.toDouble / rate + 60.0)) {
        inferenceEngine("fig7-engine") {
          model(LlmModelSpec.llama2_7b); deviceSpec(GpuDeviceSpec.a100Sxm)
          tpDegree(1); maxBatchSize(256)
          dvfsPolicy(DvfsPolicy.maxPerformance); powerBudget(Watts(400.0))
        }
        inferenceBroker("fig7-client") {
          targetEngine(0); sloTtft(SimTime(5.0)); sloTpot(SimTime(0.10))
          syntheticTrace(count = count, ratePerSecond = rate, avgPromptTokens = 256, avgOutputTokens = 64, seed = 42)
        }
      }

      simConfig.run() match
        case Right(r) =>
          val elapsed      = (System.nanoTime() - startNs) / 1_000_000_000.0
          val simReqPerSec = if elapsed > 0 then count / elapsed else 0
          val evtPerSec    = if elapsed > 0 then r.totalEventsProcessed / elapsed else 0
          println(f"$count,$elapsed%.3f,$simReqPerSec%.0f,${r.totalEventsProcessed},$evtPerSec%.0f")
        case Left(_) =>
          println(f"$count,0,0,0,0")
    }

  // ─── Figure 8: Latency CDF ────────────────────────────────────────────

  private def figure8_LatencyCdf(): Unit =
    println("\n[CSV:figure_8_latency_cdf]")
    println("config,metric,percentile,value_ms")

    case class CdfConfig(
        name: String,
        dvfs: DvfsPolicy,
        strategy: InferenceEngineActor.SchedulingStrategy,
        powerBudget: Watts
    )
    val configs = Vector(
      CdfConfig(
        "MaxPerf",
        DvfsPolicy.maxPerformance,
        InferenceEngineActor.SchedulingStrategy.ContinuousBatching,
        Watts(400.0)
      ),
      CdfConfig(
        "PA-DVFS",
        DvfsPolicy.phaseAware(false),
        InferenceEngineActor.SchedulingStrategy.ContinuousBatching,
        Watts(400.0)
      ),
      CdfConfig(
        "PCBAC+PA@300W",
        DvfsPolicy.phaseAware(false),
        InferenceEngineActor.SchedulingStrategy.PowerCapped,
        Watts(300.0)
      )
    )

    val percentiles = Vector(0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)

    configs.foreach { cfg =>
      val simConfig = simulation(s"cdf-${cfg.name}", endTime = SimTime(300.0)) {
        inferenceEngine("cdf-engine") {
          model(LlmModelSpec.llama2_7b); deviceSpec(GpuDeviceSpec.a100Sxm)
          tpDegree(1); maxBatchSize(64)
          dvfsPolicy(cfg.dvfs); powerBudget(cfg.powerBudget)
          schedulingStrategy(cfg.strategy)
        }
        inferenceBroker("cdf-client") {
          targetEngine(0); sloTtft(SimTime(2.0)); sloTpot(SimTime(0.05))
          syntheticTrace(count = 1000, ratePerSecond = 20.0, avgPromptTokens = 512, avgOutputTokens = 128, seed = 42)
        }
      }

      simConfig.run() match
        case Right(r) if r.inferenceResults.nonEmpty =>
          val ttfts = r.inferenceResults.map(_.ttft.value * 1000).sorted
          val tpots = r.inferenceResults.map(_.tpot.value * 1000).sorted
          val n     = ttfts.size

          percentiles.foreach { p =>
            val idx = (p * n).toInt.min(n - 1)
            println(f"${cfg.name},ttft,$p%.2f,${ttfts(idx)}%.4f")
            println(f"${cfg.name},tpot,$p%.2f,${tpots(idx)}%.4f")
          }
        case _ => ()
    }
