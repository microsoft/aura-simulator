// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.examples

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.gpu.*
import io.aura.inference.model.*

/** Scale benchmark: simulates up to 1M inference requests.
  *
  * Measures wall-clock time, memory, and throughput to demonstrate Aura's performance advantage over Python-based
  * simulators (Vidur, HERMES).
  *
  * Vidur reference: ~10 req/s simulation speed for LLaMA-2-7B on A100 Aura target: >1000 req/s simulation speed (100×
  * faster)
  */
object InferenceScaleBenchmark:
  def main(args: Array[String]): Unit =
    println("=" * 90)
    println("Aura-Inference Scale Benchmark — 1M Request Target")
    println("=" * 90)

    val scales = Vector(1000, 5000, 10000, 50000, 100000, 500000, 1000000)

    println(
      f"\n${"Requests"}%10s ${"Rate(rps)"}%10s ${"Wall(s)"}%10s ${"SimReq/s"}%12s ${"Events"}%12s ${"Evt/s"}%12s ${"MemMB"}%8s ${"Completed"}%10s ${"SLO%"}%7s"
    )
    println("-" * 100)
    println("\n[CSV:scale_benchmark]")
    println("requests,rate_rps,wall_sec,sim_req_per_sec,events,events_per_sec,memory_mb,completed,slo_pct")

    val runtime = Runtime.getRuntime

    scales.foldLeft(false) { (tooSlow, count) =>
      if tooSlow then true
      else
        // Scale arrival rate with request count to keep sim time bounded
        val rate       = (count.toDouble / 30.0).max(100.0).min(50000.0)
        val simEndTime = count.toDouble / rate + 120.0

        System.gc()
        val startNs = System.nanoTime()

        val simConfig = simulation(s"scale-$count", endTime = SimTime(simEndTime)) {
          inferenceEngine("scale-engine") {
            model(LlmModelSpec.llama2_7b)
            deviceSpec(GpuDeviceSpec.a100Sxm)
            tpDegree(1)
            maxBatchSize(256)
            dvfsPolicy(DvfsPolicy.maxPerformance)
            powerBudget(Watts(400.0))
          }
          inferenceBroker("scale-client") {
            targetEngine(0)
            sloTtft(SimTime(10.0))
            sloTpot(SimTime(0.20))
            syntheticTrace(count = count, ratePerSecond = rate, avgPromptTokens = 256, avgOutputTokens = 64, seed = 42)
          }
        }

        simConfig.run() match
          case Right(r) =>
            val elapsedSec   = (System.nanoTime() - startNs) / 1_000_000_000.0
            val memAfter     = (runtime.totalMemory() - runtime.freeMemory()) / (1024 * 1024)
            val simReqPerSec = if elapsedSec > 0 then count / elapsedSec else 0
            val evtPerSec    = if elapsedSec > 0 then r.totalEventsProcessed / elapsedSec else 0
            val completed    = r.inferenceResults.size
            val sloPct = if completed > 0 then r.inferenceResults.count(_.sloMet).toDouble / completed * 100 else 0.0

            println(
              f"$count%10d ${rate}%10.0f ${elapsedSec}%10.2f ${simReqPerSec}%12.0f ${r.totalEventsProcessed}%12d ${evtPerSec}%12.0f ${memAfter}%8d ${completed}%10d ${sloPct}%6.1f%%"
            )
            println(
              f"$count,$rate%.0f,$elapsedSec%.3f,$simReqPerSec%.0f,${r.totalEventsProcessed},$evtPerSec%.0f,$memAfter,$completed,$sloPct%.2f"
            )

            if elapsedSec > 300 && count < 1000000 then
              println(s"\n  (Skipping larger scales — $count took ${elapsedSec.toInt}s)")
              true
            else false
          case Left(err) =>
            val elapsedSec = (System.nanoTime() - startNs) / 1_000_000_000.0
            println(f"$count%10d ${rate}%10.0f ${elapsedSec}%10.2f FAILED: $err")
            println(f"$count,$rate%.0f,$elapsedSec%.3f,0,0,0,0,0,0")
            false
    }

    println("\n" + "=" * 90)
    println("Comparison with Python simulators (published numbers):")
    println("  Vidur (Python/NumPy):  ~10-50 sim req/s for LLaMA-2-7B")
    println("  HERMES (Python):       ~5-20 sim req/s for LLaMA-2-7B")
    println("  Aura (Scala/Pekko):    See results above")
    println("=" * 90)
