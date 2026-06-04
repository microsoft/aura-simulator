// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.examples

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*
import io.aura.gpu.*
import io.aura.inference.model.*

/** Basic LLM inference simulation demonstrating Aura inference support.
  *
  * Simulates 100 inference requests on a single A100 GPU running LLaMA-2 7B. Shows TTFT/TPOT latency, SLO compliance,
  * and GPU energy consumption.
  */
object InferenceBasicExample:
  def main(args: Array[String]): Unit =

    println("=" * 70)
    println("Aura Cloud Simulator - LLM Inference Basic Example")
    println("=" * 70)

    val config = simulation("inference-basic", endTime = SimTime(300.0)) {
      inferenceEngine("vllm-a100") {
        model(LlmModelSpec.llama2_7b)
        deviceSpec(GpuDeviceSpec.a100Sxm)
        tpDegree(1)
        maxBatchSize(64)
        iterationInterval(SimTime(0.01))
        dvfsPolicy(DvfsPolicy.maxPerformance)
        powerBudget(Watts(400.0))
      }

      inferenceBroker("client") {
        targetEngine(0)
        sloTtft(SimTime(0.5))
        sloTpot(SimTime(0.05))
        requests(count = 100, promptTokens = 512, maxOutputTokens = 128, arrivalRate = 10.0, startTime = SimTime(1.0))
      }
    }

    println(s"\nEngine: vllm-a100 (LLaMA-2 7B on A100)")
    println(s"Requests: 100 (512 prompt tokens, 128 output tokens)")
    println(s"Arrival rate: 10 req/s (Poisson)")
    println(s"SLO: TTFT < 0.5s, TPOT < 0.05s")

    println("\nStarting simulation...")
    config.run() match
      case Right(results) =>
        println("\nSimulation completed!")
        println(s"End time: ${results.simulationEndTime.value}s")
        println(s"Total events: ${results.totalEventsProcessed}")
        println()
        println("=== Inference Summary ===")
        println(results.formatInferenceSummary)
        println()
        println("=== Inference Request Details ===")
        println(results.formatInferenceTable)
        println()
        println("=== GPU Energy Report ===")
        println(results.formatGpuEnergyReport)
      case Left(error) =>
        println(s"\nSimulation failed: $error")
