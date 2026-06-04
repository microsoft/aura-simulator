// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.inference

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.inference.model.*

class InferenceProfileSpec extends AnyFlatSpec with Matchers:

  val model = LlmModelSpec.llama2_7b
  val a100  = GpuDeviceSpec.a100Sxm

  "prefillLatency" should "produce non-zero latency" in {
    val lat = InferenceProfile.prefillLatency(model, 512, 1, a100)
    lat.value should be > 0.0
    lat.value should be < 10.0 // should be well under 10 seconds for 7B on A100
  }

  it should "increase with prompt length" in {
    val short = InferenceProfile.prefillLatency(model, 128, 1, a100)
    val long  = InferenceProfile.prefillLatency(model, 2048, 1, a100)
    long.value should be > short.value
  }

  it should "increase with batch size" in {
    val single  = InferenceProfile.prefillLatency(model, 512, 1, a100)
    val batched = InferenceProfile.prefillLatency(model, 512, 8, a100)
    batched.value should be > single.value
  }

  it should "decrease with tensor parallelism" in {
    val tp1 = InferenceProfile.prefillLatency(model, 512, 1, a100, tpDegree = 1)
    val tp4 = InferenceProfile.prefillLatency(model, 512, 1, a100, tpDegree = 4)
    tp4.value should be < tp1.value
  }

  "decodeLatencyPerToken" should "produce non-zero latency" in {
    val lat = InferenceProfile.decodeLatencyPerToken(model, 1, 512, a100)
    lat.value should be > 0.0
    lat.value should be < 1.0 // should be in milliseconds range for 7B on A100
  }

  it should "be memory-bound (higher latency with larger models)" in {
    val decode7b  = InferenceProfile.decodeLatencyPerToken(model, 1, 512, a100)
    val decode70b = InferenceProfile.decodeLatencyPerToken(LlmModelSpec.llama2_70b, 1, 512, a100)
    // Larger model = more weight loading = higher decode latency
    decode70b.value should be > decode7b.value
  }

  it should "increase with sequence length (KV cache reads)" in {
    val short = InferenceProfile.decodeLatencyPerToken(model, 1, 128, a100)
    val long  = InferenceProfile.decodeLatencyPerToken(model, 1, 4096, a100)
    long.value should be > short.value
  }

  it should "decrease with tensor parallelism" in {
    val tp1 = InferenceProfile.decodeLatencyPerToken(model, 1, 512, a100, tpDegree = 1)
    val tp4 = InferenceProfile.decodeLatencyPerToken(model, 1, 512, a100, tpDegree = 4)
    tp4.value should be < tp1.value
  }

  "prefillActivity" should "show high SM utilization" in {
    val activity = InferenceProfile.prefillActivity(model, 1, a100)
    activity.smUtilization.value should be >= 0.5
  }

  "decodeActivity" should "show low SM utilization" in {
    val activity = InferenceProfile.decodeActivity(model, 1, 512, a100)
    activity.smUtilization.value should be < 0.5
  }

  it should "show high HBM bandwidth" in {
    val activity = InferenceProfile.decodeActivity(model, 1, 512, a100)
    activity.hbmReadBWGBps should be > activity.hbmWriteBWGBps
  }

  "70B model on A100" should "have higher latency than 7B" in {
    val model70b = LlmModelSpec.llama2_70b
    val lat7b    = InferenceProfile.decodeLatencyPerToken(model, 1, 512, a100)
    val lat70b   = InferenceProfile.decodeLatencyPerToken(model70b, 1, 512, a100)
    lat70b.value should be > lat7b.value
  }

  "H100 vs A100" should "give lower latency on H100" in {
    val h100    = GpuDeviceSpec.h100Sxm
    val latA100 = InferenceProfile.decodeLatencyPerToken(model, 1, 512, a100)
    val latH100 = InferenceProfile.decodeLatencyPerToken(model, 1, 512, h100)
    latH100.value should be < latA100.value
  }

  // ─── Pipeline Parallelism ──────────────────────────────────────────

  "prefillLatency with PP" should "decrease with pipeline stages" in {
    val pp1 = InferenceProfile.prefillLatency(model, 512, 1, a100, ppStages = 1)
    val pp4 = InferenceProfile.prefillLatency(model, 512, 1, a100, ppStages = 4)
    // PP splits layers across stages → each stage does less work
    pp4.value should be < pp1.value
  }

  it should "not achieve perfect PP scaling due to bubble overhead" in {
    val pp1     = InferenceProfile.prefillLatency(model, 512, 1, a100, ppStages = 1)
    val pp4     = InferenceProfile.prefillLatency(model, 512, 1, a100, ppStages = 4)
    val speedup = pp1.value / pp4.value
    // Should be less than 4x due to pipeline bubble + communication
    speedup should be < 4.0
    speedup should be > 1.5 // but still significant
  }

  "decodeLatencyPerToken with PP" should "decrease with pipeline stages" in {
    val model70b = LlmModelSpec.llama2_70b
    val pp1      = InferenceProfile.decodeLatencyPerToken(model70b, 1, 512, a100, ppStages = 1)
    val pp4      = InferenceProfile.decodeLatencyPerToken(model70b, 1, 512, a100, ppStages = 4)
    pp4.value should be < pp1.value
  }

  "hybrid TP+PP" should "combine speedups" in {
    val model405b = LlmModelSpec.llama3_405b
    val baseline  = InferenceProfile.decodeLatencyPerToken(model405b, 1, 512, a100)
    val tp8       = InferenceProfile.decodeLatencyPerToken(model405b, 1, 512, a100, tpDegree = 8)
    val tp8pp4    = InferenceProfile.decodeLatencyPerToken(model405b, 1, 512, a100, tpDegree = 8, ppStages = 4)
    tp8.value should be < baseline.value
    tp8pp4.value should be < tp8.value
  }

  // ─── MoE / Expert Parallelism ─────────────────────────────────────

  "Mixtral MoE decode" should "be faster than dense model of same total params" in {
    val mixtral = LlmModelSpec.mixtral_8x7b
    // Mixtral 46.7B params but only activates ~14B per token (topK=2 of 8)
    val moeLatency = InferenceProfile.decodeLatencyPerToken(mixtral, 1, 512, a100)
    // Compare to 70B dense (smaller than Mixtral's total but larger than active)
    val denseLatency = InferenceProfile.decodeLatencyPerToken(LlmModelSpec.llama2_70b, 1, 512, a100)
    // MoE should be faster because it only loads active expert weights
    moeLatency.value should be < denseLatency.value
  }

  "MoE decode with EP" should "be faster than without EP" in {
    val mixtral = LlmModelSpec.mixtral_8x7b
    val noEp    = InferenceProfile.decodeLatencyPerToken(mixtral, 1, 512, a100, epDegree = 1)
    val ep8     = InferenceProfile.decodeLatencyPerToken(mixtral, 1, 512, a100, epDegree = 8)
    // EP distributes expert weights across GPUs → less per-GPU memory load
    ep8.value should be < noEp.value
  }

  "MoE prefill" should "use fewer FLOPs than equivalent dense model" in {
    val mixtral    = LlmModelSpec.mixtral_8x7b
    val moePrefill = InferenceProfile.prefillLatency(mixtral, 512, 1, a100)
    // A dense model with same total params would take longer
    // Mixtral active params ~14B, so prefill should be between 7B and 70B
    val prefill7b  = InferenceProfile.prefillLatency(model, 512, 1, a100)
    val prefill70b = InferenceProfile.prefillLatency(LlmModelSpec.llama2_70b, 512, 1, a100)
    moePrefill.value should be > prefill7b.value  // more than 7B
    moePrefill.value should be < prefill70b.value // less than 70B dense
  }

  // ─── LLaMA-3 405B evaluation config ───────────────────────────────

  "LLaMA-3 405B with PP=4 TP=8" should "have feasible decode latency" in {
    val model405b = LlmModelSpec.llama3_405b
    val lat       = InferenceProfile.decodeLatencyPerToken(model405b, 1, 512, a100, tpDegree = 8, ppStages = 4)
    // Should be in the tens of ms range, not seconds
    lat.value should be > 0.0
    lat.value should be < 0.5 // under 500ms per token
  }
