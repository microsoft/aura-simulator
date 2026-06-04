// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.inference

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.inference.model.*

/** Validate Aura's latency predictions against Vidur (MLSys 2024) published numbers.
  *
  * Vidur achieves <9% error on normalized execution latency across 4 models. Our target: within 15% of known reference
  * values for decode latency per token.
  *
  * Reference values derived from:
  *   - Vidur paper (Agrawal et al., MLSys 2024), Figure 3 & Table 2
  *   - NVIDIA A100/H100 roofline analysis
  *   - Published vLLM benchmarks (LLaMA-2 on A100)
  *
  * The roofline model gives a lower bound on decode latency (best-case memory-bound). Real systems add scheduling
  * overhead, so we verify:
  *   1. Our predictions are in the right ballpark (same order of magnitude) 2. Our predictions are consistent with the
  *      memory-bandwidth roofline 3. Relative ordering across models/GPUs is correct
  */
class VidurValidationSpec extends AnyFlatSpec with Matchers:

  val a100     = GpuDeviceSpec.a100Sxm
  val h100     = GpuDeviceSpec.h100Sxm
  val llama7b  = LlmModelSpec.llama2_7b
  val llama70b = LlmModelSpec.llama2_70b

  // ── Roofline Reference Values ──────────────────────────────────────
  // These are computed from first principles:
  //   decode_time = model_bytes / (HBM_BW × efficiency)
  // and validated against published benchmarks.

  // LLaMA-2-7B on A100: ~13.5 GB weights / (2039 GB/s × 0.7) ≈ 9.5 ms
  // Published vLLM benchmark: ~8-12 ms/token at batch=1
  val expectedDecode7bA100Ms = 9.5

  // LLaMA-2-70B on A100 (TP=1): ~131 GB weights / (2039 × 0.7) ≈ 91.8 ms
  val expectedDecode70bA100Ms = 91.8

  // LLaMA-2-7B on H100: ~13.5 GB / (3352 × 0.7) ≈ 5.75 ms
  val expectedDecode7bH100Ms = 5.75

  // LLaMA-2-70B on A100 (TP=4): weights/4 + KV cache
  // ~131/4 GB weights / (2039 × 4 × 0.7) + comm overhead
  val expectedDecode70bA100Tp4Ms = 5.8 // much faster with TP

  // ── Test 1: LLaMA-2-7B decode on A100 ──────────────────────────────

  "LLaMA-2-7B decode on A100" should "match roofline prediction within 15%" in {
    val lat   = InferenceProfile.decodeLatencyPerToken(llama7b, 1, 512, a100)
    val latMs = lat.value * 1000.0
    latMs shouldBe expectedDecode7bA100Ms +- (expectedDecode7bA100Ms * 0.15)
  }

  it should "match published vLLM range (8-12 ms/token at batch=1)" in {
    val lat   = InferenceProfile.decodeLatencyPerToken(llama7b, 1, 512, a100)
    val latMs = lat.value * 1000.0
    latMs should be >= 7.0  // faster than pure roofline is unlikely
    latMs should be <= 15.0 // real systems add some overhead
  }

  // ── Test 2: LLaMA-2-70B decode on A100 ─────────────────────────────

  "LLaMA-2-70B decode on A100" should "match roofline prediction within 15%" in {
    val lat   = InferenceProfile.decodeLatencyPerToken(llama70b, 1, 512, a100)
    val latMs = lat.value * 1000.0
    latMs shouldBe expectedDecode70bA100Ms +- (expectedDecode70bA100Ms * 0.15)
  }

  it should "be ~10x slower than 7B (proportional to model size ratio)" in {
    val lat7b  = InferenceProfile.decodeLatencyPerToken(llama7b, 1, 512, a100)
    val lat70b = InferenceProfile.decodeLatencyPerToken(llama70b, 1, 512, a100)
    val ratio  = lat70b.value / lat7b.value
    // 70B is ~10.2x larger than 7B (68.98/6.74)
    ratio shouldBe 10.2 +- 2.0
  }

  // ── Test 3: LLaMA-2-7B decode on H100 ──────────────────────────────

  "LLaMA-2-7B decode on H100" should "match roofline prediction within 15%" in {
    val lat   = InferenceProfile.decodeLatencyPerToken(llama7b, 1, 512, h100)
    val latMs = lat.value * 1000.0
    latMs shouldBe expectedDecode7bH100Ms +- (expectedDecode7bH100Ms * 0.15)
  }

  it should "be ~1.6x faster than A100 (HBM bandwidth ratio)" in {
    val latA100 = InferenceProfile.decodeLatencyPerToken(llama7b, 1, 512, a100)
    val latH100 = InferenceProfile.decodeLatencyPerToken(llama7b, 1, 512, h100)
    val speedup = latA100.value / latH100.value
    // H100 HBM BW = 3352 GB/s, A100 = 2039 GB/s → ratio ~1.64
    speedup shouldBe 1.64 +- 0.3
  }

  // ── Test 4: Tensor Parallelism scaling ──────────────────────────────

  "LLaMA-2-70B with TP=4 on A100" should "be much faster than TP=1" in {
    val tp1     = InferenceProfile.decodeLatencyPerToken(llama70b, 1, 512, a100, tpDegree = 1)
    val tp4     = InferenceProfile.decodeLatencyPerToken(llama70b, 1, 512, a100, tpDegree = 4)
    val speedup = tp1.value / tp4.value
    // Ideal: 4x. With comm overhead: ~3-3.8x
    speedup should be >= 2.5
    speedup should be <= 4.0
  }

  // ── Test 5: Prefill latency (compute-bound) ────────────────────────

  "LLaMA-2-7B prefill 512 tokens on A100" should "be in the expected range" in {
    // Prefill: 2 × 6.74B × 512 = 6.9 TFLOP + attention
    // A100 FP16: 312 TFLOPS × 0.5 efficiency = 156 effective TFLOPS
    // Time ≈ 6.9 / 156 ≈ 44 ms
    val lat   = InferenceProfile.prefillLatency(llama7b, 512, 1, a100)
    val latMs = lat.value * 1000.0
    latMs should be >= 20.0  // must take some time
    latMs should be <= 200.0 // shouldn't be absurdly long
  }

  it should "produce TTFT within Vidur's SLO range (P90 < 2s)" in {
    // Even with 2048 tokens and batch=8, TTFT should be under 2s
    val lat = InferenceProfile.prefillLatency(llama7b, 2048, 8, a100)
    lat.value should be < 2.0
  }

  // ── Test 6: Normalized E2E latency comparison ──────────────────────
  // Vidur Figure 3: LLaMA2-7B (TP1) normalized execution latency
  // ≈ 0.02-0.04 s/token (execution only, no queuing)

  "Normalized E2E latency for LLaMA-2-7B" should "be in Vidur's range" in {
    val promptLen         = 512
    val outputLen         = 128
    val prefill           = InferenceProfile.prefillLatency(llama7b, promptLen, 1, a100)
    val decodePerToken    = InferenceProfile.decodeLatencyPerToken(llama7b, 1, promptLen, a100)
    val totalTime         = prefill.value + decodePerToken.value * outputLen
    val normalizedLatency = totalTime / outputLen

    // Vidur's normalized execution latency for 7B is ~0.01-0.04 s/token
    // (varies by workload trace characteristics)
    normalizedLatency should be >= 0.005
    normalizedLatency should be <= 0.05
  }

  // ── Test 7: Batch size effect on decode ─────────────────────────────

  "Batch size scaling" should "increase decode latency sub-linearly" in {
    val b1  = InferenceProfile.decodeLatencyPerToken(llama7b, 1, 512, a100)
    val b8  = InferenceProfile.decodeLatencyPerToken(llama7b, 8, 512, a100)
    val b32 = InferenceProfile.decodeLatencyPerToken(llama7b, 32, 512, a100)
    // Larger batch = more KV cache reads, but weights amortized
    // Latency should increase but less than linearly
    b8.value should be > b1.value
    b32.value should be > b8.value
    // Sub-linear: batch 32x should not take 32x as long
    (b32.value / b1.value) should be < 10.0
  }

  // ── Test 8: Vidur TBT SLO compliance ────────────────────────────────

  "Decode TBT for LLaMA-2-7B at batch=1" should "meet Vidur's TBT P99 < 200ms SLO" in {
    val lat   = InferenceProfile.decodeLatencyPerToken(llama7b, 1, 512, a100)
    val latMs = lat.value * 1000.0
    latMs should be < 200.0 // Vidur SLO: TBT P99 < 200ms
  }

  "Decode TBT for LLaMA-2-70B at TP=4" should "meet Vidur's TBT P99 < 200ms SLO" in {
    val lat   = InferenceProfile.decodeLatencyPerToken(llama70b, 1, 512, a100, tpDegree = 4)
    val latMs = lat.value * 1000.0
    latMs should be < 200.0
  }
