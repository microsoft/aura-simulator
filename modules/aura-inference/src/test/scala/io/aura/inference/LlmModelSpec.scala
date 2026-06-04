// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.inference

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.inference.model.LlmModelSpec as ModelSpec

class LlmModelSpecTest extends AnyFlatSpec with Matchers:

  "LLaMA-2-7B" should "have correct model size" in {
    val model = ModelSpec.llama2_7b
    // 6.74B params × 2 bytes = ~12.8 GB = ~13107 MB
    model.modelSizeMB.value shouldBe 12852.0 +- 200.0
  }

  it should "have correct KV cache per token" in {
    val model = ModelSpec.llama2_7b
    // 2 × 32 heads × 128 dim × 2 bytes × 32 layers = 524,288 bytes per token ≈ 0.5 MB
    model.kvCacheMBPerToken shouldBe 0.5 +- 0.1
  }

  it should "compute flops per token" in {
    val model = ModelSpec.llama2_7b
    // 2 × 6.74B = ~13.48B FLOPs per token
    model.flopsPerToken shouldBe 13.48e9 +- 0.1e9
  }

  "LLaMA-2-70B" should "use GQA (fewer KV heads)" in {
    val model = ModelSpec.llama2_70b
    model.numKvHeads shouldBe 8
    model.numHeads shouldBe 64
    // GQA reduces KV cache by factor of 8
    model.kvCacheMBPerToken should be < ModelSpec.llama2_7b.kvCacheMBPerToken * 3
  }

  it should "have much larger model size" in {
    val model = ModelSpec.llama2_70b
    model.modelSizeMB.value should be > ModelSpec.llama2_7b.modelSizeMB.value * 8
  }

  "Mixtral-8x7B" should "be marked as MoE" in {
    val model = ModelSpec.mixtral_8x7b
    model.isMoE shouldBe true
    model.numExperts shouldBe 8
    model.topKExperts shouldBe 2
  }

  it should "have larger total params than dense 7B" in {
    ModelSpec.mixtral_8x7b.parametersBillions should be > ModelSpec.llama2_7b.parametersBillions
  }

  "LLaMA-3-405B" should "support very long sequences" in {
    ModelSpec.llama3_405b.maxSeqLen shouldBe 131072
  }

  it should "be the largest model" in {
    ModelSpec.llama3_405b.modelSizeMB.value should be > ModelSpec.llama2_70b.modelSizeMB.value
  }

  "kvCacheMBForSeqLen" should "scale linearly with sequence length" in {
    val model     = ModelSpec.llama2_7b
    val cache1024 = model.kvCacheMBForSeqLen(1024)
    val cache2048 = model.kvCacheMBForSeqLen(2048)
    cache2048.value shouldBe cache1024.value * 2 +- 0.001
  }

  "attentionFlopsPerToken" should "scale linearly with sequence length" in {
    val model   = ModelSpec.llama2_7b
    val flops1k = model.attentionFlopsPerToken(1000)
    val flops2k = model.attentionFlopsPerToken(2000)
    flops2k shouldBe flops1k * 2 +- 1.0
  }
