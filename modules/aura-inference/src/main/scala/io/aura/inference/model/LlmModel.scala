// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.inference.model

import io.aura.core.types.*

/** LLM model specification.
  *
  * Captures the architectural parameters needed for latency/memory modeling: parameter count, layers, attention heads,
  * KV cache dimensions, etc.
  */
final case class LlmModelSpec(
    modelId: ModelId,
    name: String,
    parametersBillions: Double,
    numLayers: Int,
    hiddenDim: Int,
    numHeads: Int,
    numKvHeads: Int,
    headDim: Int,
    vocabSize: Int,
    maxSeqLen: Int,
    bytesPerParam: Int = 2,
    isMoE: Boolean = false,
    numExperts: Int = 1,
    topKExperts: Int = 1
):
  /** Total model size in MB (parameters × bytes per param). */
  def modelSizeMB: MegaBytes =
    MegaBytes(parametersBillions * 1e9 * bytesPerParam / (1024.0 * 1024.0))

  /** KV cache size per token per layer in bytes. \= 2 × numKvHeads × headDim × bytesPerParam (key + value)
    */
  def kvCacheBytesPerTokenPerLayer: Long =
    2L * numKvHeads * headDim * bytesPerParam

  /** Total KV cache size per token across all layers in MB. */
  def kvCacheMBPerToken: Double =
    kvCacheBytesPerTokenPerLayer.toDouble * numLayers / (1024.0 * 1024.0)

  /** Total KV cache for a sequence of given length in MB. */
  def kvCacheMBForSeqLen(seqLen: Int): MegaBytes =
    MegaBytes(kvCacheMBPerToken * seqLen)

  /** FLOPs per token for a forward pass (approximate, prefill). */
  def flopsPerToken: Double =
    // Approximate: 2 × parameters per token (matrix multiply = 2 FLOPs per param)
    2.0 * parametersBillions * 1e9

  /** FLOPs for attention computation for sequence of length seqLen. */
  def attentionFlopsPerToken(seqLen: Int): Double =
    // Attention: 2 × numHeads × headDim × seqLen per layer × numLayers
    2.0 * numHeads * headDim * seqLen.toDouble * numLayers

object LlmModelSpec:

  val llama2_7b: LlmModelSpec = LlmModelSpec(
    modelId = ModelId(1),
    name = "LLaMA-2-7B",
    parametersBillions = 6.74,
    numLayers = 32,
    hiddenDim = 4096,
    numHeads = 32,
    numKvHeads = 32,
    headDim = 128,
    vocabSize = 32000,
    maxSeqLen = 4096
  )

  val llama2_70b: LlmModelSpec = LlmModelSpec(
    modelId = ModelId(2),
    name = "LLaMA-2-70B",
    parametersBillions = 68.98,
    numLayers = 80,
    hiddenDim = 8192,
    numHeads = 64,
    numKvHeads = 8,
    headDim = 128,
    vocabSize = 32000,
    maxSeqLen = 4096
  )

  val mixtral_8x7b: LlmModelSpec = LlmModelSpec(
    modelId = ModelId(3),
    name = "Mixtral-8x7B",
    parametersBillions = 46.7,
    numLayers = 32,
    hiddenDim = 4096,
    numHeads = 32,
    numKvHeads = 8,
    headDim = 128,
    vocabSize = 32000,
    maxSeqLen = 32768,
    isMoE = true,
    numExperts = 8,
    topKExperts = 2
  )

  val llama3_405b: LlmModelSpec = LlmModelSpec(
    modelId = ModelId(4),
    name = "LLaMA-3-405B",
    parametersBillions = 405.0,
    numLayers = 126,
    hiddenDim = 16384,
    numHeads = 128,
    numKvHeads = 8,
    headDim = 128,
    vocabSize = 128256,
    maxSeqLen = 131072
  )
