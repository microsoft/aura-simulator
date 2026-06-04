// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.inference.model

import io.aura.core.types.*

/** Mixture-of-Experts (MoE) routing and parallelism model.
  *
  * Models the routing overhead and expert parallelism for MoE models like Mixtral, where only top-K experts are
  * activated per token.
  */
object MoEModel:

  /** Calculate effective parameters activated per token for MoE models. For non-MoE models, returns total parameters.
    */
  /** Shared parameter fraction: attention + embeddings + layer norms.
    *
    * In a standard transformer, attention is ~1/3 of each layer and FFN is ~2/3. For MoE models, the FFN is replicated
    * across experts while attention is shared. So: shared = attention_frac + embedding overhead ≈ 1/3 of a single
    * expert-equivalent.
    *
    * For Mixtral 8x7B (46.7B total): shared ~1.5-2B, so shared_frac ≈ 4%. We derive it: shared = (1 expert equivalent)
    * / (numExperts + 1 attention-equivalent)
    */
  def sharedFraction(model: LlmModelSpec): Double =
    if !model.isMoE || model.numExperts <= 1 then 1.0
    else
      // attention is ~1/3 per layer, FFN is ~2/3 per layer
      // total = numExperts × FFN + 1 × attention = numExperts × (2/3) + (1/3) of one layer equiv
      // But total params = numExperts × expert_FFN + shared_attention
      // shared_attention ≈ total / (numExperts * 2 + 1) — rough approximation
      // For Mixtral: 46.7 / (8*2 + 1) = 46.7 / 17 ≈ 2.75B shared → 5.9%
      1.0 / (model.numExperts * 2.0 + 1.0)

  def activeParametersBillions(model: LlmModelSpec): Double =
    if !model.isMoE || model.numExperts <= 1 then model.parametersBillions
    else
      val sf                    = sharedFraction(model)
      val expertFractionOfTotal = 1.0 - sf
      val activeFraction        = model.topKExperts.toDouble / model.numExperts
      model.parametersBillions * (sf + expertFractionOfTotal * activeFraction)

  /** Expert parallelism communication overhead.
    *
    * In EP mode, tokens are routed to different GPUs holding different experts. This incurs all-to-all communication.
    *
    * @param batchSize
    *   number of tokens in the batch
    * @param hiddenDim
    *   model hidden dimension
    * @param epDegree
    *   expert parallelism degree
    * @param bandwidthGBps
    *   interconnect bandwidth
    * @return
    *   communication time per layer
    */
  def expertParallelCommTime(
      batchSize: Int,
      hiddenDim: Int,
      epDegree: Int,
      bandwidthGBps: Double
  ): SimTime =
    if epDegree <= 1 || bandwidthGBps <= 0 then SimTime.Zero
    else
      // All-to-all: each GPU sends (batch/ep) × hidden × 2 bytes to each other GPU
      val bytesPerGpu = batchSize.toDouble * hiddenDim * 2.0 / epDegree
      val totalBytes  = bytesPerGpu * (epDegree - 1)
      val sizeGB      = totalBytes / (1024.0 * 1024.0 * 1024.0)
      SimTime(sizeGB / bandwidthGBps)

  /** Adjust decode latency for MoE models. Only loads active expert weights, reducing memory bandwidth requirements.
    */
  def moeDecodeLatencyMultiplier(model: LlmModelSpec): Double =
    if !model.isMoE || model.numExperts <= 1 then 1.0
    else
      val activeFraction = activeParametersBillions(model) / model.parametersBillions
      activeFraction
