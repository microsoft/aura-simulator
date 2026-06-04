// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.inference.model

import io.aura.core.types.*
import io.aura.gpu.*

/** Roofline-based latency model for LLM inference.
  *
  * Prefill phase: compute-bound → limited by GPU FLOPS Decode phase: memory-bound → limited by HBM bandwidth
  *
  * Supports tensor parallelism (TP), pipeline parallelism (PP), and expert parallelism (EP) for MoE models.
  */
object InferenceProfile:

  /** Compute prefill latency for a batch of prompts.
    *
    * Prefill processes all prompt tokens in parallel (compute-bound). Time ≈ FLOPs / (GPU TFLOPS × utilization)
    *
    * With PP: each stage processes numLayers/ppStages layers, plus pipeline bubble. With MoE: only active expert FLOPs
    * are computed.
    *
    * @param model
    *   LLM model spec
    * @param promptTokens
    *   number of prompt tokens
    * @param batchSize
    *   number of requests in batch
    * @param deviceSpec
    *   GPU device spec
    * @param tpDegree
    *   tensor parallelism degree
    * @param ppStages
    *   pipeline parallelism stages
    * @param epDegree
    *   expert parallelism degree (MoE only)
    * @param computeEfficiency
    *   effective GPU utilization (0-1, typically 0.3-0.7)
    */
  def prefillLatency(
      model: LlmModelSpec,
      promptTokens: Int,
      batchSize: Int,
      deviceSpec: GpuDeviceSpec,
      tpDegree: Int = 1,
      ppStages: Int = 1,
      epDegree: Int = 1,
      computeEfficiency: Double = 0.5
  ): SimTime =
    // For MoE models, prefill FLOPs use active parameters only
    // The 2×params estimate covers linear layers (attention projections + FFN)
    val effectiveParams = MoEModel.activeParametersBillions(model)
    val linearFlops     = 2.0 * effectiveParams * 1e9 * promptTokens * batchSize

    // Quadratic attention FLOPs: Q×K^T and attn×V, proportional to seqLen²
    // This is separate from linear layer FLOPs (it's the dot-product attention itself)
    val attentionFlops = model.attentionFlopsPerToken(promptTokens) * batchSize
    val totalFLOPs     = linearFlops + attentionFlops

    // With PP: each stage processes a fraction of layers → FLOPs / ppStages
    // With TP: each GPU within a stage computes its shard → FLOPs / tpDegree
    val perGpuFlops     = totalFLOPs / tpDegree / ppStages
    val effectiveTflops = deviceSpec.tflopsFloat16 * computeEfficiency * 1e12
    if effectiveTflops <= 0 then SimTime.MaxValue
    else
      val computeTime = perGpuFlops / effectiveTflops

      // TP communication: all-reduce per layer within each PP stage
      val layersPerStage = model.numLayers / ppStages.max(1)
      val tpCommOverhead = if tpDegree > 1 then
        val messageSize = (model.hiddenDim * batchSize * 2).toLong
        val commTime = ParallelismStrategy.allReduceTime(
          messageSize,
          tpDegree,
          deviceSpec.nvlinkBandwidthGBps
        )
        commTime.value * layersPerStage * 2 // 2 all-reduces per layer
      else 0.0

      // EP communication: all-to-all per MoE layer
      val epCommOverhead = if model.isMoE && epDegree > 1 then
        val moeLayerCount = layersPerStage // each layer has MoE FFN
        val commPerLayer = MoEModel.expertParallelCommTime(
          batchSize * promptTokens,
          model.hiddenDim,
          epDegree,
          deviceSpec.nvlinkBandwidthGBps
        )
        commPerLayer.value * moeLayerCount
      else 0.0

      // PP overhead: pipeline bubble during prefill
      // For prefill, we model micro-batching: split batch into ppStages micro-batches
      val microBatches   = (batchSize.max(1) * promptTokens / 512).max(ppStages)
      val bubbleFraction = ParallelismStrategy.pipelineBubbleFraction(ppStages, microBatches)

      // PP inter-stage communication: point-to-point send of activations
      val ppCommOverhead = if ppStages > 1 then
        val activationBytes = (model.hiddenDim * batchSize * 2).toLong
        val activationGB    = activationBytes.toDouble / (1024.0 * 1024.0 * 1024.0)
        // Use NVLink for intra-node, IB for inter-node (assume NVLink here)
        val sendTime = activationGB / deviceSpec.nvlinkBandwidthGBps.max(0.001)
        sendTime * (ppStages - 1)
      else 0.0

      val baseTime = computeTime + tpCommOverhead + epCommOverhead + ppCommOverhead
      SimTime(baseTime * (1.0 + bubbleFraction))

  /** Compute decode latency per token.
    *
    * Decode generates one token at a time (memory-bound). Time per token ≈ model_size / (HBM_bandwidth × utilization)
    *
    * With PP: each stage loads numLayers/ppStages layers of weights. With MoE: only active expert weights are loaded
    * per token.
    *
    * @param model
    *   LLM model spec
    * @param batchSize
    *   number of concurrent sequences being decoded
    * @param seqLen
    *   current sequence length (for KV cache reads)
    * @param deviceSpec
    *   GPU device spec
    * @param tpDegree
    *   tensor parallelism degree
    * @param ppStages
    *   pipeline parallelism stages
    * @param epDegree
    *   expert parallelism degree (MoE only)
    * @param memoryEfficiency
    *   effective HBM bandwidth utilization (0-1, typically 0.5-0.8)
    */
  def decodeLatencyPerToken(
      model: LlmModelSpec,
      batchSize: Int,
      seqLen: Int,
      deviceSpec: GpuDeviceSpec,
      tpDegree: Int = 1,
      ppStages: Int = 1,
      epDegree: Int = 1,
      memoryEfficiency: Double = 0.7
  ): SimTime =
    // Weight loading per GPU:
    // - For dense models: model_size / TP / PP
    // - For MoE: shared_weights/TP/PP + active_expert_weights/TP/PP/EP
    //   EP distributes experts across GPUs, so each GPU holds fewer experts
    val totalModelBytes = model.modelSizeMB.value * 1024.0 * 1024.0
    val weightBytes = if model.isMoE && model.numExperts > 1 then
      val sf             = MoEModel.sharedFraction(model)
      val expertFraction = 1.0 - sf
      val activeFraction = model.topKExperts.toDouble / model.numExperts
      val sharedBytes    = totalModelBytes * sf / tpDegree / ppStages
      val expertBytes    = totalModelBytes * expertFraction * activeFraction / tpDegree / ppStages / epDegree.max(1)
      sharedBytes + expertBytes
    else totalModelBytes / tpDegree / ppStages

    // KV cache reading: batch_size × seqLen × kv_per_token_per_layer × layers_per_stage / TP
    // EP doesn't affect KV cache (attention is not MoE)
    val layersPerStage = model.numLayers / ppStages.max(1)
    val kvCacheBytes   = batchSize.toDouble * seqLen * model.kvCacheBytesPerTokenPerLayer * layersPerStage / tpDegree

    val totalBytes         = weightBytes + kvCacheBytes
    val effectiveBandwidth = deviceSpec.hbmBandwidthGBps * memoryEfficiency * 1e9

    if effectiveBandwidth <= 0 then SimTime.MaxValue
    else
      val memoryTime = totalBytes / effectiveBandwidth

      // TP communication within each PP stage
      val tpCommOverhead = if tpDegree > 1 then
        val messageSize = (model.hiddenDim * batchSize * 2).toLong
        ParallelismStrategy
          .allReduceTime(
            messageSize,
            tpDegree,
            deviceSpec.nvlinkBandwidthGBps
          )
          .value * layersPerStage * 2
      else 0.0

      // EP communication for MoE layers
      val epCommOverhead = if model.isMoE && epDegree > 1 then
        val commPerLayer = MoEModel.expertParallelCommTime(
          batchSize,
          model.hiddenDim,
          epDegree,
          deviceSpec.nvlinkBandwidthGBps
        )
        commPerLayer.value * layersPerStage
      else 0.0

      // PP inter-stage: point-to-point activation transfer
      // During decode, micro-batch count equals batch size (1 token per request)
      val ppCommOverhead = if ppStages > 1 then
        val activationBytes = (model.hiddenDim * batchSize * 2).toLong
        val activationGB    = activationBytes.toDouble / (1024.0 * 1024.0 * 1024.0)
        val sendTime        = activationGB / deviceSpec.nvlinkBandwidthGBps.max(0.001)
        sendTime * (ppStages - 1)
      else 0.0

      // PP bubble for decode: micro-batches = batch_size
      val bubbleFraction = ParallelismStrategy.pipelineBubbleFraction(ppStages, batchSize.max(1))

      val baseTime = memoryTime + tpCommOverhead + epCommOverhead + ppCommOverhead
      SimTime(baseTime * (1.0 + bubbleFraction))

  /** GPU activity profile during prefill (compute-bound).
    *
    * Prefill is compute-bound: SM utilization is high and scales with batch size. HBM is used for weight loading + KV
    * cache writes, scaling modestly with batch.
    */
  def prefillActivity(
      model: LlmModelSpec,
      batchSize: Int,
      deviceSpec: GpuDeviceSpec,
      tpDegree: Int = 1
  ): GpuActivityProfile =
    // SM utilization scales with batch: single request ~60%, saturates near 95% at large batch
    val baseSmUtil = 0.6
    val batchScale = 1.0 - math.exp(-batchSize / 8.0) // sigmoid-like saturation
    val smUtil     = (baseSmUtil + (0.95 - baseSmUtil) * batchScale).min(0.95)
    // HBM reads scale modestly (weight loading amortized, but KV writes increase)
    val hbmReadFrac  = 0.3 + 0.15 * batchScale
    val hbmWriteFrac = 0.05 + 0.10 * batchScale // KV cache writes grow with batch
    GpuActivityProfile(
      smUtilization = Utilization(smUtil),
      hbmReadBWGBps = deviceSpec.hbmBandwidthGBps * hbmReadFrac,
      hbmWriteBWGBps = deviceSpec.hbmBandwidthGBps * hbmWriteFrac,
      nvlinkUtilization = if tpDegree > 1 then Utilization(0.5) else Utilization.Zero,
      currentFreqMHz = deviceSpec.frequencyMaxMHz
    )

  /** GPU activity profile during decode (memory-bound).
    *
    * Decode is memory-bound: SM utilization grows with batch size (more tokens decoded in parallel). HBM bandwidth is
    * high (weight loading dominates). At large batch sizes, decode transitions from purely memory-bound toward
    * compute-bound.
    */
  def decodeActivity(
      model: LlmModelSpec,
      batchSize: Int,
      seqLen: Int,
      deviceSpec: GpuDeviceSpec,
      tpDegree: Int = 1
  ): GpuActivityProfile =
    // SM utilization: low at small batch, grows sub-linearly
    // Real GPUs: bs=1 ~5%, bs=32 ~25-35%, bs=128 ~50-60%, bs=256 ~70%
    val computeUtil = (0.05 + 0.65 * (1.0 - math.exp(-batchSize / 64.0))).min(0.75)
    // HBM read utilization increases with batch (more KV cache reads)
    val kvCachePressure = (batchSize.toDouble * seqLen / 100000.0).min(0.25)
    val hbmReadFrac     = 0.65 + kvCachePressure
    val hbmWriteFrac    = 0.03 + 0.02 * (batchSize / 128.0).min(1.0)
    GpuActivityProfile(
      smUtilization = Utilization(computeUtil.max(0.05)),
      hbmReadBWGBps = deviceSpec.hbmBandwidthGBps * hbmReadFrac.min(0.95),
      hbmWriteBWGBps = deviceSpec.hbmBandwidthGBps * hbmWriteFrac,
      nvlinkUtilization = if tpDegree > 1 then Utilization(0.3) else Utilization.Zero,
      currentFreqMHz = deviceSpec.frequencyMaxMHz
    )
