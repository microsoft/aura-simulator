// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.inference.energy

import io.aura.core.types.*
import io.aura.inference.model.*
import io.aura.gpu.*

/** Per-phase power model for LLM inference.
  *
  * Distinguishes between prefill and decode phases, which have fundamentally different GPU utilization patterns:
  *   - Prefill: compute-bound → high SM utilization, moderate HBM
  *   - Decode: memory-bound → low SM utilization, high HBM bandwidth
  */
object InferencePowerModel:

  /** Estimate GPU power during prefill phase. */
  def prefillPower(
      model: LlmModelSpec,
      batchSize: Int,
      spec: GpuDeviceSpec,
      tpDegree: Int = 1
  ): GpuPowerComponents =
    val activity = InferenceProfile.prefillActivity(model, batchSize, spec, tpDegree)
    MultiComponentGpuPower.calculate(spec, activity)

  /** Estimate GPU power during decode phase. */
  def decodePower(
      model: LlmModelSpec,
      batchSize: Int,
      seqLen: Int,
      spec: GpuDeviceSpec,
      tpDegree: Int = 1
  ): GpuPowerComponents =
    val activity = InferenceProfile.decodeActivity(model, batchSize, seqLen, spec, tpDegree)
    MultiComponentGpuPower.calculate(spec, activity)

  /** Estimate total energy for a complete inference request (prefill + decode). */
  def requestEnergy(
      model: LlmModelSpec,
      promptTokens: Int,
      outputTokens: Int,
      batchSize: Int,
      spec: GpuDeviceSpec,
      tpDegree: Int = 1
  ): WattHours =
    // Prefill energy
    val prefillTime   = InferenceProfile.prefillLatency(model, promptTokens, batchSize, spec, tpDegree)
    val prefillW      = prefillPower(model, batchSize, spec, tpDegree).total
    val prefillEnergy = prefillW.value * prefillTime.value / 3600.0

    // Decode energy (averaged over sequence length)
    val avgSeqLen          = promptTokens + outputTokens / 2
    val decodeTimePerToken = InferenceProfile.decodeLatencyPerToken(model, batchSize, avgSeqLen, spec, tpDegree)
    val decodeW            = decodePower(model, batchSize, avgSeqLen, spec, tpDegree).total
    val decodeEnergy       = decodeW.value * decodeTimePerToken.value * outputTokens / 3600.0

    // Per-request share (divide by batch size)
    WattHours((prefillEnergy + decodeEnergy) / batchSize.max(1))

  /** Energy per output token at given operating point. */
  def energyPerToken(
      model: LlmModelSpec,
      batchSize: Int,
      seqLen: Int,
      spec: GpuDeviceSpec,
      tpDegree: Int = 1,
      freqRatio: Double = 1.0
  ): Double =
    val activity = InferenceProfile.decodeActivity(model, batchSize, seqLen, spec, tpDegree)
    val adjustedActivity = activity.copy(
      currentFreqMHz = (spec.frequencyMinMHz + (spec.frequencyMaxMHz - spec.frequencyMinMHz) * freqRatio).toInt
    )
    val power   = MultiComponentGpuPower.totalPower(spec, adjustedActivity)
    val latency = InferenceProfile.decodeLatencyPerToken(model, batchSize, seqLen, spec, tpDegree)
    // Energy per token = power × time_per_token / batch_size (amortized)
    power.value * latency.value / batchSize.max(1)
