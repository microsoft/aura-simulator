// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.gpu

import io.aura.core.types.*

/** DVFS (Dynamic Voltage and Frequency Scaling) policies for GPUs.
  *
  * A DVFS policy maps (device spec, current activity, power budget) → target frequency (MHz). These are pure functions
  * for compositional policy design.
  */
type DvfsPolicy = (GpuDeviceSpec, GpuActivityProfile, Watts) => Int

object DvfsPolicy:

  /** Max performance: always run at maximum frequency. */
  val maxPerformance: DvfsPolicy = (spec, _, _) => spec.frequencyMaxMHz

  /** Power-capped: reduce frequency if total power exceeds budget. Linearly scales frequency down to maintain power
    * within budget.
    */
  val powerCapped: DvfsPolicy = (spec, activity, powerBudget) =>
    val currentPower = MultiComponentGpuPower.totalPower(spec, activity)
    if currentPower <= powerBudget then spec.frequencyMaxMHz
    else
      val ratio = powerBudget.value / currentPower.value
      // Frequency scales as sqrt(power) since P ∝ f²V² and V ∝ f
      val freqRatio  = math.sqrt(ratio).min(1.0).max(0.0)
      val targetFreq = (spec.frequencyMinMHz + (spec.frequencyMaxMHz - spec.frequencyMinMHz) * freqRatio).toInt
      targetFreq.max(spec.frequencyMinMHz).min(spec.frequencyMaxMHz)

  /** Energy-proportional: scale frequency with compute utilization. Memory-bound workloads get lower frequency (saving
    * energy with minimal latency impact).
    */
  val energyProportional: DvfsPolicy = (spec, activity, _) =>
    val smUtil = activity.smUtilization.value
    val hbmUtil =
      if spec.hbmBandwidthGBps > 0 then (activity.hbmTotalBWGBps / spec.hbmBandwidthGBps).min(1.0)
      else 0.0

    // If memory-bound (HBM util >> SM util), lower frequency
    val boundednessRatio = if hbmUtil > 0.01 then (smUtil / hbmUtil).min(1.0) else 1.0
    val freqRange        = spec.frequencyMaxMHz - spec.frequencyMinMHz
    val targetFreq       = spec.frequencyMinMHz + (freqRange * boundednessRatio * smUtil.max(0.1)).toInt
    targetFreq.max(spec.frequencyMinMHz).min(spec.frequencyMaxMHz)

  /** Phase-aware DVFS (PA-DVFS) — the novel algorithm.
    *
    *   - Prefill phase: compute-bound → max frequency
    *   - Decode phase: memory-bound → reduce to f_max × (compute_util / bandwidth_util)
    *
    * @param isPrefillPhase
    *   function that determines if current workload is in prefill phase
    */
  def phaseAware(isPrefillPhase: Boolean): DvfsPolicy = (spec, activity, _) =>
    if isPrefillPhase then
      // Prefill is compute-bound: max frequency
      spec.frequencyMaxMHz
    else
      // Decode is memory-bound: reduce frequency proportionally
      val smUtil = activity.smUtilization.value
      val hbmUtil =
        if spec.hbmBandwidthGBps > 0 then (activity.hbmTotalBWGBps / spec.hbmBandwidthGBps).min(1.0)
        else 0.01

      val ratio     = if hbmUtil > 0.01 then (smUtil / hbmUtil).min(1.0) else 1.0
      val freqRange = spec.frequencyMaxMHz - spec.frequencyMinMHz
      // Floor at 40% of max to avoid excessive latency penalty
      val minRatio       = 0.4
      val effectiveRatio = ratio.max(minRatio)
      val targetFreq     = spec.frequencyMinMHz + (freqRange * effectiveRatio).toInt
      targetFreq.max(spec.frequencyMinMHz).min(spec.frequencyMaxMHz)

  /** Fixed frequency for testing/baselines. */
  def fixed(frequencyMHz: Int): DvfsPolicy = (spec, _, _) =>
    frequencyMHz.max(spec.frequencyMinMHz).min(spec.frequencyMaxMHz)
