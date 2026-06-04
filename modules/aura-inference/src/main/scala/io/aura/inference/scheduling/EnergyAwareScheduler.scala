// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.inference.scheduling

import io.aura.core.types.*
import io.aura.inference.state.*
import io.aura.inference.model.*
import io.aura.gpu.*

/** Energy-aware scheduling algorithms — the paper's core contribution.
  *
  * Four novel algorithms that jointly optimize latency SLOs and GPU energy:
  *   1. PA-DVFS: Phase-Aware DVFS 2. PC-BAC: Power-Capped Batch Admission Control 3. ESPS: Energy-SLO Pareto Scheduling
  *      4. CAGR: Carbon-Aware Geographic Routing
  */
object EnergyAwareScheduler:

  // ─── Algorithm 1: Phase-Aware DVFS (PA-DVFS) ─────────────────────────

  /** PA-DVFS configuration. */
  final case class PaDvfsConfig(
      minFreqRatio: Double = 0.4,
      prefillFreqRatio: Double = 1.0,
      decodeFreqFloor: Double = 0.4
  )

  /** Compute target DVFS frequency based on current inference phase.
    *
    * Insight: Decode is memory-bound → GPU SM frequency can be lowered with minimal latency impact, saving 15-30%
    * power.
    *
    * Prefill: max frequency (compute-bound) Decode: f_max × max(compute_util / bandwidth_util, floor)
    */
  def paDvfsFrequency(
      spec: GpuDeviceSpec,
      activity: GpuActivityProfile,
      isPrefill: Boolean,
      config: PaDvfsConfig = PaDvfsConfig()
  ): Int =
    if isPrefill then (spec.frequencyMaxMHz * config.prefillFreqRatio).toInt.min(spec.frequencyMaxMHz)
    else
      val smUtil = activity.smUtilization.value
      val hbmUtil =
        if spec.hbmBandwidthGBps > 0 then (activity.hbmTotalBWGBps / spec.hbmBandwidthGBps).min(1.0).max(0.01)
        else 0.01
      val ratio  = (smUtil / hbmUtil).min(1.0).max(config.decodeFreqFloor)
      val target = (spec.frequencyMinMHz + (spec.frequencyMaxMHz - spec.frequencyMinMHz) * ratio).toInt
      target.max(spec.frequencyMinMHz).min(spec.frequencyMaxMHz)

  // ─── Algorithm 2: Power-Capped Batch Admission Control (PC-BAC) ──────

  /** PC-BAC configuration. */
  final case class PcBacConfig(
      powerBudgetWatts: Watts,
      tdpFraction: Double = 0.8,
      preemptLowPriority: Boolean = true
  )

  /** Power-capped batch admission control.
    *
    * Insight: Larger decode batches are more energy-efficient per token (amortize HBM bandwidth). Admit requests while
    * total_power < budget.
    *
    * @return
    *   (requests to admit, requests to preempt)
    */
  def pcBacAdmission(
      state: EngineState,
      spec: GpuDeviceSpec,
      config: PcBacConfig
  ): (Vector[InferenceRequestId], Vector[InferenceRequestId]) =
    // Estimate current power
    val currentPower = estimateBatchPower(state, spec)

    val powerBudget = config.powerBudgetWatts

    if currentPower.value > powerBudget.value && config.preemptLowPriority then
      // Over budget: preempt lowest-priority running requests until under budget
      val sorted      = state.runningBatch.sortBy(_.priority)
      val perReqPower = estimatePerRequestPower(state, spec)
      val (toPreempt, _) = sorted.foldLeft((Vector.empty[InferenceRequestId], currentPower.value)) {
        case ((preempted, estimated), req) =>
          if estimated > powerBudget.value then (preempted :+ req.requestId, estimated - perReqPower)
          else (preempted, estimated)
      }
      (Vector.empty, toPreempt)
    else
      // Under budget: admit requests up to budget
      val perReqPower = estimatePerRequestPower(state, spec)
      val (toAdmit, _) = state.waitingQueue.foldLeft((Vector.empty[InferenceRequestId], currentPower.value)) {
        case ((admitted, estimated), req) =>
          val pagesNeeded = state.kvCache.pagesNeeded(req.promptTokens + req.maxOutputTokens)
          if estimated + perReqPower <= powerBudget.value && state.kvCache.canAllocate(pagesNeeded) then
            (admitted :+ req.requestId, estimated + perReqPower)
          else (admitted, estimated)
      }
      (toAdmit, Vector.empty)

  private def estimateBatchPower(state: EngineState, spec: GpuDeviceSpec): Watts =
    if state.currentBatchSize == 0 then spec.idleWatts
    else
      // Estimate based on batch size and decode activity profile
      val activity = InferenceProfile.decodeActivity(
        state.model,
        state.currentBatchSize,
        1024,
        spec,
        state.tpDegree
      )
      MultiComponentGpuPower.totalPower(spec, activity)

  private def estimatePerRequestPower(state: EngineState, spec: GpuDeviceSpec): Double =
    val dynamicRange = spec.tdpWatts.value - spec.idleWatts.value
    // Marginal power per request decreases with batch size (amortization)
    dynamicRange / (state.currentBatchSize + 1).toDouble.max(1.0) * 0.3

  // ─── Algorithm 3: Energy-SLO Pareto Scheduling (ESPS) ────────────────

  /** ESPS configuration. */
  final case class EspsConfig(
      sloTarget: Double = 0.95,
      batchSizeCandidates: Vector[Int] = Vector(1, 2, 4, 8, 16, 32, 64, 128, 256),
      frequencyCandidates: Vector[Double] = Vector(0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
      replicaCandidates: Vector[Int] = Vector(1, 2, 4, 8),
      arrivalRatePerSecond: Double = 10.0
  )

  /** Operating point on the energy-SLO Pareto frontier. */
  final case class OperatingPoint(
      batchSize: Int,
      frequencyRatio: Double,
      replicas: Int,
      estimatedTpotMs: Double,
      estimatedEnergyPerToken: Double,
      estimatedThroughputTokPerSec: Double,
      estimatedSloCompliance: Double,
      estimatedTotalPowerWatts: Double
  )

  /** Full Pareto frontier: all non-dominated operating points. */
  def espsParetoFrontier(
      model: LlmModelSpec,
      spec: GpuDeviceSpec,
      avgPromptTokens: Int,
      avgOutputTokens: Int,
      targetSloTpot: SimTime,
      config: EspsConfig = EspsConfig()
  ): Vector[OperatingPoint] =
    val all = espsAllCandidates(model, spec, avgPromptTokens, avgOutputTokens, targetSloTpot, config)
    // A point is Pareto-dominated if another point has both lower energy AND higher SLO compliance
    all
      .filter { p =>
        !all.exists(q =>
          q.estimatedEnergyPerToken < p.estimatedEnergyPerToken &&
            q.estimatedSloCompliance >= p.estimatedSloCompliance &&
            (q.estimatedEnergyPerToken < p.estimatedEnergyPerToken ||
              q.estimatedSloCompliance > p.estimatedSloCompliance)
        )
      }
      .sortBy(_.estimatedEnergyPerToken)

  /** Find the Pareto-optimal operating point minimizing energy subject to SLO.
    *
    * Insight: Different workloads have different optimal points. Sweeps 3 knobs: batch size × GPU frequency × replica
    * count.
    *
    * @return
    *   optimal operating point
    */
  def espsOptimalPoint(
      model: LlmModelSpec,
      spec: GpuDeviceSpec,
      avgPromptTokens: Int,
      avgOutputTokens: Int,
      targetSloTpot: SimTime,
      config: EspsConfig = EspsConfig()
  ): OperatingPoint =
    val candidates = espsAllCandidates(model, spec, avgPromptTokens, avgOutputTokens, targetSloTpot, config)
    // Filter by SLO target, then minimize energy
    val feasible = candidates.filter(_.estimatedSloCompliance >= config.sloTarget)
    if feasible.nonEmpty then feasible.minBy(_.estimatedEnergyPerToken)
    else candidates.maxBy(_.estimatedSloCompliance)

  /** Generate all candidate operating points for the 3-knob sweep. */
  private def espsAllCandidates(
      model: LlmModelSpec,
      spec: GpuDeviceSpec,
      avgPromptTokens: Int,
      avgOutputTokens: Int,
      targetSloTpot: SimTime,
      config: EspsConfig
  ): Vector[OperatingPoint] =
    val avgSeqLen = avgPromptTokens + avgOutputTokens / 2
    for
      bs   <- config.batchSizeCandidates
      fr   <- config.frequencyCandidates
      reps <- config.replicaCandidates
    yield
      val freqMHz = (spec.frequencyMinMHz + (spec.frequencyMaxMHz - spec.frequencyMinMHz) * fr).toInt

      // Base TPOT from roofline model
      val baseTpot = InferenceProfile.decodeLatencyPerToken(model, bs, avgSeqLen, spec)
      // Frequency scaling penalty: decode is memory-bound so effect is modest
      val freqPenalty  = 1.0 + (1.0 - fr) * 0.15
      val adjustedTpot = baseTpot.value * freqPenalty

      // Throughput: tokens/s per replica = batch_size / tpot_seconds
      val tokPerSecPerReplica = if adjustedTpot > 0 then bs.toDouble / adjustedTpot else 0.0
      val totalTokPerSec      = tokPerSecPerReplica * reps

      // SLO compliance: based on TPOT meeting target, and throughput meeting arrival rate
      val tpotOk             = adjustedTpot <= targetSloTpot.value
      val avgOutputTokPerReq = avgOutputTokens.toDouble
      val requiredTokPerSec  = config.arrivalRatePerSecond * avgOutputTokPerReq
      val throughputOk       = totalTokPerSec >= requiredTokPerSec
      val sloCompliance =
        if tpotOk && throughputOk then 1.0
        else if tpotOk then (totalTokPerSec / requiredTokPerSec).min(1.0)
        else
          (targetSloTpot.value / adjustedTpot).min(1.0) * (if throughputOk then 1.0
                                                           else totalTokPerSec / requiredTokPerSec).min(1.0)

      // Power at this operating point
      val activity         = InferenceProfile.decodeActivity(model, bs, avgSeqLen, spec)
      val adjustedActivity = activity.copy(currentFreqMHz = freqMHz)
      val powerPerGpu      = MultiComponentGpuPower.totalPower(spec, adjustedActivity)
      val totalPower       = powerPerGpu.value * reps

      // Energy per output token = (power × tpot) / batch_size, across all replicas
      val energyPerToken = totalPower * adjustedTpot / bs.max(1)

      OperatingPoint(bs, fr, reps, adjustedTpot * 1000, energyPerToken, totalTokPerSec, sloCompliance, totalPower)

  // ─── Algorithm 4: Carbon-Aware Geographic Routing (CAGR) ─────────────

  /** Regional carbon intensity data. */
  final case class RegionInfo(
      regionId: String,
      carbonIntensityGCO2PerKWh: Double,
      additionalLatencyMs: Double
  )

  /** CAGR configuration. */
  final case class CagrConfig(
      maxAdditionalLatencyMs: Double = 50.0,
      carbonWeight: Double = 0.7,
      latencyWeight: Double = 0.3
  )

  /** Route request to region minimizing carbon footprint subject to latency constraint.
    *
    * Score = carbonWeight × (carbon/maxCarbon) + latencyWeight × (latency/maxLatency) Select region with lowest score
    * where additionalLatency <= maxLatency.
    *
    * @return
    *   index of the best region
    */
  def cagrRouteRequest(
      regions: Vector[RegionInfo],
      config: CagrConfig = CagrConfig()
  ): Int =
    if regions.isEmpty then 0
    else if regions.size == 1 then 0
    else
      val feasible   = regions.zipWithIndex.filter(_._1.additionalLatencyMs <= config.maxAdditionalLatencyMs)
      val candidates = if feasible.nonEmpty then feasible else regions.zipWithIndex

      val maxCarbon  = candidates.map(_._1.carbonIntensityGCO2PerKWh).max.max(1.0)
      val maxLatency = candidates.map(_._1.additionalLatencyMs).max.max(1.0)

      candidates.minBy { case (region, _) =>
        config.carbonWeight * (region.carbonIntensityGCO2PerKWh / maxCarbon) +
          config.latencyWeight * (region.additionalLatencyMs / maxLatency)
      }._2
