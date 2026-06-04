// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.inference.scheduling

import io.aura.core.types.*
import io.aura.inference.state.*
import io.aura.inference.model.*
import io.aura.gpu.*

/** Continuous batching scheduler for LLM inference.
  *
  * Implements the core scheduling logic from Orca / vLLM:
  *   - Continuous batching: new requests can join/leave batch at each iteration
  *   - Chunked prefill: large prefills split across multiple iterations
  *   - Power-capped admission: PC-BAC algorithm
  *   - Preemption: low-priority requests evicted when KV cache is full or over power budget
  */
object BatchScheduler:

  /** Scheduling decision for a single iteration. */
  final case class ScheduleResult(
      toAdmit: Vector[InferenceRequestId],
      toPrefill: Vector[InferenceRequestId],
      toDecode: Vector[InferenceRequestId],
      toPreempt: Vector[InferenceRequestId]
  )

  /** FCFS continuous batching: admit requests in order up to batch size limit. */
  def continuousBatching(state: EngineState): ScheduleResult =
    val slotsAvailable = state.maxBatchSize - state.currentBatchSize

    val toAdmit = state.waitingQueue
      .take(slotsAvailable)
      .filter { req =>
        val pagesNeeded = state.kvCache.pagesNeeded(req.promptTokens + req.maxOutputTokens)
        state.kvCache.canAllocate(pagesNeeded)
      }
      .map(_.requestId)

    val (needsPrefill, readyForDecode) = state.runningBatch.partition(!_.prefillComplete)

    ScheduleResult(
      toAdmit = toAdmit,
      toPrefill = needsPrefill.map(_.requestId),
      toDecode = readyForDecode.filterNot(_.isComplete).map(_.requestId),
      toPreempt = Vector.empty
    )

  /** Chunked prefill: split large prefills across iterations.
    *
    * Limits the number of prefill tokens processed per iteration so that decode requests are not starved. In each
    * iteration, up to `chunkSize` tokens of prefill work are processed alongside decode steps.
    *
    * @param chunkSize
    *   maximum tokens to prefill per iteration
    */
  def chunkedPrefill(state: EngineState, chunkSize: Int = 512): ScheduleResult =
    val slotsAvailable = state.maxBatchSize - state.currentBatchSize

    // Accumulate admitted requests while staying within prefill token budget
    val (toAdmit, _) = state.waitingQueue
      .take(slotsAvailable)
      .foldLeft((Vector.empty[InferenceRequestId], 0)) { case ((admitted, budgetUsed), req) =>
        val pagesNeeded  = state.kvCache.pagesNeeded(req.promptTokens + req.maxOutputTokens)
        val fits         = state.kvCache.canAllocate(pagesNeeded)
        val withinBudget = budgetUsed + req.promptTokens <= chunkSize || budgetUsed == 0
        if fits && withinBudget then (admitted :+ req.requestId, budgetUsed + req.promptTokens)
        else (admitted, budgetUsed)
      }

    val (needsPrefill, readyForDecode) = state.runningBatch.partition(!_.prefillComplete)

    ScheduleResult(
      toAdmit = toAdmit,
      toPrefill = needsPrefill.map(_.requestId),
      toDecode = readyForDecode.filterNot(_.isComplete).map(_.requestId),
      toPreempt = Vector.empty
    )

  /** Priority-based admission: higher priority requests admitted first. */
  def priorityBatching(state: EngineState): ScheduleResult =
    val slotsAvailable = state.maxBatchSize - state.currentBatchSize

    val toAdmit = state.waitingQueue
      .sortBy(r => -r.priority)
      .take(slotsAvailable)
      .filter { req =>
        val pagesNeeded = state.kvCache.pagesNeeded(req.promptTokens + req.maxOutputTokens)
        state.kvCache.canAllocate(pagesNeeded)
      }
      .map(_.requestId)

    val (needsPrefill, readyForDecode) = state.runningBatch.partition(!_.prefillComplete)

    ScheduleResult(
      toAdmit = toAdmit,
      toPrefill = needsPrefill.map(_.requestId),
      toDecode = readyForDecode.filterNot(_.isComplete).map(_.requestId),
      toPreempt = Vector.empty
    )

  /** Power-Capped Batch Admission Control (PC-BAC).
    *
    * Admits requests while estimated GPU power stays under budget. If already over budget, preempts lowest-priority
    * requests.
    *
    * Insight: Larger decode batches are more energy-efficient per token because HBM bandwidth cost is amortized. PC-BAC
    * finds the batch size that maximizes throughput within the power envelope.
    */
  def powerCappedBatching(
      state: EngineState,
      spec: GpuDeviceSpec,
      powerBudget: Watts
  ): ScheduleResult =
    val currentPower = estimateBatchPower(state, spec)
    val perReqDelta  = marginalPowerPerRequest(state, spec)

    if currentPower.value > powerBudget.value then
      // Over budget — preempt lowest-priority running requests until under budget
      val sorted = state.runningBatch.sortBy(_.priority)
      val (toPreempt, _) = sorted.foldLeft((Vector.empty[InferenceRequestId], currentPower.value)) {
        case ((preempted, estimated), req) =>
          if estimated > powerBudget.value then (preempted :+ req.requestId, estimated - perReqDelta)
          else (preempted, estimated)
      }

      val remaining                      = state.runningBatch.filterNot(r => toPreempt.contains(r.requestId))
      val (needsPrefill, readyForDecode) = remaining.partition(!_.prefillComplete)

      ScheduleResult(
        toAdmit = Vector.empty,
        toPrefill = needsPrefill.map(_.requestId),
        toDecode = readyForDecode.filterNot(_.isComplete).map(_.requestId),
        toPreempt = toPreempt
      )
    else
      // Under budget — admit requests while projected power stays within budget
      val slotsAvailable = state.maxBatchSize - state.currentBatchSize
      val avgSeqLen =
        if state.runningBatch.nonEmpty then state.runningBatch.map(_.currentSeqLen).sum / state.runningBatch.size
        else 640

      val (toAdmit, _) = state.waitingQueue
        .take(slotsAvailable)
        .foldLeft((Vector.empty[InferenceRequestId], state.currentBatchSize)) {
          case ((admitted, projectedBatch), req) =>
            val pagesNeeded    = state.kvCache.pagesNeeded(req.promptTokens + req.maxOutputTokens)
            val projectedPower = estimatePowerAtBatchSize(state, spec, projectedBatch + 1, avgSeqLen)
            if projectedPower.value <= powerBudget.value && state.kvCache.canAllocate(pagesNeeded) then
              (admitted :+ req.requestId, projectedBatch + 1)
            else (admitted, projectedBatch)
        }

      val (needsPrefill, readyForDecode) = state.runningBatch.partition(!_.prefillComplete)

      ScheduleResult(
        toAdmit = toAdmit,
        toPrefill = needsPrefill.map(_.requestId),
        toDecode = readyForDecode.filterNot(_.isComplete).map(_.requestId),
        toPreempt = Vector.empty
      )

  /** Preemptive scheduling: evict requests when KV cache utilization exceeds threshold. */
  def preemptiveBatching(
      state: EngineState,
      kvCacheThreshold: Double = 0.9
  ): ScheduleResult =
    val base = continuousBatching(state)

    if state.kvCache.utilizationPercent > kvCacheThreshold * 100.0 then
      val toPreempt = state.runningBatch
        .filter(_.prefillComplete)
        .sortBy(r => (r.priority, -r.tokensGenerated))
        .take(1)
        .map(_.requestId)

      base.copy(toPreempt = toPreempt)
    else base

  /** Estimate GPU power for the current running batch. */
  private def estimateBatchPower(state: EngineState, spec: GpuDeviceSpec): Watts =
    if state.currentBatchSize == 0 then spec.idleWatts
    else
      val avgSeqLen = state.runningBatch.map(_.currentSeqLen).sum / state.runningBatch.size
      val activity = InferenceProfile.decodeActivity(
        state.model,
        state.currentBatchSize,
        avgSeqLen,
        spec,
        state.tpDegree
      )
      MultiComponentGpuPower.totalPower(spec, activity)

  /** Estimate GPU power at a given batch size. */
  private def estimatePowerAtBatchSize(state: EngineState, spec: GpuDeviceSpec, batchSize: Int, avgSeqLen: Int): Watts =
    if batchSize == 0 then spec.idleWatts
    else
      val activity = InferenceProfile.decodeActivity(state.model, batchSize, avgSeqLen, spec, state.tpDegree)
      MultiComponentGpuPower.totalPower(spec, activity)

  /** Estimate marginal power cost of adding one request.
    *
    * Computes P(batch+1) - P(batch) using the actual power model, so the budget check reflects real GPU power scaling.
    */
  private def marginalPowerPerRequest(state: EngineState, spec: GpuDeviceSpec): Double =
    val n = state.currentBatchSize.max(1)
    val avgSeqLen =
      if state.runningBatch.nonEmpty then state.runningBatch.map(_.currentSeqLen).sum / state.runningBatch.size
      else 640 // default estimate for new requests

    val currentActivity = InferenceProfile.decodeActivity(state.model, n, avgSeqLen, spec, state.tpDegree)
    val nextActivity    = InferenceProfile.decodeActivity(state.model, n + 1, avgSeqLen, spec, state.tpDegree)
    val currentPower    = MultiComponentGpuPower.totalPower(spec, currentActivity)
    val nextPower       = MultiComponentGpuPower.totalPower(spec, nextActivity)
    (nextPower.value - currentPower.value).max(0.1) // at least 0.1W marginal cost
