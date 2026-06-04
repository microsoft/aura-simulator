// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.inference.state

import io.aura.core.types.*
import io.aura.inference.model.LlmModelSpec

/** Immutable state for the inference engine actor.
  *
  * Tracks the running batch, waiting queue, and KV cache state.
  */
final case class EngineState(
    model: LlmModelSpec,
    kvCache: KvCacheState,
    waitingQueue: Vector[PendingRequest],
    runningBatch: Vector[RunningRequest],
    completedCount: Long,
    failedCount: Long,
    maxBatchSize: Int,
    tpDegree: Int,
    ppStages: Int = 1,
    epDegree: Int = 1
):
  def currentBatchSize: Int = runningBatch.size

  def canAdmit: Boolean = runningBatch.size < maxBatchSize

  def hasWaiting: Boolean = waitingQueue.nonEmpty

  def totalActiveTokens: Int = runningBatch.map(_.currentSeqLen).sum

  /** Add a request to the waiting queue. */
  def enqueue(req: PendingRequest): EngineState =
    copy(waitingQueue = waitingQueue :+ req)

  /** Move a request from waiting to running. */
  def admit(requestId: InferenceRequestId, admitTime: SimTime): Option[EngineState] =
    val idx = waitingQueue.indexWhere(_.requestId == requestId)
    if idx < 0 then None
    else
      val pending = waitingQueue(idx)
      val running = RunningRequest(
        requestId = pending.requestId,
        modelId = pending.modelId,
        promptTokens = pending.promptTokens,
        maxOutputTokens = pending.maxOutputTokens,
        sloTtft = pending.sloTtft,
        sloTpot = pending.sloTpot,
        admitTime = admitTime,
        prefillComplete = false,
        tokensGenerated = 0,
        currentSeqLen = pending.promptTokens,
        accumulatedEnergyWh = WattHours.Zero,
        priority = pending.priority
      )
      Some(
        copy(
          waitingQueue = waitingQueue.patch(idx, Nil, 1),
          runningBatch = runningBatch :+ running
        )
      )

  /** Mark a request's prefill as complete. */
  def markPrefillComplete(requestId: InferenceRequestId, completionTime: SimTime): EngineState =
    val idx = runningBatch.indexWhere(_.requestId == requestId)
    if idx < 0 then this
    else
      val updated = runningBatch(idx).copy(
        prefillComplete = true,
        prefillCompleteTime = completionTime
      )
      copy(runningBatch = runningBatch.updated(idx, updated))

  /** Record chunked prefill progress (tokens processed so far). */
  def recordPrefillProgress(requestId: InferenceRequestId, tokensProcessed: Int): EngineState =
    val idx = runningBatch.indexWhere(_.requestId == requestId)
    if idx < 0 then this
    else
      val r       = runningBatch(idx)
      val updated = r.copy(prefillTokensProcessed = r.prefillTokensProcessed + tokensProcessed)
      copy(runningBatch = runningBatch.updated(idx, updated))

  /** Record prefill energy for a request. */
  def recordPrefillEnergy(requestId: InferenceRequestId, energyWh: WattHours): EngineState =
    val idx = runningBatch.indexWhere(_.requestId == requestId)
    if idx < 0 then this
    else
      val r       = runningBatch(idx)
      val updated = r.copy(accumulatedEnergyWh = WattHours(r.accumulatedEnergyWh.value + energyWh.value))
      copy(runningBatch = runningBatch.updated(idx, updated))

  /** Record a decode step for a request. */
  def recordDecodeStep(requestId: InferenceRequestId, tokensGenerated: Int, energyWh: WattHours): EngineState =
    val idx = runningBatch.indexWhere(_.requestId == requestId)
    if idx < 0 then this
    else
      val r = runningBatch(idx)
      val updated = r.copy(
        tokensGenerated = r.tokensGenerated + tokensGenerated,
        currentSeqLen = r.currentSeqLen + tokensGenerated,
        accumulatedEnergyWh = WattHours(r.accumulatedEnergyWh.value + energyWh.value)
      )
      copy(runningBatch = runningBatch.updated(idx, updated))

  /** Remove a completed/failed request from running batch and free KV cache. */
  def removeRequest(requestId: InferenceRequestId): EngineState =
    copy(
      runningBatch = runningBatch.filterNot(_.requestId == requestId),
      kvCache = kvCache.free(requestId),
      completedCount = completedCount + 1
    )

  /** Preempt a request: move it back to the waiting queue. Frees KV cache and resets decode progress (request must
    * re-prefill).
    */
  def preemptRequest(requestId: InferenceRequestId): EngineState =
    val idx = runningBatch.indexWhere(_.requestId == requestId)
    if idx < 0 then this
    else
      val r = runningBatch(idx)
      val pending = PendingRequest(
        r.requestId,
        r.modelId,
        r.promptTokens,
        r.maxOutputTokens,
        r.sloTtft,
        r.sloTpot,
        r.admitTime,
        r.priority
      )
      copy(
        runningBatch = runningBatch.patch(idx, Nil, 1),
        waitingQueue = waitingQueue :+ pending,
        kvCache = kvCache.free(requestId)
      )

  /** Remove a failed request. */
  def failRequest(requestId: InferenceRequestId): EngineState =
    val fromWaiting = waitingQueue.filterNot(_.requestId == requestId)
    val fromRunning = runningBatch.filterNot(_.requestId == requestId)
    copy(
      waitingQueue = fromWaiting,
      runningBatch = fromRunning,
      kvCache = kvCache.free(requestId),
      failedCount = failedCount + 1
    )

/** A request waiting in the queue to be admitted. */
final case class PendingRequest(
    requestId: InferenceRequestId,
    modelId: ModelId,
    promptTokens: Int,
    maxOutputTokens: Int,
    sloTtft: SimTime,
    sloTpot: SimTime,
    arrivalTime: SimTime,
    priority: Int
)

/** A request currently being processed (prefill or decode phase). */
final case class RunningRequest(
    requestId: InferenceRequestId,
    modelId: ModelId,
    promptTokens: Int,
    maxOutputTokens: Int,
    sloTtft: SimTime,
    sloTpot: SimTime,
    admitTime: SimTime,
    prefillComplete: Boolean,
    prefillCompleteTime: SimTime = SimTime.Zero,
    prefillTokensProcessed: Int = 0,
    tokensGenerated: Int,
    currentSeqLen: Int,
    accumulatedEnergyWh: WattHours,
    priority: Int
):
  def isComplete: Boolean = tokensGenerated >= maxOutputTokens
  def ttft: SimTime = if prefillComplete then SimTime(prefillCompleteTime.value - admitTime.value) else SimTime.Zero
