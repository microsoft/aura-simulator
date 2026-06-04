// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.inference

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.inference.model.*
import io.aura.inference.state.*

class EngineStateSpec extends AnyFlatSpec with Matchers:

  val model = LlmModelSpec.llama2_7b

  def makeKvCache(totalPages: Int = 1000): KvCacheState =
    KvCacheState(totalPages, pageSize = 16, Map.empty, (0 until totalPages).toVector)

  def makePending(id: Long): PendingRequest =
    PendingRequest(
      InferenceRequestId(id),
      ModelId(0),
      512,
      128,
      SimTime(0.5),
      SimTime(0.05),
      SimTime(1.0),
      priority = 0
    )

  def emptyState: EngineState =
    EngineState(model, makeKvCache(), Vector.empty, Vector.empty, 0, 0, 64, 1)

  "enqueue" should "add request to waiting queue" in {
    val state = emptyState.enqueue(makePending(1))
    state.waitingQueue.size shouldBe 1
    state.hasWaiting shouldBe true
  }

  "admit" should "move request from waiting to running" in {
    val state    = emptyState.enqueue(makePending(1))
    val admitted = state.admit(InferenceRequestId(1), SimTime(2.0))
    admitted shouldBe defined
    val s = admitted.get
    s.waitingQueue shouldBe empty
    s.runningBatch.size shouldBe 1
    s.runningBatch.head.admitTime shouldBe SimTime(2.0)
    s.runningBatch.head.prefillComplete shouldBe false
  }

  it should "return None for unknown request" in {
    emptyState.admit(InferenceRequestId(999), SimTime(1.0)) shouldBe None
  }

  "markPrefillComplete" should "set prefillComplete and completion time" in {
    val state = emptyState
      .enqueue(makePending(1))
      .admit(InferenceRequestId(1), SimTime(2.0))
      .get
      .markPrefillComplete(InferenceRequestId(1), SimTime(2.044))

    val req = state.runningBatch.head
    req.prefillComplete shouldBe true
    req.prefillCompleteTime shouldBe SimTime(2.044)
  }

  "RunningRequest.ttft" should "compute correct TTFT from admit to prefill complete" in {
    val state = emptyState
      .enqueue(makePending(1))
      .admit(InferenceRequestId(1), SimTime(2.0))
      .get
      .markPrefillComplete(InferenceRequestId(1), SimTime(2.044))

    val req = state.runningBatch.head
    req.ttft.value shouldBe 0.044 +- 0.001
  }

  it should "return zero when prefill not complete" in {
    val state = emptyState
      .enqueue(makePending(1))
      .admit(InferenceRequestId(1), SimTime(2.0))
      .get
    state.runningBatch.head.ttft shouldBe SimTime.Zero
  }

  "recordPrefillProgress" should "accumulate tokens processed" in {
    val state = emptyState
      .enqueue(makePending(1))
      .admit(InferenceRequestId(1), SimTime(2.0))
      .get
      .recordPrefillProgress(InferenceRequestId(1), 256)
      .recordPrefillProgress(InferenceRequestId(1), 256)

    state.runningBatch.head.prefillTokensProcessed shouldBe 512
  }

  "recordDecodeStep" should "increment tokens and sequence length" in {
    val state = emptyState
      .enqueue(makePending(1))
      .admit(InferenceRequestId(1), SimTime(2.0))
      .get
      .markPrefillComplete(InferenceRequestId(1), SimTime(2.044))
      .recordDecodeStep(InferenceRequestId(1), 1, WattHours(0.001))

    val req = state.runningBatch.head
    req.tokensGenerated shouldBe 1
    req.currentSeqLen shouldBe 513
    req.accumulatedEnergyWh.value shouldBe 0.001
  }

  "isComplete" should "be true when all output tokens generated" in {
    var state = emptyState
      .enqueue(makePending(1))
      .admit(InferenceRequestId(1), SimTime(2.0))
      .get
      .markPrefillComplete(InferenceRequestId(1), SimTime(2.044))

    for i <- 1 to 128 do state = state.recordDecodeStep(InferenceRequestId(1), 1, WattHours(0.0001))

    state.runningBatch.head.isComplete shouldBe true
    state.runningBatch.head.tokensGenerated shouldBe 128
  }

  "removeRequest" should "free KV cache and increment completed count" in {
    val state = emptyState
      .enqueue(makePending(1))
      .admit(InferenceRequestId(1), SimTime(2.0))
      .get

    // Allocate some KV cache
    val reqId            = InferenceRequestId(1)
    val (kvWithAlloc, _) = state.kvCache.allocate(reqId, 10).get
    val stateWithKv      = state.copy(kvCache = kvWithAlloc)

    val removed = stateWithKv.removeRequest(reqId)
    removed.runningBatch shouldBe empty
    removed.completedCount shouldBe 1
    removed.kvCache.usedPages shouldBe 0
  }

  "canAdmit" should "respect max batch size" in {
    emptyState.canAdmit shouldBe true
    val full = emptyState.copy(maxBatchSize = 0)
    full.canAdmit shouldBe false
  }

  "totalActiveTokens" should "sum current sequence lengths" in {
    val state = emptyState
      .enqueue(makePending(1))
      .enqueue(makePending(2))
      .admit(InferenceRequestId(1), SimTime(1.0))
      .get
      .admit(InferenceRequestId(2), SimTime(1.0))
      .get

    state.totalActiveTokens shouldBe 1024 // 512 + 512
  }

  "preemptRequest" should "move request back to waiting queue" in {
    val state = emptyState
      .enqueue(makePending(1))
      .admit(InferenceRequestId(1), SimTime(2.0))
      .get
      .markPrefillComplete(InferenceRequestId(1), SimTime(2.044))
      .recordDecodeStep(InferenceRequestId(1), 10, WattHours(0.01))

    val preempted = state.preemptRequest(InferenceRequestId(1))
    preempted.runningBatch shouldBe empty
    preempted.waitingQueue.size shouldBe 1
    preempted.waitingQueue.head.requestId shouldBe InferenceRequestId(1)
  }

  it should "free KV cache on preemption" in {
    val state = emptyState
      .enqueue(makePending(1))
      .admit(InferenceRequestId(1), SimTime(2.0))
      .get

    val reqId        = InferenceRequestId(1)
    val (kvAlloc, _) = state.kvCache.allocate(reqId, 10).get
    val stateWithKv  = state.copy(kvCache = kvAlloc)

    val preempted = stateWithKv.preemptRequest(reqId)
    preempted.kvCache.usedPages shouldBe 0
  }

  it should "be a no-op for unknown request" in {
    val state = emptyState
    state.preemptRequest(InferenceRequestId(999)) shouldBe state
  }
