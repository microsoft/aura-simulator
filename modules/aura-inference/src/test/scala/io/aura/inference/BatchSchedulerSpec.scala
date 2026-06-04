// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.inference

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.inference.model.*
import io.aura.inference.state.*
import io.aura.inference.scheduling.BatchScheduler

class BatchSchedulerSpec extends AnyFlatSpec with Matchers:

  val model = LlmModelSpec.llama2_7b

  def makeKvCache(totalPages: Int = 1000): KvCacheState =
    KvCacheState(totalPages, pageSize = 16, Map.empty, (0 until totalPages).toVector)

  def makePending(id: Long, promptTokens: Int = 512, maxOut: Int = 128): PendingRequest =
    PendingRequest(
      InferenceRequestId(id),
      ModelId(0),
      promptTokens,
      maxOut,
      SimTime(0.5),
      SimTime(0.05),
      SimTime(1.0),
      priority = 0
    )

  def makeRunning(id: Long, prefillComplete: Boolean = false, tokens: Int = 0): RunningRequest =
    RunningRequest(
      InferenceRequestId(id),
      ModelId(0),
      512,
      128,
      SimTime(0.5),
      SimTime(0.05),
      SimTime(1.0),
      prefillComplete = prefillComplete,
      prefillCompleteTime = if prefillComplete then SimTime(1.044) else SimTime.Zero,
      tokensGenerated = tokens,
      currentSeqLen = 512 + tokens,
      accumulatedEnergyWh = WattHours.Zero,
      priority = 0
    )

  def emptyState(maxBatch: Int = 64): EngineState =
    EngineState(model, makeKvCache(), Vector.empty, Vector.empty, 0, 0, maxBatch, 1)

  // ── Continuous Batching ──────────────────────────────────────────

  "continuousBatching" should "admit waiting requests up to batch limit" in {
    val state = emptyState(maxBatch = 2).copy(
      waitingQueue = Vector(makePending(1), makePending(2), makePending(3))
    )
    val result = BatchScheduler.continuousBatching(state)
    result.toAdmit.size shouldBe 2
  }

  it should "not admit when batch is full" in {
    val state = emptyState(maxBatch = 1).copy(
      waitingQueue = Vector(makePending(2)),
      runningBatch = Vector(makeRunning(1))
    )
    val result = BatchScheduler.continuousBatching(state)
    result.toAdmit shouldBe empty
  }

  it should "separate prefill-pending from decode-ready" in {
    val state = emptyState().copy(
      runningBatch = Vector(
        makeRunning(1, prefillComplete = false),
        makeRunning(2, prefillComplete = true),
        makeRunning(3, prefillComplete = true, tokens = 128) // complete
      )
    )
    val result = BatchScheduler.continuousBatching(state)
    result.toPrefill.size shouldBe 1
    result.toDecode.size shouldBe 1 // excludes completed request
  }

  it should "reject requests that exceed KV cache capacity" in {
    val state = emptyState().copy(
      kvCache = makeKvCache(totalPages = 1), // very small cache
      waitingQueue = Vector(makePending(1, promptTokens = 2048, maxOut = 2048))
    )
    val result = BatchScheduler.continuousBatching(state)
    result.toAdmit shouldBe empty
  }

  // ── Chunked Prefill ──────────────────────────────────────────────

  "chunkedPrefill" should "limit admissions by prefill token budget" in {
    val state = emptyState().copy(
      waitingQueue = Vector(
        makePending(1, promptTokens = 400),
        makePending(2, promptTokens = 400),
        makePending(3, promptTokens = 400)
      )
    )
    // chunkSize=512 means budget for ~1 request of 400 tokens, maybe 2
    val result = BatchScheduler.chunkedPrefill(state, chunkSize = 512)
    result.toAdmit.size shouldBe 1
  }

  it should "admit at least one request even if it exceeds budget" in {
    val state = emptyState().copy(
      waitingQueue = Vector(makePending(1, promptTokens = 2048))
    )
    val result = BatchScheduler.chunkedPrefill(state, chunkSize = 512)
    result.toAdmit.size shouldBe 1
  }

  it should "admit multiple small requests within budget" in {
    val state = emptyState().copy(
      waitingQueue = Vector(
        makePending(1, promptTokens = 100),
        makePending(2, promptTokens = 100),
        makePending(3, promptTokens = 100),
        makePending(4, promptTokens = 100),
        makePending(5, promptTokens = 100)
      )
    )
    val result = BatchScheduler.chunkedPrefill(state, chunkSize = 512)
    result.toAdmit.size shouldBe 5
  }

  // ── Priority Batching ────────────────────────────────────────────

  "priorityBatching" should "admit higher priority first" in {
    val state = emptyState(maxBatch = 1).copy(
      waitingQueue = Vector(
        makePending(1).copy(priority = 0),
        makePending(2).copy(priority = 10),
        makePending(3).copy(priority = 5)
      )
    )
    val result = BatchScheduler.priorityBatching(state)
    result.toAdmit.size shouldBe 1
    result.toAdmit.head shouldBe InferenceRequestId(2) // highest priority
  }

  // ── Power-Capped Batch Admission (PC-BAC) ──────────────────────

  val a100 = GpuDeviceSpec.a100Sxm

  "powerCappedBatching" should "admit requests when under power budget" in {
    val state = emptyState().copy(
      waitingQueue = Vector(makePending(1), makePending(2))
    )
    val result = BatchScheduler.powerCappedBatching(state, a100, Watts(400.0))
    result.toAdmit.size should be > 0
    result.toPreempt shouldBe empty
  }

  it should "limit admissions to stay within power budget" in {
    // With a very low budget, should admit fewer requests
    val state = emptyState().copy(
      waitingQueue = (1L to 20L).map(makePending(_)).toVector
    )
    val loose = BatchScheduler.powerCappedBatching(state, a100, Watts(400.0))
    val tight = BatchScheduler.powerCappedBatching(state, a100, Watts(55.0)) // barely above idle
    tight.toAdmit.size should be <= loose.toAdmit.size
  }

  it should "preempt when over power budget" in {
    // Create state where running batch makes power exceed budget
    val state = emptyState().copy(
      runningBatch = (1L to 50L).map(id => makeRunning(id, prefillComplete = true).copy(priority = 0)).toVector
    )
    val result = BatchScheduler.powerCappedBatching(state, a100, Watts(55.0)) // very low budget
    result.toPreempt.size should be > 0
    result.toAdmit shouldBe empty
  }

  // ── Preemptive Batching ────────────────────────────────────────

  "preemptiveBatching" should "not preempt when KV cache is under threshold" in {
    val state = emptyState().copy(
      runningBatch = Vector(makeRunning(1, prefillComplete = true))
    )
    val result = BatchScheduler.preemptiveBatching(state)
    result.toPreempt shouldBe empty
  }

  it should "preempt lowest-priority request when KV cache is over threshold" in {
    // Fill KV cache to above threshold
    val cache            = makeKvCache(totalPages = 100)
    val (filledCache, _) = cache.allocate(InferenceRequestId(99), 95).get // 95% utilization
    val state = emptyState().copy(
      kvCache = filledCache,
      runningBatch = Vector(
        makeRunning(1, prefillComplete = true).copy(priority = 10),
        makeRunning(2, prefillComplete = true).copy(priority = 0) // lowest priority
      )
    )
    val result = BatchScheduler.preemptiveBatching(state, kvCacheThreshold = 0.9)
    result.toPreempt.size shouldBe 1
    result.toPreempt.head shouldBe InferenceRequestId(2)
  }
