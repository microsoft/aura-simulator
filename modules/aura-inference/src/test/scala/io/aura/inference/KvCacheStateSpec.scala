// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.inference

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.inference.state.*
import io.aura.inference.model.*

class KvCacheStateSpec extends AnyFlatSpec with Matchers:

  val model = LlmModelSpec.llama2_7b
  val req1  = InferenceRequestId(1)
  val req2  = InferenceRequestId(2)

  "KvCacheState.fromGpuMemory" should "initialize with correct page count" in {
    val cache = KvCacheState.fromGpuMemory(
      gpuMemoryMB = MegaBytes(81920.0),
      modelSizeMB = model.modelSizeMB,
      kvCacheMBPerToken = model.kvCacheMBPerToken
    )
    cache.totalPages should be > 0
    cache.freePages.size shouldBe cache.totalPages
    cache.usedPages shouldBe 0
  }

  it should "reserve space for model weights and overhead" in {
    val cache = KvCacheState.fromGpuMemory(
      gpuMemoryMB = MegaBytes(81920.0),
      modelSizeMB = model.modelSizeMB,
      kvCacheMBPerToken = model.kvCacheMBPerToken,
      reservedFraction = 0.1
    )
    // Available = 81920 - modelSize - 10% overhead, so pages < total memory / page size
    val maxPossible = (81920.0 / (model.kvCacheMBPerToken * 16)).toInt
    cache.totalPages should be < maxPossible
  }

  it should "handle zero memory gracefully" in {
    val cache = KvCacheState.fromGpuMemory(
      gpuMemoryMB = MegaBytes(0.0),
      modelSizeMB = model.modelSizeMB,
      kvCacheMBPerToken = model.kvCacheMBPerToken
    )
    cache.totalPages shouldBe 0
  }

  "allocate" should "allocate pages for a request" in {
    val cache = KvCacheState.fromGpuMemory(
      MegaBytes(81920.0),
      model.modelSizeMB,
      model.kvCacheMBPerToken
    )
    val result = cache.allocate(req1, 10)
    result shouldBe defined
    val (updated, pages) = result.get
    pages.size shouldBe 10
    updated.usedPages shouldBe 10
    updated.freePages.size shouldBe cache.totalPages - 10
  }

  it should "fail when not enough free pages" in {
    val cache = KvCacheState(totalPages = 5, pageSize = 16, Map.empty, (0 until 5).toVector)
    cache.allocate(req1, 10) shouldBe None
  }

  it should "track allocations per request" in {
    val cache         = KvCacheState(totalPages = 100, pageSize = 16, Map.empty, (0 until 100).toVector)
    val Some((c1, _)) = cache.allocate(req1, 10): @unchecked
    val Some((c2, _)) = c1.allocate(req2, 20): @unchecked
    c2.allocatedPages(req1).size shouldBe 10
    c2.allocatedPages(req2).size shouldBe 20
    c2.usedPages shouldBe 30
  }

  "free" should "return pages to the free pool" in {
    val cache                = KvCacheState(totalPages = 50, pageSize = 16, Map.empty, (0 until 50).toVector)
    val Some((allocated, _)) = cache.allocate(req1, 10): @unchecked
    val freed                = allocated.free(req1)
    freed.freePages.size shouldBe 50
    freed.usedPages shouldBe 0
    freed.allocatedPages.contains(req1) shouldBe false
  }

  it should "handle freeing unknown request" in {
    val cache = KvCacheState(totalPages = 10, pageSize = 16, Map.empty, (0 until 10).toVector)
    val freed = cache.free(InferenceRequestId(999))
    freed shouldBe cache
  }

  "pagesNeeded" should "round up correctly" in {
    val cache = KvCacheState(totalPages = 100, pageSize = 16, Map.empty, (0 until 100).toVector)
    cache.pagesNeeded(1) shouldBe 1
    cache.pagesNeeded(16) shouldBe 1
    cache.pagesNeeded(17) shouldBe 2
    cache.pagesNeeded(32) shouldBe 2
    cache.pagesNeeded(33) shouldBe 3
  }

  "cachedTokens" should "return tokens for allocated request" in {
    val cache                = KvCacheState(totalPages = 100, pageSize = 16, Map.empty, (0 until 100).toVector)
    val Some((allocated, _)) = cache.allocate(req1, 5): @unchecked
    allocated.cachedTokens(req1) shouldBe 80 // 5 pages × 16 tokens
  }

  it should "return zero for unknown request" in {
    val cache = KvCacheState(totalPages = 100, pageSize = 16, Map.empty, (0 until 100).toVector)
    cache.cachedTokens(InferenceRequestId(999)) shouldBe 0
  }

  "utilizationPercent" should "track utilization" in {
    val cache = KvCacheState(totalPages = 100, pageSize = 16, Map.empty, (0 until 100).toVector)
    cache.utilizationPercent shouldBe 0.0
    val Some((half, _)) = cache.allocate(req1, 50): @unchecked
    half.utilizationPercent shouldBe 50.0
  }
