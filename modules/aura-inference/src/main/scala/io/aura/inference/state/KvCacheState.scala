// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.inference.state

import io.aura.core.types.*

/** Paged KV cache state (PagedAttention model).
  *
  * Models the paged memory management used by vLLM for KV cache storage. Each page holds a fixed number of tokens
  * across all layers.
  */
final case class KvCacheState(
    totalPages: Int,
    pageSize: Int,
    allocatedPages: Map[InferenceRequestId, Vector[Int]],
    freePages: Vector[Int]
):
  def usedPages: Int                      = totalPages - freePages.size
  def utilizationPercent: Double          = usedPages.toDouble / totalPages * 100.0
  def canAllocate(numPages: Int): Boolean = freePages.size >= numPages

  /** Allocate pages for a request. Returns updated state and allocated page IDs. */
  def allocate(requestId: InferenceRequestId, numPages: Int): Option[(KvCacheState, Vector[Int])] =
    if !canAllocate(numPages) then None
    else
      val (allocated, remaining) = freePages.splitAt(numPages)
      val updated = copy(
        allocatedPages =
          allocatedPages + (requestId -> (allocatedPages.getOrElse(requestId, Vector.empty) ++ allocated)),
        freePages = remaining
      )
      Some((updated, allocated))

  /** Free all pages for a request. */
  def free(requestId: InferenceRequestId): KvCacheState =
    allocatedPages.get(requestId) match
      case Some(pages) =>
        copy(
          allocatedPages = allocatedPages - requestId,
          freePages = freePages ++ pages
        )
      case None => this

  /** Number of tokens currently cached for a request. */
  def cachedTokens(requestId: InferenceRequestId): Int =
    allocatedPages.get(requestId).map(_.size * pageSize).getOrElse(0)

  /** Required pages for a given number of tokens. */
  def pagesNeeded(tokens: Int): Int =
    (tokens + pageSize - 1) / pageSize

object KvCacheState:
  /** Initialize KV cache from GPU memory budget.
    *
    * @param gpuMemoryMB
    *   total GPU memory
    * @param modelSizeMB
    *   memory used by model weights
    * @param kvCacheMBPerToken
    *   MB per token across all layers
    * @param pageSize
    *   tokens per page
    * @param reservedFraction
    *   fraction reserved for activations/overhead
    */
  def fromGpuMemory(
      gpuMemoryMB: MegaBytes,
      modelSizeMB: MegaBytes,
      kvCacheMBPerToken: Double,
      pageSize: Int = 16,
      reservedFraction: Double = 0.1
  ): KvCacheState =
    val availableMB      = gpuMemoryMB.value - modelSizeMB.value - (gpuMemoryMB.value * reservedFraction)
    val kvCacheMBPerPage = kvCacheMBPerToken * pageSize
    val totalPages       = if kvCacheMBPerPage > 0 then (availableMB / kvCacheMBPerPage).toInt.max(0) else 0
    KvCacheState(
      totalPages = totalPages,
      pageSize = pageSize,
      allocatedPages = Map.empty,
      freePages = (0 until totalPages).toVector
    )
