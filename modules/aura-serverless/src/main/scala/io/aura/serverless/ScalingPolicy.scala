// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.serverless

import io.aura.core.types.*

/** Pure function: (currentConcurrency, functionId) => target warm container count.
  *
  * Determines how many warm containers to maintain for a function.
  */
object ScalingPolicy:

  type ScalingPolicy = (Int, FunctionId) => Int

  /** No pre-warming — containers created on demand (default). */
  val reactive: ScalingPolicy = (_, _) => 0

  /** Maintain a fixed number of warm containers. */
  def provisioned(count: Int): ScalingPolicy = (_, _) => count
