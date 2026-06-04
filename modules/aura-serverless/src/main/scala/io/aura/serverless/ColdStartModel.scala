// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.serverless

import io.aura.core.types.*

/** Runtime environments for serverless functions. */
enum Runtime:
  case Python, NodeJs, Java, Go, Rust, DotNet

/** Pure function: (memoryMB, runtime) => cold start delay.
  *
  * Models the container initialization overhead when no warm container is available.
  */
object ColdStartModel:

  type ColdStartModel = (MegaBytes, Runtime) => SimTime

  /** Constant cold start delay regardless of function/runtime. */
  def fixed(delay: SimTime): ColdStartModel =
    (_, _) => delay

  /** Cold start varies by runtime — based on real-world measurements. */
  val byRuntime: ColdStartModel = (_, runtime) =>
    runtime match
      case Runtime.Python => SimTime(0.250)
      case Runtime.NodeJs => SimTime(0.170)
      case Runtime.Java   => SimTime(3.500)
      case Runtime.Go     => SimTime(0.100)
      case Runtime.Rust   => SimTime(0.080)
      case Runtime.DotNet => SimTime(1.200)

  /** Cold start scales with memory size (larger containers take longer to initialize). */
  def loadDependent(baseFactor: Double): ColdStartModel =
    (memoryMB, runtime) =>
      val baseDelay = byRuntime(memoryMB, runtime)
      val memFactor = memoryMB.value / 256.0 * baseFactor
      SimTime(baseDelay.value * (1.0 + memFactor))
