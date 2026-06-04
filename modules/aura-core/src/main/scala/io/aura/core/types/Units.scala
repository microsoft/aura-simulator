// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

/** Type-safe physical and simulation units using Scala 3 opaque types.
  *
  * These prevent accidental mixing of incompatible quantities at compile time. For example, you cannot add MIPS to
  * MegaBytes or pass a VmId where a HostId is expected.
  */

// ─── Simulation Time ────────────────────────────────────────────────────────

opaque type SimTime = Double

object SimTime:
  def apply(value: Double): SimTime =
    require(value >= 0.0, s"SimTime must be non-negative, got $value")
    value
  val Zero: SimTime     = 0.0
  val MaxValue: SimTime = Double.MaxValue

  given Ordering[SimTime] = Ordering.Double.TotalOrdering

  extension (t: SimTime)
    def value: Double                = t
    def +(other: SimTime): SimTime   = t + other
    def -(other: SimTime): SimTime   = t - other
    def *(factor: Double): SimTime   = t * factor
    def /(divisor: Double): SimTime  = t / divisor
    def <(other: SimTime): Boolean   = t < other
    def <=(other: SimTime): Boolean  = t <= other
    def >(other: SimTime): Boolean   = t > other
    def >=(other: SimTime): Boolean  = t >= other
    def max(other: SimTime): SimTime = math.max(t, other)
    def min(other: SimTime): SimTime = math.min(t, other)
    def isZero: Boolean              = t == 0.0

// ─── Computing Units ────────────────────────────────────────────────────────

/** Million Instructions Per Second */
opaque type MIPS = Double

object MIPS:
  def apply(value: Double): MIPS =
    require(value >= 0.0, s"MIPS must be non-negative, got $value")
    value
  val Zero: MIPS = 0.0

  extension (m: MIPS)
    def value: Double            = m
    def +(other: MIPS): MIPS     = m + other
    def -(other: MIPS): MIPS     = m - other
    def *(factor: Double): MIPS  = m * factor
    def /(divisor: Double): MIPS = m / divisor
    def <(other: MIPS): Boolean  = m < other
    def <=(other: MIPS): Boolean = m <= other
    def >(other: MIPS): Boolean  = m > other
    def >=(other: MIPS): Boolean = m >= other

/** Million Instructions (workload size) */
opaque type MI = Double

object MI:
  def apply(value: Double): MI =
    require(value >= 0.0, s"MI must be non-negative, got $value")
    value
  val Zero: MI = 0.0

  extension (mi: MI)
    def value: Double         = mi
    def +(other: MI): MI      = mi + other
    def -(other: MI): MI      = mi - other
    def *(factor: Double): MI = mi * factor
    def executionTime(mips: MIPS): SimTime =
      if mips.value > 0 then SimTime(mi / mips.value) else SimTime.MaxValue

// ─── Storage Units ──────────────────────────────────────────────────────────

opaque type MegaBytes = Double

object MegaBytes:
  def apply(value: Double): MegaBytes =
    require(value >= 0.0, s"MegaBytes must be non-negative, got $value")
    value
  val Zero: MegaBytes = 0.0

  extension (mb: MegaBytes)
    def value: Double                  = mb
    def +(other: MegaBytes): MegaBytes = mb + other
    def -(other: MegaBytes): MegaBytes = mb - other
    def <(other: MegaBytes): Boolean   = mb < other
    def <=(other: MegaBytes): Boolean  = mb <= other
    def >(other: MegaBytes): Boolean   = mb > other
    def >=(other: MegaBytes): Boolean  = mb >= other

// ─── Network Units ──────────────────────────────────────────────────────────

/** Megabits per second */
opaque type Mbps = Double

object Mbps:
  def apply(value: Double): Mbps =
    require(value >= 0.0, s"Mbps must be non-negative, got $value")
    value
  val Zero: Mbps = 0.0

  extension (bw: Mbps)
    def value: Double            = bw
    def +(other: Mbps): Mbps     = bw + other
    def -(other: Mbps): Mbps     = bw - other
    def <(other: Mbps): Boolean  = bw < other
    def <=(other: Mbps): Boolean = bw <= other
    def >(other: Mbps): Boolean  = bw > other
    def >=(other: Mbps): Boolean = bw >= other

// ─── Power Units ────────────────────────────────────────────────────────────

opaque type Watts = Double

object Watts:
  def apply(value: Double): Watts =
    require(value >= 0.0, s"Watts must be non-negative, got $value")
    value
  val Zero: Watts = 0.0

  extension (w: Watts)
    def value: Double              = w
    def +(other: Watts): Watts     = w + other
    def -(other: Watts): Watts     = w - other
    def *(seconds: Double): Double = w * seconds // watt-seconds (joules)
    def <(other: Watts): Boolean   = w < other
    def <=(other: Watts): Boolean  = w <= other

/** Energy in Watt-hours */
opaque type WattHours = Double

object WattHours:
  def apply(value: Double): WattHours =
    require(value >= 0.0, s"WattHours must be non-negative, got $value")
    value
  val Zero: WattHours = 0.0

  extension (wh: WattHours)
    def value: Double                  = wh
    def +(other: WattHours): WattHours = wh + other
    def toKWh: Double                  = wh / 1000.0

// ─── Billing Units ─────────────────────────────────────────────────────────

/** Gigabyte-seconds — standard serverless billing unit. */
opaque type GBSeconds = Double

object GBSeconds:
  def apply(value: Double): GBSeconds =
    require(value >= 0.0, s"GBSeconds must be non-negative, got $value")
    value
  val Zero: GBSeconds = 0.0

  given Ordering[GBSeconds] = Ordering.Double.TotalOrdering

  extension (gbs: GBSeconds)
    def value: Double                  = gbs
    def +(other: GBSeconds): GBSeconds = gbs + other
    def *(factor: Double): GBSeconds   = gbs * factor
    def <(other: GBSeconds): Boolean   = gbs < other
    def <=(other: GBSeconds): Boolean  = gbs <= other
    def >(other: GBSeconds): Boolean   = gbs > other
    def >=(other: GBSeconds): Boolean  = gbs >= other

// ─── Cost Units ───────────────────────────────────────────────────────────

opaque type Cost = Double

object Cost:
  def apply(value: Double): Cost =
    require(value >= 0.0, s"Cost must be non-negative, got $value")
    value
  val Zero: Cost = 0.0

  given Ordering[Cost] = Ordering.Double.TotalOrdering

  extension (c: Cost)
    def value: Double            = c
    def +(other: Cost): Cost     = c + other
    def *(factor: Double): Cost  = c * factor
    def <(other: Cost): Boolean  = c < other
    def <=(other: Cost): Boolean = c <= other
    def >(other: Cost): Boolean  = c > other
    def >=(other: Cost): Boolean = c >= other

// ─── PE (Processing Element) count ──────────────────────────────────────────

opaque type PEs = Int

object PEs:
  def apply(value: Int): PEs =
    require(value >= 0, s"PEs must be non-negative, got $value")
    value
  val Zero: PEs = 0

  extension (pe: PEs)
    def value: Int              = pe
    def +(other: PEs): PEs      = pe + other
    def -(other: PEs): PEs      = pe - other
    def <(other: PEs): Boolean  = pe < other
    def <=(other: PEs): Boolean = pe <= other
    def >(other: PEs): Boolean  = pe > other
    def >=(other: PEs): Boolean = pe >= other
    def toDouble: Double        = pe.toDouble
