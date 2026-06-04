// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

/** Immutable resource specifications and utilization tracking. */

/** Specification for a VM or Host's available resources. */
final case class ResourceSpec(
    pes: PEs,
    mips: MIPS,
    ram: MegaBytes,
    bw: Mbps,
    storage: MegaBytes
)

/** Tracks currently available (free) resources on a Host. */
final case class AvailableResources(
    pes: PEs,
    mips: MIPS,
    ram: MegaBytes,
    bw: Mbps,
    storage: MegaBytes
):
  def canFit(spec: ResourceSpec): Boolean =
    pes >= spec.pes &&
      ram >= spec.ram &&
      bw >= spec.bw &&
      storage >= spec.storage

  def allocate(spec: ResourceSpec): AvailableResources =
    AvailableResources(
      pes = pes - spec.pes,
      mips = mips - spec.mips,
      ram = ram - spec.ram,
      bw = bw - spec.bw,
      storage = storage - spec.storage
    )

  def release(spec: ResourceSpec): AvailableResources =
    AvailableResources(
      pes = pes + spec.pes,
      mips = mips + spec.mips,
      ram = ram + spec.ram,
      bw = bw + spec.bw,
      storage = storage + spec.storage
    )

object AvailableResources:
  def fromSpec(spec: ResourceSpec): AvailableResources =
    AvailableResources(
      pes = spec.pes,
      mips = spec.mips,
      ram = spec.ram,
      bw = spec.bw,
      storage = spec.storage
    )

/** CPU utilization as a fraction [0.0, 1.0]. */
opaque type Utilization = Double

object Utilization:
  def apply(value: Double): Utilization =
    math.max(0.0, math.min(1.0, value))

  val Zero: Utilization = 0.0
  val Full: Utilization = 1.0

  extension (u: Utilization)
    def value: Double                      = u
    def +(other: Utilization): Utilization = apply(u + other)
    def -(other: Utilization): Utilization = apply(u - other)
    def *(factor: Double): Utilization     = apply(u * factor)
