// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

/** Resource oversubscription model.
  *
  * When a host's physical RAM or bandwidth is exhausted, oversubscription allows placement with degraded performance
  * rather than hard rejection.
  *
  * Models:
  *   - RAM oversubscription: virtual memory (swap), increased latency
  *   - Bandwidth oversubscription: reduced throughput per VM
  *   - CPU oversubscription: time-sharing with reduced effective MIPS
  */

/** Oversubscription policy configuration. */
final case class OversubscriptionConfig(
    ramOversubscriptionRatio: Double = 1.0, // 1.0 = no oversub, 1.5 = 50% more than physical
    bwOversubscriptionRatio: Double = 1.0,  // 1.0 = no oversub
    cpuOversubscriptionRatio: Double = 1.0, // 1.0 = no oversub
    swapPenaltyFactor: Double = 10.0        // memory access 10x slower when swapping
)

object OversubscriptionConfig:
  /** No oversubscription — strict resource enforcement. */
  val none: OversubscriptionConfig = OversubscriptionConfig()

  /** Conservative oversubscription typical of enterprise clouds. */
  val conservative: OversubscriptionConfig = OversubscriptionConfig(
    ramOversubscriptionRatio = 1.25,
    bwOversubscriptionRatio = 1.5,
    cpuOversubscriptionRatio = 1.5,
    swapPenaltyFactor = 10.0
  )

  /** Aggressive oversubscription for high-density hosting. */
  val aggressive: OversubscriptionConfig = OversubscriptionConfig(
    ramOversubscriptionRatio = 2.0,
    bwOversubscriptionRatio = 3.0,
    cpuOversubscriptionRatio = 4.0,
    swapPenaltyFactor = 10.0
  )

/** Extended resource availability that accounts for oversubscription. */
final case class OversubscribableResources(
    physicalSpec: ResourceSpec,
    allocated: ResourceSpec, // total resources allocated to VMs
    config: OversubscriptionConfig
):
  /** Maximum allocatable resources (physical * oversubscription ratio). */
  def effectiveCapacity: ResourceSpec = ResourceSpec(
    pes = PEs((physicalSpec.pes.value * config.cpuOversubscriptionRatio).toInt),
    mips = MIPS(physicalSpec.mips.value * config.cpuOversubscriptionRatio),
    ram = MegaBytes(physicalSpec.ram.value * config.ramOversubscriptionRatio),
    bw = Mbps(physicalSpec.bw.value * config.bwOversubscriptionRatio),
    storage = physicalSpec.storage // storage is not oversubscribed
  )

  /** Check if a VM can be placed (with oversubscription). */
  def canFit(vmSpec: ResourceSpec): Boolean =
    val cap = effectiveCapacity
    val afterAlloc = ResourceSpec(
      pes = PEs(allocated.pes.value + vmSpec.pes.value),
      mips = MIPS(allocated.mips.value + vmSpec.mips.value),
      ram = MegaBytes(allocated.ram.value + vmSpec.ram.value),
      bw = Mbps(allocated.bw.value + vmSpec.bw.value),
      storage = MegaBytes(allocated.storage.value + vmSpec.storage.value)
    )
    afterAlloc.pes.value <= cap.pes.value &&
    afterAlloc.ram.value <= cap.ram.value &&
    afterAlloc.bw.value <= cap.bw.value &&
    afterAlloc.storage.value <= cap.storage.value

  /** Allocate resources for a VM. */
  def allocate(vmSpec: ResourceSpec): OversubscribableResources =
    copy(allocated =
      ResourceSpec(
        pes = PEs(allocated.pes.value + vmSpec.pes.value),
        mips = MIPS(allocated.mips.value + vmSpec.mips.value),
        ram = MegaBytes(allocated.ram.value + vmSpec.ram.value),
        bw = Mbps(allocated.bw.value + vmSpec.bw.value),
        storage = MegaBytes(allocated.storage.value + vmSpec.storage.value)
      )
    )

  /** Release resources from a VM. */
  def release(vmSpec: ResourceSpec): OversubscribableResources =
    copy(allocated =
      ResourceSpec(
        pes = PEs(math.max(0, allocated.pes.value - vmSpec.pes.value)),
        mips = MIPS(math.max(0.0, allocated.mips.value - vmSpec.mips.value)),
        ram = MegaBytes(math.max(0.0, allocated.ram.value - vmSpec.ram.value)),
        bw = Mbps(math.max(0.0, allocated.bw.value - vmSpec.bw.value)),
        storage = MegaBytes(math.max(0.0, allocated.storage.value - vmSpec.storage.value))
      )
    )

  /** Whether RAM is currently oversubscribed (swap active). */
  def isRamOversubscribed: Boolean =
    allocated.ram.value > physicalSpec.ram.value

  /** Whether bandwidth is currently oversubscribed. */
  def isBwOversubscribed: Boolean =
    allocated.bw.value > physicalSpec.bw.value

  /** Whether CPU is currently oversubscribed. */
  def isCpuOversubscribed: Boolean =
    allocated.pes.value > physicalSpec.pes.value

  /** RAM utilization including oversubscription (can exceed 1.0). */
  def ramUtilization: Double =
    if physicalSpec.ram.value <= 0 then 0.0
    else allocated.ram.value / physicalSpec.ram.value

  /** Effective MIPS available to a VM considering oversubscription.
    *
    * When CPU is oversubscribed, each VM gets a proportionally reduced share of MIPS.
    */
  def effectiveMips(vmSpec: ResourceSpec): MIPS =
    if !isCpuOversubscribed then vmSpec.mips
    else
      val ratio = physicalSpec.mips.value / allocated.mips.value
      MIPS(vmSpec.mips.value * ratio)

  /** Effective bandwidth available to a VM considering oversubscription. */
  def effectiveBandwidth(vmSpec: ResourceSpec): Mbps =
    if !isBwOversubscribed then vmSpec.bw
    else
      val ratio = physicalSpec.bw.value / allocated.bw.value
      Mbps(vmSpec.bw.value * ratio)

  /** Performance degradation factor due to RAM swapping.
    *
    * Returns 1.0 when no swap is needed, < 1.0 when swapping. Swap penalty increases linearly with the amount of
    * oversubscription.
    */
  def memoryPerformanceFactor: Double =
    if !isRamOversubscribed then 1.0
    else
      val oversubRatio = allocated.ram.value / physicalSpec.ram.value
      val swapFraction = 1.0 - (1.0 / oversubRatio)
      val penalty      = 1.0 / (1.0 + swapFraction * (config.swapPenaltyFactor - 1.0))
      math.max(0.01, penalty) // never fully zero

object OversubscribableResources:
  def fromSpec(
      spec: ResourceSpec,
      config: OversubscriptionConfig = OversubscriptionConfig.none
  ): OversubscribableResources =
    OversubscribableResources(
      physicalSpec = spec,
      allocated = ResourceSpec(PEs(0), MIPS.Zero, MegaBytes.Zero, Mbps.Zero, MegaBytes.Zero),
      config = config
    )
