// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

/** Models host and VM startup/shutdown delays and boot-time power consumption.
  *
  * Real hosts take 30-120 seconds to boot (BIOS, OS, services). VMs take 5-60 seconds depending on image size and
  * hypervisor. During boot, hosts consume power but cannot serve workloads.
  */

/** Boot configuration for a host. */
final case class HostBootConfig(
    startupDelay: SimTime = SimTime(60.0),  // time to boot (BIOS + OS)
    shutdownDelay: SimTime = SimTime(10.0), // time to gracefully shut down
    bootPowerFraction: Double = 0.5         // fraction of max power consumed during boot
)

object HostBootConfig:
  /** Fast boot (SSD, minimal services). */
  val fast: HostBootConfig = HostBootConfig(
    startupDelay = SimTime(15.0),
    shutdownDelay = SimTime(5.0),
    bootPowerFraction = 0.4
  )

  /** Standard server boot. */
  val standard: HostBootConfig = HostBootConfig(
    startupDelay = SimTime(60.0),
    shutdownDelay = SimTime(10.0),
    bootPowerFraction = 0.5
  )

  /** Slow boot (legacy hardware, full POST). */
  val slow: HostBootConfig = HostBootConfig(
    startupDelay = SimTime(120.0),
    shutdownDelay = SimTime(20.0),
    bootPowerFraction = 0.6
  )

  /** Instant boot (no delay — backwards compatible default). */
  val instant: HostBootConfig = HostBootConfig(
    startupDelay = SimTime.Zero,
    shutdownDelay = SimTime.Zero,
    bootPowerFraction = 0.0
  )

/** Boot configuration for a VM. */
final case class VmBootConfig(
    startupDelay: SimTime = SimTime(10.0),          // time to boot VM (hypervisor + OS)
    shutdownDelay: SimTime = SimTime(3.0),          // time to gracefully shut down
    bootCpuOverhead: Utilization = Utilization(0.3) // CPU used during boot
)

object VmBootConfig:
  /** Light VM (minimal OS, containers-like). */
  val light: VmBootConfig = VmBootConfig(
    startupDelay = SimTime(5.0),
    shutdownDelay = SimTime(2.0),
    bootCpuOverhead = Utilization(0.2)
  )

  /** Standard VM (full OS). */
  val standard: VmBootConfig = VmBootConfig(
    startupDelay = SimTime(30.0),
    shutdownDelay = SimTime(5.0),
    bootCpuOverhead = Utilization(0.3)
  )

  /** Heavy VM (large image, complex init). */
  val heavy: VmBootConfig = VmBootConfig(
    startupDelay = SimTime(60.0),
    shutdownDelay = SimTime(10.0),
    bootCpuOverhead = Utilization(0.5)
  )

  /** Instant boot (no delay — backwards compatible default). */
  val instant: VmBootConfig = VmBootConfig(
    startupDelay = SimTime.Zero,
    shutdownDelay = SimTime.Zero,
    bootCpuOverhead = Utilization.Zero
  )

/** Host lifecycle state tracking boot phases. */
enum HostLifecycleState:
  case Off
  case Booting(startedAt: SimTime, readyAt: SimTime)
  case Running
  case ShuttingDown(startedAt: SimTime, offAt: SimTime)

object HostLifecycleState:
  /** Check if host can accept VM placements. */
  def isReady(state: HostLifecycleState): Boolean = state match
    case Running => true
    case _       => false

  /** Check if host is consuming power. */
  def isPowered(state: HostLifecycleState): Boolean = state match
    case Off => false
    case _   => true

  /** Get boot power fraction for current state. */
  def powerFraction(state: HostLifecycleState, bootConfig: HostBootConfig): Double = state match
    case Off                => 0.0
    case Booting(_, _)      => bootConfig.bootPowerFraction
    case Running            => 1.0 // normal power model applies
    case ShuttingDown(_, _) => bootConfig.bootPowerFraction * 0.5

/** VM lifecycle state tracking boot phases. */
enum VmLifecycleState:
  case Creating(startedAt: SimTime, readyAt: SimTime)
  case Running
  case ShuttingDown(startedAt: SimTime, offAt: SimTime)
  case Destroyed

object VmLifecycleState:
  def isReady(state: VmLifecycleState): Boolean = state match
    case Running => true
    case _       => false

/** Compute boot-time energy consumption.
  *
  * @param maxPower
  *   Maximum power of the host
  * @param bootConfig
  *   Boot configuration
  * @return
  *   Energy consumed during boot in watt-hours
  */
def bootEnergy(maxPower: Watts, bootConfig: HostBootConfig): WattHours =
  val powerDuringBoot = Watts(maxPower.value * bootConfig.bootPowerFraction)
  WattHours(powerDuringBoot.value * bootConfig.startupDelay.value / 3600.0)

/** Compute shutdown energy consumption. */
def shutdownEnergy(maxPower: Watts, bootConfig: HostBootConfig): WattHours =
  val powerDuringShutdown = Watts(maxPower.value * bootConfig.bootPowerFraction * 0.5)
  WattHours(powerDuringShutdown.value * bootConfig.shutdownDelay.value / 3600.0)
