// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import io.aura.core.types.*
import io.aura.iaas.state.HostState

/** Energy-aware VM consolidation policies (Beloglazov-style).
  *
  * Dynamic consolidation reduces energy by migrating VMs from underloaded hosts (which can be switched off) and
  * overloaded hosts (which risk SLA violations). Implemented as composable pure functions.
  */

// ── Overload Detection ──────────────────────────────────────────────

/** Determines if a host is overloaded (should migrate VMs away). */
type OverloadDetector = HostState => Boolean

object OverloadDetector:

  /** Static threshold: overloaded if CPU > threshold. */
  def staticThreshold(upper: Double): OverloadDetector =
    host => host.active && host.cpuUtilization.value > upper

  /** Median Absolute Deviation (MAD): adaptive threshold. Upper threshold = 1 - safety * MAD, where MAD is computed
    * from utilization history. Based on Beloglazov & Buyya (2012).
    */
  def mad(safety: Double = 2.5, minHistory: Int = 12): OverloadDetector =
    host =>
      if !host.active || host.utilizationHistory.size < minHistory then false
      else
        val values     = host.utilizationHistory.map(_._2.value)
        val median     = percentile(values, 0.5)
        val deviations = values.map(v => math.abs(v - median))
        val madValue   = percentile(deviations, 0.5) * 1.4826 // scale to match std dev
        val threshold  = math.min(1.0, 1.0 - safety * madValue)
        host.cpuUtilization.value > threshold

  /** Interquartile Range (IQR): adaptive threshold. Upper threshold = 1 - safety * IQR. Based on Beloglazov & Buyya
    * (2012).
    */
  def iqr(safety: Double = 1.5, minHistory: Int = 12): OverloadDetector =
    host =>
      if !host.active || host.utilizationHistory.size < minHistory then false
      else
        val values    = host.utilizationHistory.map(_._2.value)
        val q1        = percentile(values, 0.25)
        val q3        = percentile(values, 0.75)
        val iqrValue  = q3 - q1
        val threshold = math.min(1.0, 1.0 - safety * iqrValue)
        host.cpuUtilization.value > threshold

  /** Local Regression (LR): predict next utilization via linear trend. Overloaded if predicted utilization > threshold.
    */
  def localRegression(threshold: Double = 0.9, minHistory: Int = 6): OverloadDetector =
    host =>
      if !host.active || host.utilizationHistory.size < minHistory then false
      else
        val n     = host.utilizationHistory.size.toDouble
        val xs    = (0 until host.utilizationHistory.size).map(_.toDouble)
        val ys    = host.utilizationHistory.map(_._2.value)
        val xMean = xs.sum / n
        val yMean = ys.sum / n
        val num   = xs.zip(ys).map((x, y) => (x - xMean) * (y - yMean)).sum
        val den   = xs.map(x => (x - xMean) * (x - xMean)).sum
        if den == 0 then host.cpuUtilization.value > threshold
        else
          val slope     = num / den
          val intercept = yMean - slope * xMean
          val predicted = slope * n + intercept // predict next step
          predicted > threshold

  private def percentile(values: Vector[Double], p: Double): Double =
    val sorted = values.sorted
    val idx    = math.max(0, math.min(sorted.size - 1, (sorted.size * p).toInt))
    sorted(idx)

// ── Underload Detection ─────────────────────────────────────────────

/** Determines if a host is underloaded (all VMs can be migrated away, host powered off). */
type UnderloadDetector = HostState => Boolean

object UnderloadDetector:

  /** Static threshold: underloaded if CPU < threshold. */
  def staticThreshold(lower: Double): UnderloadDetector =
    host => host.active && host.vms.nonEmpty && host.cpuUtilization.value < lower

  /** Idle: host has VMs but near-zero utilization. */
  val idle: UnderloadDetector =
    host => host.active && host.vms.nonEmpty && host.cpuUtilization.value < 0.01

// ── VM Selection ────────────────────────────────────────────────────

/** Select which VMs to migrate from an overloaded host. Returns the VMs that should be migrated (in order of priority).
  */
type VmSelectionPolicy = HostState => Vector[VmId]

object VmSelectionPolicy:

  /** Minimum Migration Time: select VM with the smallest RAM (fastest to migrate). */
  val minimumMigrationTime: VmSelectionPolicy = host =>
    host.vms.values.toVector
      .sortBy(_.spec.ram.value)
      .map(_.id)
      .take(1)

  /** Maximum Correlation: select VMs whose utilization is most correlated with host. Simplified: select VM using the
    * most CPU (highest impact on reducing load).
    */
  val highestUtilization: VmSelectionPolicy = host =>
    host.vms.values.toVector
      .sortBy(vm => -vm.spec.mips.value)
      .map(_.id)
      .take(1)

  /** Random: select a random VM. Deterministic via seed. */
  def randomVm(seed: Long): VmSelectionPolicy =
    val rng = new scala.util.Random(seed)
    host =>
      val vmIds = host.vms.keys.toVector
      if vmIds.isEmpty then Vector.empty
      else Vector(vmIds(rng.nextInt(vmIds.size)))

  /** Minimum utilization: select VM with lowest CPU usage (least impact on remaining load). */
  val minimumUtilization: VmSelectionPolicy = host =>
    host.vms.values.toVector
      .sortBy(_.spec.mips.value)
      .map(_.id)
      .take(1)

// ── Consolidation Plan ──────────────────────────────────────────────

/** A planned VM migration for consolidation. */
final case class ConsolidationMigration(
    vmId: VmId,
    sourceHostId: HostId,
    targetHostId: HostId,
    reason: ConsolidationReason
)

enum ConsolidationReason:
  case OverloadRelief
  case UnderloadEvacuation

/** Summary of a consolidation round. */
final case class ConsolidationPlan(
    migrations: Vector[ConsolidationMigration],
    hostsToDeactivate: Set[HostId],
    overloadedHosts: Set[HostId],
    underloadedHosts: Set[HostId]
):
  def migrationCount: Int  = migrations.size
  def hostsSavedCount: Int = hostsToDeactivate.size

// ── Consolidation Engine ────────────────────────────────────────────

object ConsolidationEngine:

  /** Run a full consolidation analysis.
    *   1. Detect overloaded hosts → select VMs → find targets 2. Detect underloaded hosts → migrate all VMs → mark for
    *      deactivation
    */
  def planConsolidation(
      hosts: Vector[HostState],
      overloadDetector: OverloadDetector,
      underloadDetector: UnderloadDetector,
      vmSelection: VmSelectionPolicy,
      allocationPolicy: VmAllocationPolicy
  ): ConsolidationPlan =
    val activeHosts = hosts.filter(_.active)

    // Phase 1: Detect overloaded and underloaded
    val overloaded  = activeHosts.filter(overloadDetector)
    val underloaded = activeHosts.filter(h => underloadDetector(h) && !overloadDetector(h))
    // Phase 2: Relieve overloaded hosts
    val (migrationsAfterOverload, availAfterOverload) =
      overloaded.foldLeft((Vector.empty[ConsolidationMigration], hosts.map(h => h.id -> h.available).toMap)) {
        case ((migs, availability), host) =>
          val vmsToMigrate = vmSelection(host)
          vmsToMigrate.foldLeft((migs, availability)) { case ((m, avail), vmId) =>
            host.vms.get(vmId) match
              case Some(vm) =>
                val candidates = hosts.filter { h =>
                  h.id != host.id && h.active && !overloaded.exists(_.id == h.id) &&
                  avail.get(h.id).exists(_.canFit(vm.spec))
                }
                if candidates.nonEmpty then
                  val asHostStates = candidates.map(c => c.copy(available = avail(c.id))).toIndexedSeq
                  allocationPolicy(asHostStates, vm.spec) match
                    case Some(targetId) =>
                      (
                        m :+ ConsolidationMigration(vmId, host.id, targetId, ConsolidationReason.OverloadRelief),
                        avail.updated(targetId, avail(targetId).allocate(vm.spec))
                      )
                    case None => (m, avail)
                else (m, avail)
              case None => (m, avail)
          }
      }

    // Phase 3: Evacuate underloaded hosts
    val (finalMigrations, _, hostsToDeactivate) =
      underloaded.foldLeft((migrationsAfterOverload, availAfterOverload, Set.empty[HostId])) {
        case ((migs, availability, deactivate), host) =>
          val allVms = host.vms.values.toVector
          // Try to evacuate all VMs from this host
          val evacuationResult = allVms.foldLeft(Option((Vector.empty[ConsolidationMigration], availability))) {
            case (None, _) => None // Failed to evacuate a previous VM
            case (Some((pending, avail)), vm) =>
              val candidates = hosts.filter { h =>
                h.id != host.id && h.active && !deactivate.contains(h.id) &&
                !underloaded.exists(_.id == h.id) &&
                avail.get(h.id).exists(_.canFit(vm.spec))
              }
              val asHostStates = candidates.map(c => c.copy(available = avail(c.id))).toIndexedSeq
              allocationPolicy(asHostStates, vm.spec) match
                case Some(targetId) =>
                  Some(
                    (
                      pending :+ ConsolidationMigration(
                        vm.id,
                        host.id,
                        targetId,
                        ConsolidationReason.UnderloadEvacuation
                      ),
                      avail.updated(targetId, avail(targetId).allocate(vm.spec))
                    )
                  )
                case None => None
          }

          evacuationResult match
            case Some((pendingMigrations, updatedAvail)) if allVms.nonEmpty =>
              (migs ++ pendingMigrations, updatedAvail, deactivate + host.id)
            case _ =>
              (migs, availability, deactivate)
      }

    ConsolidationPlan(
      migrations = finalMigrations,
      hostsToDeactivate = hostsToDeactivate,
      overloadedHosts = overloaded.map(_.id).toSet,
      underloadedHosts = underloaded.map(_.id).toSet
    )

  /** Estimate energy saved by deactivating hosts. Returns estimated watts saved (idle power of deactivated hosts).
    */
  def estimatedPowerSaved(
      hostsToDeactivate: Set[HostId],
      hosts: Vector[HostState],
      idlePowerWatts: Double = 100.0
  ): Watts =
    Watts(hostsToDeactivate.size * idlePowerWatts)

  /** Compute the SLA violation rate: fraction of overloaded hosts. */
  def slaViolationRate(hosts: Vector[HostState], overloadDetector: OverloadDetector): Double =
    val active = hosts.filter(_.active)
    if active.isEmpty then 0.0
    else active.count(overloadDetector).toDouble / active.size

  /** Compute overall data center utilization. */
  def dataCenterUtilization(hosts: Vector[HostState]): Double =
    val active = hosts.filter(_.active)
    if active.isEmpty then 0.0
    else active.map(_.cpuUtilization.value).sum / active.size

  /** Count how many hosts could be powered off (have no VMs). */
  def idleHostCount(hosts: Vector[HostState]): Int =
    hosts.count(h => h.active && h.vms.isEmpty)
