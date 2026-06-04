// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import io.aura.core.types.*

/** Parameters for computing a migration plan. */
final case class MigrationParams(
    vmRam: MegaBytes,
    bandwidth: Mbps
)

/** Result of a migration plan computation. */
final case class MigrationPlan(
    totalTime: SimTime,
    downtime: SimTime,
    totalDataTransferred: MegaBytes
)

/** Migration model as a pure function: MigrationParams => MigrationPlan */
type MigrationModel = MigrationParams => MigrationPlan

object MigrationModel:

  /** Simple pre-copy migration: time = ramMB * 8 / bandwidthMbps Converts RAM from megabytes to megabits, then divides
    * by bandwidth.
    */
  val preCopy: MigrationModel = (params: MigrationParams) =>
    val dataMbits   = params.vmRam.value * 8.0
    val timeSeconds = dataMbits / params.bandwidth.value
    MigrationPlan(
      totalTime = SimTime(timeSeconds),
      downtime = SimTime(timeSeconds * 0.1), // 10% downtime for final sync
      totalDataTransferred = params.vmRam
    )

  /** Iterative pre-copy migration with dirty page tracking.
    *
    * Each round transfers dirty pages. The dirty rate decreases each round as fewer pages change. Uses foldLeft over
    * rounds for functional purity.
    *
    * @param maxRounds
    *   Maximum number of pre-copy rounds
    * @param dirtyPageRate
    *   Fraction of pages dirtied per second (default 0.1)
    */
  def preCopyIterative(maxRounds: Int = 10, dirtyPageRate: Double = 0.1): MigrationModel =
    (params: MigrationParams) =>
      val bandwidthMBps = params.bandwidth.value / 8.0 // convert Mbps to MBps

      case class RoundState(
          remainingDirty: MegaBytes,
          totalTime: SimTime,
          totalTransferred: MegaBytes
      )

      val initial = RoundState(
        remainingDirty = params.vmRam,
        totalTime = SimTime.Zero,
        totalTransferred = MegaBytes.Zero
      )

      val finalState = (1 to maxRounds).foldLeft(initial) { (state, _) =>
        if state.remainingDirty.value < 1.0 then state // convergence threshold: < 1MB
        else
          val transferTime = state.remainingDirty.value / bandwidthMBps
          val newDirty     = MegaBytes(state.remainingDirty.value * dirtyPageRate * transferTime)
          RoundState(
            remainingDirty = newDirty,
            totalTime = SimTime(state.totalTime.value + transferTime),
            totalTransferred = MegaBytes(state.totalTransferred.value + state.remainingDirty.value)
          )
      }

      // Final round: stop-and-copy remaining dirty pages
      val finalTransferTime = finalState.remainingDirty.value / bandwidthMBps

      MigrationPlan(
        totalTime = SimTime(finalState.totalTime.value + finalTransferTime),
        downtime = SimTime(finalTransferTime),
        totalDataTransferred = MegaBytes(finalState.totalTransferred.value + finalState.remainingDirty.value)
      )

  /** Post-copy migration: transfer minimal state first, fetch remaining on demand.
    *
    * Transfers CPU state + page table (~minimalStateRatio of RAM) first, then remaining pages are fetched on-demand at
    * onDemandFetchRate. Downtime = initial minimal state transfer time only (much lower than pre-copy).
    *
    * @param minimalStateRatio
    *   Fraction of RAM transferred upfront (default 0.02 = ~2%)
    * @param onDemandFetchRate
    *   Multiplier for on-demand fetch overhead (default 1.5)
    */
  def postCopy(minimalStateRatio: Double = 0.02, onDemandFetchRate: Double = 1.5): MigrationModel =
    (params: MigrationParams) =>
      val bandwidthMBps  = params.bandwidth.value / 8.0
      val minimalState   = params.vmRam.value * minimalStateRatio
      val remainingPages = params.vmRam.value * (1.0 - minimalStateRatio)

      val initialTransferTime = minimalState / bandwidthMBps
      val onDemandTime        = (remainingPages / bandwidthMBps) * onDemandFetchRate

      MigrationPlan(
        totalTime = SimTime(initialTransferTime + onDemandTime),
        downtime = SimTime(initialTransferTime),
        totalDataTransferred = params.vmRam
      )

  /** Non-live (cold) migration: stop VM, transfer all RAM, restart.
    *
    * Simplest model with highest downtime (= total time).
    *
    * @param shutdownDelay
    *   Time to gracefully shut down VM (seconds)
    * @param bootDelay
    *   Time to boot VM at destination (seconds)
    */
  def nonLive(shutdownDelay: SimTime = SimTime(1.0), bootDelay: SimTime = SimTime(2.0)): MigrationModel =
    (params: MigrationParams) =>
      val dataMbits    = params.vmRam.value * 8.0
      val transferTime = dataMbits / params.bandwidth.value
      val totalTime    = shutdownDelay.value + transferTime + bootDelay.value

      MigrationPlan(
        totalTime = SimTime(totalTime),
        downtime = SimTime(totalTime),
        totalDataTransferred = params.vmRam
      )
