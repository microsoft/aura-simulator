// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import io.aura.core.types.*
import io.aura.core.events.WorkloadSpec

/** Context provided to a fault recovery policy when a workload is preempted. */
final case class RecoveryContext(
    workloadSpec: WorkloadSpec,
    executedMI: MI,
    retryCount: Int
)

/** Action to take for a preempted workload. */
sealed trait FaultRecoveryAction

object FaultRecoveryAction:
  /** Drop the workload — no recovery. */
  case object Drop extends FaultRecoveryAction

  /** Resubmit the workload with the given (possibly adjusted) spec. */
  case class Resubmit(spec: WorkloadSpec) extends FaultRecoveryAction

/** A fault recovery policy determines what happens to preempted workloads.
  *
  * Takes a RecoveryContext and returns a FaultRecoveryAction.
  */
type FaultRecoveryPolicy = RecoveryContext => FaultRecoveryAction

object FaultRecoveryPolicy:

  /** No recovery — preempted workloads are dropped. */
  val none: FaultRecoveryPolicy = _ => FaultRecoveryAction.Drop

  /** Resubmit the original workload from scratch, up to maxRetries attempts. */
  def resubmit(maxRetries: Int = 3): FaultRecoveryPolicy =
    ctx =>
      if ctx.retryCount < maxRetries then FaultRecoveryAction.Resubmit(ctx.workloadSpec)
      else FaultRecoveryAction.Drop

  /** Resubmit from the last checkpoint, reducing remaining work.
    *
    * Checkpoints save workload progress every `checkpointInterval` MI. On failure, only work since the last checkpoint
    * is lost. The resubmitted workload has reduced length: totalMI - checkpointedMI.
    *
    * @param checkpointInterval
    *   MI between checkpoints
    * @param maxRetries
    *   maximum retry attempts
    */
  def checkpointResubmit(
      checkpointInterval: MI,
      maxRetries: Int = 3
  ): FaultRecoveryPolicy =
    ctx =>
      if ctx.retryCount < maxRetries then
        val checkpointedMI = if checkpointInterval.value > 0 then
          val checkpoints = math.floor(ctx.executedMI.value / checkpointInterval.value)
          MI(checkpoints * checkpointInterval.value)
        else MI.Zero
        val remainingMI = MI(ctx.workloadSpec.length.value - checkpointedMI.value)
        if remainingMI.value <= 0 then
          // Workload was effectively complete at last checkpoint
          FaultRecoveryAction.Drop
        else
          val adjusted = ctx.workloadSpec.copy(length = remainingMI)
          FaultRecoveryAction.Resubmit(adjusted)
      else FaultRecoveryAction.Drop
