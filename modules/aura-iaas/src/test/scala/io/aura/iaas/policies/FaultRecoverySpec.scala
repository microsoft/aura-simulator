// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.types.*
import io.aura.core.events.WorkloadSpec

class FaultRecoverySpec extends AnyFlatSpec with Matchers:

  val workload: WorkloadSpec = WorkloadSpec.simple(
    id = WorkloadId(1L),
    length = MI(10000.0),
    pes = PEs(2)
  )

  // ─── none policy ────────────────────────────────────────────────────

  "FaultRecoveryPolicy.none" should "always drop workloads" in {
    val result = FaultRecoveryPolicy.none(RecoveryContext(workload, MI(5000.0), 0))
    result shouldBe FaultRecoveryAction.Drop
  }

  // ─── resubmit policy ───────────────────────────────────────────────

  "FaultRecoveryPolicy.resubmit" should "resubmit within retry limit" in {
    val policy = FaultRecoveryPolicy.resubmit(maxRetries = 3)
    val result = policy(RecoveryContext(workload, MI(5000.0), retryCount = 0))
    result shouldBe a[FaultRecoveryAction.Resubmit]
    val FaultRecoveryAction.Resubmit(spec) = result: @unchecked
    spec.length.value shouldBe 10000.0 // Full restart
  }

  it should "resubmit up to maxRetries - 1" in {
    val policy = FaultRecoveryPolicy.resubmit(maxRetries = 3)
    val result = policy(RecoveryContext(workload, MI(5000.0), retryCount = 2))
    result shouldBe a[FaultRecoveryAction.Resubmit]
  }

  it should "drop after reaching maxRetries" in {
    val policy = FaultRecoveryPolicy.resubmit(maxRetries = 3)
    val result = policy(RecoveryContext(workload, MI(5000.0), retryCount = 3))
    result shouldBe FaultRecoveryAction.Drop
  }

  // ─── checkpointResubmit policy ──────────────────────────────────────

  "FaultRecoveryPolicy.checkpointResubmit" should "reduce remaining work based on checkpoints" in {
    val policy = FaultRecoveryPolicy.checkpointResubmit(checkpointInterval = MI(2000.0), maxRetries = 3)
    // Executed 5500 MI out of 10000, with checkpoint every 2000 MI
    // Last checkpoint at 4000 MI (floor(5500/2000) * 2000 = 4000)
    // Remaining: 10000 - 4000 = 6000 MI
    val result = policy(RecoveryContext(workload, MI(5500.0), retryCount = 0))
    result shouldBe a[FaultRecoveryAction.Resubmit]
    val FaultRecoveryAction.Resubmit(spec) = result: @unchecked
    spec.length.value shouldBe 6000.0
  }

  it should "restart from scratch when no checkpoints reached" in {
    val policy = FaultRecoveryPolicy.checkpointResubmit(checkpointInterval = MI(5000.0), maxRetries = 3)
    // Executed 3000 MI, checkpoint interval 5000 — no checkpoint reached
    val result = policy(RecoveryContext(workload, MI(3000.0), retryCount = 0))
    result shouldBe a[FaultRecoveryAction.Resubmit]
    val FaultRecoveryAction.Resubmit(spec) = result: @unchecked
    spec.length.value shouldBe 10000.0 // Full restart
  }

  it should "drop if workload was effectively complete at last checkpoint" in {
    val policy = FaultRecoveryPolicy.checkpointResubmit(checkpointInterval = MI(5000.0), maxRetries = 3)
    // totalMI = 10000, executed = 10001, last checkpoint = 10000
    // Remaining = 10000 - 10000 = 0 → drop
    val wl     = workload.copy(length = MI(10000.0))
    val result = policy(RecoveryContext(wl, MI(10001.0), retryCount = 0))
    result shouldBe FaultRecoveryAction.Drop
  }

  it should "drop after reaching maxRetries" in {
    val policy = FaultRecoveryPolicy.checkpointResubmit(checkpointInterval = MI(2000.0), maxRetries = 2)
    val result = policy(RecoveryContext(workload, MI(5000.0), retryCount = 2))
    result shouldBe FaultRecoveryAction.Drop
  }

  it should "handle zero checkpoint interval by restarting from scratch" in {
    val policy = FaultRecoveryPolicy.checkpointResubmit(checkpointInterval = MI(0.0), maxRetries = 3)
    val result = policy(RecoveryContext(workload, MI(5000.0), retryCount = 0))
    result shouldBe a[FaultRecoveryAction.Resubmit]
    val FaultRecoveryAction.Resubmit(spec) = result: @unchecked
    spec.length.value shouldBe 10000.0 // Full restart since no checkpoint
  }
