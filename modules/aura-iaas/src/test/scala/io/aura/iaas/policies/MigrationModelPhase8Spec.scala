// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.types.*

class MigrationModelPhase8Spec extends AnyFlatSpec with Matchers:

  val defaultParams: MigrationParams = MigrationParams(MegaBytes(4096.0), Mbps(10000.0))

  "postCopy migration" should "produce lower downtime than preCopy" in {
    val preCopyPlan  = MigrationModel.preCopy(defaultParams)
    val postCopyPlan = MigrationModel.postCopy()(defaultParams)

    postCopyPlan.downtime.value should be < preCopyPlan.downtime.value
  }

  it should "transfer less data initially than iterative preCopy" in {
    val iterativePlan = MigrationModel.preCopyIterative()(defaultParams)
    val postCopyPlan  = MigrationModel.postCopy()(defaultParams)

    // Post-copy downtime (initial transfer) is much shorter
    postCopyPlan.downtime.value should be < iterativePlan.totalTime.value
  }

  it should "scale with bandwidth" in {
    val slow = MigrationModel.postCopy()(MigrationParams(MegaBytes(4096.0), Mbps(5000.0)))
    val fast = MigrationModel.postCopy()(MigrationParams(MegaBytes(4096.0), Mbps(10000.0)))

    slow.totalTime.value should be > fast.totalTime.value
    slow.downtime.value should be > fast.downtime.value
  }

  "nonLive migration" should "have downtime equal to total time" in {
    val plan = MigrationModel.nonLive()(defaultParams)

    plan.downtime.value shouldBe plan.totalTime.value +- 0.001
  }

  it should "include shutdown and boot delays" in {
    val shutdownDelay = SimTime(2.0)
    val bootDelay     = SimTime(3.0)
    val plan          = MigrationModel.nonLive(shutdownDelay, bootDelay)(defaultParams)

    // Transfer time = 4096 * 8 / 10000 = 3.2768s
    val expectedTransfer = 4096.0 * 8.0 / 10000.0
    plan.totalTime.value shouldBe (2.0 + expectedTransfer + 3.0) +- 0.001
  }
