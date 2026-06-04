// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*

class MigrationModelSpec extends AnyFlatSpec with Matchers:

  "preCopy migration model" should "compute time based on RAM and bandwidth" in {
    val params = MigrationParams(MegaBytes(1024.0), Mbps(10000.0))
    val plan   = MigrationModel.preCopy(params)

    // time = 1024 MB * 8 bits/byte / 10000 Mbps = 0.8192 seconds
    plan.totalTime.value shouldBe 0.8192 +- 0.001
    plan.downtime.value shouldBe (0.8192 * 0.1) +- 0.001
    plan.totalDataTransferred.value shouldBe 1024.0
  }

  it should "scale linearly with RAM size" in {
    val small = MigrationModel.preCopy(MigrationParams(MegaBytes(512.0), Mbps(10000.0)))
    val large = MigrationModel.preCopy(MigrationParams(MegaBytes(1024.0), Mbps(10000.0)))

    large.totalTime.value shouldBe (small.totalTime.value * 2.0) +- 0.001
  }

  it should "scale inversely with bandwidth" in {
    val slow = MigrationModel.preCopy(MigrationParams(MegaBytes(1024.0), Mbps(5000.0)))
    val fast = MigrationModel.preCopy(MigrationParams(MegaBytes(1024.0), Mbps(10000.0)))

    slow.totalTime.value shouldBe (fast.totalTime.value * 2.0) +- 0.001
  }

  "preCopyIterative migration model" should "produce a valid plan" in {
    val params = MigrationParams(MegaBytes(4096.0), Mbps(10000.0))
    val plan   = MigrationModel.preCopyIterative(maxRounds = 10)(params)

    plan.totalTime.value should be > 0.0
    plan.downtime.value should be > 0.0
    plan.downtime.value should be < plan.totalTime.value
    plan.totalDataTransferred.value should be >= params.vmRam.value
  }

  it should "transfer more total data than simple preCopy (due to dirty pages)" in {
    val params    = MigrationParams(MegaBytes(4096.0), Mbps(10000.0))
    val simple    = MigrationModel.preCopy(params)
    val iterative = MigrationModel.preCopyIterative(maxRounds = 10, dirtyPageRate = 0.1)(params)

    iterative.totalDataTransferred.value should be >= simple.totalDataTransferred.value
  }

  it should "converge with enough rounds" in {
    val params = MigrationParams(MegaBytes(2048.0), Mbps(10000.0))
    val plan   = MigrationModel.preCopyIterative(maxRounds = 20, dirtyPageRate = 0.05)(params)

    // With low dirty rate and many rounds, downtime should be small
    plan.downtime.value should be < (plan.totalTime.value * 0.5)
  }
