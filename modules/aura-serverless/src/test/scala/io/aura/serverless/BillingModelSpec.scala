// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.serverless

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*

class BillingModelSpec extends AnyFlatSpec with Matchers:

  "BillingModel.awsLambda" should "compute GB-seconds correctly" in {
    // 1024 MB = 1 GB, running for 1 second = 1 GB-s
    val billed = BillingModel.awsLambda(MegaBytes(1024.0), SimTime(1.0))
    billed.value shouldBe 1.0
  }

  it should "apply 1ms minimum duration" in {
    // Very short duration should be billed at 1ms minimum
    val billed = BillingModel.awsLambda(MegaBytes(1024.0), SimTime(0.0001))
    billed.value shouldBe (1024.0 / 1024.0 * 0.001) +- 0.0001
  }

  it should "round up to 1ms granularity" in {
    // 1.5ms should be rounded up to 2ms
    val billed = BillingModel.awsLambda(MegaBytes(1024.0), SimTime(0.0015))
    billed.value shouldBe (1024.0 / 1024.0 * 0.002) +- 0.0001
  }

  it should "convert memory to GB" in {
    // 512 MB = 0.5 GB, running for 2 seconds = 1 GB-s
    val billed = BillingModel.awsLambda(MegaBytes(512.0), SimTime(2.0))
    billed.value shouldBe 1.0
  }

  "BillingModel.custom" should "support custom granularity" in {
    // 100ms granularity
    val model = BillingModel.custom(
      minDuration = SimTime(0.1),
      granularity = SimTime(0.1)
    )
    // 150ms should round up to 200ms
    val billed = model(MegaBytes(1024.0), SimTime(0.15))
    billed.value shouldBe (1.0 * 0.2) +- 0.0001
  }

  it should "apply minimum duration" in {
    val model = BillingModel.custom(
      minDuration = SimTime(1.0),
      granularity = SimTime(0.001)
    )
    // Duration 0.5 should be bumped to minimum 1.0
    val billed = model(MegaBytes(1024.0), SimTime(0.5))
    billed.value shouldBe 1.0
  }
