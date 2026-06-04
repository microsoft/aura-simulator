// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.serverless

import io.aura.core.types.*

/** Pure function: (memoryMB, executionDuration) => GBSeconds billed.
  *
  * Models provider-specific billing granularity and minimums.
  */
object BillingModel:

  type BillingModel = (MegaBytes, SimTime) => GBSeconds

  /** AWS Lambda billing: GB-seconds with 1ms minimum, 1ms granularity. */
  val awsLambda: BillingModel = custom(
    minDuration = SimTime(0.001),
    granularity = SimTime(0.001)
  )

  /** Configurable billing model with minimum duration and granularity. */
  def custom(minDuration: SimTime, granularity: SimTime): BillingModel =
    (memoryMB, duration) =>
      val effectiveDuration = duration.value.max(minDuration.value)
      val roundedDuration =
        if granularity.value > 0 then math.ceil(effectiveDuration / granularity.value) * granularity.value
        else effectiveDuration
      val gbMemory = memoryMB.value / 1024.0
      GBSeconds(gbMemory * roundedDuration)
