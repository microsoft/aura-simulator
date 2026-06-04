// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.types.*
import io.aura.core.events.*

class SpotInstanceSpec extends AnyFlatSpec with Matchers:

  val brokerRef = EntityRef("broker-0", EntityType.Broker)

  "SpotInstanceConfig.periodic" should "generate evenly-spaced interruption times" in {
    val config = SpotInstanceConfig.periodic(
      interval = SimTime(100.0),
      startAfter = SimTime(200.0),
      endBefore = SimTime(500.0)
    )
    config.interruptionTimes should have size 3
    config.interruptionTimes(0).value shouldBe 200.0
    config.interruptionTimes(1).value shouldBe 300.0
    config.interruptionTimes(2).value shouldBe 400.0
  }

  it should "use default notice period of 120 seconds" in {
    val config = SpotInstanceConfig.periodic(interval = SimTime(100.0))
    config.noticePeriod.value shouldBe 120.0
  }

  "SpotInterruptionScheduler" should "generate events for all VMs at each interruption time" in {
    val vmIds = Vector(VmId(0L), VmId(1L))
    val config = SpotInstanceConfig(
      interruptionTimes = Vector(SimTime(100.0), SimTime(200.0)),
      noticePeriod = SimTime(60.0)
    )

    val events = SpotInterruptionScheduler.generateEvents(brokerRef, vmIds, config)

    events should have size 4 // 2 VMs * 2 times
    events.head.payload shouldBe a[SimEventPayload.SpotInterruption]
    val SimEventPayload.SpotInterruption(vmId, notice) = events.head.payload: @unchecked
    vmId shouldBe VmId(0L)
    notice.value shouldBe 60.0
  }

  it should "generate no events when VM list is empty" in {
    val config = SpotInstanceConfig(interruptionTimes = Vector(SimTime(100.0)))
    SpotInterruptionScheduler.generateEvents(brokerRef, Vector.empty, config) shouldBe empty
  }

  it should "generate no events when interruption times are empty" in {
    SpotInterruptionScheduler.generateEvents(brokerRef, Vector(VmId(0L)), SpotInstanceConfig()) shouldBe empty
  }

  "SpotInstanceConfig" should "support fallback to on-demand by default" in {
    val config = SpotInstanceConfig.periodic(interval = SimTime(100.0))
    config.fallbackToOnDemand shouldBe true
  }

  it should "support disabling fallback" in {
    val config = SpotInstanceConfig.periodic(
      interval = SimTime(100.0),
      fallbackToOnDemand = false
    )
    config.fallbackToOnDemand shouldBe false
  }
