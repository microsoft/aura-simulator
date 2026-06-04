// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.actors

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.core.events.*
import io.aura.core.events.SimEventPayload.*
import io.aura.iaas.policies.{HorizontalScalingPolicy, SpotInstanceConfig}

class BrokerActorSpec extends AnyFlatSpec with Matchers:

  private val dcRef    = EntityRef("dc-0", EntityType.Datacenter)
  private val testSpec = ResourceSpec(PEs(1), MIPS(1000), MegaBytes(512), Mbps(100), MegaBytes(1000))

  private def minimalConfig(
      vmRequests: Vector[BrokerActor.VmRequest] = Vector.empty,
      horizontalScaling: Option[HorizontalScalingPolicy] = None,
      spotConfig: Option[SpotInstanceConfig] = None
  ): BrokerActor.Config =
    BrokerActor.Config(
      brokerId = BrokerId(0),
      vmRequests = vmRequests,
      workloads = Vector.empty,
      datacenterRef = dcRef,
      coordinator = null, // not used by generateEvents
      horizontalScaling = horizontalScaling,
      spotConfig = spotConfig
    )

  // ─── generateEvents ──────────────────────────────────────────────────

  "generateEvents" should "return empty vector for empty config" in {
    val events = BrokerActor.generateEvents(minimalConfig())
    events shouldBe empty
  }

  it should "create VmCreateRequest events for each VM request" in {
    val vms = Vector(
      BrokerActor.VmRequest(VmId(1), testSpec, SimTime(0.0)),
      BrokerActor.VmRequest(VmId(2), testSpec, SimTime(5.0))
    )
    val events = BrokerActor.generateEvents(minimalConfig(vmRequests = vms))
    events should have size 2
    events(0).payload shouldBe a[VmCreateRequest]
    events(0).time shouldBe SimTime(0.0)
    events(1).time shouldBe SimTime(5.0)
    events(0).destination shouldBe dcRef
  }

  it should "include VmCreateRequest with correct brokerId and spec" in {
    val vms    = Vector(BrokerActor.VmRequest(VmId(1), testSpec, SimTime(0.0)))
    val events = BrokerActor.generateEvents(minimalConfig(vmRequests = vms))
    events.head.payload match
      case VmCreateRequest(vmId, brokerId, s) =>
        vmId shouldBe VmId(1)
        brokerId shouldBe BrokerId(0)
        s shouldBe testSpec
      case _ => fail("Expected VmCreateRequest")
  }

  it should "set source to broker entity ref" in {
    val vms    = Vector(BrokerActor.VmRequest(VmId(1), testSpec, SimTime(0.0)))
    val events = BrokerActor.generateEvents(minimalConfig(vmRequests = vms))
    events.head.source.name shouldBe "broker-0"
    events.head.source.entityType shouldBe EntityType.Broker
  }

  it should "include ScalingCheck event when horizontalScaling is configured" in {
    val config = minimalConfig(
      horizontalScaling = Some(HorizontalScalingPolicy.cpuThreshold(0.8, testSpec))
    )
    val events = BrokerActor.generateEvents(config)
    events should have size 1
    events.head.payload shouldBe a[ScalingCheck]
    events.head.time shouldBe SimTime(50.0) // default scalingCheckInterval
  }

  it should "include spot interruption events when spotConfig is configured" in {
    val vms = Vector(BrokerActor.VmRequest(VmId(1), testSpec, SimTime(0.0)))
    val config = minimalConfig(
      vmRequests = vms,
      spotConfig = Some(SpotInstanceConfig(interruptionTimes = Vector(SimTime(100.0))))
    )
    val events = BrokerActor.generateEvents(config)
    // Should have: 1 VmCreate + spot interruption events
    events.size should be >= 2
    events.head.payload shouldBe a[VmCreateRequest]
  }

  it should "combine VM + scaling + spot events" in {
    val vms = Vector(BrokerActor.VmRequest(VmId(1), testSpec, SimTime(0.0)))
    val config = minimalConfig(
      vmRequests = vms,
      horizontalScaling = Some(HorizontalScalingPolicy.cpuThreshold(0.8, testSpec)),
      spotConfig = Some(SpotInstanceConfig(interruptionTimes = Vector(SimTime(100.0))))
    )
    val events = BrokerActor.generateEvents(config)
    // VM event + scaling event + spot events
    events.size should be >= 3
  }
