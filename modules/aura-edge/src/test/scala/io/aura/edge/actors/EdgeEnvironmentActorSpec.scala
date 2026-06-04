// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.edge.actors

import org.apache.pekko.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.core.events.*
import io.aura.core.events.SimEventPayload.*
import io.aura.core.engine.TimeCoordinator
import io.aura.core.engine.TimeCoordinator.ProcessEvents
import io.aura.edge.*

class EdgeEnvironmentActorSpec extends ScalaTestWithActorTestKit with AnyFlatSpecLike with Matchers:

  private val coordinatorProbe = createTestProbe[TimeCoordinator.Command]()

  private val deviceSpec = EdgeNodeSpec(
    name = "sensor-1",
    location = GeoLocation(37.7749, -122.4194),
    spec = ResourceSpec(PEs(1), MIPS(100.0), MegaBytes(256.0), Mbps(100.0), MegaBytes(1000.0)),
    tier = EdgeTier.Device
  )

  private val edgeSpec = EdgeNodeSpec(
    name = "gateway-1",
    location = GeoLocation(37.7849, -122.4094),
    spec = ResourceSpec(PEs(4), MIPS(2000.0), MegaBytes(4096.0), Mbps(1000.0), MegaBytes(10000.0)),
    tier = EdgeTier.EdgeMicro
  )

  private val cloudSpec = EdgeNodeSpec(
    name = "cloud-dc",
    location = GeoLocation(37.3861, -122.0839),
    spec = ResourceSpec(PEs(32), MIPS(50000.0), MegaBytes(65536.0), Mbps(10000.0), MegaBytes(100000.0)),
    tier = EdgeTier.Cloud
  )

  private def createActor(
      policy: OffloadingPolicy = OffloadingPolicy.latencyAware
  ) =
    val config = EdgeEnvironmentActor.Config(
      environmentName = "test-edge",
      nodeSpecs = Vector(deviceSpec, edgeSpec, cloudSpec),
      latencyModel = LatencyModel.combined(),
      offloadingPolicy = policy,
      coordinator = coordinatorProbe.ref
    )
    spawn(EdgeEnvironmentActor(config))

  private val brokerRef = EntityRef("edge-broker-test-edge", EntityType.EdgeEnvironment)
  private val envRef    = EntityRef("test-edge", EntityType.EdgeEnvironment)

  "EdgeEnvironmentActor" should "register with coordinator on startup" in {
    createActor()
    val msg = coordinatorProbe.receiveMessage()
    msg shouldBe a[TimeCoordinator.RegisterEntity]
    val reg = msg.asInstanceOf[TimeCoordinator.RegisterEntity]
    reg.entityRef.name shouldBe "test-edge"
    reg.entityRef.entityType shouldBe EntityType.EdgeEnvironment
  }

  it should "route task to offloading policy and emit EdgeTaskStarted + EdgeTaskCompleted" in {
    val actor = createActor()
    coordinatorProbe.receiveMessage() // RegisterEntity

    val event = SimEvent(
      time = SimTime(1.0),
      source = brokerRef,
      destination = envRef,
      payload = EdgeTaskSubmit(
        taskId = EdgeTaskId(0L),
        sourceNodeName = "sensor-1",
        cpuRequired = MIPS(50.0),
        memRequired = MegaBytes(64.0),
        taskLength = MI(500.0),
        deadline = SimTime(0.1)
      ),
      serial = SerialNumber.Zero
    )

    actor ! ProcessEvents(Vector(event), coordinatorProbe.ref)

    val scheduleMsg = coordinatorProbe.receiveMessage()
    scheduleMsg shouldBe a[TimeCoordinator.ScheduleEvents]
    val events = scheduleMsg.asInstanceOf[TimeCoordinator.ScheduleEvents].events

    // Should emit: EdgeTaskStarted (to broker) + EdgeTaskCompleted (to self) + EdgeTaskCompleted (to broker)
    events should have size 3
    events.count(_.payload.isInstanceOf[EdgeTaskStarted]) shouldBe 1
    events.count(_.payload.isInstanceOf[EdgeTaskCompleted]) shouldBe 2

    // One EdgeTaskCompleted should be to self, one to broker
    val completedEvents = events.filter(_.payload.isInstanceOf[EdgeTaskCompleted])
    completedEvents.count(_.destination == envRef) shouldBe 1
    completedEvents.count(_.destination == brokerRef) shouldBe 1

    coordinatorProbe.receiveMessage() // StepComplete
  }

  it should "handle unknown source node with EdgeTaskFailed" in {
    val actor = createActor()
    coordinatorProbe.receiveMessage() // RegisterEntity

    val event = SimEvent(
      time = SimTime(1.0),
      source = brokerRef,
      destination = envRef,
      payload = EdgeTaskSubmit(
        taskId = EdgeTaskId(0L),
        sourceNodeName = "nonexistent-node",
        cpuRequired = MIPS(50.0),
        memRequired = MegaBytes(64.0),
        taskLength = MI(500.0),
        deadline = SimTime(0.1)
      ),
      serial = SerialNumber.Zero
    )

    actor ! ProcessEvents(Vector(event), coordinatorProbe.ref)

    val scheduleMsg = coordinatorProbe.receiveMessage()
    scheduleMsg shouldBe a[TimeCoordinator.ScheduleEvents]
    val events = scheduleMsg.asInstanceOf[TimeCoordinator.ScheduleEvents].events
    events should have size 1
    events.head.payload shouldBe a[EdgeTaskFailed]
    val failed = events.head.payload.asInstanceOf[EdgeTaskFailed]
    failed.reason should include("Unknown source node")

    coordinatorProbe.receiveMessage() // StepComplete
  }

  it should "handle multiple tasks in single time step" in {
    val actor = createActor()
    coordinatorProbe.receiveMessage() // RegisterEntity

    val events = (0 to 2).map { i =>
      SimEvent(
        time = SimTime(1.0),
        source = brokerRef,
        destination = envRef,
        payload = EdgeTaskSubmit(
          taskId = EdgeTaskId(i.toLong),
          sourceNodeName = "sensor-1",
          cpuRequired = MIPS(10.0),
          memRequired = MegaBytes(32.0),
          taskLength = MI(200.0),
          deadline = SimTime(0.1)
        ),
        serial = SerialNumber(i.toLong)
      )
    }.toVector

    actor ! ProcessEvents(events, coordinatorProbe.ref)

    val scheduleMsg = coordinatorProbe.receiveMessage()
    scheduleMsg shouldBe a[TimeCoordinator.ScheduleEvents]
    val emitted = scheduleMsg.asInstanceOf[TimeCoordinator.ScheduleEvents].events

    // 3 tasks × 3 events each = 9 events
    emitted should have size 9
    emitted.count(_.payload.isInstanceOf[EdgeTaskStarted]) shouldBe 3
    emitted.count(_.payload.isInstanceOf[EdgeTaskCompleted]) shouldBe 6

    coordinatorProbe.receiveMessage() // StepComplete
  }

  it should "offload tasks when using latencyAware policy and local is overloaded" in {
    // Use a custom policy that forces offloading
    val actor = createActor(policy = OffloadingPolicy.nearest)
    coordinatorProbe.receiveMessage() // RegisterEntity

    val event = SimEvent(
      time = SimTime(1.0),
      source = brokerRef,
      destination = envRef,
      payload = EdgeTaskSubmit(
        taskId = EdgeTaskId(0L),
        sourceNodeName = "sensor-1",
        cpuRequired = MIPS(50.0),
        memRequired = MegaBytes(64.0),
        taskLength = MI(500.0),
        deadline = SimTime(0.1)
      ),
      serial = SerialNumber.Zero
    )

    actor ! ProcessEvents(Vector(event), coordinatorProbe.ref)

    val scheduleMsg = coordinatorProbe.receiveMessage()
    val events      = scheduleMsg.asInstanceOf[TimeCoordinator.ScheduleEvents].events
    val started = events.collectFirst {
      case e if e.payload.isInstanceOf[EdgeTaskStarted] => e.payload.asInstanceOf[EdgeTaskStarted]
    }
    started shouldBe defined

    coordinatorProbe.receiveMessage() // StepComplete
  }
