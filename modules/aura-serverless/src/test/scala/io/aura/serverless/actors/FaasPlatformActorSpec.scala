// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.serverless.actors

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterAll
import org.apache.pekko.actor.testkit.typed.scaladsl.ActorTestKit

import io.aura.core.types.*
import io.aura.core.events.*
import io.aura.core.events.SimEventPayload.*
import io.aura.core.engine.TimeCoordinator
import io.aura.core.engine.TimeCoordinator.{EntityCommand, ProcessEvents, StepComplete}
import io.aura.serverless.*

class FaasPlatformActorSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll:

  val testKit = ActorTestKit("FaasPlatformActorSpec")

  val pythonFunc = FunctionDeploySpec(
    name = "test-func",
    runtime = "Python",
    memoryMB = MegaBytes(256.0),
    timeout = SimTime(30.0),
    concurrencyLimit = 1000
  )

  val javaFunc = FunctionDeploySpec(
    name = "java-func",
    runtime = "Java",
    memoryMB = MegaBytes(512.0),
    timeout = SimTime(10.0),
    concurrencyLimit = 2
  )

  def createPlatform(
      coldStartModel: ColdStartModel.ColdStartModel = ColdStartModel.byRuntime,
      billingModel: BillingModel.BillingModel = BillingModel.awsLambda,
      containerTtl: SimTime = SimTime(600.0),
      executionMips: MIPS = MIPS(1000.0)
  ): (
      org.apache.pekko.actor.typed.ActorRef[EntityCommand],
      org.apache.pekko.actor.testkit.typed.scaladsl.TestProbe[TimeCoordinator.Command]
  ) =
    val coordinatorProbe = testKit.createTestProbe[TimeCoordinator.Command]()
    val platform = testKit.spawn(
      FaasPlatformActor(
        FaasPlatformActor.Config(
          platformName = "test-platform",
          coldStartModel = coldStartModel,
          billingModel = billingModel,
          scalingPolicy = ScalingPolicy.reactive,
          containerTtl = containerTtl,
          coordinator = coordinatorProbe.ref,
          executionMips = executionMips
        )
      )
    )
    // Consume the RegisterEntity message
    coordinatorProbe.receiveMessage()
    (platform, coordinatorProbe)

  val platformRef = EntityRef("test-platform", EntityType.FaasPlatform)
  val brokerRef   = EntityRef("test-broker", EntityType.ServerlessBroker)

  "FaasPlatformActor" should "deploy a function" in {
    val (platform, probe) = createPlatform()
    val replyProbe        = testKit.createTestProbe[TimeCoordinator.Command]()

    val deployEvent = SimEvent(
      time = SimTime.Zero,
      source = platformRef,
      destination = platformRef,
      payload = FunctionDeploy(FunctionId(0L), pythonFunc),
      serial = SerialNumber.Zero
    )

    platform ! ProcessEvents(Vector(deployEvent), replyProbe.ref)
    replyProbe.expectMessageType[StepComplete]
  }

  it should "produce cold start on first invocation" in {
    val (platform, coordProbe) = createPlatform(executionMips = MIPS(1000.0))
    val replyProbe             = testKit.createTestProbe[TimeCoordinator.Command]()

    // Deploy function
    val deployEvent = SimEvent(
      time = SimTime.Zero,
      source = platformRef,
      destination = platformRef,
      payload = FunctionDeploy(FunctionId(0L), pythonFunc),
      serial = SerialNumber.Zero
    )
    platform ! ProcessEvents(Vector(deployEvent), replyProbe.ref)
    replyProbe.expectMessageType[StepComplete]

    // Invoke function
    val invokeEvent = SimEvent(
      time = SimTime(1.0),
      source = brokerRef,
      destination = platformRef,
      payload = FunctionInvoke(InvocationId(0L), FunctionId(0L), MI(5000.0), MegaBytes(1.0)),
      serial = SerialNumber.Zero
    )
    platform ! ProcessEvents(Vector(invokeEvent), replyProbe.ref)
    replyProbe.expectMessageType[StepComplete]

    // Should get ScheduleEvents with InvocationStarted + InvocationComplete (self) + InvocationComplete (broker)
    val scheduled = coordProbe.receiveMessage().asInstanceOf[TimeCoordinator.ScheduleEvents]
    scheduled.events.size shouldBe 3

    // Check InvocationStarted
    val started = scheduled.events.collectFirst {
      case e if e.payload.isInstanceOf[InvocationStarted] =>
        e.payload.asInstanceOf[InvocationStarted]
    }
    started shouldBe defined
    started.get.coldStart shouldBe true
  }

  it should "reuse warm container on second invocation" in {
    val (platform, coordProbe) = createPlatform(executionMips = MIPS(1000.0))
    val replyProbe             = testKit.createTestProbe[TimeCoordinator.Command]()

    // Deploy
    platform ! ProcessEvents(
      Vector(
        SimEvent(
          time = SimTime.Zero,
          source = platformRef,
          destination = platformRef,
          payload = FunctionDeploy(FunctionId(0L), pythonFunc),
          serial = SerialNumber.Zero
        )
      ),
      replyProbe.ref
    )
    replyProbe.expectMessageType[StepComplete]

    // First invocation (cold start)
    platform ! ProcessEvents(
      Vector(
        SimEvent(
          time = SimTime(1.0),
          source = brokerRef,
          destination = platformRef,
          payload = FunctionInvoke(InvocationId(0L), FunctionId(0L), MI(1000.0), MegaBytes(1.0)),
          serial = SerialNumber.Zero
        )
      ),
      replyProbe.ref
    )
    replyProbe.expectMessageType[StepComplete]
    coordProbe.receiveMessage() // consume first ScheduleEvents

    // Simulate InvocationComplete (self-delivered) to release container
    // Execution: MI(1000)/MIPS(1000) = 1.0s, cold start: 0.25s
    // Start: 1.0 + 0.25 = 1.25, Finish: 1.25 + 1.0 = 2.25
    platform ! ProcessEvents(
      Vector(
        SimEvent(
          time = SimTime(2.25),
          source = platformRef,
          destination = platformRef,
          payload =
            InvocationComplete(InvocationId(0L), FunctionId(0L), SimTime(1.25), SimTime(2.25), GBSeconds(0.25), true),
          serial = SerialNumber.Zero
        )
      ),
      replyProbe.ref
    )
    replyProbe.expectMessageType[StepComplete]

    // Second invocation (should be warm)
    platform ! ProcessEvents(
      Vector(
        SimEvent(
          time = SimTime(3.0),
          source = brokerRef,
          destination = platformRef,
          payload = FunctionInvoke(InvocationId(1L), FunctionId(0L), MI(1000.0), MegaBytes(1.0)),
          serial = SerialNumber.Zero
        )
      ),
      replyProbe.ref
    )
    replyProbe.expectMessageType[StepComplete]

    val scheduled = coordProbe.receiveMessage().asInstanceOf[TimeCoordinator.ScheduleEvents]
    val started = scheduled.events.collectFirst {
      case e if e.payload.isInstanceOf[InvocationStarted] =>
        e.payload.asInstanceOf[InvocationStarted]
    }
    started shouldBe defined
    started.get.coldStart shouldBe false // warm!
  }

  it should "throttle when concurrency limit is reached" in {
    val (platform, coordProbe) = createPlatform(executionMips = MIPS(1000.0))
    val replyProbe             = testKit.createTestProbe[TimeCoordinator.Command]()

    // Deploy function with concurrency limit of 2
    platform ! ProcessEvents(
      Vector(
        SimEvent(
          time = SimTime.Zero,
          source = platformRef,
          destination = platformRef,
          payload = FunctionDeploy(FunctionId(0L), javaFunc),
          serial = SerialNumber.Zero
        )
      ),
      replyProbe.ref
    )
    replyProbe.expectMessageType[StepComplete]

    // Send 3 invocations at the same time — third should be throttled
    val invocations = (0 until 3).map { i =>
      SimEvent(
        time = SimTime(1.0),
        source = brokerRef,
        destination = platformRef,
        payload = FunctionInvoke(InvocationId(i.toLong), FunctionId(0L), MI(5000.0), MegaBytes(1.0)),
        serial = SerialNumber(i.toLong)
      )
    }.toVector

    platform ! ProcessEvents(invocations, replyProbe.ref)
    replyProbe.expectMessageType[StepComplete]

    val scheduled = coordProbe.receiveMessage().asInstanceOf[TimeCoordinator.ScheduleEvents]

    // Count throttled events
    val throttled = scheduled.events.collect {
      case e if e.payload.isInstanceOf[InvocationThrottled] => e
    }
    throttled.size shouldBe 1

    // Count started events (should be 2)
    val started = scheduled.events.collect {
      case e if e.payload.isInstanceOf[InvocationStarted] => e
    }
    started.size shouldBe 2
  }

  it should "detect timeout" in {
    val (platform, coordProbe) = createPlatform(
      executionMips = MIPS(1000.0),
      coldStartModel = ColdStartModel.fixed(SimTime.Zero)
    )
    val replyProbe = testKit.createTestProbe[TimeCoordinator.Command]()

    // Deploy function with 10s timeout
    platform ! ProcessEvents(
      Vector(
        SimEvent(
          time = SimTime.Zero,
          source = platformRef,
          destination = platformRef,
          payload = FunctionDeploy(FunctionId(0L), javaFunc), // timeout = 10s
          serial = SerialNumber.Zero
        )
      ),
      replyProbe.ref
    )
    replyProbe.expectMessageType[StepComplete]

    // Invoke with MI(20000) at MIPS(1000) = 20s execution — exceeds 10s timeout
    platform ! ProcessEvents(
      Vector(
        SimEvent(
          time = SimTime(1.0),
          source = brokerRef,
          destination = platformRef,
          payload = FunctionInvoke(InvocationId(0L), FunctionId(0L), MI(20000.0), MegaBytes(1.0)),
          serial = SerialNumber.Zero
        )
      ),
      replyProbe.ref
    )
    replyProbe.expectMessageType[StepComplete]

    val scheduled = coordProbe.receiveMessage().asInstanceOf[TimeCoordinator.ScheduleEvents]
    val timedOut = scheduled.events.collect {
      case e if e.payload.isInstanceOf[InvocationTimedOut] => e.payload.asInstanceOf[InvocationTimedOut]
    }
    timedOut.size shouldBe 1
    timedOut.head.invocationId shouldBe InvocationId(0L)
  }

  it should "compute billing correctly" in {
    val (platform, coordProbe) = createPlatform(
      executionMips = MIPS(1000.0),
      coldStartModel = ColdStartModel.fixed(SimTime.Zero)
    )
    val replyProbe = testKit.createTestProbe[TimeCoordinator.Command]()

    // Deploy: 1024 MB memory, 30s timeout
    val spec = FunctionDeploySpec("billing-test", "Python", MegaBytes(1024.0), SimTime(30.0), 1000)
    platform ! ProcessEvents(
      Vector(
        SimEvent(
          time = SimTime.Zero,
          source = platformRef,
          destination = platformRef,
          payload = FunctionDeploy(FunctionId(0L), spec),
          serial = SerialNumber.Zero
        )
      ),
      replyProbe.ref
    )
    replyProbe.expectMessageType[StepComplete]

    // Invoke: MI(2000) at MIPS(1000) = 2.0s execution
    // 1024 MB = 1 GB, 2.0s → 2.0 GB-seconds
    platform ! ProcessEvents(
      Vector(
        SimEvent(
          time = SimTime(1.0),
          source = brokerRef,
          destination = platformRef,
          payload = FunctionInvoke(InvocationId(0L), FunctionId(0L), MI(2000.0), MegaBytes(1.0)),
          serial = SerialNumber.Zero
        )
      ),
      replyProbe.ref
    )
    replyProbe.expectMessageType[StepComplete]

    val scheduled = coordProbe.receiveMessage().asInstanceOf[TimeCoordinator.ScheduleEvents]
    val completeEvents = scheduled.events.collect {
      case e if e.payload.isInstanceOf[InvocationComplete] => e.payload.asInstanceOf[InvocationComplete]
    }
    completeEvents should not be empty
    // Both self and broker copies should have same billing
    completeEvents.head.billedGBSeconds.value shouldBe 2.0
  }

  override def afterAll(): Unit =
    try testKit.shutdownTestKit()
    catch case _: Exception => ()
