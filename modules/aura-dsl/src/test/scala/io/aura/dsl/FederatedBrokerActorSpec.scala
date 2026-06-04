// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.dsl

import org.apache.pekko.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.core.events.*
import io.aura.core.events.SimEventPayload.*
import io.aura.core.engine.TimeCoordinator
import io.aura.core.engine.TimeCoordinator.ProcessEvents

class FederatedBrokerActorSpec extends ScalaTestWithActorTestKit with AnyFlatSpecLike with Matchers:

  private val coordinatorProbe = createTestProbe[TimeCoordinator.Command]()

  private val edgeRef     = EntityRef("test-edge", EntityType.EdgeEnvironment)
  private val platformRef = EntityRef("test-lambda", EntityType.FaasPlatform)
  private val clusterRef  = EntityRef("test-k8s", EntityType.K8sCluster)

  private var edgeIdCounter = 5000L
  private var invIdCounter  = 6000L
  private var podIdCounter  = 7000L

  private def resetCounters(): Unit =
    edgeIdCounter = 5000L
    invIdCounter = 6000L
    podIdCounter = 7000L

  private def makeConfig(
      tierSelection: TierSelectionPolicy = TierSelectionPolicy.edgeFirst,
      escalation: EscalationPolicy = EscalationPolicy.cascade,
      tiers: Map[ExecutionTier, FederatedBrokerActor.TierMapping] = Map.empty
  ): FederatedBrokerActor.Config =
    val defaultTiers =
      if tiers.nonEmpty then tiers
      else
        Map(
          ExecutionTier.Edge -> FederatedBrokerActor.TierMapping(
            ExecutionTier.Edge,
            edgeRef,
            () =>
              val id = edgeIdCounter; edgeIdCounter += 1; id
          ),
          ExecutionTier.Serverless -> FederatedBrokerActor.TierMapping(
            ExecutionTier.Serverless,
            platformRef,
            () =>
              val id = invIdCounter; invIdCounter += 1; id
          ),
          ExecutionTier.K8s -> FederatedBrokerActor.TierMapping(
            ExecutionTier.K8s,
            clusterRef,
            () =>
              val id = podIdCounter; podIdCounter += 1; id
          )
        )

    FederatedBrokerActor.Config(
      brokerId = "fed-broker-test",
      tierSelection = tierSelection,
      escalation = escalation,
      tierMappings = defaultTiers,
      coordinator = coordinatorProbe.ref,
      cpuRequired = MIPS(100.0),
      memRequired = MegaBytes(64.0),
      taskLength = MI(500.0),
      deadline = SimTime(1.0),
      edgeSourceNode = "sensor-1",
      serverlessFunctionId = FunctionId(0L),
      k8sDeploymentId = DeploymentId(0L)
    )

  private val brokerEntityRef = EntityRef("fed-broker-test", EntityType.FederatedBroker)

  "FederatedBrokerActor" should "register with coordinator on startup" in {
    resetCounters()
    spawn(FederatedBrokerActor(makeConfig(), totalTasks = 1))
    val msg = coordinatorProbe.receiveMessage()
    msg shouldBe a[TimeCoordinator.RegisterEntity]
    val reg = msg.asInstanceOf[TimeCoordinator.RegisterEntity]
    reg.entityRef.name shouldBe "fed-broker-test"
    reg.entityRef.entityType shouldBe EntityType.FederatedBroker
  }

  it should "route task to Edge tier with edgeFirst policy" in {
    resetCounters()
    val actor = spawn(FederatedBrokerActor(makeConfig(), totalTasks = 1))
    coordinatorProbe.receiveMessage() // RegisterEntity

    val submitEvent = SimEvent(
      time = SimTime(1.0),
      source = brokerEntityRef,
      destination = brokerEntityRef,
      payload = FederatedTaskSubmit(FederatedTaskId(0L), MIPS(100.0), MegaBytes(64.0), MI(500.0), SimTime(1.0)),
      serial = SerialNumber.Zero
    )

    actor ! ProcessEvents(Vector(submitEvent), coordinatorProbe.ref)

    val scheduleMsg = coordinatorProbe.receiveMessage()
    scheduleMsg shouldBe a[TimeCoordinator.ScheduleEvents]
    val events = scheduleMsg.asInstanceOf[TimeCoordinator.ScheduleEvents].events

    // Should emit: EdgeTaskSubmit (to edge env) + FederatedTaskRouted (to self)
    events should have size 2
    val edgeSubmit = events.find(_.payload.isInstanceOf[EdgeTaskSubmit])
    edgeSubmit shouldBe defined
    edgeSubmit.get.destination shouldBe edgeRef

    val routed = events.find(_.payload.isInstanceOf[FederatedTaskRouted])
    routed shouldBe defined
    routed.get.payload.asInstanceOf[FederatedTaskRouted].tier shouldBe ExecutionTier.Edge

    coordinatorProbe.receiveMessage() // StepComplete
  }

  it should "complete task on EdgeTaskCompleted" in {
    resetCounters()
    val actor = spawn(FederatedBrokerActor(makeConfig(), totalTasks = 1))
    coordinatorProbe.receiveMessage() // RegisterEntity

    // Submit task
    val submitEvent = SimEvent(
      time = SimTime(1.0),
      source = brokerEntityRef,
      destination = brokerEntityRef,
      payload = FederatedTaskSubmit(FederatedTaskId(0L), MIPS(100.0), MegaBytes(64.0), MI(500.0), SimTime(1.0)),
      serial = SerialNumber.Zero
    )
    actor ! ProcessEvents(Vector(submitEvent), coordinatorProbe.ref)
    coordinatorProbe.receiveMessage() // ScheduleEvents
    coordinatorProbe.receiveMessage() // StepComplete

    // Simulate EdgeTaskCompleted from edge environment
    val edgeTaskId = EdgeTaskId(5000L)
    val completedEvent = SimEvent(
      time = SimTime(2.0),
      source = edgeRef,
      destination = brokerEntityRef,
      payload = EdgeTaskCompleted(
        edgeTaskId,
        "sensor-1",
        "gateway-1",
        true,
        SimTime(0.01),
        SimTime(1.0),
        SimTime(2.0),
        "latency"
      ),
      serial = SerialNumber.Zero
    )
    actor ! ProcessEvents(Vector(completedEvent), coordinatorProbe.ref)

    val scheduleMsg = coordinatorProbe.receiveMessage()
    scheduleMsg shouldBe a[TimeCoordinator.ScheduleEvents]
    val events = scheduleMsg.asInstanceOf[TimeCoordinator.ScheduleEvents].events

    // Should emit: FederatedTaskCompleted (no SimulationEnd — termination is
    // coordinator-driven via quiescence, not broker-emitted)
    events.count(_.payload.isInstanceOf[FederatedTaskCompleted]) shouldBe 1
    events.count(_.payload == SimulationEnd) shouldBe 0

    val completed = events.collectFirst {
      case e if e.payload.isInstanceOf[FederatedTaskCompleted] =>
        e.payload.asInstanceOf[FederatedTaskCompleted]
    }.get
    completed.taskId shouldBe FederatedTaskId(0L)
    completed.initialTier shouldBe ExecutionTier.Edge
    completed.finalTier shouldBe ExecutionTier.Edge
    completed.escalations shouldBe 0

    coordinatorProbe.receiveMessage() // StepComplete
  }

  it should "escalate on EdgeTaskFailed from Edge to Serverless" in {
    resetCounters()
    val actor = spawn(FederatedBrokerActor(makeConfig(), totalTasks = 1))
    coordinatorProbe.receiveMessage() // RegisterEntity

    // Submit task
    val submitEvent = SimEvent(
      time = SimTime(1.0),
      source = brokerEntityRef,
      destination = brokerEntityRef,
      payload = FederatedTaskSubmit(FederatedTaskId(0L), MIPS(100.0), MegaBytes(64.0), MI(500.0), SimTime(1.0)),
      serial = SerialNumber.Zero
    )
    actor ! ProcessEvents(Vector(submitEvent), coordinatorProbe.ref)
    coordinatorProbe.receiveMessage() // ScheduleEvents
    coordinatorProbe.receiveMessage() // StepComplete

    // Simulate EdgeTaskFailed
    val edgeTaskId = EdgeTaskId(5000L)
    val failedEvent = SimEvent(
      time = SimTime(1.5),
      source = edgeRef,
      destination = brokerEntityRef,
      payload = EdgeTaskFailed(edgeTaskId, "Node at capacity"),
      serial = SerialNumber.Zero
    )
    actor ! ProcessEvents(Vector(failedEvent), coordinatorProbe.ref)

    val scheduleMsg = coordinatorProbe.receiveMessage()
    scheduleMsg shouldBe a[TimeCoordinator.ScheduleEvents]
    val events = scheduleMsg.asInstanceOf[TimeCoordinator.ScheduleEvents].events

    // Should escalate: FunctionInvoke (to platform) + FederatedTaskRouted (to self)
    val invoke = events.find(_.payload.isInstanceOf[FunctionInvoke])
    invoke shouldBe defined
    invoke.get.destination shouldBe platformRef

    val routed = events.find(_.payload.isInstanceOf[FederatedTaskRouted])
    routed shouldBe defined
    val routedPayload = routed.get.payload.asInstanceOf[FederatedTaskRouted]
    routedPayload.tier shouldBe ExecutionTier.Serverless
    routedPayload.attempt shouldBe 2

    coordinatorProbe.receiveMessage() // StepComplete
  }

  it should "cascade from Edge to Serverless to K8s on repeated failures" in {
    resetCounters()
    val actor = spawn(FederatedBrokerActor(makeConfig(), totalTasks = 1))
    coordinatorProbe.receiveMessage() // RegisterEntity

    // Submit task
    val submitEvent = SimEvent(
      time = SimTime(1.0),
      source = brokerEntityRef,
      destination = brokerEntityRef,
      payload = FederatedTaskSubmit(FederatedTaskId(0L), MIPS(100.0), MegaBytes(64.0), MI(500.0), SimTime(1.0)),
      serial = SerialNumber.Zero
    )
    actor ! ProcessEvents(Vector(submitEvent), coordinatorProbe.ref)
    coordinatorProbe.receiveMessage() // ScheduleEvents
    coordinatorProbe.receiveMessage() // StepComplete

    // Fail on Edge
    val failEvent1 = SimEvent(
      time = SimTime(1.5),
      source = edgeRef,
      destination = brokerEntityRef,
      payload = EdgeTaskFailed(EdgeTaskId(5000L), "Edge failure"),
      serial = SerialNumber.Zero
    )
    actor ! ProcessEvents(Vector(failEvent1), coordinatorProbe.ref)
    val msg1 = coordinatorProbe.receiveMessage().asInstanceOf[TimeCoordinator.ScheduleEvents]
    msg1.events.exists(_.payload.isInstanceOf[FunctionInvoke]) shouldBe true
    coordinatorProbe.receiveMessage() // StepComplete

    // Fail on Serverless
    val failEvent2 = SimEvent(
      time = SimTime(2.0),
      source = platformRef,
      destination = brokerEntityRef,
      payload = InvocationThrottled(InvocationId(6000L), FunctionId(0L), "Concurrency limit"),
      serial = SerialNumber.Zero
    )
    actor ! ProcessEvents(Vector(failEvent2), coordinatorProbe.ref)
    val msg2 = coordinatorProbe.receiveMessage().asInstanceOf[TimeCoordinator.ScheduleEvents]
    // Should escalate to K8s
    msg2.events.exists(_.payload.isInstanceOf[PodScheduleRequest]) shouldBe true
    coordinatorProbe.receiveMessage() // StepComplete

    // Complete on K8s
    val completedEvent = SimEvent(
      time = SimTime(3.0),
      source = clusterRef,
      destination = brokerEntityRef,
      payload = PodCompleted(PodId(7000L), HostId(0L), SimTime(3.0)),
      serial = SerialNumber.Zero
    )
    actor ! ProcessEvents(Vector(completedEvent), coordinatorProbe.ref)
    val msg3 = coordinatorProbe.receiveMessage().asInstanceOf[TimeCoordinator.ScheduleEvents]

    val fedCompleted = msg3.events.collectFirst {
      case e if e.payload.isInstanceOf[FederatedTaskCompleted] =>
        e.payload.asInstanceOf[FederatedTaskCompleted]
    }
    fedCompleted shouldBe defined
    fedCompleted.get.initialTier shouldBe ExecutionTier.Edge
    fedCompleted.get.finalTier shouldBe ExecutionTier.K8s
    fedCompleted.get.escalations shouldBe 2
    msg3.events.exists(_.payload == SimulationEnd) shouldBe false

    coordinatorProbe.receiveMessage() // StepComplete
  }

  it should "not emit SimulationEnd when all tasks are resolved (quiescence-driven termination)" in {
    resetCounters()
    val actor = spawn(FederatedBrokerActor(makeConfig(), totalTasks = 2))
    coordinatorProbe.receiveMessage() // RegisterEntity

    // Submit 2 tasks
    val events = (0 to 1).map { i =>
      SimEvent(
        time = SimTime(1.0),
        source = brokerEntityRef,
        destination = brokerEntityRef,
        payload = FederatedTaskSubmit(FederatedTaskId(i.toLong), MIPS(100.0), MegaBytes(64.0), MI(500.0), SimTime(1.0)),
        serial = SerialNumber(i.toLong)
      )
    }.toVector

    actor ! ProcessEvents(events, coordinatorProbe.ref)
    coordinatorProbe.receiveMessage() // ScheduleEvents
    coordinatorProbe.receiveMessage() // StepComplete

    // Complete first task
    val comp1 = SimEvent(
      time = SimTime(2.0),
      source = edgeRef,
      destination = brokerEntityRef,
      payload = EdgeTaskCompleted(
        EdgeTaskId(5000L),
        "sensor-1",
        "gateway-1",
        true,
        SimTime(0.01),
        SimTime(1.0),
        SimTime(2.0),
        "latency"
      ),
      serial = SerialNumber.Zero
    )
    actor ! ProcessEvents(Vector(comp1), coordinatorProbe.ref)
    val msg1 = coordinatorProbe.receiveMessage().asInstanceOf[TimeCoordinator.ScheduleEvents]
    msg1.events.exists(_.payload.isInstanceOf[FederatedTaskCompleted]) shouldBe true
    msg1.events.exists(_.payload == SimulationEnd) shouldBe false
    coordinatorProbe.receiveMessage() // StepComplete

    // Complete second task — federated broker emits the completion metric but
    // does NOT emit SimulationEnd; the coordinator drives termination via
    // quiescence + FinalSnapshot handshake.
    val comp2 = SimEvent(
      time = SimTime(2.5),
      source = edgeRef,
      destination = brokerEntityRef,
      payload = EdgeTaskCompleted(
        EdgeTaskId(5001L),
        "sensor-1",
        "gateway-1",
        true,
        SimTime(0.01),
        SimTime(1.0),
        SimTime(2.5),
        "latency"
      ),
      serial = SerialNumber.Zero
    )
    actor ! ProcessEvents(Vector(comp2), coordinatorProbe.ref)
    val msg2 = coordinatorProbe.receiveMessage().asInstanceOf[TimeCoordinator.ScheduleEvents]
    msg2.events.exists(_.payload.isInstanceOf[FederatedTaskCompleted]) shouldBe true
    msg2.events.exists(_.payload == SimulationEnd) shouldBe false
    coordinatorProbe.receiveMessage() // StepComplete
  }

  it should "record final failure when escalation policy is none" in {
    resetCounters()
    val config = makeConfig(escalation = EscalationPolicy.none)
    val actor  = spawn(FederatedBrokerActor(config, totalTasks = 1))
    coordinatorProbe.receiveMessage() // RegisterEntity

    // Submit
    val submitEvent = SimEvent(
      time = SimTime(1.0),
      source = brokerEntityRef,
      destination = brokerEntityRef,
      payload = FederatedTaskSubmit(FederatedTaskId(0L), MIPS(100.0), MegaBytes(64.0), MI(500.0), SimTime(1.0)),
      serial = SerialNumber.Zero
    )
    actor ! ProcessEvents(Vector(submitEvent), coordinatorProbe.ref)
    coordinatorProbe.receiveMessage() // ScheduleEvents
    coordinatorProbe.receiveMessage() // StepComplete

    // Fail on Edge
    val failEvent = SimEvent(
      time = SimTime(1.5),
      source = edgeRef,
      destination = brokerEntityRef,
      payload = EdgeTaskFailed(EdgeTaskId(5000L), "Capacity exceeded"),
      serial = SerialNumber.Zero
    )
    actor ! ProcessEvents(Vector(failEvent), coordinatorProbe.ref)
    val msg = coordinatorProbe.receiveMessage().asInstanceOf[TimeCoordinator.ScheduleEvents]

    // Should NOT escalate — final failure
    val fedFailed = msg.events.collectFirst {
      case e if e.payload.isInstanceOf[FederatedTaskFailed] =>
        e.payload.asInstanceOf[FederatedTaskFailed]
    }
    fedFailed shouldBe defined
    fedFailed.get.reason shouldBe "Capacity exceeded"
    msg.events.exists(_.payload == SimulationEnd) shouldBe false

    coordinatorProbe.receiveMessage() // StepComplete
  }

  it should "generate events correctly" in {
    val brokerRef = EntityRef("test-broker", EntityType.FederatedBroker)
    val (events, count) = FederatedBrokerActor.generateEvents(
      brokerRef,
      count = 5,
      startTime = SimTime(1.0),
      interArrivalTime = SimTime(2.0),
      nextFederatedTaskId = 100L,
      cpuRequired = MIPS(100.0),
      memRequired = MegaBytes(64.0),
      taskLength = MI(500.0),
      deadline = SimTime(1.0)
    )

    count shouldBe 5
    events should have size 5
    events.head.time shouldBe SimTime(1.0)
    events(1).time shouldBe SimTime(3.0)
    events(2).time shouldBe SimTime(5.0)
    events.head.payload.asInstanceOf[FederatedTaskSubmit].taskId shouldBe FederatedTaskId(100L)
    events.last.payload.asInstanceOf[FederatedTaskSubmit].taskId shouldBe FederatedTaskId(104L)
    events.foreach { e =>
      e.source shouldBe brokerRef
      e.destination shouldBe brokerRef
    }
  }
