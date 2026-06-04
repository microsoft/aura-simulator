// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.engine

import org.apache.pekko.actor.testkit.typed.scaladsl.ActorTestKit
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.core.events.*

class TimeCoordinatorSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll:

  val testKit = ActorTestKit()

  override def afterAll(): Unit =
    try testKit.shutdownTestKit()
    catch case _: Exception => ()

  "TimeCoordinator" should "register entities" in {
    val coordinator = testKit.spawn(TimeCoordinator(SimTime(100.0)))
    val entityProbe = testKit.createTestProbe[TimeCoordinator.EntityCommand]()
    val entityRef   = EntityRef("test-entity", EntityType.Host)

    coordinator ! TimeCoordinator.RegisterEntity(entityRef, entityProbe.ref)

    // Should not crash — registration is fire-and-forget
    val statusProbe = testKit.createTestProbe[TimeCoordinator.SimulationStatus]()
    coordinator ! TimeCoordinator.GetStatus(statusProbe.ref)
    val status = statusProbe.receiveMessage()
    status shouldBe a[TimeCoordinator.Running]
  }

  it should "complete immediately with no events" in {
    val coordinator = testKit.spawn(TimeCoordinator(SimTime(100.0)), "empty-coord")
    val statusProbe = testKit.createTestProbe[TimeCoordinator.SimulationStatus]()

    coordinator ! TimeCoordinator.StartSimulation(statusProbe.ref)

    val result = statusProbe.receiveMessage()
    result shouldBe a[TimeCoordinator.Completed]
    result.asInstanceOf[TimeCoordinator.Completed].totalEventsProcessed shouldBe 0
  }

  it should "dispatch events to registered entities" in {
    val coordinator = testKit.spawn(TimeCoordinator(SimTime(100.0)), "dispatch-coord")
    val entityProbe = testKit.createTestProbe[TimeCoordinator.EntityCommand]()
    val statusProbe = testKit.createTestProbe[TimeCoordinator.SimulationStatus]()
    val entityRef   = EntityRef("test-host", EntityType.Host)

    coordinator ! TimeCoordinator.RegisterEntity(entityRef, entityProbe.ref)

    // Schedule an event to the entity
    val event = SimEvent(
      time = SimTime(10.0),
      source = EntityRef("source", EntityType.Broker),
      destination = entityRef,
      payload = SimEventPayload.SimulationStart,
      serial = SerialNumber.Zero
    )
    coordinator ! TimeCoordinator.ScheduleEvent(event)

    // Start simulation
    coordinator ! TimeCoordinator.StartSimulation(statusProbe.ref)

    // Entity should receive the event
    val processEvents = entityProbe.receiveMessage()
    processEvents shouldBe a[TimeCoordinator.ProcessEvents]

    val pe = processEvents.asInstanceOf[TimeCoordinator.ProcessEvents]
    pe.events should have size 1
    pe.events.head.payload shouldBe SimEventPayload.SimulationStart

    // Signal completion
    pe.replyTo ! TimeCoordinator.StepComplete(entityRef)

    // Acknowledge the FinalSnapshot handshake the coordinator broadcasts at
    // quiescence before producing Completed.
    val fs = entityProbe.receiveMessage().asInstanceOf[TimeCoordinator.FinalSnapshot]
    fs.replyTo ! TimeCoordinator.FinalSnapshotComplete(entityRef)

    // Simulation should complete
    val result = statusProbe.receiveMessage()
    result shouldBe a[TimeCoordinator.Completed]
  }

  it should "maintain event ordering by time and serial within same destination" in {
    val coordinator = testKit.spawn(TimeCoordinator(SimTime(100.0)), "ordering-coord")
    val entityProbe = testKit.createTestProbe[TimeCoordinator.EntityCommand]()
    val statusProbe = testKit.createTestProbe[TimeCoordinator.SimulationStatus]()
    val entityRef   = EntityRef("test-host", EntityType.Host)

    coordinator ! TimeCoordinator.RegisterEntity(entityRef, entityProbe.ref)

    // Schedule multiple events at the same time
    val source = EntityRef("source", EntityType.Broker)
    val events = Vector(
      SimEvent(SimTime(5.0), source, entityRef, SimEventPayload.WorkloadUpdate(SimTime(5.0)), SerialNumber.Zero),
      SimEvent(SimTime(5.0), source, entityRef, SimEventPayload.SimulationStart, SerialNumber.Zero),
      SimEvent(SimTime(5.0), source, entityRef, SimEventPayload.SimulationEnd, SerialNumber.Zero)
    )
    coordinator ! TimeCoordinator.ScheduleEvents(events)
    coordinator ! TimeCoordinator.StartSimulation(statusProbe.ref)

    // All three events at time 5.0 should arrive in one batch, ordered by serial
    val pe = entityProbe.receiveMessage().asInstanceOf[TimeCoordinator.ProcessEvents]
    pe.events should have size 3
    // Serials should be monotonically increasing
    val serials = pe.events.map(_.serial.value)
    serials shouldBe sorted

    pe.replyTo ! TimeCoordinator.StepComplete(entityRef)
    val fs = entityProbe.receiveMessage().asInstanceOf[TimeCoordinator.FinalSnapshot]
    fs.replyTo ! TimeCoordinator.FinalSnapshotComplete(entityRef)
    statusProbe.receiveMessage() shouldBe a[TimeCoordinator.Completed]
  }

  it should "barrier-sync before advancing to next time step" in {
    val coordinator  = testKit.spawn(TimeCoordinator(SimTime(100.0)), "barrier-coord")
    val entity1Probe = testKit.createTestProbe[TimeCoordinator.EntityCommand]()
    val entity2Probe = testKit.createTestProbe[TimeCoordinator.EntityCommand]()
    val statusProbe  = testKit.createTestProbe[TimeCoordinator.SimulationStatus]()

    val ref1   = EntityRef("host-1", EntityType.Host)
    val ref2   = EntityRef("host-2", EntityType.Host)
    val source = EntityRef("source", EntityType.Broker)

    coordinator ! TimeCoordinator.RegisterEntity(ref1, entity1Probe.ref)
    coordinator ! TimeCoordinator.RegisterEntity(ref2, entity2Probe.ref)

    // Events at time 10 for both entities
    coordinator ! TimeCoordinator.ScheduleEvent(
      SimEvent(SimTime(10.0), source, ref1, SimEventPayload.SimulationStart, SerialNumber.Zero)
    )
    coordinator ! TimeCoordinator.ScheduleEvent(
      SimEvent(SimTime(10.0), source, ref2, SimEventPayload.SimulationStart, SerialNumber.Zero)
    )
    // Event at time 20 for entity1 only
    coordinator ! TimeCoordinator.ScheduleEvent(
      SimEvent(SimTime(20.0), source, ref1, SimEventPayload.SimulationEnd, SerialNumber.Zero)
    )

    coordinator ! TimeCoordinator.StartSimulation(statusProbe.ref)

    // Both entities get events at time 10
    val pe1 = entity1Probe.receiveMessage().asInstanceOf[TimeCoordinator.ProcessEvents]
    val pe2 = entity2Probe.receiveMessage().asInstanceOf[TimeCoordinator.ProcessEvents]

    // Complete entity1 first
    pe1.replyTo ! TimeCoordinator.StepComplete(ref1)

    // entity1 should NOT receive its time=20 event yet (barrier not complete)
    entity1Probe.expectNoMessage(scala.concurrent.duration.DurationInt(200).millis)

    // Now complete entity2
    pe2.replyTo ! TimeCoordinator.StepComplete(ref2)

    // NOW entity1 should receive the time=20 event
    val pe3 = entity1Probe.receiveMessage().asInstanceOf[TimeCoordinator.ProcessEvents]
    pe3.events.head.time.value shouldBe 20.0

    pe3.replyTo ! TimeCoordinator.StepComplete(ref1)

    // FinalSnapshot broadcast to both entities at quiescence
    val fs1 = entity1Probe.receiveMessage().asInstanceOf[TimeCoordinator.FinalSnapshot]
    val fs2 = entity2Probe.receiveMessage().asInstanceOf[TimeCoordinator.FinalSnapshot]
    fs1.replyTo ! TimeCoordinator.FinalSnapshotComplete(ref1)
    fs2.replyTo ! TimeCoordinator.FinalSnapshotComplete(ref2)

    statusProbe.receiveMessage() shouldBe a[TimeCoordinator.Completed]
  }

  it should "stop at endTime" in {
    val coordinator = testKit.spawn(TimeCoordinator(SimTime(50.0)), "endtime-coord")
    val entityProbe = testKit.createTestProbe[TimeCoordinator.EntityCommand]()
    val statusProbe = testKit.createTestProbe[TimeCoordinator.SimulationStatus]()
    val entityRef   = EntityRef("host", EntityType.Host)
    val source      = EntityRef("source", EntityType.Broker)

    coordinator ! TimeCoordinator.RegisterEntity(entityRef, entityProbe.ref)

    // Schedule event beyond endTime
    coordinator ! TimeCoordinator.ScheduleEvent(
      SimEvent(SimTime(10.0), source, entityRef, SimEventPayload.SimulationStart, SerialNumber.Zero)
    )
    coordinator ! TimeCoordinator.ScheduleEvent(
      SimEvent(SimTime(100.0), source, entityRef, SimEventPayload.SimulationEnd, SerialNumber.Zero)
    )

    coordinator ! TimeCoordinator.StartSimulation(statusProbe.ref)

    // Should get time=10 event
    val pe = entityProbe.receiveMessage().asInstanceOf[TimeCoordinator.ProcessEvents]
    pe.replyTo ! TimeCoordinator.StepComplete(entityRef)

    // FinalSnapshot ack
    val fs = entityProbe.receiveMessage().asInstanceOf[TimeCoordinator.FinalSnapshot]
    fs.replyTo ! TimeCoordinator.FinalSnapshotComplete(entityRef)

    // time=100 event should be skipped; simulation completes
    val result = statusProbe.receiveMessage()
    result shouldBe a[TimeCoordinator.Completed]
  }
