// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.edge.actors

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.core.events.*
import io.aura.core.events.SimEventPayload.*
import io.aura.edge.actors.EdgeBrokerActor.TaskBatch

class EdgeBrokerActorSpec extends AnyFlatSpec with Matchers:

  private val brokerRef = EntityRef("edge-broker", EntityType.EdgeEnvironment)
  private val envRef    = EntityRef("edge-env", EntityType.EdgeEnvironment)

  // ─── generateEvents ──────────────────────────────────────────────────

  "generateEvents" should "return empty for empty batches" in {
    val (events, ids) = EdgeBrokerActor.generateEvents(brokerRef, envRef, Vector.empty, SimTime.Zero, 0L)
    events shouldBe empty
    ids shouldBe empty
  }

  it should "generate correct number of events for a single batch" in {
    val batch = TaskBatch(
      sourceNodeName = "edge-node-1",
      count = 5,
      cpuRequired = MIPS(100.0),
      memRequired = MegaBytes(64.0),
      taskLength = MI(500.0),
      deadline = SimTime(10.0),
      interArrivalTime = SimTime(1.0)
    )
    val (events, ids) = EdgeBrokerActor.generateEvents(brokerRef, envRef, Vector(batch), SimTime.Zero, 0L)
    events should have size 5
    ids should have size 5
  }

  it should "create EdgeTaskSubmit payloads with correct fields" in {
    val batch = TaskBatch(
      sourceNodeName = "sensor-node",
      count = 2,
      cpuRequired = MIPS(200.0),
      memRequired = MegaBytes(128.0),
      taskLength = MI(1000.0),
      deadline = SimTime(20.0),
      interArrivalTime = SimTime(5.0)
    )
    val (events, _) = EdgeBrokerActor.generateEvents(brokerRef, envRef, Vector(batch), SimTime.Zero, 0L)
    events.foreach { e =>
      e.payload match
        case EdgeTaskSubmit(_, srcNode, cpu, mem, length, deadline) =>
          srcNode shouldBe "sensor-node"
          cpu shouldBe MIPS(200.0)
          mem shouldBe MegaBytes(128.0)
          length shouldBe MI(1000.0)
          deadline shouldBe SimTime(20.0)
        case _ => fail(s"Expected EdgeTaskSubmit, got ${e.payload}")
    }
  }

  it should "assign sequential task IDs starting from nextTaskId" in {
    val batch         = TaskBatch("node-1", 3, MIPS(100.0), MegaBytes(64.0), MI(500.0), SimTime(10.0), SimTime(1.0))
    val (events, ids) = EdgeBrokerActor.generateEvents(brokerRef, envRef, Vector(batch), SimTime.Zero, 50L)
    ids shouldBe Set(EdgeTaskId(50), EdgeTaskId(51), EdgeTaskId(52))
  }

  it should "space arrival times by interArrivalTime" in {
    val batch       = TaskBatch("node-1", 4, MIPS(100.0), MegaBytes(64.0), MI(500.0), SimTime(10.0), SimTime(2.0))
    val (events, _) = EdgeBrokerActor.generateEvents(brokerRef, envRef, Vector(batch), SimTime(10.0), 0L)
    events.map(_.time) shouldBe Vector(SimTime(10.0), SimTime(12.0), SimTime(14.0), SimTime(16.0))
  }

  it should "set source and destination correctly" in {
    val batch       = TaskBatch("node-1", 1, MIPS(100.0), MegaBytes(64.0), MI(500.0), SimTime(10.0), SimTime(1.0))
    val (events, _) = EdgeBrokerActor.generateEvents(brokerRef, envRef, Vector(batch), SimTime.Zero, 0L)
    events.head.source shouldBe brokerRef
    events.head.destination shouldBe envRef
  }

  it should "handle multiple batches with continuous ID assignment" in {
    val batch1        = TaskBatch("node-1", 3, MIPS(100.0), MegaBytes(64.0), MI(500.0), SimTime(10.0), SimTime(1.0))
    val batch2        = TaskBatch("node-2", 2, MIPS(200.0), MegaBytes(128.0), MI(1000.0), SimTime(20.0), SimTime(2.0))
    val (events, ids) = EdgeBrokerActor.generateEvents(brokerRef, envRef, Vector(batch1, batch2), SimTime.Zero, 0L)
    events should have size 5
    ids should have size 5
    ids shouldBe Set(EdgeTaskId(0), EdgeTaskId(1), EdgeTaskId(2), EdgeTaskId(3), EdgeTaskId(4))
  }

  it should "handle single-task batch" in {
    val batch         = TaskBatch("node-1", 1, MIPS(100.0), MegaBytes(64.0), MI(500.0), SimTime(10.0), SimTime(1.0))
    val (events, ids) = EdgeBrokerActor.generateEvents(brokerRef, envRef, Vector(batch), SimTime.Zero, 0L)
    events should have size 1
    ids should have size 1
    ids shouldBe Set(EdgeTaskId(0))
  }
