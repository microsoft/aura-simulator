// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.serverless.actors

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.core.events.*
import io.aura.core.events.SimEventPayload.*
import io.aura.serverless.ArrivalPattern

class ServerlessBrokerActorSpec extends AnyFlatSpec with Matchers:

  private val platformRef = EntityRef("faas-platform", EntityType.FaasPlatform)

  private def config(
      batches: Vector[ServerlessBrokerActor.InvocationBatch],
      startTime: SimTime = SimTime.Zero
  ): ServerlessBrokerActor.Config =
    ServerlessBrokerActor.Config(
      brokerId = "test-broker",
      platformRef = platformRef,
      batches = batches,
      coordinator = null, // not used by generateEvents
      startTime = startTime
    )

  // ─── generateEvents ──────────────────────────────────────────────────

  "generateEvents" should "return empty for empty batches" in {
    val (events, ids) = ServerlessBrokerActor.generateEvents(config(Vector.empty), 0L)
    events shouldBe empty
    ids shouldBe empty
  }

  it should "generate correct number of events for a single batch" in {
    val batch = ServerlessBrokerActor.InvocationBatch(
      functionId = FunctionId(1),
      count = 5,
      executionLength = MI(100.0),
      inputSize = MegaBytes(1.0),
      arrivalPattern = ArrivalPattern.uniform(SimTime(10.0))
    )
    val (events, ids) = ServerlessBrokerActor.generateEvents(config(Vector(batch)), 0L)
    events should have size 5
    ids should have size 5
  }

  it should "generate FunctionInvoke payloads with correct function ID" in {
    val batch = ServerlessBrokerActor.InvocationBatch(
      functionId = FunctionId(42),
      count = 3,
      executionLength = MI(200.0),
      inputSize = MegaBytes(2.0),
      arrivalPattern = ArrivalPattern.uniform(SimTime(1.0))
    )
    val (events, _) = ServerlessBrokerActor.generateEvents(config(Vector(batch)), 0L)
    events.foreach { e =>
      e.payload match
        case FunctionInvoke(_, funcId, execLen, inputSz) =>
          funcId shouldBe FunctionId(42)
          execLen shouldBe MI(200.0)
          inputSz shouldBe MegaBytes(2.0)
        case _ => fail(s"Expected FunctionInvoke, got ${e.payload}")
    }
  }

  it should "assign sequential invocation IDs starting from nextInvocationId" in {
    val batch = ServerlessBrokerActor.InvocationBatch(
      functionId = FunctionId(1),
      count = 3,
      executionLength = MI(100.0),
      inputSize = MegaBytes(1.0),
      arrivalPattern = ArrivalPattern.uniform(SimTime(10.0))
    )
    val (events, ids) = ServerlessBrokerActor.generateEvents(config(Vector(batch)), 100L)
    ids shouldBe Set(InvocationId(100), InvocationId(101), InvocationId(102))
    events.map(_.payload).collect { case FunctionInvoke(id, _, _, _) => id } shouldBe
      Vector(InvocationId(100), InvocationId(101), InvocationId(102))
  }

  it should "use uniform arrival times" in {
    val batch = ServerlessBrokerActor.InvocationBatch(
      functionId = FunctionId(1),
      count = 3,
      executionLength = MI(100.0),
      inputSize = MegaBytes(1.0),
      arrivalPattern = ArrivalPattern.uniform(SimTime(10.0))
    )
    val (events, _) = ServerlessBrokerActor.generateEvents(config(Vector(batch)), 0L)
    events.map(_.time) shouldBe Vector(SimTime(0.0), SimTime(10.0), SimTime(20.0))
  }

  it should "set destination to platform ref" in {
    val batch = ServerlessBrokerActor.InvocationBatch(
      functionId = FunctionId(1),
      count = 1,
      executionLength = MI(100.0),
      inputSize = MegaBytes(1.0),
      arrivalPattern = ArrivalPattern.uniform(SimTime(10.0))
    )
    val (events, _) = ServerlessBrokerActor.generateEvents(config(Vector(batch)), 0L)
    events.head.destination shouldBe platformRef
  }

  it should "set source to broker entity ref" in {
    val batch = ServerlessBrokerActor.InvocationBatch(
      functionId = FunctionId(1),
      count = 1,
      executionLength = MI(100.0),
      inputSize = MegaBytes(1.0),
      arrivalPattern = ArrivalPattern.uniform(SimTime(10.0))
    )
    val (events, _) = ServerlessBrokerActor.generateEvents(config(Vector(batch)), 0L)
    events.head.source.name shouldBe "test-broker"
    events.head.source.entityType shouldBe EntityType.ServerlessBroker
  }

  it should "handle multiple batches with continuous ID assignment" in {
    val batch1 = ServerlessBrokerActor.InvocationBatch(
      functionId = FunctionId(1),
      count = 3,
      executionLength = MI(100.0),
      inputSize = MegaBytes(1.0),
      arrivalPattern = ArrivalPattern.uniform(SimTime(10.0))
    )
    val batch2 = ServerlessBrokerActor.InvocationBatch(
      functionId = FunctionId(2),
      count = 2,
      executionLength = MI(200.0),
      inputSize = MegaBytes(2.0),
      arrivalPattern = ArrivalPattern.uniform(SimTime(5.0))
    )
    val (events, ids) = ServerlessBrokerActor.generateEvents(config(Vector(batch1, batch2)), 0L)
    events should have size 5
    ids should have size 5
    ids shouldBe Set(InvocationId(0), InvocationId(1), InvocationId(2), InvocationId(3), InvocationId(4))
  }

  it should "respect startTime offset" in {
    val batch = ServerlessBrokerActor.InvocationBatch(
      functionId = FunctionId(1),
      count = 2,
      executionLength = MI(100.0),
      inputSize = MegaBytes(1.0),
      arrivalPattern = ArrivalPattern.uniform(SimTime(10.0))
    )
    val (events, _) = ServerlessBrokerActor.generateEvents(config(Vector(batch), startTime = SimTime(100.0)), 0L)
    events.map(_.time) shouldBe Vector(SimTime(100.0), SimTime(110.0))
  }

  it should "be deterministic (same inputs produce same outputs)" in {
    val batch = ServerlessBrokerActor.InvocationBatch(
      functionId = FunctionId(1),
      count = 10,
      executionLength = MI(100.0),
      inputSize = MegaBytes(1.0),
      arrivalPattern = ArrivalPattern.poisson(0.1)
    )
    val (events1, ids1) = ServerlessBrokerActor.generateEvents(config(Vector(batch)), 0L)
    val (events2, ids2) = ServerlessBrokerActor.generateEvents(config(Vector(batch)), 0L)
    events1.map(_.time) shouldBe events2.map(_.time)
    ids1 shouldBe ids2
  }
