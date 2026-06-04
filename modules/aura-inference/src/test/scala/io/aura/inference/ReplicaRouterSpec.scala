// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.inference

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.core.events.*
import io.aura.inference.scheduling.ReplicaRouter

class ReplicaRouterSpec extends AnyFlatSpec with Matchers:

  def makeReplica(name: String, batchSize: Int, maxBatch: Int = 64, powerW: Double = 200.0): ReplicaRouter.ReplicaInfo =
    ReplicaRouter.ReplicaInfo(
      EntityRef(name, EntityType.InferenceEngine),
      batchSize,
      maxBatch,
      kvCacheUtilization = 0.5,
      Watts(powerW)
    )

  val dummySpec = InferenceRequestSpec(InferenceRequestId(1), ModelId(0), 512, 128, SimTime(0.5), SimTime(0.05), 0)

  "leastLoaded" should "route to replica with lowest load" in {
    val replicas = Vector(
      makeReplica("r1", batchSize = 32),
      makeReplica("r2", batchSize = 8), // least loaded
      makeReplica("r3", batchSize = 50)
    )
    ReplicaRouter.leastLoaded(replicas, dummySpec) shouldBe Some(1)
  }

  it should "return None for empty replicas" in {
    ReplicaRouter.leastLoaded(Vector.empty, dummySpec) shouldBe None
  }

  "roundRobin" should "cycle through replicas" in {
    val replicas = Vector(makeReplica("r1", 0), makeReplica("r2", 0), makeReplica("r3", 0))
    ReplicaRouter.roundRobin(0)(replicas, dummySpec) shouldBe Some(0)
    ReplicaRouter.roundRobin(1)(replicas, dummySpec) shouldBe Some(1)
    ReplicaRouter.roundRobin(2)(replicas, dummySpec) shouldBe Some(2)
    ReplicaRouter.roundRobin(3)(replicas, dummySpec) shouldBe Some(0) // wraps
  }

  "powerAware" should "prefer active replicas with capacity" in {
    val replicas = Vector(
      makeReplica("r1", batchSize = 32, powerW = 300.0), // active, some load
      makeReplica("r2", batchSize = 0, powerW = 50.0),   // idle
      makeReplica("r3", batchSize = 16, powerW = 200.0)  // active, less loaded
    )
    val idx = ReplicaRouter.powerAware(replicas, dummySpec)
    idx shouldBe Some(2) // active + least loaded
  }

  it should "pick idle GPU with lowest power when all idle" in {
    val replicas = Vector(
      makeReplica("r1", batchSize = 0, powerW = 75.0),
      makeReplica("r2", batchSize = 0, powerW = 50.0), // lowest power
      makeReplica("r3", batchSize = 0, powerW = 60.0)
    )
    val idx = ReplicaRouter.powerAware(replicas, dummySpec)
    idx shouldBe Some(1)
  }

  it should "fall back to least loaded when all near capacity" in {
    val replicas = Vector(
      makeReplica("r1", batchSize = 60), // 93.75% full
      makeReplica("r2", batchSize = 58), // 90.6% full
      makeReplica("r3", batchSize = 62)  // 96.9% full
    )
    val idx = ReplicaRouter.powerAware(replicas, dummySpec)
    idx shouldBe Some(1) // least loaded overall
  }
