// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.edge

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*

class OffloadingPolicySpec extends AnyFlatSpec with Matchers:

  private val deviceNode = EdgeNodeSpec(
    name = "device-1",
    location = GeoLocation(37.7749, -122.4194), // San Francisco
    spec = ResourceSpec(PEs(1), MIPS(100.0), MegaBytes(256.0), Mbps(100.0), MegaBytes(1000.0)),
    tier = EdgeTier.Device
  )

  private val edgeNode = EdgeNodeSpec(
    name = "edge-1",
    location = GeoLocation(37.7849, -122.4094), // ~1.4km away
    spec = ResourceSpec(PEs(4), MIPS(2000.0), MegaBytes(4096.0), Mbps(1000.0), MegaBytes(10000.0)),
    tier = EdgeTier.EdgeMicro
  )

  private val cloudNode = EdgeNodeSpec(
    name = "cloud-1",
    location = GeoLocation(37.3861, -122.0839), // Mountain View, ~50km
    spec = ResourceSpec(PEs(32), MIPS(50000.0), MegaBytes(65536.0), Mbps(10000.0), MegaBytes(100000.0)),
    tier = EdgeTier.Cloud
  )

  private val allNodes = Vector(deviceNode, edgeNode, cloudNode)

  private def makeContext(
      sourceNode: EdgeNodeSpec = deviceNode,
      utilizations: Map[String, Utilization] = Map.empty,
      latencyModel: LatencyModel = LatencyModel.combined()
  ): OffloadingContext =
    val defaultUtils = Map(
      "device-1" -> Utilization.Zero,
      "edge-1"   -> Utilization.Zero,
      "cloud-1"  -> Utilization.Zero
    ) ++ utilizations
    OffloadingContext(
      sourceNode = sourceNode,
      candidateNodes = allNodes,
      taskCpuRequired = MIPS(50.0),
      taskMemRequired = MegaBytes(64.0),
      taskDeadline = SimTime(0.1),
      currentNodeUtilizations = defaultUtils,
      latencyModel = latencyModel
    )

  // ─── localFirst ─────────────────────────────────────────────────────

  "OffloadingPolicy.localFirst" should "return local node when utilization < 0.9" in {
    val ctx      = makeContext(utilizations = Map("device-1" -> Utilization(0.5)))
    val decision = OffloadingPolicy.localFirst(ctx)
    decision.targetNode shouldBe "device-1"
    decision.reason should include("Local")
  }

  it should "offload when local node is overloaded" in {
    val ctx      = makeContext(utilizations = Map("device-1" -> Utilization(0.95)))
    val decision = OffloadingPolicy.localFirst(ctx)
    decision.targetNode should not be "device-1"
  }

  // ─── nearest ────────────────────────────────────────────────────────

  "OffloadingPolicy.nearest" should "return closest available node" in {
    val ctx      = makeContext()
    val decision = OffloadingPolicy.nearest(ctx)
    // device-1 is closest to itself, then edge-1, then cloud-1
    decision.targetNode shouldBe "device-1"
  }

  it should "skip overloaded nodes" in {
    val ctx      = makeContext(utilizations = Map("device-1" -> Utilization(0.96)))
    val decision = OffloadingPolicy.nearest(ctx)
    decision.targetNode shouldBe "edge-1"
  }

  // ─── latencyAware ──────────────────────────────────────────────────

  "OffloadingPolicy.latencyAware" should "minimize total latency" in {
    val ctx      = makeContext()
    val decision = OffloadingPolicy.latencyAware(ctx)
    // With all nodes idle, device-1 has zero network latency + minimal queueing
    decision.targetNode shouldBe "device-1"
  }

  it should "prefer remote nodes when local has high queueing delay" in {
    val ctx      = makeContext(utilizations = Map("device-1" -> Utilization(0.98)))
    val decision = OffloadingPolicy.latencyAware(ctx)
    // High utilization means high queueing delay, edge-1 may be better
    decision.targetNode should not be "device-1"
  }

  // ─── deadlineAware ─────────────────────────────────────────────────

  "OffloadingPolicy.deadlineAware" should "respect deadline constraints" in {
    val ctx      = makeContext()
    val policy   = OffloadingPolicy.deadlineAware()
    val decision = policy(ctx)
    decision.estimatedLatency.value should be < ctx.taskDeadline.value
  }

  // ─── energyAware ──────────────────────────────────────────────────

  "OffloadingPolicy.energyAware" should "prefer lower-tier nodes" in {
    val ctx      = makeContext()
    val decision = OffloadingPolicy.energyAware(ctx)
    // Device tier has lowest ordinal
    decision.targetNode shouldBe "device-1"
  }

  it should "skip overloaded low-tier nodes" in {
    val ctx      = makeContext(utilizations = Map("device-1" -> Utilization(0.95)))
    val decision = OffloadingPolicy.energyAware(ctx)
    decision.targetNode shouldBe "edge-1"
  }

  // ─── LatencyModel ──────────────────────────────────────────────────

  "LatencyModel.distanceBased" should "compute propagation delay from distance" in {
    val latency = LatencyModel.distanceBased(
      deviceNode.location,
      cloudNode.location,
      EdgeTier.Device,
      EdgeTier.Cloud
    )
    // ~50km distance: propagation = 50/200 = 0.25ms + 0.5ms processing = 0.75ms
    latency.value should be > 0.0
    latency.value should be < 0.01 // Less than 10ms in seconds
  }

  "LatencyModel.tierBased" should "return typical latency for tier pair" in {
    val latency = LatencyModel.tierBased(
      deviceNode.location,
      cloudNode.location,
      EdgeTier.Device,
      EdgeTier.Cloud
    )
    // Device → Cloud = 50ms = 0.05s
    latency.value shouldBe 0.05
  }

  "LatencyModel.combined" should "blend distance and tier latencies" in {
    val model = LatencyModel.combined(0.6, 0.4)
    val latency = model(
      deviceNode.location,
      edgeNode.location,
      EdgeTier.Device,
      EdgeTier.EdgeMicro
    )
    latency.value should be > 0.0
  }

  // ─── GeoLocation ──────────────────────────────────────────────────

  "GeoLocation.distanceTo" should "compute Haversine distance correctly" in {
    val sf       = GeoLocation(37.7749, -122.4194)
    val mv       = GeoLocation(37.3861, -122.0839)
    val distance = sf.distanceTo(mv)
    // SF to Mountain View is approximately 48km
    distance should be > 40.0
    distance should be < 60.0
  }

  it should "return zero for same location" in {
    val loc = GeoLocation(37.7749, -122.4194)
    loc.distanceTo(loc) shouldBe 0.0
  }
