// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.edge

import io.aura.core.types.*

/** Edge computing models for multi-tier cloud-edge simulation.
  *
  * Models edge nodes with geographic locations, latency models, and offloading policies for deciding where to execute
  * workloads.
  */

// ─── Geographic Location ───────────────────────────────────────────────

final case class GeoLocation(
    latitude: Double,
    longitude: Double
):
  /** Haversine distance to another location in kilometers. */
  def distanceTo(other: GeoLocation): Double =
    val R    = 6371.0 // Earth radius in km
    val dLat = math.toRadians(other.latitude - latitude)
    val dLon = math.toRadians(other.longitude - longitude)
    val a = math.sin(dLat / 2) * math.sin(dLat / 2) +
      math.cos(math.toRadians(latitude)) * math.cos(math.toRadians(other.latitude)) *
      math.sin(dLon / 2) * math.sin(dLon / 2)
    val c = 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))
    R * c

// ─── Edge Node Specification ───────────────────────────────────────────

final case class EdgeNodeSpec(
    name: String,
    location: GeoLocation,
    spec: ResourceSpec,
    tier: EdgeTier,
    coverageRadiusKm: Double = 10.0,
    maxDeviceConnections: Int = 1000
)

enum EdgeTier:
  case Device     // IoT devices, sensors
  case EdgeMicro  // Micro edge (single-board computers, gateways)
  case EdgeSmall  // Small edge (single-server)
  case EdgeMedium // Medium edge (small cluster, 2-10 servers)
  case Fog        // Fog layer (larger cluster, ISP edge)
  case Cloud      // Full cloud datacenter

// ─── Latency Models ────────────────────────────────────────────────────

/** Latency model: computes one-way latency between two locations/tiers. */
type LatencyModel = (GeoLocation, GeoLocation, EdgeTier, EdgeTier) => SimTime

object LatencyModel:

  /** Distance-based latency: propagation delay ≈ distance / speed_of_light_in_fiber. */
  val distanceBased: LatencyModel = (src, dst, _, _) =>
    val distKm = src.distanceTo(dst)
    // Speed of light in fiber ≈ 200,000 km/s, plus processing overhead
    val propagationMs = distKm / 200.0 // ms
    val processingMs  = 0.5            // typical switch/router processing
    SimTime((propagationMs + processingMs) / 1000.0) // convert to seconds

  /** Tier-based latency: uses typical values for each tier pair. */
  val tierBased: LatencyModel = (_, _, srcTier, dstTier) =>
    val latencyMs = (srcTier, dstTier) match
      case (EdgeTier.Device, EdgeTier.EdgeMicro)    => 1.0
      case (EdgeTier.Device, EdgeTier.EdgeSmall)    => 5.0
      case (EdgeTier.Device, EdgeTier.EdgeMedium)   => 10.0
      case (EdgeTier.Device, EdgeTier.Fog)          => 20.0
      case (EdgeTier.Device, EdgeTier.Cloud)        => 50.0
      case (EdgeTier.EdgeMicro, EdgeTier.EdgeSmall) => 2.0
      case (EdgeTier.EdgeMicro, EdgeTier.Fog)       => 15.0
      case (EdgeTier.EdgeMicro, EdgeTier.Cloud)     => 45.0
      case (EdgeTier.EdgeSmall, EdgeTier.Fog)       => 10.0
      case (EdgeTier.EdgeSmall, EdgeTier.Cloud)     => 40.0
      case (EdgeTier.Fog, EdgeTier.Cloud)           => 25.0
      case (a, b) if a == b                         => 0.5
      case _                                        => 30.0 // default cross-tier
    SimTime(latencyMs / 1000.0)

  /** Combined model: uses both distance and tier for more realistic latency. */
  def combined(distWeight: Double = 0.6, tierWeight: Double = 0.4): LatencyModel =
    (src, dst, srcTier, dstTier) =>
      val dLat = distanceBased(src, dst, srcTier, dstTier)
      val tLat = tierBased(src, dst, srcTier, dstTier)
      SimTime(dLat.value * distWeight + tLat.value * tierWeight)

// ─── Offloading Policies ───────────────────────────────────────────────

/** Decision about where to execute a task. */
final case class OffloadingDecision(
    targetNode: String,
    estimatedLatency: SimTime,
    reason: String
)

/** Offloading context for policy decisions. */
final case class OffloadingContext(
    sourceNode: EdgeNodeSpec,
    candidateNodes: Vector[EdgeNodeSpec],
    taskCpuRequired: MIPS,
    taskMemRequired: MegaBytes,
    taskDeadline: SimTime,
    currentNodeUtilizations: Map[String, Utilization],
    latencyModel: LatencyModel
)

type OffloadingPolicy = OffloadingContext => OffloadingDecision

object OffloadingPolicy:

  /** Local-first: always execute locally if resources are available. */
  val localFirst: OffloadingPolicy = ctx =>
    val localUtil = ctx.currentNodeUtilizations.getOrElse(ctx.sourceNode.name, Utilization.Zero)
    if localUtil.value < 0.9 then OffloadingDecision(ctx.sourceNode.name, SimTime.Zero, "Local execution preferred")
    else nearestAvailable(ctx)

  /** Nearest available: offload to the nearest node with capacity. */
  val nearest: OffloadingPolicy = ctx => nearestAvailable(ctx)

  private def nearestAvailable(ctx: OffloadingContext): OffloadingDecision =
    val candidates = ctx.candidateNodes
      .filter { node =>
        val util = ctx.currentNodeUtilizations.getOrElse(node.name, Utilization.Zero)
        util.value < 0.95
      }
      .sortBy(_.location.distanceTo(ctx.sourceNode.location))

    candidates.headOption match
      case Some(node) =>
        val latency = ctx.latencyModel(
          ctx.sourceNode.location,
          node.location,
          ctx.sourceNode.tier,
          node.tier
        )
        OffloadingDecision(node.name, latency, s"Nearest available: ${node.name}")
      case None =>
        OffloadingDecision(ctx.sourceNode.name, SimTime.Zero, "No remote capacity - local fallback")

  /** Latency-aware: minimize total latency (network + queueing). */
  val latencyAware: OffloadingPolicy = ctx =>
    val candidates = ctx.candidateNodes.map { node =>
      val networkLatency = ctx.latencyModel(
        ctx.sourceNode.location,
        node.location,
        ctx.sourceNode.tier,
        node.tier
      )
      val util = ctx.currentNodeUtilizations.getOrElse(node.name, Utilization.Zero)
      // Simple queueing model: delay increases as utilization approaches 1
      val queuingDelay = SimTime(util.value / (1.0 - math.min(util.value, 0.99)) * 0.001)
      val totalLatency = networkLatency + queuingDelay
      (node, totalLatency)
    }

    candidates.minByOption(_._2.value) match
      case Some((node, latency)) =>
        OffloadingDecision(node.name, latency, s"Latency-optimal: ${node.name}")
      case None =>
        OffloadingDecision(ctx.sourceNode.name, SimTime.Zero, "No candidates")

  /** Deadline-aware: only offload if we can meet the deadline. */
  def deadlineAware(fallback: OffloadingPolicy = nearest): OffloadingPolicy = ctx =>
    val decision = latencyAware(ctx)
    if decision.estimatedLatency < ctx.taskDeadline then decision
    else
      // Try local first
      val localUtil = ctx.currentNodeUtilizations.getOrElse(ctx.sourceNode.name, Utilization.Zero)
      if localUtil.value < 0.95 then
        OffloadingDecision(ctx.sourceNode.name, SimTime.Zero, "Deadline-driven local execution")
      else fallback(ctx)

  /** Energy-aware: prefer nodes at lower tiers (less powerful, less energy). */
  val energyAware: OffloadingPolicy = ctx =>
    val candidates = ctx.candidateNodes
      .filter { node =>
        val util = ctx.currentNodeUtilizations.getOrElse(node.name, Utilization.Zero)
        util.value < 0.9
      }
      .sortBy(_.tier.ordinal) // prefer lower tiers (Device < Edge < Fog < Cloud)

    candidates.headOption match
      case Some(node) =>
        val latency = ctx.latencyModel(
          ctx.sourceNode.location,
          node.location,
          ctx.sourceNode.tier,
          node.tier
        )
        OffloadingDecision(node.name, latency, s"Energy-optimal: ${node.name}")
      case None =>
        OffloadingDecision(ctx.sourceNode.name, SimTime.Zero, "No low-tier capacity")
