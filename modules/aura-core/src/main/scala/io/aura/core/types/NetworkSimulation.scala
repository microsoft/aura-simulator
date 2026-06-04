// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

/** Packet-level network simulation with 3-tier switch architecture.
  *
  * Models a fat-tree datacenter network topology with Edge, Aggregate, and Root switches. Supports packet routing,
  * bandwidth allocation, and latency calculation at per-packet granularity.
  */

// ─── Switch Hierarchy ──────────────────────────────────────────────

/** Configuration for a network switch at any tier. */
final case class SwitchSpec(
    ports: Int,
    switchingDelay: SimTime, // processing delay per packet
    downlinkBandwidth: Mbps, // bandwidth per downlink port
    uplinkBandwidth: Mbps    // bandwidth per uplink port
)

object SwitchSpec:
  /** Edge switch: connects hosts, links up to aggregate layer. */
  val edge: SwitchSpec = SwitchSpec(
    ports = 48,
    switchingDelay = SimTime(0.00001), // 10 microseconds
    downlinkBandwidth = Mbps(10000.0), // 10 Gbps to hosts
    uplinkBandwidth = Mbps(40000.0)    // 40 Gbps uplink
  )

  /** Aggregate switch: connects edge switches, links up to root. */
  val aggregate: SwitchSpec = SwitchSpec(
    ports = 48,
    switchingDelay = SimTime(0.000008), // 8 microseconds
    downlinkBandwidth = Mbps(40000.0),  // 40 Gbps to edge
    uplinkBandwidth = Mbps(100000.0)    // 100 Gbps uplink
  )

  /** Root (core) switch: connects aggregate switches. */
  val root: SwitchSpec = SwitchSpec(
    ports = 64,
    switchingDelay = SimTime(0.000005), // 5 microseconds
    downlinkBandwidth = Mbps(100000.0), // 100 Gbps
    uplinkBandwidth = Mbps(100000.0)    // 100 Gbps (symmetric at top)
  )

enum SwitchTier:
  case Edge, Aggregate, Root

/** A switch in the datacenter network. */
final case class NetworkSwitch(
    id: Int,
    tier: SwitchTier,
    spec: SwitchSpec,
    connectedHosts: Vector[HostId] = Vector.empty,
    connectedSwitches: Vector[Int] = Vector.empty, // IDs of connected switches
    pendingPackets: Vector[NetworkPacket] = Vector.empty
):
  def addHost(hostId: HostId): NetworkSwitch =
    copy(connectedHosts = connectedHosts :+ hostId)

  def connectSwitch(switchId: Int): NetworkSwitch =
    copy(connectedSwitches = connectedSwitches :+ switchId)

  def enqueue(packet: NetworkPacket): NetworkSwitch =
    copy(pendingPackets = pendingPackets :+ packet)

  def dequeueAll: (NetworkSwitch, Vector[NetworkPacket]) =
    (copy(pendingPackets = Vector.empty), pendingPackets)

// ─── Packet Types ──────────────────────────────────────────────────

/** A network packet flowing through the switch hierarchy. */
final case class NetworkPacket(
    id: Long,
    sourceVm: VmId,
    destinationVm: VmId,
    sourceHost: HostId,
    destinationHost: HostId,
    sizeBytes: Long,
    sendTime: SimTime,
    receiveTime: SimTime = SimTime.Zero,
    hops: Int = 0
):
  def withHop(switchDelay: SimTime): NetworkPacket =
    copy(
      hops = hops + 1,
      receiveTime = SimTime(
        math.max(receiveTime.value, sendTime.value) + switchDelay.value
      )
    )

  def sizeInMegabits: Double = sizeBytes * 8.0 / 1_000_000.0

// ─── Datacenter Network ────────────────────────────────────────────

/** Immutable 3-tier fat-tree datacenter network.
  *
  * Topology: Hosts → EdgeSwitch → AggregateSwitch → RootSwitch Each edge switch connects a pod of hosts. Aggregate
  * switches connect edge switches in the same pod. Root switches provide full bisection bandwidth across pods.
  */
final case class DatacenterNetwork(
    switches: Map[Int, NetworkSwitch],
    hostToEdgeSwitch: Map[HostId, Int], // which edge switch each host is on
    edgeToAggregate: Map[Int, Vector[Int]],
    aggregateToRoot: Map[Int, Vector[Int]],
    nextPacketId: Long = 0L
):

  /** Route a packet from source host to destination host.
    *
    * @return
    *   (updated network, routed packet with latency computed)
    */
  def routePacket(
      sourceVm: VmId,
      destVm: VmId,
      sourceHost: HostId,
      destHost: HostId,
      sizeBytes: Long,
      sendTime: SimTime
  ): (DatacenterNetwork, NetworkPacket) =
    val packet = NetworkPacket(nextPacketId, sourceVm, destVm, sourceHost, destHost, sizeBytes, sendTime)

    if sourceHost == destHost then
      // Same host — no network traversal
      val delivered = packet.copy(receiveTime = sendTime)
      (copy(nextPacketId = nextPacketId + 1), delivered)
    else
      val route = computeRoute(sourceHost, destHost)
      val routedPacket = route.foldLeft(packet) { (p, switchId) =>
        switches.get(switchId) match
          case Some(sw) => p.withHop(sw.spec.switchingDelay)
          case None     => p
      }
      // Add transmission delay based on packet size and bottleneck bandwidth
      val bottleneckBw = bottleneckBandwidth(route)
      val transmissionDelay =
        if bottleneckBw.value > 0 then SimTime(routedPacket.sizeInMegabits / bottleneckBw.value)
        else SimTime.Zero
      val delivered = routedPacket.copy(
        receiveTime = SimTime(routedPacket.receiveTime.value + transmissionDelay.value)
      )
      (copy(nextPacketId = nextPacketId + 1), delivered)

  /** Compute the switch route between two hosts. */
  def computeRoute(source: HostId, dest: HostId): Vector[Int] =
    val srcEdge = hostToEdgeSwitch.getOrElse(source, -1)
    val dstEdge = hostToEdgeSwitch.getOrElse(dest, -1)

    if srcEdge == dstEdge then
      // Same edge switch — 1 hop
      Vector(srcEdge)
    else
      // Check if same aggregate
      val srcAggregates   = edgeToAggregate.getOrElse(srcEdge, Vector.empty)
      val dstAggregates   = edgeToAggregate.getOrElse(dstEdge, Vector.empty)
      val commonAggregate = srcAggregates.intersect(dstAggregates)

      if commonAggregate.nonEmpty then
        // Same pod — edge → aggregate → edge (3 hops)
        Vector(srcEdge, commonAggregate.head, dstEdge)
      else
        // Different pods — edge → aggregate → root → aggregate → edge (5 hops)
        val srcAgg       = srcAggregates.headOption.getOrElse(-1)
        val dstAgg       = dstAggregates.headOption.getOrElse(-1)
        val rootSwitches = aggregateToRoot.getOrElse(srcAgg, Vector.empty)
        val rootSwitch   = rootSwitches.headOption.getOrElse(-1)
        Vector(srcEdge, srcAgg, rootSwitch, dstAgg, dstEdge)

  /** Get the bottleneck bandwidth along a route. */
  def bottleneckBandwidth(route: Vector[Int]): Mbps =
    if route.isEmpty then Mbps.Zero
    else
      val bandwidths = route.flatMap(id => switches.get(id).map(_.spec.downlinkBandwidth))
      if bandwidths.isEmpty then Mbps.Zero
      else bandwidths.minBy(_.value)

  /** Count hops between two hosts. */
  def hopCount(source: HostId, dest: HostId): Int =
    if source == dest then 0
    else computeRoute(source, dest).size

  /** Compute end-to-end latency between two hosts (without packet size). */
  def latency(source: HostId, dest: HostId): SimTime =
    val route      = computeRoute(source, dest)
    val totalDelay = route.flatMap(id => switches.get(id).map(_.spec.switchingDelay.value)).sum
    SimTime(totalDelay)

object DatacenterNetwork:

  val empty: DatacenterNetwork = DatacenterNetwork(Map.empty, Map.empty, Map.empty, Map.empty)

  /** Build a standard fat-tree topology for a set of hosts.
    *
    * @param hosts
    *   Host IDs to connect
    * @param hostsPerEdgeSwitch
    *   Number of hosts per edge (ToR) switch
    * @param edgeSwitchesPerAggregate
    *   Edge switches per aggregate switch
    * @param edgeSpec
    *   Switch spec for edge tier
    * @param aggregateSpec
    *   Switch spec for aggregate tier
    * @param rootSpec
    *   Switch spec for root tier
    */
  def fatTree(
      hosts: Vector[HostId],
      hostsPerEdgeSwitch: Int = 8,
      edgeSwitchesPerAggregate: Int = 4,
      edgeSpec: SwitchSpec = SwitchSpec.edge,
      aggregateSpec: SwitchSpec = SwitchSpec.aggregate,
      rootSpec: SwitchSpec = SwitchSpec.root
  ): DatacenterNetwork =
    if hosts.isEmpty then empty
    else
      case class FatTreeState(
          switches: Map[Int, NetworkSwitch],
          hostToEdge: Map[HostId, Int],
          edgeToAgg: Map[Int, Vector[Int]],
          aggToRoot: Map[Int, Vector[Int]],
          nextId: Int
      )

      val initial = FatTreeState(Map.empty, Map.empty, Map.empty, Map.empty, 0)

      // Phase 1: Create edge switches and assign hosts
      val hostGroups = hosts.grouped(hostsPerEdgeSwitch).toVector
      val (afterEdge, edgeSwitchIds) = hostGroups.foldLeft((initial, Vector.empty[Int])) {
        case ((state, edgeIds), group) =>
          val switchId = state.nextId
          val sw = group.foldLeft(NetworkSwitch(switchId, SwitchTier.Edge, edgeSpec)) { (sw, hostId) =>
            sw.addHost(hostId)
          }
          val newHostToEdge = group.foldLeft(state.hostToEdge)((m, hostId) => m + (hostId -> switchId))
          (
            state.copy(
              switches = state.switches + (switchId -> sw),
              hostToEdge = newHostToEdge,
              nextId = state.nextId + 1
            ),
            edgeIds :+ switchId
          )
      }

      // Phase 2: Create aggregate switches and connect edge switches
      val edgeGroups = edgeSwitchIds.grouped(edgeSwitchesPerAggregate).toVector
      val (afterAgg, aggSwitchIds) = edgeGroups.foldLeft((afterEdge, Vector.empty[Int])) {
        case ((state, aggIds), edgeGroup) =>
          val switchId = state.nextId
          val sw = edgeGroup.foldLeft(NetworkSwitch(switchId, SwitchTier.Aggregate, aggregateSpec)) { (sw, edgeId) =>
            sw.connectSwitch(edgeId)
          }
          val updatedSwitches = edgeGroup.foldLeft(state.switches + (switchId -> sw)) { (switches, edgeId) =>
            switches.updatedWith(edgeId)(_.map(_.connectSwitch(switchId)))
          }
          val updatedEdgeToAgg = edgeGroup.foldLeft(state.edgeToAgg) { (m, edgeId) =>
            m + (edgeId -> (m.getOrElse(edgeId, Vector.empty) :+ switchId))
          }
          (
            state.copy(switches = updatedSwitches, edgeToAgg = updatedEdgeToAgg, nextId = state.nextId + 1),
            aggIds :+ switchId
          )
      }

      // Phase 3: Create root switch(es) and connect aggregate switches
      val finalState = if aggSwitchIds.nonEmpty then
        val rootId = afterAgg.nextId
        val rootSw = aggSwitchIds.foldLeft(NetworkSwitch(rootId, SwitchTier.Root, rootSpec)) { (sw, aggId) =>
          sw.connectSwitch(aggId)
        }
        val updatedSwitches = aggSwitchIds.foldLeft(afterAgg.switches + (rootId -> rootSw)) { (switches, aggId) =>
          switches.updatedWith(aggId)(_.map(_.connectSwitch(rootId)))
        }
        val updatedAggToRoot = aggSwitchIds.foldLeft(afterAgg.aggToRoot) { (m, aggId) =>
          m + (aggId -> (m.getOrElse(aggId, Vector.empty) :+ rootId))
        }
        afterAgg.copy(switches = updatedSwitches, aggToRoot = updatedAggToRoot)
      else afterAgg

      DatacenterNetwork(finalState.switches, finalState.hostToEdge, finalState.edgeToAgg, finalState.aggToRoot)
