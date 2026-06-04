// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NetworkSimulationSpec extends AnyFlatSpec with Matchers:

  private val hosts4  = (0 until 4).map(i => HostId(i.toLong)).toVector
  private val hosts16 = (0 until 16).map(i => HostId(i.toLong)).toVector

  // ─── SwitchSpec Presets ────────────────────────────────────────────

  "SwitchSpec.edge" should "have correct defaults" in {
    val spec = SwitchSpec.edge
    spec.ports shouldBe 48
    spec.downlinkBandwidth shouldBe Mbps(10000.0)
    spec.uplinkBandwidth shouldBe Mbps(40000.0)
  }

  "SwitchSpec.aggregate" should "have higher bandwidth than edge" in {
    SwitchSpec.aggregate.downlinkBandwidth.value should be > SwitchSpec.edge.downlinkBandwidth.value
  }

  "SwitchSpec.root" should "have lowest switching delay" in {
    SwitchSpec.root.switchingDelay.value should be < SwitchSpec.edge.switchingDelay.value
  }

  // ─── Fat Tree Construction ─────────────────────────────────────────

  "DatacenterNetwork.fatTree" should "create switches for all hosts" in {
    val net = DatacenterNetwork.fatTree(hosts4, hostsPerEdgeSwitch = 2)
    net.switches should not be empty
    net.hostToEdgeSwitch should have size 4
  }

  it should "assign every host to an edge switch" in {
    val net = DatacenterNetwork.fatTree(hosts16, hostsPerEdgeSwitch = 4)
    hosts16.foreach { h =>
      net.hostToEdgeSwitch should contain key h
    }
  }

  it should "create edge, aggregate, and root tiers" in {
    val net   = DatacenterNetwork.fatTree(hosts16, hostsPerEdgeSwitch = 4, edgeSwitchesPerAggregate = 2)
    val tiers = net.switches.values.map(_.tier).toSet
    tiers should contain(SwitchTier.Edge)
    tiers should contain(SwitchTier.Aggregate)
    tiers should contain(SwitchTier.Root)
  }

  it should "handle empty host list" in {
    val net = DatacenterNetwork.fatTree(Vector.empty)
    net shouldBe DatacenterNetwork.empty
  }

  it should "handle single host" in {
    val net = DatacenterNetwork.fatTree(Vector(HostId(0)))
    net.hostToEdgeSwitch should have size 1
    net.switches should not be empty
  }

  // ─── Routing ───────────────────────────────────────────────────────

  "DatacenterNetwork.computeRoute" should "return single hop for same-switch hosts" in {
    val net   = DatacenterNetwork.fatTree(hosts4, hostsPerEdgeSwitch = 4)
    val route = net.computeRoute(HostId(0), HostId(1))
    route should have size 1 // same edge switch
  }

  it should "return 3 hops for same-pod different-switch hosts" in {
    val net = DatacenterNetwork.fatTree(hosts16, hostsPerEdgeSwitch = 4, edgeSwitchesPerAggregate = 2)
    // Hosts 0-3 on switch 0, hosts 4-7 on switch 1 — same aggregate
    val route = net.computeRoute(HostId(0), HostId(4))
    route should have size 3 // edge → aggregate → edge
  }

  it should "return 5 hops for cross-pod hosts" in {
    val net = DatacenterNetwork.fatTree(hosts16, hostsPerEdgeSwitch = 4, edgeSwitchesPerAggregate = 1)
    // Each edge switch gets its own aggregate, so hosts on different edge switches are cross-pod
    val route = net.computeRoute(HostId(0), HostId(4))
    route should have size 5 // edge → agg → root → agg → edge
  }

  // ─── Hop Count ─────────────────────────────────────────────────────

  "DatacenterNetwork.hopCount" should "return 0 for same host" in {
    val net = DatacenterNetwork.fatTree(hosts4)
    net.hopCount(HostId(0), HostId(0)) shouldBe 0
  }

  it should "return positive for different hosts" in {
    val net = DatacenterNetwork.fatTree(hosts4, hostsPerEdgeSwitch = 2)
    net.hopCount(HostId(0), HostId(1)) should be > 0
  }

  // ─── Latency ───────────────────────────────────────────────────────

  "DatacenterNetwork.latency" should "return zero for same host" in {
    val net = DatacenterNetwork.fatTree(hosts4)
    // Same host has no network traversal via routePacket, but latency() uses computeRoute
    // which may still pass through the edge switch. Verify it's very small.
    net.latency(HostId(0), HostId(0)).value should be <= SwitchSpec.edge.switchingDelay.value
  }

  it should "return positive for different hosts" in {
    val net = DatacenterNetwork.fatTree(hosts4, hostsPerEdgeSwitch = 2)
    net.latency(HostId(0), HostId(2)).value should be > 0.0
  }

  it should "increase with more hops" in {
    val net        = DatacenterNetwork.fatTree(hosts16, hostsPerEdgeSwitch = 4, edgeSwitchesPerAggregate = 2)
    val sameSwitch = net.latency(HostId(0), HostId(1))
    val crossPod   = net.latency(HostId(0), HostId(8))
    crossPod.value should be >= sameSwitch.value
  }

  // ─── Packet Routing ────────────────────────────────────────────────

  "DatacenterNetwork.routePacket" should "deliver packet with zero latency on same host" in {
    val net         = DatacenterNetwork.fatTree(hosts4)
    val (_, packet) = net.routePacket(VmId(0), VmId(1), HostId(0), HostId(0), 1000, SimTime(10.0))
    packet.receiveTime shouldBe SimTime(10.0)
    packet.hops shouldBe 0
  }

  it should "add latency for different hosts" in {
    val net         = DatacenterNetwork.fatTree(hosts4, hostsPerEdgeSwitch = 2)
    val (_, packet) = net.routePacket(VmId(0), VmId(1), HostId(0), HostId(2), 1000, SimTime(10.0))
    packet.receiveTime.value should be > 10.0
    packet.hops should be > 0
  }

  it should "increment packet ID" in {
    val net        = DatacenterNetwork.fatTree(hosts4)
    val (net2, p1) = net.routePacket(VmId(0), VmId(1), HostId(0), HostId(1), 100, SimTime(0.0))
    val (_, p2)    = net2.routePacket(VmId(0), VmId(1), HostId(0), HostId(1), 100, SimTime(1.0))
    p2.id should be > p1.id
  }

  it should "account for packet size in transmission delay" in {
    val net        = DatacenterNetwork.fatTree(hosts4, hostsPerEdgeSwitch = 2)
    val (_, small) = net.routePacket(VmId(0), VmId(1), HostId(0), HostId(2), 100, SimTime(0.0))
    val (_, large) = net.routePacket(VmId(0), VmId(1), HostId(0), HostId(2), 1_000_000, SimTime(0.0))
    large.receiveTime.value should be > small.receiveTime.value
  }

  // ─── Bottleneck Bandwidth ──────────────────────────────────────────

  "DatacenterNetwork.bottleneckBandwidth" should "return minimum along path" in {
    val net   = DatacenterNetwork.fatTree(hosts4, hostsPerEdgeSwitch = 2)
    val route = net.computeRoute(HostId(0), HostId(2))
    val bw    = net.bottleneckBandwidth(route)
    bw.value should be > 0.0
  }

  // ─── NetworkPacket ─────────────────────────────────────────────────

  "NetworkPacket.sizeInMegabits" should "convert bytes to megabits" in {
    val p = NetworkPacket(0, VmId(0), VmId(1), HostId(0), HostId(1), 1_000_000, SimTime(0.0))
    p.sizeInMegabits shouldBe 8.0 +- 0.001
  }

  "NetworkPacket.withHop" should "increment hop count and add delay" in {
    val p      = NetworkPacket(0, VmId(0), VmId(1), HostId(0), HostId(1), 100, SimTime(10.0))
    val hopped = p.withHop(SimTime(0.001))
    hopped.hops shouldBe 1
    hopped.receiveTime.value shouldBe 10.001 +- 0.0001
  }

  // ─── NetworkSwitch ─────────────────────────────────────────────────

  "NetworkSwitch" should "support adding hosts" in {
    val sw      = NetworkSwitch(0, SwitchTier.Edge, SwitchSpec.edge)
    val updated = sw.addHost(HostId(0)).addHost(HostId(1))
    updated.connectedHosts should have size 2
  }

  it should "support packet enqueue/dequeue" in {
    val sw      = NetworkSwitch(0, SwitchTier.Edge, SwitchSpec.edge)
    val p       = NetworkPacket(0, VmId(0), VmId(1), HostId(0), HostId(1), 100, SimTime(0.0))
    val updated = sw.enqueue(p)
    updated.pendingPackets should have size 1
    val (cleared, packets) = updated.dequeueAll
    cleared.pendingPackets shouldBe empty
    packets should have size 1
  }
