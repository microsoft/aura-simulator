// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NetworkTopologySpec extends AnyFlatSpec with Matchers:

  "NetworkTopology.empty" should "return zero delay for any pair" in {
    val topo = NetworkTopology.empty
    topo.delay(DatacenterId(0L), DatacenterId(1L)).value shouldBe 0.0
  }

  "NetworkTopology.fromLinks" should "compute direct link delays" in {
    val links = Vector(
      NetworkLink(DatacenterId(0L), DatacenterId(1L), SimTime(0.050), Mbps(10000.0))
    )
    val topo = NetworkTopology.fromLinks(links)

    topo.delay(DatacenterId(0L), DatacenterId(1L)).value shouldBe 0.050 +- 0.001
    topo.delay(DatacenterId(1L), DatacenterId(0L)).value shouldBe 0.050 +- 0.001 // bidirectional
  }

  it should "return zero delay for same datacenter" in {
    val links = Vector(
      NetworkLink(DatacenterId(0L), DatacenterId(1L), SimTime(0.050), Mbps(10000.0))
    )
    val topo = NetworkTopology.fromLinks(links)

    topo.delay(DatacenterId(0L), DatacenterId(0L)).value shouldBe 0.0
  }

  it should "compute shortest path via Floyd-Warshall" in {
    // Triangle: A-B = 100ms, B-C = 50ms, A-C = 200ms
    // Shortest A→C should be via B: 100+50 = 150ms < 200ms
    val links = Vector(
      NetworkLink(DatacenterId(0L), DatacenterId(1L), SimTime(0.100), Mbps(10000.0)),
      NetworkLink(DatacenterId(1L), DatacenterId(2L), SimTime(0.050), Mbps(10000.0)),
      NetworkLink(DatacenterId(0L), DatacenterId(2L), SimTime(0.200), Mbps(10000.0))
    )
    val topo = NetworkTopology.fromLinks(links)

    topo.delay(DatacenterId(0L), DatacenterId(2L)).value shouldBe 0.150 +- 0.001
  }

  it should "track bandwidth between DCs" in {
    val links = Vector(
      NetworkLink(DatacenterId(0L), DatacenterId(1L), SimTime(0.050), Mbps(10000.0)),
      NetworkLink(DatacenterId(1L), DatacenterId(2L), SimTime(0.030), Mbps(5000.0))
    )
    val topo = NetworkTopology.fromLinks(links)

    topo.bandwidth(DatacenterId(0L), DatacenterId(1L)).value shouldBe 10000.0
    topo.bandwidth(DatacenterId(1L), DatacenterId(2L)).value shouldBe 5000.0
  }

  it should "return MaxValue bandwidth for same-DC" in {
    val topo = NetworkTopology.fromLinks(
      Vector(
        NetworkLink(DatacenterId(0L), DatacenterId(1L), SimTime(0.050), Mbps(1000.0))
      )
    )
    topo.bandwidth(DatacenterId(0L), DatacenterId(0L)).value shouldBe Double.MaxValue
  }

  it should "handle empty links" in {
    val topo = NetworkTopology.fromLinks(Vector.empty)
    topo shouldBe NetworkTopology.empty
  }

  it should "handle multiple DCs in a chain" in {
    // Chain: A - B - C - D, each 10ms apart
    val links = Vector(
      NetworkLink(DatacenterId(0L), DatacenterId(1L), SimTime(0.010), Mbps(10000.0)),
      NetworkLink(DatacenterId(1L), DatacenterId(2L), SimTime(0.010), Mbps(10000.0)),
      NetworkLink(DatacenterId(2L), DatacenterId(3L), SimTime(0.010), Mbps(10000.0))
    )
    val topo = NetworkTopology.fromLinks(links)

    // A to D should be 30ms (via B and C)
    topo.delay(DatacenterId(0L), DatacenterId(3L)).value shouldBe 0.030 +- 0.001
  }
