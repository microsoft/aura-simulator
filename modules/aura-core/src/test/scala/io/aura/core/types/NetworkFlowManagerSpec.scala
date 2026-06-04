// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NetworkFlowManagerSpec extends AnyFlatSpec with Matchers:

  val dc0: DatacenterId = DatacenterId(0)
  val dc1: DatacenterId = DatacenterId(1)
  val dc2: DatacenterId = DatacenterId(2)

  val topology: NetworkTopology = NetworkTopology.fromLinks(
    Vector(
      NetworkLink(dc0, dc1, SimTime(0.050), Mbps(10000.0)),
      NetworkLink(dc1, dc2, SimTime(0.100), Mbps(5000.0))
    )
  )

  "NetworkFlowManager" should "provide full bandwidth with no contention" in {
    val (mgr, flowId) = NetworkFlowManager.empty.addFlow(dc0, dc1, VmId(1), SimTime.Zero)
    val bw            = mgr.effectiveBandwidth(dc0, dc1, topology)

    bw.value shouldBe 10000.0
  }

  it should "split bandwidth equally among concurrent flows" in {
    val (mgr1, _) = NetworkFlowManager.empty.addFlow(dc0, dc1, VmId(1), SimTime.Zero)
    val (mgr2, _) = mgr1.addFlow(dc0, dc1, VmId(2), SimTime.Zero)
    val bw        = mgr2.effectiveBandwidth(dc0, dc1, topology)

    bw.value shouldBe 5000.0
  }

  it should "track flows independently per DC pair" in {
    val (mgr1, _) = NetworkFlowManager.empty.addFlow(dc0, dc1, VmId(1), SimTime.Zero)
    val (mgr2, _) = mgr1.addFlow(dc1, dc2, VmId(2), SimTime.Zero)

    // dc0→dc1 has 1 flow, dc1→dc2 has 1 flow — no contention on either
    mgr2.effectiveBandwidth(dc0, dc1, topology).value shouldBe 10000.0
    mgr2.effectiveBandwidth(dc1, dc2, topology).value shouldBe 5000.0
  }

  it should "restore full bandwidth after flow removal" in {
    val (mgr1, flow1) = NetworkFlowManager.empty.addFlow(dc0, dc1, VmId(1), SimTime.Zero)
    val (mgr2, flow2) = mgr1.addFlow(dc0, dc1, VmId(2), SimTime.Zero)

    mgr2.effectiveBandwidth(dc0, dc1, topology).value shouldBe 5000.0

    val mgr3 = mgr2.removeFlow(flow2)
    mgr3.effectiveBandwidth(dc0, dc1, topology).value shouldBe 10000.0
  }
