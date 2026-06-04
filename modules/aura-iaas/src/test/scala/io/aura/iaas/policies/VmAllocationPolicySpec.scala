// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.iaas.state.HostState

class VmAllocationPolicySpec extends AnyFlatSpec with Matchers:

  private def makeHost(id: Long, freeMips: Double): HostState =
    val spec = ResourceSpec(PEs(8), MIPS(10000.0), MegaBytes(16384.0), Mbps(1000.0), MegaBytes(100000.0))
    val host = HostState.create(HostId(id), DatacenterId(0L), spec)
    // Simulate partial allocation by reducing available MIPS
    val usedMips = 10000.0 - freeMips
    if usedMips > 0 then
      val allocSpec = ResourceSpec(PEs(0), MIPS(usedMips), MegaBytes(0.0), Mbps(0.0), MegaBytes(0.0))
      host.copy(available = host.available.allocate(allocSpec))
    else host

  val vmSpec = ResourceSpec(PEs(2), MIPS(2000.0), MegaBytes(4096.0), Mbps(500.0), MegaBytes(10000.0))

  val hosts: IndexedSeq[HostState] = IndexedSeq(
    makeHost(0, 8000.0), // Most free
    makeHost(1, 3000.0), // Least free
    makeHost(2, 5000.0)  // Medium free
  )

  "FirstFit" should "select the first suitable host" in {
    val result = VmAllocationPolicy.firstFit(hosts, vmSpec)
    result shouldBe Some(HostId(0L))
  }

  it should "return None when no host fits" in {
    val hugeSpec = ResourceSpec(PEs(16), MIPS(50000.0), MegaBytes(65536.0), Mbps(5000.0), MegaBytes(500000.0))
    VmAllocationPolicy.firstFit(hosts, hugeSpec) shouldBe None
  }

  "BestFit" should "select the host with least available resources" in {
    val result = VmAllocationPolicy.bestFit(hosts, vmSpec)
    result shouldBe Some(HostId(1L)) // 3000 MIPS free = tightest fit
  }

  "WorstFit" should "select the host with most available resources" in {
    val result = VmAllocationPolicy.worstFit(hosts, vmSpec)
    result shouldBe Some(HostId(0L)) // 8000 MIPS free = most room
  }

  "RoundRobin" should "cycle through hosts" in {
    val (policy0, next0) = VmAllocationPolicy.roundRobin(0)
    val (policy1, next1) = VmAllocationPolicy.roundRobin(next0)
    val (policy2, _)     = VmAllocationPolicy.roundRobin(next1)

    val r0 = policy0(hosts, vmSpec)
    val r1 = policy1(hosts, vmSpec)
    val r2 = policy2(hosts, vmSpec)

    // Should cycle: 0, 1, 2
    r0 shouldBe defined
    r1 shouldBe defined
    r2 shouldBe defined
    Set(r0.get, r1.get, r2.get).size shouldBe 3
  }

  "All policies" should "return None for empty host list" in {
    VmAllocationPolicy.firstFit(IndexedSeq.empty, vmSpec) shouldBe None
    VmAllocationPolicy.bestFit(IndexedSeq.empty, vmSpec) shouldBe None
    VmAllocationPolicy.worstFit(IndexedSeq.empty, vmSpec) shouldBe None
  }
