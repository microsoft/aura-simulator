// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HardwareTopologySpec extends AnyFlatSpec with Matchers:

  // ── CpuArchitecture ──────────────────────────────────────────────

  "CpuArchitecture.relativeIpc" should "have x86_64 as baseline 1.0" in {
    CpuArchitecture.relativeIpc(CpuArchitecture.X86_64) shouldBe 1.0
  }

  it should "show ARM64 has lower IPC but better efficiency" in {
    CpuArchitecture.relativeIpc(CpuArchitecture.ARM64) should be < 1.0
    CpuArchitecture.powerEfficiency(CpuArchitecture.ARM64) should be > 1.0
  }

  "CpuArchitecture.isCompatible" should "only allow same architecture" in {
    CpuArchitecture.isCompatible(CpuArchitecture.X86_64, CpuArchitecture.X86_64) shouldBe true
    CpuArchitecture.isCompatible(CpuArchitecture.ARM64, CpuArchitecture.X86_64) shouldBe false
    CpuArchitecture.isCompatible(CpuArchitecture.X86_64, CpuArchitecture.ARM64) shouldBe false
  }

  // ── CpuGeneration ────────────────────────────────────────────────

  "CpuGeneration" should "compute MIPS per core" in {
    val graviton = CpuGeneration.graviton3
    graviton.mipsPerCore.value should be > 0.0
    graviton.totalMips.value should be > graviton.mipsPerCore.value
  }

  it should "provide preset CPU models" in {
    CpuGeneration.intelXeonGold6348.architecture shouldBe CpuArchitecture.X86_64
    CpuGeneration.graviton3.architecture shouldBe CpuArchitecture.ARM64
    CpuGeneration.amdEpyc9654.coresPerSocket shouldBe 96
  }

  it should "compute performance per watt" in {
    val intel    = CpuGeneration.intelXeonGold6348
    val graviton = CpuGeneration.graviton3
    intel.performancePerWatt should be > 0.0
    // Graviton should have better perf/watt due to ARM efficiency
    graviton.performancePerWatt should be > intel.performancePerWatt
  }

  "CpuGeneration presets" should "have AVX-512 on Intel/AMD" in {
    CpuGeneration.intelXeonGold6348.avx512 shouldBe true
    CpuGeneration.amdEpyc9654.avx512 shouldBe true
    CpuGeneration.graviton3.avx512 shouldBe false
  }

  it should "have SVE on Graviton" in {
    CpuGeneration.graviton3.sveBitWidth shouldBe 256
    CpuGeneration.intelXeonGold6348.sveBitWidth shouldBe 0
  }

  // ── NumaTopology ─────────────────────────────────────────────────

  "NumaTopology.singleSocket" should "create 1 NUMA node" in {
    val topo = NumaTopology.singleSocket(16, MegaBytes(32768))
    topo.nodeCount shouldBe 1
    topo.totalCores shouldBe 16
    topo.totalMemoryMB.value shouldBe 32768.0
  }

  "NumaTopology.dualSocket" should "create 2 NUMA nodes" in {
    val topo = NumaTopology.dualSocket(28, MegaBytes(65536))
    topo.nodeCount shouldBe 2
    topo.totalCores shouldBe 56
    topo.totalMemoryMB.value shouldBe 131072.0
  }

  it should "have lower latency for local access" in {
    val topo = NumaTopology.dualSocket(28, MegaBytes(65536))
    topo.latencyFactor(0, 0) shouldBe 1.0    // local
    topo.latencyFactor(0, 1) should be > 1.0 // remote
  }

  "NumaTopology.quadSocket" should "create 4 NUMA nodes" in {
    val topo = NumaTopology.quadSocket(24, MegaBytes(32768))
    topo.nodeCount shouldBe 4
    topo.totalCores shouldBe 96
    topo.totalMemoryMB.value shouldBe 131072.0
  }

  "NumaTopology.bestNodeForAllocation" should "find node with most available memory" in {
    val topo = NumaTopology.dualSocket(28, MegaBytes(65536))
    // Allocate some memory from node 0
    val updated = topo.copy(nodes = topo.nodes.updated(0, topo.nodes(0).allocateMemory(MegaBytes(60000))))
    // Node 1 has more free memory, should be preferred
    updated.bestNodeForAllocation(MegaBytes(10000)) shouldBe Some(1)
  }

  it should "return None when no node can fit" in {
    val topo = NumaTopology.singleSocket(8, MegaBytes(1024))
    topo.bestNodeForAllocation(MegaBytes(2000)) shouldBe None
  }

  // ── NumaNode ──────────────────────────────────────────────────────

  "NumaNode" should "track allocated memory" in {
    val node      = NumaNode(0, (0 until 8).toVector, MegaBytes(16384))
    val allocated = node.allocateMemory(MegaBytes(4096))
    allocated.availableMemoryMB.value shouldBe 12288.0
    val released = allocated.releaseMemory(MegaBytes(4096))
    released.availableMemoryMB.value shouldBe 16384.0
  }

  // ── HeterogeneousHost ─────────────────────────────────────────────

  "HeterogeneousHost" should "compute total resources" in {
    val host = HeterogeneousHost(
      HostId(1),
      CpuGeneration.graviton3,
      socketCount = 1,
      NumaTopology.singleSocket(64, MegaBytes(131072))
    )
    host.totalCores shouldBe 64
    host.architecture shouldBe CpuArchitecture.ARM64
    host.totalMips.value should be > 0.0
  }

  // ── HeterogeneousScheduler ────────────────────────────────────────

  "HeterogeneousScheduler.isCompatible" should "reject wrong architecture" in {
    val armHost = HeterogeneousHost(
      HostId(1),
      CpuGeneration.graviton3,
      1,
      NumaTopology.singleSocket(64, MegaBytes(131072))
    )
    val x86Req = HeterogeneousWorkloadReq(MIPS(1000), MegaBytes(4096), requiredArch = Some(CpuArchitecture.X86_64))
    HeterogeneousScheduler.isCompatible(armHost, x86Req) shouldBe false
  }

  it should "accept matching architecture" in {
    val armHost = HeterogeneousHost(
      HostId(1),
      CpuGeneration.graviton3,
      1,
      NumaTopology.singleSocket(64, MegaBytes(131072))
    )
    val armReq = HeterogeneousWorkloadReq(MIPS(1000), MegaBytes(4096), requiredArch = Some(CpuArchitecture.ARM64))
    HeterogeneousScheduler.isCompatible(armHost, armReq) shouldBe true
  }

  it should "check AVX-512 requirement" in {
    val armHost = HeterogeneousHost(
      HostId(1),
      CpuGeneration.graviton3,
      1,
      NumaTopology.singleSocket(64, MegaBytes(131072))
    )
    val avxReq = HeterogeneousWorkloadReq(MIPS(1000), MegaBytes(4096), requiresAvx512 = true)
    HeterogeneousScheduler.isCompatible(armHost, avxReq) shouldBe false

    val intelHost = HeterogeneousHost(
      HostId(2),
      CpuGeneration.intelXeonGold6348,
      1,
      NumaTopology.singleSocket(28, MegaBytes(131072))
    )
    HeterogeneousScheduler.isCompatible(intelHost, avxReq) shouldBe true
  }

  "HeterogeneousScheduler.bestFit" should "prefer best perf/watt" in {
    val armHost = HeterogeneousHost(
      HostId(1),
      CpuGeneration.graviton3,
      1,
      NumaTopology.singleSocket(64, MegaBytes(131072))
    )
    val intelHost = HeterogeneousHost(
      HostId(2),
      CpuGeneration.intelXeonGold6348,
      1,
      NumaTopology.singleSocket(28, MegaBytes(131072))
    )
    val req  = HeterogeneousWorkloadReq(MIPS(1000), MegaBytes(4096))
    val best = HeterogeneousScheduler.bestFit(Vector(armHost, intelHost), req)
    best shouldBe defined
    best.get shouldBe HostId(1) // Graviton has better perf/watt
  }

  "HeterogeneousScheduler.numaAwareFit" should "prefer local NUMA node" in {
    val host = HeterogeneousHost(
      HostId(1),
      CpuGeneration.intelXeonGold6348,
      2,
      NumaTopology.dualSocket(28, MegaBytes(65536))
    )
    val req    = HeterogeneousWorkloadReq(MIPS(1000), MegaBytes(32000), numaAware = true)
    val result = HeterogeneousScheduler.numaAwareFit(Vector(host), req)
    result shouldBe defined
    result.get._2 shouldBe defined // Should have a NUMA node assignment
  }

  "HeterogeneousScheduler.rankByEfficiency" should "sort by perf/watt" in {
    val hosts = Vector(
      HeterogeneousHost(
        HostId(1),
        CpuGeneration.intelXeonGold6348,
        1,
        NumaTopology.singleSocket(28, MegaBytes(131072))
      ),
      HeterogeneousHost(HostId(2), CpuGeneration.graviton3, 1, NumaTopology.singleSocket(64, MegaBytes(131072)))
    )
    val ranked = HeterogeneousScheduler.rankByEfficiency(hosts)
    ranked.head._1.hostId shouldBe HostId(2) // Graviton first (better efficiency)
  }

  "HeterogeneousScheduler.numaAccessPenalty" should "be 1.0 for local access" in {
    val topo = NumaTopology.dualSocket(28, MegaBytes(65536))
    HeterogeneousScheduler.numaAccessPenalty(topo, 0, 0) shouldBe 1.0
    HeterogeneousScheduler.numaAccessPenalty(topo, 0, 1) should be > 1.0
  }
