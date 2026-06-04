// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

/** Hardware heterogeneity modeling: CPU architectures, instruction set compatibility, NUMA topology, and heterogeneous
  * scheduling — as immutable data and pure functions.
  */

// ── CPU Architecture ────────────────────────────────────────────────

enum CpuArchitecture:
  case X86_64
  case ARM64 // AWS Graviton, Ampere Altra
  case RISC_V
  case Power // IBM POWER

object CpuArchitecture:
  /** Base IPC (Instructions Per Clock) relative performance factor. Normalized to x86_64 = 1.0.
    */
  def relativeIpc(arch: CpuArchitecture): Double = arch match
    case CpuArchitecture.X86_64 => 1.0
    case CpuArchitecture.ARM64  => 0.9 // Graviton3 ~90% IPC, but better perf/watt
    case CpuArchitecture.RISC_V => 0.7 // Early implementations
    case CpuArchitecture.Power  => 1.1 // POWER10 high single-thread

  /** Power efficiency (perf/watt) relative factor. */
  def powerEfficiency(arch: CpuArchitecture): Double = arch match
    case CpuArchitecture.X86_64 => 1.0
    case CpuArchitecture.ARM64  => 1.5 // ARM is significantly more power efficient
    case CpuArchitecture.RISC_V => 1.3 // Good efficiency, still maturing
    case CpuArchitecture.Power  => 0.8 // High power draw

  /** Check if a workload compiled for `required` can run on `host`. */
  def isCompatible(required: CpuArchitecture, host: CpuArchitecture): Boolean =
    required == host // No cross-architecture compatibility (no emulation)

// ── CPU Generation ──────────────────────────────────────────────────

/** Models a specific CPU generation/SKU with architecture and microarch details. */
final case class CpuGeneration(
    name: String,
    architecture: CpuArchitecture,
    coresPerSocket: Int,
    baseClockGHz: Double,
    boostClockGHz: Double,
    l3CacheMB: Double,
    tdpWatts: Double,
    launchYear: Int,
    avx512: Boolean = false, // x86 SIMD support
    sveBitWidth: Int = 0     // ARM SVE vector width (0 = not supported)
):
  /** Effective MIPS per core (single-threaded performance). */
  def mipsPerCore: MIPS =
    val ipc = CpuArchitecture.relativeIpc(architecture)
    MIPS(boostClockGHz * 1000.0 * ipc) // rough: GHz * 1000 * IPC ≈ MIPS

  /** Total MIPS across all cores. */
  def totalMips: MIPS = MIPS(mipsPerCore.value * coresPerSocket)

  /** Performance per watt (MIPS/W). */
  def performancePerWatt: Double =
    if tdpWatts > 0 then totalMips.value / tdpWatts else 0.0

object CpuGeneration:
  // Intel x86_64
  val intelXeonGold6348: CpuGeneration = CpuGeneration(
    "Intel Xeon Gold 6348",
    CpuArchitecture.X86_64,
    28,
    2.6,
    3.5,
    42.0,
    235.0,
    2021,
    avx512 = true
  )
  val intelXeonPlatinum8480: CpuGeneration = CpuGeneration(
    "Intel Xeon Platinum 8480+",
    CpuArchitecture.X86_64,
    56,
    2.0,
    3.8,
    105.0,
    350.0,
    2023,
    avx512 = true
  )
  // AMD x86_64
  val amdEpyc9654: CpuGeneration = CpuGeneration(
    "AMD EPYC 9654",
    CpuArchitecture.X86_64,
    96,
    2.4,
    3.7,
    384.0,
    360.0,
    2022,
    avx512 = true
  )
  // AWS Graviton (ARM64)
  val graviton3: CpuGeneration = CpuGeneration(
    "AWS Graviton3",
    CpuArchitecture.ARM64,
    64,
    2.6,
    2.6,
    32.0,
    150.0,
    2022,
    sveBitWidth = 256
  )
  val graviton4: CpuGeneration = CpuGeneration(
    "AWS Graviton4",
    CpuArchitecture.ARM64,
    96,
    2.8,
    2.8,
    64.0,
    180.0,
    2024,
    sveBitWidth = 256
  )
  // Ampere ARM64
  val amperAltra: CpuGeneration = CpuGeneration(
    "Ampere Altra",
    CpuArchitecture.ARM64,
    80,
    3.0,
    3.0,
    32.0,
    250.0,
    2020,
    sveBitWidth = 0
  )

// ── NUMA Topology ───────────────────────────────────────────────────

/** A NUMA node represents a group of cores with local memory. */
final case class NumaNode(
    nodeId: Int,
    coreIds: Vector[Int],
    localMemoryMB: MegaBytes,
    allocatedMemoryMB: MegaBytes = MegaBytes.Zero
):
  def coreCount: Int               = coreIds.size
  def availableMemoryMB: MegaBytes = MegaBytes(localMemoryMB.value - allocatedMemoryMB.value)

  def allocateMemory(mb: MegaBytes): NumaNode =
    copy(allocatedMemoryMB = MegaBytes(allocatedMemoryMB.value + mb.value))

  def releaseMemory(mb: MegaBytes): NumaNode =
    copy(allocatedMemoryMB = MegaBytes(math.max(0, allocatedMemoryMB.value - mb.value)))

/** Inter-NUMA node latency/bandwidth. */
final case class NumaDistance(
    fromNode: Int,
    toNode: Int,
    relativeLatency: Double // 10 = local, 21 = remote on same socket, 32 = cross-socket
)

/** NUMA topology for a server. */
final case class NumaTopology(
    nodes: Vector[NumaNode],
    distances: Vector[NumaDistance]
):
  def nodeCount: Int           = nodes.size
  def totalCores: Int          = nodes.map(_.coreCount).sum
  def totalMemoryMB: MegaBytes = MegaBytes(nodes.map(_.localMemoryMB.value).sum)

  /** Get latency factor between two NUMA nodes. 1.0 = local. */
  def latencyFactor(from: Int, to: Int): Double =
    if from == to then 1.0
    else
      distances
        .find(d => d.fromNode == from && d.toNode == to)
        .map(_.relativeLatency / 10.0) // normalize: 10 = local = 1.0
        .getOrElse(2.0)                // default: 2x penalty for remote access

  /** Find best NUMA node for a memory allocation (most available). */
  def bestNodeForAllocation(requiredMB: MegaBytes): Option[Int] =
    nodes
      .filter(_.availableMemoryMB.value >= requiredMB.value)
      .sortBy(-_.availableMemoryMB.value)
      .headOption
      .map(_.nodeId)

object NumaTopology:
  /** Single-socket system (1 NUMA node). */
  def singleSocket(cores: Int, memoryMB: MegaBytes): NumaTopology =
    NumaTopology(
      Vector(NumaNode(0, (0 until cores).toVector, memoryMB)),
      Vector(NumaDistance(0, 0, 10))
    )

  /** Dual-socket system (2 NUMA nodes, uniform split). */
  def dualSocket(coresPerSocket: Int, memoryPerSocketMB: MegaBytes): NumaTopology =
    NumaTopology(
      Vector(
        NumaNode(0, (0 until coresPerSocket).toVector, memoryPerSocketMB),
        NumaNode(1, (coresPerSocket until coresPerSocket * 2).toVector, memoryPerSocketMB)
      ),
      Vector(
        NumaDistance(0, 0, 10),
        NumaDistance(0, 1, 21),
        NumaDistance(1, 0, 21),
        NumaDistance(1, 1, 10)
      )
    )

  /** Quad-socket system (4 NUMA nodes). */
  def quadSocket(coresPerSocket: Int, memoryPerSocketMB: MegaBytes): NumaTopology =
    val nodes = (0 until 4).map { i =>
      NumaNode(i, (i * coresPerSocket until (i + 1) * coresPerSocket).toVector, memoryPerSocketMB)
    }.toVector
    val distances = for
      from <- 0 until 4
      to   <- 0 until 4
    yield
      if from == to then NumaDistance(from, to, 10)
      else NumaDistance(from, to, 32)
    NumaTopology(nodes, distances.toVector)

// ── Heterogeneous Host ──────────────────────────────────────────────

/** A host with detailed hardware specification. */
final case class HeterogeneousHost(
    hostId: HostId,
    cpu: CpuGeneration,
    socketCount: Int,
    numaTopology: NumaTopology,
    gpuSpec: GpuSpec = GpuSpec.none
):
  def totalCores: Int               = cpu.coresPerSocket * socketCount
  def totalMips: MIPS               = MIPS(cpu.totalMips.value * socketCount)
  def totalTdpWatts: Double         = cpu.tdpWatts * socketCount
  def architecture: CpuArchitecture = cpu.architecture

// ── Heterogeneous Scheduler ─────────────────────────────────────────

/** Workload requirements that may be architecture-specific. */
final case class HeterogeneousWorkloadReq(
    requiredMips: MIPS,
    requiredMemoryMB: MegaBytes,
    requiredArch: Option[CpuArchitecture] = None,
    requiresAvx512: Boolean = false,
    requiresSve: Boolean = false,
    requiresGpu: Boolean = false,
    numaAware: Boolean = false
)

object HeterogeneousScheduler:

  /** Check if a host is compatible with workload requirements. */
  def isCompatible(host: HeterogeneousHost, req: HeterogeneousWorkloadReq): Boolean =
    val archOk = req.requiredArch.forall(_ == host.cpu.architecture)
    val avxOk  = !req.requiresAvx512 || host.cpu.avx512
    val sveOk  = !req.requiresSve || host.cpu.sveBitWidth > 0
    val gpuOk  = !req.requiresGpu || host.gpuSpec.gpuCount > 0
    val mipsOk = host.totalMips.value >= req.requiredMips.value
    val memOk  = host.numaTopology.totalMemoryMB.value >= req.requiredMemoryMB.value
    archOk && avxOk && sveOk && gpuOk && mipsOk && memOk

  /** Find the best host for a workload from a pool of heterogeneous hosts. */
  def bestFit(
      hosts: Vector[HeterogeneousHost],
      req: HeterogeneousWorkloadReq
  ): Option[HostId] =
    val compatible = hosts.filter(isCompatible(_, req))
    if compatible.isEmpty then None
    else
      // Prefer: 1) Matching arch 2) Best perf/watt 3) Least total MIPS (bin-pack)
      val scored = compatible.map { h =>
        val archBonus   = if req.requiredArch.contains(h.cpu.architecture) then 1000.0 else 0.0
        val perfPerWatt = h.cpu.performancePerWatt
        h -> (archBonus + perfPerWatt)
      }
      Some(scored.maxBy(_._2)._1.hostId)

  /** Schedule with NUMA awareness: prefer hosts where memory fits in a single NUMA node. */
  def numaAwareFit(
      hosts: Vector[HeterogeneousHost],
      req: HeterogeneousWorkloadReq
  ): Option[(HostId, Option[Int])] =
    val compatible = hosts.filter(isCompatible(_, req))
    if compatible.isEmpty then None
    else
      // Try to find a host with a NUMA node that can fit the whole allocation
      val withNuma = compatible.flatMap { h =>
        h.numaTopology.bestNodeForAllocation(req.requiredMemoryMB) match
          case Some(nodeId) => Some((h.hostId, Some(nodeId)))
          case None         => if !req.numaAware then Some((h.hostId, None)) else None
      }
      withNuma.headOption

  /** Estimate performance penalty for cross-NUMA memory access. */
  def numaAccessPenalty(topology: NumaTopology, fromNode: Int, toNode: Int): Double =
    topology.latencyFactor(fromNode, toNode)

  /** Rank hosts by power efficiency for green scheduling. */
  def rankByEfficiency(hosts: Vector[HeterogeneousHost]): Vector[(HeterogeneousHost, Double)] =
    hosts.map(h => (h, h.cpu.performancePerWatt)).sortBy(-_._2)
