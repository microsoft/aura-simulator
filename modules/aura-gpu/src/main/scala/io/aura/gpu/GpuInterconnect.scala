// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.gpu

import io.aura.core.types.*

/** GPU cluster interconnect topology modeling.
  *
  * Models NVLink, PCIe, and InfiniBand interconnects for tensor parallelism, pipeline parallelism, and expert
  * parallelism.
  */
final case class GpuClusterTopology(
    nodes: Vector[GpuServerConfig],
    interNodeInterconnect: InterconnectType,
    interNodeBandwidthGBps: Double
):
  def totalGpus: Int = nodes.map(_.gpuCount).sum
  def totalGpuMemoryMB: MegaBytes =
    MegaBytes(nodes.map(_.totalGpuMemoryMB.value).sum)

object GpuClusterTopology:
  /** Single-node cluster (e.g., 1× DGX H100). */
  def singleNode(server: GpuServerConfig): GpuClusterTopology =
    GpuClusterTopology(
      nodes = Vector(server),
      interNodeInterconnect = InterconnectType.NVSwitch,
      interNodeBandwidthGBps = server.deviceSpec.nvlinkBandwidthGBps
    )

  /** Multi-node cluster with InfiniBand. */
  def multiNode(server: GpuServerConfig, nodeCount: Int, ibBandwidthGBps: Double = 50.0): GpuClusterTopology =
    GpuClusterTopology(
      nodes = Vector.fill(nodeCount)(server),
      interNodeInterconnect = InterconnectType.InfiniBand,
      interNodeBandwidthGBps = ibBandwidthGBps
    )

/** Parallelism strategy for distributing model across GPUs. */
enum ParallelismStrategy:
  case TensorParallel(degree: Int)
  case PipelineParallel(stages: Int)
  case ExpertParallel(degree: Int)
  case Hybrid(tp: Int, pp: Int)

object ParallelismStrategy:
  /** Calculate communication overhead for all-reduce (tensor parallel). */
  def allReduceTime(
      messageSizeBytes: Long,
      tpDegree: Int,
      bandwidthGBps: Double
  ): SimTime =
    if tpDegree <= 1 || bandwidthGBps <= 0 then SimTime.Zero
    else
      // Ring all-reduce: 2 × (n-1)/n × size / bandwidth
      val factor = 2.0 * (tpDegree - 1).toDouble / tpDegree
      val sizeGB = messageSizeBytes.toDouble / (1024.0 * 1024.0 * 1024.0)
      SimTime(factor * sizeGB / bandwidthGBps)

  /** Calculate pipeline bubble overhead. */
  def pipelineBubbleFraction(ppStages: Int, microBatches: Int): Double =
    if ppStages <= 1 || microBatches <= 0 then 0.0
    else (ppStages - 1).toDouble / (ppStages + microBatches - 1).toDouble
