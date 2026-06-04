// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.gpu.state

import io.aura.core.types.*
import io.aura.gpu.GpuServerConfig

/** Immutable state for a GPU cluster (multiple GPU nodes). */
final case class GpuClusterState(
    clusterId: GpuClusterId,
    nodes: Map[GpuNodeId, GpuNodeState],
    serverConfig: GpuServerConfig
):
  def totalGpuCount: Int = nodes.values.map(_.totalDeviceCount).sum

  def totalAllocatedMemoryMB: MegaBytes =
    MegaBytes(nodes.values.map(_.totalAllocatedMemoryMB.value).sum)

  def nodeCount: Int = nodes.size

  def getNode(nodeId: GpuNodeId): Option[GpuNodeState] = nodes.get(nodeId)

  def updateNode(nodeId: GpuNodeId, state: GpuNodeState): GpuClusterState =
    copy(nodes = nodes + (nodeId -> state))

object GpuClusterState:
  def initial(
      clusterId: GpuClusterId,
      serverConfig: GpuServerConfig,
      nodeCount: Int
  ): GpuClusterState =
    val nodes = (0 until nodeCount).map { i =>
      val nodeId = GpuNodeId(clusterId.value * 1000 + i)
      nodeId -> GpuNodeState.initial(nodeId, serverConfig.gpuCount, serverConfig.deviceSpec.frequencyMaxMHz)
    }.toMap
    GpuClusterState(clusterId, nodes, serverConfig)
