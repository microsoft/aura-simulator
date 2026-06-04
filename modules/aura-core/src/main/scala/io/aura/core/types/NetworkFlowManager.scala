// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

/** A single active network flow (e.g., a VM migration in progress). */
final case class NetworkFlow(
    flowId: Long,
    sourceDc: DatacenterId,
    targetDc: DatacenterId,
    vmId: VmId,
    startTime: SimTime
)

/** Immutable manager tracking active network flows for bandwidth contention. */
final case class NetworkFlowManager(
    activeFlows: Vector[NetworkFlow],
    nextFlowId: Long = 0L
):

  /** Register a new flow, returning updated manager and flow ID. */
  def addFlow(
      sourceDc: DatacenterId,
      targetDc: DatacenterId,
      vmId: VmId,
      startTime: SimTime
  ): (NetworkFlowManager, Long) =
    val flow = NetworkFlow(nextFlowId, sourceDc, targetDc, vmId, startTime)
    (copy(activeFlows = activeFlows :+ flow, nextFlowId = nextFlowId + 1), nextFlowId)

  /** Remove a flow by ID. */
  def removeFlow(flowId: Long): NetworkFlowManager =
    copy(activeFlows = activeFlows.filterNot(_.flowId == flowId))

  /** Count of active flows on a specific DC pair (in either direction). */
  def flowCount(sourceDc: DatacenterId, targetDc: DatacenterId): Int =
    activeFlows.count { f =>
      (f.sourceDc == sourceDc && f.targetDc == targetDc) ||
      (f.sourceDc == targetDc && f.targetDc == sourceDc)
    }

  /** Effective bandwidth for a DC pair, dividing equally among concurrent flows. */
  def effectiveBandwidth(
      sourceDc: DatacenterId,
      targetDc: DatacenterId,
      topology: NetworkTopology
  ): Mbps =
    val totalBw = topology.bandwidth(sourceDc, targetDc)
    val count   = flowCount(sourceDc, targetDc)
    if count <= 1 then totalBw
    else Mbps(totalBw.value / count.toDouble)

object NetworkFlowManager:
  val empty: NetworkFlowManager = NetworkFlowManager(Vector.empty, 0L)
