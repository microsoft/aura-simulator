// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.containers.state

import io.aura.core.types.*
import io.aura.core.events.EntityRef
import io.aura.containers.*

/** Immutable cluster state tracking nodes, pods, and deployments. */
final case class ClusterState(
    nodes: Map[String, NodeState],
    pods: Map[PodId, PodRecord],
    deployments: Map[DeploymentId, DeploymentRecord],
    nextPodSuffix: Int,
    podRequesters: Map[PodId, EntityRef] = Map.empty
):
  def initializeNodes(
      hosts: Vector[(String, HostId, MIPS, MegaBytes, Map[String, String], Vector[Taint])]
  ): ClusterState =
    val nodeMap = hosts.map { case (name, hostId, cpu, memory, labels, taints) =>
      name -> NodeState(
        name = name,
        hostId = hostId,
        totalCpu = cpu,
        totalMemory = memory,
        allocatedCpu = MIPS.Zero,
        allocatedMemory = MegaBytes.Zero,
        pods = Vector.empty,
        labels = labels,
        taints = taints
      )
    }.toMap
    copy(nodes = nodeMap)

  def addDeployment(record: DeploymentRecord): ClusterState =
    copy(deployments = deployments + (record.deploymentId -> record))

  def addPod(record: PodRecord): ClusterState =
    copy(pods = pods + (record.podId -> record))

  def scheduleOnNode(podId: PodId, nodeName: String, time: SimTime): ClusterState =
    pods.get(podId) match
      case Some(podRecord) =>
        val node        = nodes(nodeName)
        val updatedNode = node.schedulePod(podRecord.spec, time)
        copy(
          nodes = nodes + (nodeName -> updatedNode),
          pods = pods + (podId -> podRecord.copy(
            phase = PodPhase.Running,
            nodeName = Some(nodeName),
            startTime = Some(time)
          ))
        )
      case None => this

  def markPodRunning(podId: PodId, time: SimTime): ClusterState =
    pods.get(podId) match
      case Some(podRecord) =>
        copy(pods =
          pods + (podId -> podRecord.copy(
            phase = PodPhase.Running,
            startTime = podRecord.startTime.orElse(Some(time))
          ))
        )
      case None => this

  def markPodCompleted(podId: PodId, time: SimTime): ClusterState =
    pods.get(podId) match
      case Some(podRecord) =>
        copy(pods = pods + (podId -> podRecord.copy(phase = PodPhase.Succeeded)))
      case None => this

  def markPodFailed(podId: PodId, reason: String): ClusterState =
    pods.get(podId) match
      case Some(podRecord) =>
        copy(pods = pods + (podId -> podRecord.copy(phase = PodPhase.Failed)))
      case None => this

  def removePodFromNode(podId: PodId): ClusterState =
    pods.get(podId) match
      case Some(podRecord) =>
        podRecord.nodeName match
          case Some(nodeName) =>
            nodes.get(nodeName) match
              case Some(node) =>
                val updatedNode = node.removePod(podRecord.spec.name)
                copy(nodes = nodes + (nodeName -> updatedNode))
              case None => this
          case None => this
      case None => this

  def incrementRestarts(podId: PodId): ClusterState =
    pods.get(podId) match
      case Some(podRecord) =>
        copy(pods =
          pods + (podId -> podRecord.copy(
            restartCount = podRecord.restartCount + 1,
            phase = PodPhase.Pending,
            nodeName = None,
            startTime = None
          ))
        )
      case None => this

  def setRequester(podId: PodId, ref: EntityRef): ClusterState =
    copy(podRequesters = podRequesters + (podId -> ref))

  def requesterFor(podId: PodId, fallback: EntityRef): EntityRef =
    podRequesters.getOrElse(podId, fallback)

final case class PodRecord(
    podId: PodId,
    deploymentId: DeploymentId,
    spec: PodSpec,
    phase: PodPhase,
    nodeName: Option[String],
    startTime: Option[SimTime],
    restartCount: Int,
    hasWorkload: Boolean,
    vmId: Option[VmId] = None
)

final case class DeploymentRecord(
    deploymentId: DeploymentId,
    name: String,
    replicaCount: Int,
    podTemplate: PodSpec,
    workloadSpec: Option[WorkloadConfig]
)

final case class WorkloadConfig(
    length: MI,
    pes: PEs
)

object ClusterState:
  def empty: ClusterState = ClusterState(
    nodes = Map.empty,
    pods = Map.empty,
    deployments = Map.empty,
    nextPodSuffix = 0
  )
