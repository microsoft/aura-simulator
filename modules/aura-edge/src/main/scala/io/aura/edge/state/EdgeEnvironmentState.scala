// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.edge.state

import io.aura.core.types.*

/** Phase of an edge task's lifecycle. */
enum EdgeTaskPhase:
  case Pending, Running, Completed, Failed

/** Record tracking the lifecycle of an edge task. */
final case class EdgeTaskRecord(
    taskId: EdgeTaskId,
    sourceNodeName: String,
    executionNodeName: Option[String],
    phase: EdgeTaskPhase,
    offloaded: Boolean,
    networkLatency: SimTime,
    startTime: Option[SimTime],
    finishTime: Option[SimTime],
    offloadingReason: String
)

/** Runtime state of an edge node including active task tracking. */
final case class EdgeNodeRuntimeState(
    spec: io.aura.edge.EdgeNodeSpec,
    activeTasks: Int,
    currentUtilization: Utilization
):
  def addTask(cpuRequired: MIPS): EdgeNodeRuntimeState =
    val newActive = activeTasks + 1
    val newUtil   = math.min(1.0, currentUtilization.value + cpuRequired.value / spec.spec.mips.value)
    copy(activeTasks = newActive, currentUtilization = Utilization(newUtil))

  def removeTask(cpuRequired: MIPS): EdgeNodeRuntimeState =
    val newActive = math.max(0, activeTasks - 1)
    val newUtil   = math.max(0.0, currentUtilization.value - cpuRequired.value / spec.spec.mips.value)
    copy(activeTasks = newActive, currentUtilization = Utilization(newUtil))

  def canAcceptTask(cpuRequired: MIPS, memRequired: MegaBytes): Boolean =
    currentUtilization.value < 0.95

/** Immutable state for the edge environment actor. */
final case class EdgeEnvironmentState(
    nodes: Map[String, EdgeNodeRuntimeState],
    tasks: Map[EdgeTaskId, EdgeTaskRecord],
    taskCpuMap: Map[EdgeTaskId, MIPS]
):
  def initializeNodes(specs: Vector[io.aura.edge.EdgeNodeSpec]): EdgeEnvironmentState =
    val nodeMap = specs.map(s => s.name -> EdgeNodeRuntimeState(s, 0, Utilization.Zero)).toMap
    copy(nodes = nodeMap)

  def addTask(taskId: EdgeTaskId, sourceNodeName: String): EdgeEnvironmentState =
    val record = EdgeTaskRecord(
      taskId = taskId,
      sourceNodeName = sourceNodeName,
      executionNodeName = None,
      phase = EdgeTaskPhase.Pending,
      offloaded = false,
      networkLatency = SimTime.Zero,
      startTime = None,
      finishTime = None,
      offloadingReason = ""
    )
    copy(tasks = tasks + (taskId -> record))

  def startTaskOnNode(
      taskId: EdgeTaskId,
      executionNodeName: String,
      offloaded: Boolean,
      networkLatency: SimTime,
      startTime: SimTime,
      cpuRequired: MIPS,
      offloadingReason: String
  ): EdgeEnvironmentState =
    val updatedTask = tasks
      .get(taskId)
      .map(
        _.copy(
          executionNodeName = Some(executionNodeName),
          phase = EdgeTaskPhase.Running,
          offloaded = offloaded,
          networkLatency = networkLatency,
          startTime = Some(startTime),
          offloadingReason = offloadingReason
        )
      )
    val updatedNodes = nodes.get(executionNodeName).map(_.addTask(cpuRequired))
    copy(
      tasks = updatedTask.map(t => tasks + (taskId -> t)).getOrElse(tasks),
      nodes = updatedNodes.map(n => nodes + (executionNodeName -> n)).getOrElse(nodes),
      taskCpuMap = taskCpuMap + (taskId -> cpuRequired)
    )

  def completeTask(taskId: EdgeTaskId, finishTime: SimTime): EdgeEnvironmentState =
    val updatedTask = tasks
      .get(taskId)
      .map(
        _.copy(
          phase = EdgeTaskPhase.Completed,
          finishTime = Some(finishTime)
        )
      )
    val cpuRequired = taskCpuMap.getOrElse(taskId, MIPS(0.0))
    val execNode    = tasks.get(taskId).flatMap(_.executionNodeName)
    val updatedNodes = execNode
      .flatMap(n => nodes.get(n).map(ns => nodes + (n -> ns.removeTask(cpuRequired))))
      .getOrElse(nodes)
    copy(
      tasks = updatedTask.map(t => tasks + (taskId -> t)).getOrElse(tasks),
      nodes = updatedNodes
    )

  def failTask(taskId: EdgeTaskId, reason: String): EdgeEnvironmentState =
    val updatedTask = tasks
      .get(taskId)
      .map(
        _.copy(
          phase = EdgeTaskPhase.Failed,
          offloadingReason = reason
        )
      )
    copy(tasks = updatedTask.map(t => tasks + (taskId -> t)).getOrElse(tasks))

  def utilizationMap: Map[String, Utilization] =
    nodes.map { case (name, state) => name -> state.currentUtilization }

object EdgeEnvironmentState:
  val empty: EdgeEnvironmentState = EdgeEnvironmentState(
    nodes = Map.empty,
    tasks = Map.empty,
    taskCpuMap = Map.empty
  )
