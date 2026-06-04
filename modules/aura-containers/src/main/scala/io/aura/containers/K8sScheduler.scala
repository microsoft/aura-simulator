// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.containers

import io.aura.core.types.*

/** Kubernetes-style container orchestration models.
  *
  * Models container specs, pods, nodes, and scheduling policies inspired by the Kubernetes scheduler architecture.
  */

// ─── Container and Pod Specifications ──────────────────────────────────

final case class ContainerSpec(
    name: String,
    image: String,
    cpuRequest: MIPS,
    cpuLimit: MIPS,
    memoryRequest: MegaBytes,
    memoryLimit: MegaBytes,
    ports: Vector[Int] = Vector.empty
)

final case class PodSpec(
    name: String,
    namespace: String = "default",
    containers: Vector[ContainerSpec],
    restartPolicy: RestartPolicy = RestartPolicy.Always,
    priority: Int = 0,
    nodeSelector: Map[String, String] = Map.empty,
    tolerations: Vector[Toleration] = Vector.empty
):
  def totalCpuRequest: MIPS =
    MIPS(containers.map(_.cpuRequest.value).sum)

  def totalMemoryRequest: MegaBytes =
    MegaBytes(containers.map(_.memoryRequest.value).sum)

  def totalCpuLimit: MIPS =
    MIPS(containers.map(_.cpuLimit.value).sum)

  def totalMemoryLimit: MegaBytes =
    MegaBytes(containers.map(_.memoryLimit.value).sum)

enum RestartPolicy:
  case Always, OnFailure, Never

final case class Toleration(
    key: String,
    operator: TolerationOperator = TolerationOperator.Equal,
    value: String = "",
    effect: TaintEffect = TaintEffect.NoSchedule
)

enum TolerationOperator:
  case Equal, Exists

final case class Taint(
    key: String,
    value: String,
    effect: TaintEffect
)

enum TaintEffect:
  case NoSchedule, PreferNoSchedule, NoExecute

// ─── Node State ────────────────────────────────────────────────────────

final case class NodeState(
    name: String,
    hostId: HostId,
    totalCpu: MIPS,
    totalMemory: MegaBytes,
    allocatedCpu: MIPS,
    allocatedMemory: MegaBytes,
    pods: Vector[PodState],
    labels: Map[String, String] = Map.empty,
    taints: Vector[Taint] = Vector.empty,
    conditions: NodeConditions = NodeConditions()
):
  def availableCpu: MIPS         = MIPS(totalCpu.value - allocatedCpu.value)
  def availableMemory: MegaBytes = MegaBytes(totalMemory.value - allocatedMemory.value)

  def canSchedulePod(pod: PodSpec): Boolean =
    availableCpu >= pod.totalCpuRequest &&
      availableMemory >= pod.totalMemoryRequest &&
      matchesNodeSelector(pod) &&
      toleratesTaints(pod)

  private def matchesNodeSelector(pod: PodSpec): Boolean =
    pod.nodeSelector.forall { case (k, v) => labels.get(k).contains(v) }

  private def toleratesTaints(pod: PodSpec): Boolean =
    taints.forall { taint =>
      pod.tolerations.exists { t =>
        (t.operator == TolerationOperator.Exists && t.key == taint.key) ||
        (t.key == taint.key && t.value == taint.value && t.effect == taint.effect)
      }
    }

  def schedulePod(pod: PodSpec, startTime: SimTime): NodeState =
    copy(
      allocatedCpu = MIPS(allocatedCpu.value + pod.totalCpuRequest.value),
      allocatedMemory = MegaBytes(allocatedMemory.value + pod.totalMemoryRequest.value),
      pods = pods :+ PodState(pod, PodPhase.Running, startTime)
    )

  def removePod(podName: String): NodeState =
    pods.find(_.spec.name == podName) match
      case Some(podState) =>
        copy(
          allocatedCpu = MIPS(allocatedCpu.value - podState.spec.totalCpuRequest.value),
          allocatedMemory = MegaBytes(allocatedMemory.value - podState.spec.totalMemoryRequest.value),
          pods = pods.filterNot(_.spec.name == podName)
        )
      case None => this

final case class NodeConditions(
    ready: Boolean = true,
    memoryPressure: Boolean = false,
    diskPressure: Boolean = false,
    pidPressure: Boolean = false
)

final case class PodState(
    spec: PodSpec,
    phase: PodPhase,
    startTime: SimTime,
    restartCount: Int = 0
)

enum PodPhase:
  case Pending, Running, Succeeded, Failed, Unknown

// ─── K8s Scheduler ─────────────────────────────────────────────────────

/** Kubernetes-style scheduler as pure functions. */
type K8sSchedulingPolicy = (Vector[NodeState], PodSpec) => Option[String] // returns node name

object K8sScheduler:

  /** LeastRequestedPriority: prefers nodes with lowest resource utilization. */
  val leastRequested: K8sSchedulingPolicy = (nodes, pod) =>
    nodes
      .filter(_.canSchedulePod(pod))
      .sortBy { node =>
        val cpuFraction = node.allocatedCpu.value / node.totalCpu.value
        val memFraction = node.allocatedMemory.value / node.totalMemory.value
        (cpuFraction + memFraction) / 2.0
      }
      .headOption
      .map(_.name)

  /** MostRequestedPriority: prefers nodes with highest utilization (bin-packing). */
  val mostRequested: K8sSchedulingPolicy = (nodes, pod) =>
    nodes
      .filter(_.canSchedulePod(pod))
      .sortBy { node =>
        val cpuFraction = node.allocatedCpu.value / node.totalCpu.value
        val memFraction = node.allocatedMemory.value / node.totalMemory.value
        -((cpuFraction + memFraction) / 2.0)
      }
      .headOption
      .map(_.name)

  /** BalancedResourceAllocation: minimizes CPU/memory imbalance. */
  val balancedResource: K8sSchedulingPolicy = (nodes, pod) =>
    nodes
      .filter(_.canSchedulePod(pod))
      .sortBy { node =>
        val cpuFraction = (node.allocatedCpu.value + pod.totalCpuRequest.value) / node.totalCpu.value
        val memFraction = (node.allocatedMemory.value + pod.totalMemoryRequest.value) / node.totalMemory.value
        math.abs(cpuFraction - memFraction)
      }
      .headOption
      .map(_.name)

  /** Priority-aware scheduling: schedules higher priority pods first. */
  val priorityBased: K8sSchedulingPolicy = (nodes, pod) => leastRequested(nodes, pod)
