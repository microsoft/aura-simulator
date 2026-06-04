// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

/** Batch / HPC job scheduling: gang scheduling, backfill, job queues with priorities, resource reservations — as
  * immutable data and pure functions.
  */

opaque type JobId = Long
object JobId:
  def apply(value: Long): JobId         = value
  extension (id: JobId) def value: Long = id
  given Ordering[JobId] with
    def compare(x: JobId, y: JobId): Int = java.lang.Long.compare(x, y)

/** Priority class for batch jobs. */
enum JobPriority(val level: Int):
  case Low      extends JobPriority(0)
  case Normal   extends JobPriority(1)
  case High     extends JobPriority(2)
  case Critical extends JobPriority(3)

/** State of a batch job. */
enum JobState:
  case Queued, Running, Completed, Failed, Cancelled

/** A batch/HPC job requesting resources. */
final case class BatchJob(
    id: JobId,
    name: String,
    priority: JobPriority,
    requiredNodes: Int,
    cpuPerNode: MIPS,
    ramPerNode: MegaBytes,
    estimatedRuntime: SimTime,
    submitTime: SimTime,
    state: JobState = JobState.Queued,
    startTime: Option[SimTime] = None,
    endTime: Option[SimTime] = None,
    isGang: Boolean = false // gang job requires all nodes simultaneously
):
  def waitTime(currentTime: SimTime): SimTime =
    SimTime(currentTime.value - submitTime.value)

  def turnaroundTime: Option[SimTime] =
    endTime.map(e => SimTime(e.value - submitTime.value))

  def resourceFootprint: Double = requiredNodes * cpuPerNode.value

/** A compute node available for batch jobs. */
final case class ComputeNode(
    nodeId: Long,
    totalCpu: MIPS,
    totalRam: MegaBytes,
    allocatedCpu: MIPS = MIPS.Zero,
    allocatedRam: MegaBytes = MegaBytes.Zero,
    assignedJobs: Set[JobId] = Set.empty
):
  def availableCpu: MIPS      = MIPS(totalCpu.value - allocatedCpu.value)
  def availableRam: MegaBytes = MegaBytes(totalRam.value - allocatedRam.value)

  def canFit(cpuReq: MIPS, ramReq: MegaBytes): Boolean =
    availableCpu.value >= cpuReq.value && availableRam.value >= ramReq.value

  def allocate(jobId: JobId, cpu: MIPS, ram: MegaBytes): ComputeNode =
    copy(
      allocatedCpu = MIPS(allocatedCpu.value + cpu.value),
      allocatedRam = MegaBytes(allocatedRam.value + ram.value),
      assignedJobs = assignedJobs + jobId
    )

  def release(jobId: JobId, cpu: MIPS, ram: MegaBytes): ComputeNode =
    copy(
      allocatedCpu = MIPS(math.max(0, allocatedCpu.value - cpu.value)),
      allocatedRam = MegaBytes(math.max(0, allocatedRam.value - ram.value)),
      assignedJobs = assignedJobs - jobId
    )

/** Resource reservation for a future job start. */
final case class Reservation(
    jobId: JobId,
    nodeIds: Vector[Long],
    startTime: SimTime,
    endTime: SimTime
)

/** Immutable job queue state. */
final case class JobQueue(
    pending: Vector[BatchJob] = Vector.empty,
    running: Vector[BatchJob] = Vector.empty,
    completed: Vector[BatchJob] = Vector.empty,
    nodes: Vector[ComputeNode] = Vector.empty,
    reservations: Vector[Reservation] = Vector.empty
):
  def totalJobs: Int  = pending.size + running.size + completed.size
  def queueDepth: Int = pending.size
  def utilization: Double =
    if nodes.isEmpty then 0.0
    else
      val totalCpu = nodes.map(_.totalCpu.value).sum
      val usedCpu  = nodes.map(_.allocatedCpu.value).sum
      if totalCpu == 0 then 0.0 else usedCpu / totalCpu

  def averageWaitTime(currentTime: SimTime): SimTime =
    if pending.isEmpty then SimTime.Zero
    else SimTime(pending.map(_.waitTime(currentTime).value).sum / pending.size)

/** Pure-function batch scheduler. */
object BatchScheduler:

  /** First Come First Served: schedule jobs in submission order. */
  def fcfs(queue: JobQueue, currentTime: SimTime): JobQueue =
    scheduleByOrder(queue, currentTime, _.submitTime.value)

  /** Shortest Job First: schedule shortest estimated runtime first. */
  def sjf(queue: JobQueue, currentTime: SimTime): JobQueue =
    scheduleByOrder(queue, currentTime, _.estimatedRuntime.value)

  /** Priority-based: schedule highest priority first, then by submit time. */
  def priorityBased(queue: JobQueue, currentTime: SimTime): JobQueue =
    scheduleByOrder(queue, currentTime, j => -j.priority.level * 1e12 + j.submitTime.value)

  private def scheduleByOrder(queue: JobQueue, currentTime: SimTime, sortKey: BatchJob => Double): JobQueue =
    val sorted = queue.pending.sortBy(sortKey)
    val (updatedNodes, nowRunning, stillPending) =
      sorted.foldLeft((queue.nodes, queue.running, Vector.empty[BatchJob])) { case ((nodes, running, pending), job) =>
        val fittingNodes = findNodes(nodes, job)
        if fittingNodes.size >= job.requiredNodes then
          val selectedNodes = fittingNodes.take(job.requiredNodes)
          (
            allocateOnNodes(nodes, selectedNodes, job),
            running :+ job.copy(state = JobState.Running, startTime = Some(currentTime)),
            pending
          )
        else (nodes, running, pending :+ job)
      }

    queue.copy(pending = stillPending, running = nowRunning, nodes = updatedNodes)

  /** Gang scheduling: only schedule jobs that can get ALL required nodes at once. */
  def gangSchedule(queue: JobQueue, currentTime: SimTime): JobQueue =
    val sorted = queue.pending.sortBy(j => -j.priority.level * 1e12 + j.submitTime.value)
    val (updatedNodes, nowRunning, stillPending) =
      sorted.foldLeft((queue.nodes, queue.running, Vector.empty[BatchJob])) { case ((nodes, running, pending), job) =>
        if job.isGang then
          val fittingNodes = findNodes(nodes, job)
          if fittingNodes.size >= job.requiredNodes then
            val selectedNodes = fittingNodes.take(job.requiredNodes)
            (
              allocateOnNodes(nodes, selectedNodes, job),
              running :+ job.copy(state = JobState.Running, startTime = Some(currentTime)),
              pending
            )
          else (nodes, running, pending :+ job)
        else
          // Non-gang jobs: schedule normally if possible
          val fittingNodes = findNodes(nodes, job)
          if fittingNodes.nonEmpty then
            val selectedNodes = fittingNodes.take(job.requiredNodes)
            if selectedNodes.size >= job.requiredNodes then
              (
                allocateOnNodes(nodes, selectedNodes, job),
                running :+ job.copy(state = JobState.Running, startTime = Some(currentTime)),
                pending
              )
            else (nodes, running, pending :+ job)
          else (nodes, running, pending :+ job)
      }

    queue.copy(pending = stillPending, running = nowRunning, nodes = updatedNodes)

  /** Backfill scheduling: schedule top-priority jobs first, then fill gaps with smaller lower-priority jobs that won't
    * delay the top job.
    */
  def backfill(queue: JobQueue, currentTime: SimTime): JobQueue =
    val sorted = queue.pending.sortBy(j => -j.priority.level * 1e12 + j.submitTime.value)
    if sorted.isEmpty then queue
    else
      // First pass: schedule high-priority jobs in priority order
      val (nodesAfterPriority, runningAfterPriority, deferredJobs) =
        sorted.foldLeft((queue.nodes, queue.running, Vector.empty[BatchJob])) {
          case ((nodes, running, deferred), job) =>
            val fittingNodes = findNodes(nodes, job)
            if fittingNodes.size >= job.requiredNodes then
              val selectedNodes = fittingNodes.take(job.requiredNodes)
              (
                allocateOnNodes(nodes, selectedNodes, job),
                running :+ job.copy(state = JobState.Running, startTime = Some(currentTime)),
                deferred
              )
            else (nodes, running, deferred :+ job)
        }

      // Second pass: backfill deferred jobs into remaining capacity (sorted by shortest runtime)
      val backfillSorted = deferredJobs.sortBy(_.estimatedRuntime.value)
      val (updatedNodes, nowRunning, stillPending) =
        backfillSorted.foldLeft((nodesAfterPriority, runningAfterPriority, Vector.empty[BatchJob])) {
          case ((nodes, running, pending), job) =>
            val fittingNodes = findNodes(nodes, job)
            if fittingNodes.size >= job.requiredNodes then
              val selectedNodes = fittingNodes.take(job.requiredNodes)
              (
                allocateOnNodes(nodes, selectedNodes, job),
                running :+ job.copy(state = JobState.Running, startTime = Some(currentTime)),
                pending
              )
            else (nodes, running, pending :+ job)
        }

      queue.copy(pending = stillPending, running = nowRunning, nodes = updatedNodes)

  /** Complete a running job: release its resources. */
  def completeJob(queue: JobQueue, jobId: JobId, currentTime: SimTime): JobQueue =
    queue.running.find(_.id == jobId) match
      case None => queue
      case Some(job) =>
        val updatedNodes = queue.nodes.map { node =>
          if node.assignedJobs.contains(jobId) then node.release(jobId, job.cpuPerNode, job.ramPerNode)
          else node
        }
        val finishedJob = job.copy(state = JobState.Completed, endTime = Some(currentTime))
        queue.copy(
          running = queue.running.filterNot(_.id == jobId),
          completed = queue.completed :+ finishedJob,
          nodes = updatedNodes
        )

  /** Cancel a queued or running job. */
  def cancelJob(queue: JobQueue, jobId: JobId, currentTime: SimTime): JobQueue =
    val inPending = queue.pending.find(_.id == jobId)
    val inRunning = queue.running.find(_.id == jobId)

    inPending match
      case Some(job) =>
        queue.copy(
          pending = queue.pending.filterNot(_.id == jobId),
          completed = queue.completed :+ job.copy(state = JobState.Cancelled, endTime = Some(currentTime))
        )
      case None =>
        inRunning match
          case Some(job) =>
            val updatedNodes = queue.nodes.map { node =>
              if node.assignedJobs.contains(jobId) then node.release(jobId, job.cpuPerNode, job.ramPerNode)
              else node
            }
            queue.copy(
              running = queue.running.filterNot(_.id == jobId),
              completed = queue.completed :+ job.copy(state = JobState.Cancelled, endTime = Some(currentTime)),
              nodes = updatedNodes
            )
          case None => queue

  /** Compute scheduling statistics. */
  def stats(queue: JobQueue): BatchSchedulerStats =
    val completedJobs = queue.completed.filter(_.state == JobState.Completed)
    val turnarounds   = completedJobs.flatMap(_.turnaroundTime)
    BatchSchedulerStats(
      totalJobs = queue.totalJobs,
      pendingJobs = queue.pending.size,
      runningJobs = queue.running.size,
      completedJobs = completedJobs.size,
      avgTurnaround =
        if turnarounds.isEmpty then SimTime.Zero
        else SimTime(turnarounds.map(_.value).sum / turnarounds.size),
      clusterUtilization = queue.utilization,
      queueDepth = queue.queueDepth
    )

  // ── Private helpers ─────────────────────────────────────────────────

  private def findNodes(nodes: Vector[ComputeNode], job: BatchJob): Vector[ComputeNode] =
    nodes.filter(_.canFit(job.cpuPerNode, job.ramPerNode))

  private def allocateOnNodes(
      nodes: Vector[ComputeNode],
      selected: Vector[ComputeNode],
      job: BatchJob
  ): Vector[ComputeNode] =
    val selectedIds = selected.map(_.nodeId).toSet
    nodes.map { node =>
      if selectedIds.contains(node.nodeId) then node.allocate(job.id, job.cpuPerNode, job.ramPerNode)
      else node
    }

final case class BatchSchedulerStats(
    totalJobs: Int,
    pendingJobs: Int,
    runningJobs: Int,
    completedJobs: Int,
    avgTurnaround: SimTime,
    clusterUtilization: Double,
    queueDepth: Int
)
