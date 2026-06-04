// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BatchSchedulingSpec extends AnyFlatSpec with Matchers:

  private def node(id: Long, cpu: Double = 1000.0, ram: Double = 8192.0) =
    ComputeNode(id, MIPS(cpu), MegaBytes(ram))

  private def job(
      id: Long,
      name: String,
      nodes: Int = 1,
      cpu: Double = 200.0,
      ram: Double = 1024.0,
      runtime: Double = 100.0,
      submitTime: Double = 0.0,
      priority: JobPriority = JobPriority.Normal,
      gang: Boolean = false
  ) =
    BatchJob(
      JobId(id),
      name,
      priority,
      nodes,
      MIPS(cpu),
      MegaBytes(ram),
      SimTime(runtime),
      SimTime(submitTime),
      isGang = gang
    )

  private val nodes4 = Vector(node(0), node(1), node(2), node(3))

  // ── JobId ────────────────────────────────────────────────────────

  "JobId" should "wrap Long values" in {
    JobId(42).value shouldBe 42L
  }

  // ── BatchJob ─────────────────────────────────────────────────────

  "BatchJob" should "compute wait time" in {
    val j = job(1, "test", submitTime = 10.0)
    j.waitTime(SimTime(25.0)).value shouldBe 15.0
  }

  it should "compute turnaround time" in {
    val j = job(1, "test", submitTime = 10.0).copy(endTime = Some(SimTime(50.0)))
    j.turnaroundTime.get.value shouldBe 40.0
  }

  it should "compute resource footprint" in {
    val j = job(1, "test", nodes = 4, cpu = 500.0)
    j.resourceFootprint shouldBe 2000.0
  }

  // ── ComputeNode ──────────────────────────────────────────────────

  "ComputeNode" should "track available resources" in {
    val n = node(0, 1000, 8192)
    n.canFit(MIPS(500), MegaBytes(4096)) shouldBe true
    n.canFit(MIPS(1500), MegaBytes(4096)) shouldBe false
  }

  it should "allocate and release" in {
    val n         = node(0, 1000, 8192)
    val allocated = n.allocate(JobId(1), MIPS(400), MegaBytes(2048))
    allocated.availableCpu.value shouldBe 600.0
    allocated.assignedJobs should contain(JobId(1))

    val released = allocated.release(JobId(1), MIPS(400), MegaBytes(2048))
    released.availableCpu.value shouldBe 1000.0
    released.assignedJobs shouldBe empty
  }

  // ── FCFS Scheduling ──────────────────────────────────────────────

  "BatchScheduler.fcfs" should "schedule jobs in submission order" in {
    val j1    = job(1, "first", submitTime = 0.0)
    val j2    = job(2, "second", submitTime = 1.0)
    val queue = JobQueue(pending = Vector(j2, j1), nodes = nodes4)

    val result = BatchScheduler.fcfs(queue, SimTime(5.0))
    result.running should have size 2
    result.pending shouldBe empty
    // Both fit, both scheduled
    result.running.map(_.id) should contain allOf (JobId(1), JobId(2))
  }

  it should "leave jobs pending when no capacity" in {
    val bigJob = job(1, "big", nodes = 5, cpu = 1000.0) // needs 5 nodes, only 4 available
    val queue  = JobQueue(pending = Vector(bigJob), nodes = nodes4)

    val result = BatchScheduler.fcfs(queue, SimTime(0.0))
    result.pending should have size 1
    result.running shouldBe empty
  }

  // ── SJF Scheduling ───────────────────────────────────────────────

  "BatchScheduler.sjf" should "schedule shortest job first" in {
    val longJob  = job(1, "long", runtime = 1000.0, cpu = 800.0) // uses most of a node
    val shortJob = job(2, "short", runtime = 10.0, cpu = 800.0)
    // Only 4 nodes, each can fit 1 job at 800 MIPS
    val queue = JobQueue(pending = Vector(longJob, shortJob), nodes = nodes4)

    val result = BatchScheduler.sjf(queue, SimTime(0.0))
    result.running should have size 2
    // Short job should be scheduled first in ordering
    result.running.head.name shouldBe "short"
  }

  // ── Priority Scheduling ──────────────────────────────────────────

  "BatchScheduler.priorityBased" should "schedule high priority first" in {
    val lowJob  = job(1, "low", priority = JobPriority.Low, cpu = 600.0)
    val highJob = job(2, "high", priority = JobPriority.High, cpu = 600.0)
    // Only 1 node with 1000 MIPS — can only fit 1 job at 600 MIPS
    val queue = JobQueue(pending = Vector(lowJob, highJob), nodes = Vector(node(0)))

    val result = BatchScheduler.priorityBased(queue, SimTime(0.0))
    result.running should have size 1
    result.running.head.name shouldBe "high"
    result.pending should have size 1
  }

  // ── Gang Scheduling ──────────────────────────────────────────────

  "BatchScheduler.gangSchedule" should "schedule gang jobs only when all nodes available" in {
    val gangJob = job(1, "mpi-job", nodes = 3, gang = true)
    val queue   = JobQueue(pending = Vector(gangJob), nodes = nodes4)

    val result = BatchScheduler.gangSchedule(queue, SimTime(0.0))
    result.running should have size 1
    result.running.head.isGang shouldBe true
  }

  it should "block gang jobs when insufficient nodes" in {
    val gangJob = job(1, "mpi-job", nodes = 5, gang = true) // needs 5, only 4
    val queue   = JobQueue(pending = Vector(gangJob), nodes = nodes4)

    val result = BatchScheduler.gangSchedule(queue, SimTime(0.0))
    result.running shouldBe empty
    result.pending should have size 1
  }

  it should "schedule non-gang jobs alongside blocked gang jobs" in {
    val gangJob  = job(1, "mpi-job", nodes = 5, gang = true) // blocked
    val smallJob = job(2, "small", nodes = 1, gang = false)  // should schedule
    val queue    = JobQueue(pending = Vector(gangJob, smallJob), nodes = nodes4)

    val result = BatchScheduler.gangSchedule(queue, SimTime(0.0))
    result.running should have size 1
    result.running.head.name shouldBe "small"
    result.pending should have size 1
  }

  // ── Backfill Scheduling ──────────────────────────────────────────

  "BatchScheduler.backfill" should "fill gaps with smaller jobs" in {
    // 4 nodes, big job needs 3, small job needs 1 — both should fit
    val bigJob   = job(1, "big", nodes = 3, priority = JobPriority.High)
    val smallJob = job(2, "small", nodes = 1, priority = JobPriority.Low)
    val queue    = JobQueue(pending = Vector(bigJob, smallJob), nodes = nodes4)

    val result = BatchScheduler.backfill(queue, SimTime(0.0))
    result.running should have size 2
    result.pending shouldBe empty
  }

  it should "prefer high-priority jobs" in {
    val highJob = job(1, "high", nodes = 3, priority = JobPriority.Critical, cpu = 500.0)
    val lowJob  = job(2, "low", nodes = 3, priority = JobPriority.Low, cpu = 500.0)
    // Only 4 nodes with 1000 MIPS each — both need 3 nodes at 500 MIPS, can potentially share
    // But since each needs 3 nodes and there are only 4, after first job takes 3, second needs 3 more
    val queue = JobQueue(pending = Vector(lowJob, highJob), nodes = nodes4)

    val result = BatchScheduler.backfill(queue, SimTime(0.0))
    // High priority scheduled first, low-priority backfilled if enough nodes remain
    result.running.head.name shouldBe "high"
  }

  // ── Job Completion ───────────────────────────────────────────────

  "BatchScheduler.completeJob" should "release resources on completion" in {
    val j         = job(1, "test")
    val queue     = JobQueue(pending = Vector(j), nodes = nodes4)
    val scheduled = BatchScheduler.fcfs(queue, SimTime(0.0))
    scheduled.running should have size 1

    val completed = BatchScheduler.completeJob(scheduled, JobId(1), SimTime(100.0))
    completed.running shouldBe empty
    completed.completed should have size 1
    completed.completed.head.state shouldBe JobState.Completed
    completed.completed.head.endTime shouldBe Some(SimTime(100.0))
    // Node resources restored
    completed.nodes.forall(_.assignedJobs.isEmpty) shouldBe true
  }

  // ── Job Cancellation ─────────────────────────────────────────────

  "BatchScheduler.cancelJob" should "cancel pending job" in {
    val j     = job(1, "test")
    val queue = JobQueue(pending = Vector(j), nodes = nodes4)

    val cancelled = BatchScheduler.cancelJob(queue, JobId(1), SimTime(5.0))
    cancelled.pending shouldBe empty
    cancelled.completed should have size 1
    cancelled.completed.head.state shouldBe JobState.Cancelled
  }

  it should "cancel running job and release resources" in {
    val j         = job(1, "test")
    val queue     = JobQueue(pending = Vector(j), nodes = nodes4)
    val scheduled = BatchScheduler.fcfs(queue, SimTime(0.0))

    val cancelled = BatchScheduler.cancelJob(scheduled, JobId(1), SimTime(50.0))
    cancelled.running shouldBe empty
    cancelled.completed should have size 1
    cancelled.completed.head.state shouldBe JobState.Cancelled
  }

  // ── Statistics ───────────────────────────────────────────────────

  "BatchScheduler.stats" should "compute scheduling statistics" in {
    val j1 = job(1, "done", submitTime = 0.0).copy(state = JobState.Completed, endTime = Some(SimTime(50.0)))
    val j2 = job(2, "running")
    val j3 = job(3, "pending")
    val queue = JobQueue(
      pending = Vector(j3),
      running = Vector(j2),
      completed = Vector(j1),
      nodes = nodes4
    )

    val s = BatchScheduler.stats(queue)
    s.totalJobs shouldBe 3
    s.pendingJobs shouldBe 1
    s.runningJobs shouldBe 1
    s.completedJobs shouldBe 1
    s.queueDepth shouldBe 1
  }

  // ── JobQueue ─────────────────────────────────────────────────────

  "JobQueue" should "compute utilization" in {
    val n     = node(0, 1000, 8192).allocate(JobId(1), MIPS(500), MegaBytes(2048))
    val queue = JobQueue(nodes = Vector(n))
    queue.utilization shouldBe 0.5 +- 0.01
  }

  it should "compute average wait time" in {
    val j1    = job(1, "a", submitTime = 0.0)
    val j2    = job(2, "b", submitTime = 5.0)
    val queue = JobQueue(pending = Vector(j1, j2))
    queue.averageWaitTime(SimTime(10.0)).value shouldBe 7.5 +- 0.01
  }
