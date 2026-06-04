// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.types.*
import io.aura.core.events.WorkloadSpec
import io.aura.iaas.policies.DagScheduler.*

class DagSchedulerSpec extends AnyFlatSpec with Matchers:

  private def task(id: Long, length: Double, preds: Set[Long] = Set.empty): WorkloadSpec =
    WorkloadSpec
      .simple(WorkloadId(id), MI(length), PEs(1))
      .copy(
        predecessors = preds.map(WorkloadId(_))
      )

  // ─── validate ───────────────────────────────────────────────────────

  "DagScheduler.validate" should "accept a valid DAG" in {
    val workloads = Vector(
      task(0, 1000),
      task(1, 2000, Set(0)),
      task(2, 1500, Set(0)),
      task(3, 500, Set(1, 2))
    )
    DagScheduler.validate(workloads) shouldBe DagValid
  }

  it should "detect missing predecessors" in {
    val workloads = Vector(
      task(0, 1000),
      task(1, 2000, Set(99)) // 99 doesn't exist
    )
    DagScheduler.validate(workloads) shouldBe a[DagMissingPredecessor]
  }

  it should "detect cycles" in {
    val workloads = Vector(
      task(0, 1000, Set(2)),
      task(1, 2000, Set(0)),
      task(2, 1500, Set(1))
    )
    DagScheduler.validate(workloads) shouldBe a[DagCycleDetected]
  }

  // ─── topologicalSort ───────────────────────────────────────────────

  "DagScheduler.topologicalSort" should "order tasks by dependencies" in {
    val workloads = Vector(
      task(3, 500, Set(1, 2)),
      task(0, 1000),
      task(1, 2000, Set(0)),
      task(2, 1500, Set(0))
    )
    val sorted = DagScheduler.topologicalSort(workloads)
    sorted should have size 4

    // Task 0 must come before 1, 2, 3
    val indexOf = sorted.map(_.id.value).zipWithIndex.toMap
    indexOf(0) should be < indexOf(1)
    indexOf(0) should be < indexOf(2)
    indexOf(1) should be < indexOf(3)
    indexOf(2) should be < indexOf(3)
  }

  it should "handle independent tasks" in {
    val workloads = Vector(task(0, 1000), task(1, 2000), task(2, 1500))
    val sorted    = DagScheduler.topologicalSort(workloads)
    sorted should have size 3
  }

  // ─── criticalPathLength ─────────────────────────────────────────────

  "DagScheduler.criticalPathLength" should "compute longest path through DAG" in {
    // Diamond: 0→1, 0→2, 1→3, 2→3
    // Path 0→1→3: 1000+2000+500 = 3500
    // Path 0→2→3: 1000+1500+500 = 3000
    // Critical path = 3500
    val workloads = Vector(
      task(0, 1000),
      task(1, 2000, Set(0)),
      task(2, 1500, Set(0)),
      task(3, 500, Set(1, 2))
    )
    DagScheduler.criticalPathLength(workloads).value shouldBe 3500.0
  }

  it should "return single task length for no dependencies" in {
    val workloads = Vector(task(0, 5000))
    DagScheduler.criticalPathLength(workloads).value shouldBe 5000.0
  }

  it should "return zero for empty workloads" in {
    DagScheduler.criticalPathLength(Vector.empty).value shouldBe 0.0
  }

  // ─── rootTasks / leafTasks ──────────────────────────────────────────

  "DagScheduler.rootTasks" should "return tasks with no predecessors" in {
    val workloads = Vector(
      task(0, 1000),
      task(1, 2000, Set(0)),
      task(2, 1500)
    )
    val roots = DagScheduler.rootTasks(workloads)
    roots.map(_.id.value).toSet shouldBe Set(0L, 2L)
  }

  "DagScheduler.leafTasks" should "return tasks with no successors" in {
    val workloads = Vector(
      task(0, 1000),
      task(1, 2000, Set(0)),
      task(2, 1500, Set(0))
    )
    val leaves = DagScheduler.leafTasks(workloads)
    leaves.map(_.id.value).toSet shouldBe Set(1L, 2L)
  }
