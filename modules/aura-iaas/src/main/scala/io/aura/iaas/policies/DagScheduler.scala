// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import io.aura.core.types.*
import io.aura.core.events.WorkloadSpec

/** Utilities for DAG-based workload scheduling.
  *
  * Provides topological sorting, critical path analysis, and validation for workloads with predecessor dependencies.
  */
object DagScheduler:

  /** Result of DAG validation. */
  sealed trait DagValidation
  case object DagValid                                                            extends DagValidation
  case class DagCycleDetected(involvedIds: Set[WorkloadId])                       extends DagValidation
  case class DagMissingPredecessor(workloadId: WorkloadId, missingId: WorkloadId) extends DagValidation

  /** Validate a set of workloads for DAG correctness. Checks for missing predecessors and cycles.
    */
  def validate(workloads: Vector[WorkloadSpec]): DagValidation =
    val ids = workloads.map(_.id).toSet
    // Check for missing predecessors
    val missingPred = workloads.collectFirst {
      case wl if wl.predecessors.exists(predId => !ids.contains(predId)) =>
        val predId = wl.predecessors.find(p => !ids.contains(p)).get
        DagMissingPredecessor(wl.id, predId)
    }
    missingPred match
      case Some(err) => err
      case None      =>
        // Check for cycles using immutable Kahn's algorithm
        val adjList         = workloads.map(wl => wl.id -> wl.predecessors).toMap
        val initialInDegree = adjList.map((id, preds) => id -> preds.size)
        val successors = workloads.foldLeft(Map.empty[WorkloadId, Set[WorkloadId]]) { (acc, wl) =>
          wl.predecessors.foldLeft(acc) { (a, predId) =>
            a.updated(predId, a.getOrElse(predId, Set.empty) + wl.id)
          }
        }
        val initialQueue = initialInDegree.filter(_._2 == 0).keys.toVector

        @scala.annotation.tailrec
        def processQueue(
            queue: Vector[WorkloadId],
            inDegree: Map[WorkloadId, Int],
            visited: Int
        ): (Int, Map[WorkloadId, Int]) =
          if queue.isEmpty then (visited, inDegree)
          else
            val current = queue.head
            val rest    = queue.tail
            val succs   = successors.getOrElse(current, Set.empty)
            val (updatedInDegree, newReady) = succs.foldLeft((inDegree, Vector.empty[WorkloadId])) {
              case ((deg, ready), succId) =>
                val newDeg     = deg(succId) - 1
                val updatedDeg = deg.updated(succId, newDeg)
                if newDeg == 0 then (updatedDeg, ready :+ succId)
                else (updatedDeg, ready)
            }
            processQueue(rest ++ newReady, updatedInDegree, visited + 1)

        val (totalVisited, finalInDegree) = processQueue(initialQueue, initialInDegree, 0)
        if totalVisited == workloads.size then DagValid
        else DagCycleDetected(finalInDegree.filter(_._2 > 0).keys.toSet)

  /** Topological sort of workloads by dependencies (Kahn's algorithm). Returns workloads in execution order
    * (predecessors before successors).
    */
  def topologicalSort(workloads: Vector[WorkloadSpec]): Vector[WorkloadSpec] =
    val byId            = workloads.map(wl => wl.id -> wl).toMap
    val initialInDegree = workloads.map(wl => wl.id -> wl.predecessors.size).toMap
    val successors = workloads.foldLeft(Map.empty[WorkloadId, Vector[WorkloadId]]) { (acc, wl) =>
      wl.predecessors.foldLeft(acc) { (a, predId) =>
        a.updated(predId, a.getOrElse(predId, Vector.empty) :+ wl.id)
      }
    }
    val initialQueue = initialInDegree.filter(_._2 == 0).keys.toVector

    @scala.annotation.tailrec
    def processQueue(
        queue: Vector[WorkloadId],
        inDegree: Map[WorkloadId, Int],
        result: Vector[WorkloadSpec]
    ): Vector[WorkloadSpec] =
      if queue.isEmpty then result
      else
        val current = queue.head
        val rest    = queue.tail
        val succs   = successors.getOrElse(current, Vector.empty)
        val (updatedInDegree, newReady) = succs.foldLeft((inDegree, Vector.empty[WorkloadId])) {
          case ((deg, ready), succId) =>
            val newDeg     = deg(succId) - 1
            val updatedDeg = deg.updated(succId, newDeg)
            if newDeg == 0 then (updatedDeg, ready :+ succId)
            else (updatedDeg, ready)
        }
        processQueue(rest ++ newReady, updatedInDegree, result :+ byId(current))

    processQueue(initialQueue, initialInDegree, Vector.empty)

  /** Compute the critical path length (longest path in the DAG by MI). */
  def criticalPathLength(workloads: Vector[WorkloadSpec]): MI =
    if workloads.isEmpty then MI.Zero
    else
      val sorted = topologicalSort(workloads)
      val distances = sorted.foldLeft(Map.empty[WorkloadId, Double]) { (dist, wl) =>
        val predMax =
          if wl.predecessors.isEmpty then 0.0
          else wl.predecessors.map(pid => dist.getOrElse(pid, 0.0)).max
        dist.updated(wl.id, predMax + wl.length.value)
      }
      MI(distances.values.max)

  /** Identify root tasks (no predecessors). */
  def rootTasks(workloads: Vector[WorkloadSpec]): Vector[WorkloadSpec] =
    workloads.filter(_.predecessors.isEmpty)

  /** Identify leaf tasks (no successors). */
  def leafTasks(workloads: Vector[WorkloadSpec]): Vector[WorkloadSpec] =
    val allPredecessors = workloads.flatMap(_.predecessors).toSet
    workloads.filterNot(wl => allPredecessors.contains(wl.id))
