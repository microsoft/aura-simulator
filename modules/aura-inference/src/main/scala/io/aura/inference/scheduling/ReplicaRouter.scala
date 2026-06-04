// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.inference.scheduling

import io.aura.core.types.*
import io.aura.core.events.{EntityRef, InferenceRequestSpec}

/** Cluster-level request routing across inference engine replicas.
  *
  * Routes incoming requests to the least loaded or most energy-efficient replica.
  */
object ReplicaRouter:

  /** Replica load information. */
  final case class ReplicaInfo(
      engineRef: EntityRef,
      currentBatchSize: Int,
      maxBatchSize: Int,
      kvCacheUtilization: Double,
      currentPowerWatts: Watts
  ):
    def loadFraction: Double = currentBatchSize.toDouble / maxBatchSize.max(1)

  /** Routing policy function type. */
  type RoutingPolicy = (Vector[ReplicaInfo], InferenceRequestSpec) => Option[Int]

  /** Least-loaded routing: route to replica with lowest batch occupancy. */
  val leastLoaded: RoutingPolicy = (replicas, _) =>
    if replicas.isEmpty then None
    else Some(replicas.zipWithIndex.minBy(_._1.loadFraction)._2)

  /** Round-robin routing. */
  def roundRobin(counter: Long): RoutingPolicy = (replicas, _) =>
    if replicas.isEmpty then None
    else Some((counter % replicas.size).toInt)

  /** Power-aware routing: route to replica with lowest current power consumption. Helps consolidate load to fewer GPUs,
    * allowing others to enter low-power states.
    */
  val powerAware: RoutingPolicy = (replicas, _) =>
    if replicas.isEmpty then None
    else
      // Prefer replicas that are already active (avoid cold starts) but least loaded
      val active = replicas.zipWithIndex.filter(_._1.currentBatchSize > 0)
      if active.nonEmpty then
        val withCapacity = active.filter(_._1.loadFraction < 0.9)
        if withCapacity.nonEmpty then Some(withCapacity.minBy(_._1.loadFraction)._2)
        else Some(replicas.zipWithIndex.minBy(_._1.loadFraction)._2) // All full, pick least loaded
      else Some(replicas.zipWithIndex.minBy(_._1.currentPowerWatts.value)._2)
