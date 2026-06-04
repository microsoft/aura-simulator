// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.containers

import io.aura.core.types.*

/** Horizontal Pod Autoscaler (HPA) context — metrics observed at evaluation time. */
final case class HpaContext(
    currentReplicas: Int,
    targetCpuUtilization: Double,
    observedCpuUtilization: Double,
    minReplicas: Int,
    maxReplicas: Int
)

/** HPA scaling decision. */
enum HpaDecision:
  case ScaleUp(targetReplicas: Int)
  case ScaleDown(targetReplicas: Int)
  case NoChange

/** Horizontal Pod Autoscaler policies.
  *
  * Models Kubernetes HPA behavior: adjusts replica count based on observed vs target CPU utilization.
  *
  * Formula (matches K8s): desiredReplicas = ceil(currentReplicas * (current / target))
  */
type HpaPolicy = HpaContext => HpaDecision

object HpaPolicy:

  /** Standard CPU-based HPA following the Kubernetes formula.
    *
    * desiredReplicas = ceil(currentReplicas * (observedCpu / targetCpu)) Clamped to [minReplicas, maxReplicas].
    *
    * @param tolerance
    *   Dead zone around target to prevent flapping (default 10%)
    */
  def cpuBased(tolerance: Double = 0.1): HpaPolicy =
    ctx =>
      if ctx.currentReplicas == 0 then HpaDecision.NoChange
      else
        val ratio = ctx.observedCpuUtilization / ctx.targetCpuUtilization
        if ratio > 1.0 + tolerance then
          val desired = math.ceil(ctx.currentReplicas * ratio).toInt
          val clamped = math.min(desired, ctx.maxReplicas)
          if clamped > ctx.currentReplicas then HpaDecision.ScaleUp(clamped)
          else HpaDecision.NoChange
        else if ratio < 1.0 - tolerance then
          val desired = math.ceil(ctx.currentReplicas * ratio).toInt
          val clamped = math.max(desired, ctx.minReplicas)
          if clamped < ctx.currentReplicas then HpaDecision.ScaleDown(clamped)
          else HpaDecision.NoChange
        else HpaDecision.NoChange

  /** Threshold-based HPA: scale up above high watermark, down below low watermark. */
  def threshold(
      scaleUpThreshold: Double = 0.8,
      scaleDownThreshold: Double = 0.3,
      scaleUpStep: Int = 1,
      scaleDownStep: Int = 1
  ): HpaPolicy =
    ctx =>
      if ctx.observedCpuUtilization > scaleUpThreshold then
        val target = math.min(ctx.currentReplicas + scaleUpStep, ctx.maxReplicas)
        if target > ctx.currentReplicas then HpaDecision.ScaleUp(target)
        else HpaDecision.NoChange
      else if ctx.observedCpuUtilization < scaleDownThreshold then
        val target = math.max(ctx.currentReplicas - scaleDownStep, ctx.minReplicas)
        if target < ctx.currentReplicas then HpaDecision.ScaleDown(target)
        else HpaDecision.NoChange
      else HpaDecision.NoChange

  /** No autoscaling (always returns NoChange). */
  val none: HpaPolicy = _ => HpaDecision.NoChange

/** Vertical Pod Autoscaler (VPA) context. */
final case class VpaContext(
    currentCpuRequest: MIPS,
    currentMemoryRequest: MegaBytes,
    observedCpuUsage: MIPS,
    observedMemoryUsage: MegaBytes,
    cpuLimit: MIPS,
    memoryLimit: MegaBytes
)

/** VPA recommendation for resource adjustment. */
final case class VpaRecommendation(
    cpuRequest: MIPS,
    memoryRequest: MegaBytes,
    cpuLimit: MIPS,
    memoryLimit: MegaBytes,
    changed: Boolean
)

/** Vertical Pod Autoscaler policies.
  *
  * Models Kubernetes VPA behavior: adjusts container resource requests/limits based on observed usage.
  */
type VpaPolicy = VpaContext => VpaRecommendation

object VpaPolicy:

  /** Target-utilization VPA: set requests so observed usage equals target fraction.
    *
    * @param targetCpuUtilization
    *   Target CPU utilization (e.g. 0.7 = 70%)
    * @param targetMemoryUtilization
    *   Target memory utilization
    * @param headroom
    *   Extra capacity above recommendation (e.g. 0.15 = 15% headroom)
    */
  def targetUtilization(
      targetCpuUtilization: Double = 0.7,
      targetMemoryUtilization: Double = 0.8,
      headroom: Double = 0.15
  ): VpaPolicy =
    ctx =>
      val recommendedCpu = MIPS(ctx.observedCpuUsage.value / targetCpuUtilization * (1.0 + headroom))
      val recommendedMem = MegaBytes(ctx.observedMemoryUsage.value / targetMemoryUtilization * (1.0 + headroom))

      // Clamp to limits
      val clampedCpu = MIPS(math.min(recommendedCpu.value, ctx.cpuLimit.value))
      val clampedMem = MegaBytes(math.min(recommendedMem.value, ctx.memoryLimit.value))

      val cpuChanged = math.abs(clampedCpu.value - ctx.currentCpuRequest.value) > ctx.currentCpuRequest.value * 0.1
      val memChanged =
        math.abs(clampedMem.value - ctx.currentMemoryRequest.value) > ctx.currentMemoryRequest.value * 0.1

      VpaRecommendation(
        cpuRequest = clampedCpu,
        memoryRequest = clampedMem,
        cpuLimit = ctx.cpuLimit,
        memoryLimit = ctx.memoryLimit,
        changed = cpuChanged || memChanged
      )

  /** Bounded VPA: adjusts within min/max bounds using a multiplier. */
  def bounded(
      minCpu: MIPS = MIPS(100.0),
      maxCpu: MIPS = MIPS(100000.0),
      minMemory: MegaBytes = MegaBytes(128.0),
      maxMemory: MegaBytes = MegaBytes(65536.0),
      growthFactor: Double = 1.5,
      shrinkFactor: Double = 0.75,
      upperThreshold: Double = 0.9,
      lowerThreshold: Double = 0.3
  ): VpaPolicy =
    ctx =>
      val cpuUtilization =
        if ctx.currentCpuRequest.value > 0 then ctx.observedCpuUsage.value / ctx.currentCpuRequest.value else 0.0
      val memUtilization =
        if ctx.currentMemoryRequest.value > 0 then ctx.observedMemoryUsage.value / ctx.currentMemoryRequest.value
        else 0.0

      val newCpu =
        if cpuUtilization > upperThreshold then MIPS(math.min(ctx.currentCpuRequest.value * growthFactor, maxCpu.value))
        else if cpuUtilization < lowerThreshold then
          MIPS(math.max(ctx.currentCpuRequest.value * shrinkFactor, minCpu.value))
        else ctx.currentCpuRequest

      val newMem =
        if memUtilization > upperThreshold then
          MegaBytes(math.min(ctx.currentMemoryRequest.value * growthFactor, maxMemory.value))
        else if memUtilization < lowerThreshold then
          MegaBytes(math.max(ctx.currentMemoryRequest.value * shrinkFactor, minMemory.value))
        else ctx.currentMemoryRequest

      val cpuChanged = math.abs(newCpu.value - ctx.currentCpuRequest.value) > 0.01
      val memChanged = math.abs(newMem.value - ctx.currentMemoryRequest.value) > 0.01

      VpaRecommendation(
        cpuRequest = newCpu,
        memoryRequest = newMem,
        cpuLimit = ctx.cpuLimit,
        memoryLimit = ctx.memoryLimit,
        changed = cpuChanged || memChanged
      )

  /** No vertical scaling (returns current values unchanged). */
  val none: VpaPolicy =
    ctx =>
      VpaRecommendation(
        cpuRequest = ctx.currentCpuRequest,
        memoryRequest = ctx.currentMemoryRequest,
        cpuLimit = ctx.cpuLimit,
        memoryLimit = ctx.memoryLimit,
        changed = false
      )
