// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import io.aura.core.types.*

/** Horizontal scaling context provided to the policy. */
final case class HorizontalScalingContext(
    activeVmCount: Int,
    pendingWorkloads: Int,
    runningWorkloads: Int,
    avgCpuUtilization: Utilization,
    currentTime: SimTime
)

/** State threaded through predictive scaling evaluations. */
final case class PredictiveScalingState(
    history: TimeSeries = TimeSeries.empty,
    lastScaleTime: SimTime = SimTime.Zero
)

/** A horizontal scaling policy returns Some(vmSpec) to create a new VM, None otherwise. */
type HorizontalScalingPolicy = HorizontalScalingContext => Option[ResourceSpec]

object HorizontalScalingPolicy:
  /** Scale out when average CPU utilization exceeds threshold. */
  def cpuThreshold(
      upperThreshold: Double = 0.8,
      vmTemplate: ResourceSpec
  ): HorizontalScalingPolicy =
    ctx =>
      if ctx.avgCpuUtilization.value > upperThreshold && ctx.activeVmCount > 0 then Some(vmTemplate)
      else None

  /** Scale out when pending workloads exceed a count. */
  def queueLength(
      maxPending: Int = 5,
      vmTemplate: ResourceSpec
  ): HorizontalScalingPolicy =
    ctx =>
      if ctx.pendingWorkloads > maxPending then Some(vmTemplate)
      else None

  /** Target tracking auto-scaling: scale out when CPU exceeds target + tolerance, similar to AWS Target Tracking
    * Scaling Policies.
    */
  def targetUtilization(
      target: Double = 0.7,
      tolerance: Double = 0.1,
      vmTemplate: ResourceSpec
  ): HorizontalScalingPolicy =
    ctx =>
      if ctx.activeVmCount > 0 && ctx.avgCpuUtilization.value > target + tolerance then Some(vmTemplate)
      else None

  /** Scale out when workloads-per-VM ratio exceeds threshold. */
  def workloadRatio(
      maxWorkloadsPerVm: Double = 5.0,
      vmTemplate: ResourceSpec
  ): HorizontalScalingPolicy =
    ctx =>
      if ctx.activeVmCount > 0 then
        val ratio = (ctx.pendingWorkloads + ctx.runningWorkloads).toDouble / ctx.activeVmCount
        if ratio > maxWorkloadsPerVm then Some(vmTemplate)
        else None
      else if ctx.pendingWorkloads > 0 then Some(vmTemplate)
      else None

  /** Compose multiple scaling policies — first match wins. */
  def combined(policies: HorizontalScalingPolicy*): HorizontalScalingPolicy =
    ctx => policies.iterator.map(_(ctx)).collectFirst { case Some(spec) => spec }

  /** Pure predictive evaluation: returns scaling decision + updated state. */
  def predictiveEval(
      config: PredictiveScalingConfig,
      vmTemplate: ResourceSpec,
      state: PredictiveScalingState,
      ctx: HorizontalScalingContext
  ): (Option[ResourceSpec], PredictiveScalingState) =
    val updatedHistory = state.history.append(ctx.currentTime, ctx.avgCpuUtilization.value)
    val decision = PredictiveScalingPolicy.decide(
      updatedHistory,
      ctx.activeVmCount,
      config,
      state.lastScaleTime,
      ctx.currentTime
    )
    decision match
      case ScalingDecision.ScaleUp(_, _) =>
        (Some(vmTemplate), PredictiveScalingState(updatedHistory, ctx.currentTime))
      case _ =>
        (None, PredictiveScalingState(updatedHistory, state.lastScaleTime))

  /** Predictive auto-scaling using ML-based forecasting. Tracks utilization history and uses PredictiveScalingPolicy to
    * forecast demand. Scales out proactively before overload occurs.
    */
  @deprecated("Use predictiveEval with explicit state threading instead", "1.0")
  def predictive(
      config: PredictiveScalingConfig = PredictiveScalingConfig(),
      vmTemplate: ResourceSpec
  ): HorizontalScalingPolicy =
    var history       = TimeSeries.empty
    var lastScaleTime = SimTime.Zero
    ctx =>
      history = history.append(ctx.currentTime, ctx.avgCpuUtilization.value)
      val decision = PredictiveScalingPolicy.decide(
        history,
        ctx.activeVmCount,
        config,
        lastScaleTime,
        ctx.currentTime
      )
      decision match
        case ScalingDecision.ScaleUp(_, _) =>
          lastScaleTime = ctx.currentTime
          Some(vmTemplate)
        case _ => None

  /** No auto-scaling (default behavior). */
  val none: HorizontalScalingPolicy = _ => None
