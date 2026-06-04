// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import io.aura.core.types.*

/** Context for SLA-aware scaling decisions, extending the base scaling context with SLA-specific metrics.
  */
final case class SlaScalingContext(
    base: HorizontalScalingContext,
    completedWorkloads: Int,
    failedWorkloads: Int,
    avgCompletionTime: SimTime,
    maxCompletionTime: SimTime,
    slaViolationCount: Int
)

/** SLA-aware scaling policy type. */
type SlaScalingPolicy = SlaScalingContext => Option[ResourceSpec]

/** SLA-driven auto-scaling policies.
  *
  * These policies bridge SLA contracts with scaling decisions:
  *   - Scale up when completion times approach SLA limits
  *   - Scale up when availability drops below SLA thresholds
  *   - Scale up proactively based on violation trends
  */
object SlaScalingPolicy:

  /** Scale when average completion time exceeds a fraction of the SLA deadline.
    *
    * @param maxCompletionTime
    *   SLA deadline (from SlaContract.maxCompletionTime)
    * @param triggerFraction
    *   Fraction of deadline that triggers scaling (e.g. 0.8 = 80%)
    * @param vmTemplate
    *   VM spec to create when scaling
    */
  def completionTimeBased(
      maxCompletionTime: SimTime,
      triggerFraction: Double = 0.8,
      vmTemplate: ResourceSpec
  ): SlaScalingPolicy =
    ctx =>
      if ctx.completedWorkloads > 0 then
        val threshold = SimTime(maxCompletionTime.value * triggerFraction)
        if ctx.avgCompletionTime > threshold then Some(vmTemplate)
        else None
      else None

  /** Scale when availability (success rate) drops near SLA minimum.
    *
    * @param minAvailability
    *   SLA minimum availability percentage (e.g. 99.0)
    * @param triggerBuffer
    *   Buffer above minimum that triggers scaling (e.g. 2.0 = scale at 97% if SLA is 95%)
    * @param vmTemplate
    *   VM spec to create when scaling
    */
  def availabilityBased(
      minAvailability: Double,
      triggerBuffer: Double = 2.0,
      vmTemplate: ResourceSpec
  ): SlaScalingPolicy =
    ctx =>
      val total = ctx.completedWorkloads + ctx.failedWorkloads
      if total > 0 then
        val currentAvailability = ctx.completedWorkloads.toDouble / total * 100.0
        if currentAvailability < minAvailability + triggerBuffer then Some(vmTemplate)
        else None
      else None

  /** Scale when SLA violations are detected.
    *
    * @param maxViolationsBeforeScale
    *   Number of violations tolerated before scaling
    * @param vmTemplate
    *   VM spec to create
    */
  def violationBased(
      maxViolationsBeforeScale: Int = 0,
      vmTemplate: ResourceSpec
  ): SlaScalingPolicy =
    ctx =>
      if ctx.slaViolationCount > maxViolationsBeforeScale then Some(vmTemplate)
      else None

  /** Composite SLA policy: combines completion time, availability, and violation checks. First triggered policy wins.
    */
  def composite(
      contract: SlaContract,
      vmTemplate: ResourceSpec,
      completionTimeTrigger: Double = 0.8,
      availabilityBuffer: Double = 2.0,
      maxViolations: Int = 0
  ): SlaScalingPolicy =
    val policies = Vector.newBuilder[SlaScalingPolicy]

    contract.maxCompletionTime.foreach { maxTime =>
      policies += completionTimeBased(SimTime(maxTime), completionTimeTrigger, vmTemplate)
    }
    contract.minAvailability.foreach { minAvail =>
      policies += availabilityBased(minAvail, availabilityBuffer, vmTemplate)
    }
    policies += violationBased(maxViolations, vmTemplate)

    val allPolicies = policies.result()
    ctx => allPolicies.iterator.map(_(ctx)).collectFirst { case Some(spec) => spec }

  /** Adapt an SlaScalingPolicy to work as a HorizontalScalingPolicy. Uses zero defaults for SLA-specific fields not
    * available in HorizontalScalingContext.
    */
  def toHorizontalPolicy(
      slaPolicy: SlaScalingPolicy,
      completedWorkloads: () => Int = () => 0,
      failedWorkloads: () => Int = () => 0,
      avgCompletionTime: () => SimTime = () => SimTime.Zero,
      maxCompletionTime: () => SimTime = () => SimTime.Zero,
      slaViolationCount: () => Int = () => 0
  ): HorizontalScalingPolicy =
    ctx =>
      val slaCtx = SlaScalingContext(
        base = ctx,
        completedWorkloads = completedWorkloads(),
        failedWorkloads = failedWorkloads(),
        avgCompletionTime = avgCompletionTime(),
        maxCompletionTime = maxCompletionTime(),
        slaViolationCount = slaViolationCount()
      )
      slaPolicy(slaCtx)

  /** No SLA-driven scaling. */
  val none: SlaScalingPolicy = _ => None
