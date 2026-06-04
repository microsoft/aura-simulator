// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

enum ExecutionTier:
  case Edge, Serverless, K8s

final case class FederatedContext(
    taskCpuRequired: MIPS,
    taskMemRequired: MegaBytes,
    taskLength: MI,
    taskDeadline: SimTime,
    availableTiers: Vector[ExecutionTier]
)

type TierSelectionPolicy = FederatedContext => ExecutionTier

object TierSelectionPolicy:
  val edgeFirst: TierSelectionPolicy = ctx =>
    Vector(ExecutionTier.Edge, ExecutionTier.Serverless, ExecutionTier.K8s)
      .find(ctx.availableTiers.contains)
      .getOrElse(ctx.availableTiers.head)

  val latencyFirst: TierSelectionPolicy = edgeFirst

  val costFirst: TierSelectionPolicy = ctx =>
    Vector(ExecutionTier.K8s, ExecutionTier.Edge, ExecutionTier.Serverless)
      .find(ctx.availableTiers.contains)
      .getOrElse(ctx.availableTiers.head)

  val serverlessFirst: TierSelectionPolicy = ctx =>
    Vector(ExecutionTier.Serverless, ExecutionTier.Edge, ExecutionTier.K8s)
      .find(ctx.availableTiers.contains)
      .getOrElse(ctx.availableTiers.head)

type EscalationPolicy = (ExecutionTier, String, Vector[ExecutionTier]) => Option[ExecutionTier]

object EscalationPolicy:
  val cascade: EscalationPolicy = (_, _, remaining) => remaining.headOption

  val none: EscalationPolicy = (_, _, _) => None

  val skipFailed: EscalationPolicy = (failed, _, remaining) => remaining.find(_ != failed)
