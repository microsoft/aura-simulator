// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FederatedTypesSpec extends AnyFlatSpec with Matchers:

  val allTiers = Vector(ExecutionTier.Edge, ExecutionTier.Serverless, ExecutionTier.K8s)

  def ctx(available: Vector[ExecutionTier] = allTiers): FederatedContext =
    FederatedContext(
      taskCpuRequired = MIPS(100.0),
      taskMemRequired = MegaBytes(64.0),
      taskLength = MI(500.0),
      taskDeadline = SimTime(1.0),
      availableTiers = available
    )

  // ─── TierSelectionPolicy ──────────────────────────────────────────

  "TierSelectionPolicy.edgeFirst" should "select Edge when available" in {
    TierSelectionPolicy.edgeFirst(ctx()) shouldBe ExecutionTier.Edge
  }

  it should "select Serverless when Edge is not available" in {
    TierSelectionPolicy.edgeFirst(
      ctx(Vector(ExecutionTier.Serverless, ExecutionTier.K8s))
    ) shouldBe ExecutionTier.Serverless
  }

  it should "select K8s when only K8s is available" in {
    TierSelectionPolicy.edgeFirst(ctx(Vector(ExecutionTier.K8s))) shouldBe ExecutionTier.K8s
  }

  "TierSelectionPolicy.costFirst" should "select K8s when available" in {
    TierSelectionPolicy.costFirst(ctx()) shouldBe ExecutionTier.K8s
  }

  it should "select Edge when K8s is not available" in {
    TierSelectionPolicy.costFirst(ctx(Vector(ExecutionTier.Edge, ExecutionTier.Serverless))) shouldBe ExecutionTier.Edge
  }

  "TierSelectionPolicy.serverlessFirst" should "select Serverless when available" in {
    TierSelectionPolicy.serverlessFirst(ctx()) shouldBe ExecutionTier.Serverless
  }

  it should "fall back to Edge when Serverless is not available" in {
    TierSelectionPolicy.serverlessFirst(ctx(Vector(ExecutionTier.Edge, ExecutionTier.K8s))) shouldBe ExecutionTier.Edge
  }

  "TierSelectionPolicy.latencyFirst" should "behave like edgeFirst" in {
    TierSelectionPolicy.latencyFirst(ctx()) shouldBe ExecutionTier.Edge
  }

  // ─── EscalationPolicy ─────────────────────────────────────────────

  "EscalationPolicy.cascade" should "return next remaining tier" in {
    val remaining = Vector(ExecutionTier.Serverless, ExecutionTier.K8s)
    EscalationPolicy.cascade(ExecutionTier.Edge, "capacity", remaining) shouldBe Some(ExecutionTier.Serverless)
  }

  it should "return None when no tiers remain" in {
    EscalationPolicy.cascade(ExecutionTier.K8s, "failed", Vector.empty) shouldBe None
  }

  "EscalationPolicy.none" should "always return None" in {
    EscalationPolicy.none(ExecutionTier.Edge, "any reason", allTiers) shouldBe None
  }

  "EscalationPolicy.skipFailed" should "skip the failed tier" in {
    val remaining = Vector(ExecutionTier.Edge, ExecutionTier.Serverless, ExecutionTier.K8s)
    EscalationPolicy.skipFailed(ExecutionTier.Edge, "timeout", remaining) shouldBe Some(ExecutionTier.Serverless)
  }

  it should "return None when remaining only contains the failed tier" in {
    EscalationPolicy.skipFailed(ExecutionTier.Edge, "err", Vector(ExecutionTier.Edge)) shouldBe None
  }

  // ─── FederatedTaskId ──────────────────────────────────────────────

  "FederatedTaskId" should "wrap a Long value" in {
    val id = FederatedTaskId(42L)
    id.value shouldBe 42L
    id.toLong shouldBe 42L
  }

  it should "support ordering" in {
    val ids = Vector(FederatedTaskId(3L), FederatedTaskId(1L), FederatedTaskId(2L))
    ids.sorted.map(_.value) shouldBe Vector(1L, 2L, 3L)
  }

  // ─── ExecutionTier ────────────────────────────────────────────────

  "ExecutionTier" should "have three values" in {
    ExecutionTier.values.length shouldBe 3
    ExecutionTier.values should contain allOf (ExecutionTier.Edge, ExecutionTier.Serverless, ExecutionTier.K8s)
  }
