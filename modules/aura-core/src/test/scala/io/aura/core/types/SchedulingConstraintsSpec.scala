// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SchedulingConstraintsSpec extends AnyFlatSpec with Matchers:

  import SchedulingConstraint.*

  // ── Helper fixtures ──────────────────────────────────────────────────────

  private val zoneA   = HostLabels.zone("us-east-1a") ++ Map("topology.aura.io/rack" -> "rack-1")
  private val zoneB   = HostLabels.zone("us-east-1b") ++ Map("topology.aura.io/rack" -> "rack-2")
  private val gpuHost = HostLabels.withGpu ++ Map("topology.aura.io/zone" -> "us-east-1a")

  private def host(id: Long, labels: HostLabels, canFit: Boolean = true) =
    ConstraintHost(HostId(id), labels, canFit)

  private def placed(vmId: Long, hostId: Long, labels: VmLabels = VmLabels.empty) =
    PlacedVm(VmId(vmId), HostId(hostId), labels)

  // ── HostLabels ───────────────────────────────────────────────────────────

  "HostLabels" should "match required labels" in {
    val labels = HostLabels.zone("us-east-1a") + ("tier" -> "compute")
    labels.matches(Map("topology.aura.io/zone" -> "us-east-1a")) shouldBe true
    labels.matches(Map("tier" -> "compute")) shouldBe true
    labels.matches(Map("tier" -> "storage")) shouldBe false
  }

  it should "extract zone and rack" in {
    zoneA.zone shouldBe Some("us-east-1a")
    zoneA.rack shouldBe Some("rack-1")
    HostLabels.empty.zone shouldBe None
  }

  it should "support preset constructors" in {
    HostLabels.withGpu.get("node.aura.io/gpu") shouldBe Some("true")
    HostLabels.withSsd.get("node.aura.io/ssd") shouldBe Some("true")
  }

  // ── VmLabels ─────────────────────────────────────────────────────────────

  "VmLabels" should "match selectors" in {
    val labels = VmLabels.app("web") + ("version" -> "v2")
    labels.matches(Map("app" -> "web")) shouldBe true
    labels.matches(Map("app" -> "api")) shouldBe false
    labels.matches(Map("app" -> "web", "version" -> "v2")) shouldBe true
  }

  // ── RequiredNodeAffinity ────────────────────────────────────────────────

  "RequiredNodeAffinity" should "reject hosts without matching labels" in {
    val h           = host(1, HostLabels.zone("us-west-1a"))
    val constraints = Vector(RequiredNodeAffinity(Map("topology.aura.io/zone" -> "us-east-1a")))
    ConstraintEvaluator.satisfiesHard(h, constraints, Vector.empty) shouldBe false
  }

  it should "accept hosts with matching labels" in {
    val h           = host(1, zoneA)
    val constraints = Vector(RequiredNodeAffinity(Map("topology.aura.io/zone" -> "us-east-1a")))
    ConstraintEvaluator.satisfiesHard(h, constraints, Vector.empty) shouldBe true
  }

  it should "reject hosts that cannot fit" in {
    val h           = host(1, zoneA, canFit = false)
    val constraints = Vector(RequiredNodeAffinity(Map("topology.aura.io/zone" -> "us-east-1a")))
    ConstraintEvaluator.satisfiesHard(h, constraints, Vector.empty) shouldBe false
  }

  // ── ResourceTagRequired ─────────────────────────────────────────────────

  "ResourceTagRequired" should "require specific resource tags" in {
    val h           = host(1, gpuHost)
    val constraints = Vector(ResourceTagRequired("node.aura.io/gpu", "true"))
    ConstraintEvaluator.satisfiesHard(h, constraints, Vector.empty) shouldBe true
  }

  it should "reject hosts without the tag" in {
    val h           = host(1, zoneA)
    val constraints = Vector(ResourceTagRequired("node.aura.io/gpu", "true"))
    ConstraintEvaluator.satisfiesHard(h, constraints, Vector.empty) shouldBe false
  }

  // ── PodAntiAffinity ─────────────────────────────────────────────────────

  "PodAntiAffinity" should "reject hosts in same zone as matching VMs" in {
    val hosts       = Vector(host(1, zoneA), host(2, zoneB))
    val placements  = Vector(placed(100, 1, VmLabels.app("web")))
    val constraints = Vector(PodAntiAffinity("topology.aura.io/zone", Map("app" -> "web")))

    val ranked = ConstraintEvaluator.filterAndRankWithHostMap(hosts, constraints, placements)
    ranked.map(_._1) should contain(HostId(2))
    ranked.map(_._1) should not contain HostId(1)
  }

  it should "allow hosts in different zones" in {
    val h           = host(2, zoneB)
    val placements  = Vector(placed(100, 1, VmLabels.app("web")))
    val hostMap     = Map(HostId(1) -> host(1, zoneA), HostId(2) -> h)
    val constraints = Vector(PodAntiAffinity("topology.aura.io/zone", Map("app" -> "web")))

    ConstraintEvaluator.satisfiesHardWithHostMap(h, constraints, placements, hostMap) shouldBe true
  }

  // ── PodAffinity ─────────────────────────────────────────────────────────

  "PodAffinity" should "prefer hosts in same zone as matching VMs" in {
    val hosts       = Vector(host(1, zoneA), host(2, zoneB))
    val placements  = Vector(placed(100, 1, VmLabels.app("db")))
    val constraints = Vector(PodAffinity("topology.aura.io/zone", Map("app" -> "db")))

    val ranked = ConstraintEvaluator.filterAndRankWithHostMap(hosts, constraints, placements)
    // Host 1 is in same zone as the db VM
    ranked.map(_._1) should contain(HostId(1))
  }

  it should "allow any host when no matching VMs exist" in {
    val h           = host(2, zoneB)
    val constraints = Vector(PodAffinity("topology.aura.io/zone", Map("app" -> "db")))
    ConstraintEvaluator.satisfiesHard(h, constraints, Vector.empty) shouldBe true
  }

  // ── PreferredNodeAffinity ───────────────────────────────────────────────

  "PreferredNodeAffinity" should "score matching hosts higher" in {
    val h1          = host(1, gpuHost)
    val h2          = host(2, zoneB)
    val constraints = Vector(PreferredNodeAffinity(Map("node.aura.io/gpu" -> "true"), weight = 100))

    val score1 = ConstraintEvaluator.scoreHost(h1, constraints, Vector.empty)
    val score2 = ConstraintEvaluator.scoreHost(h2, constraints, Vector.empty)
    score1 should be > score2
  }

  it should "not reject non-matching hosts (soft constraint)" in {
    val h           = host(1, zoneA)
    val constraints = Vector(PreferredNodeAffinity(Map("node.aura.io/gpu" -> "true")))
    ConstraintEvaluator.satisfiesHard(h, constraints, Vector.empty) shouldBe true
  }

  // ── TopologySpreadConstraint ────────────────────────────────────────────

  "TopologySpreadConstraint" should "prefer underrepresented domains" in {
    val hosts       = Vector(host(1, zoneA), host(2, zoneB))
    val placements  = Vector(placed(100, 1, VmLabels.empty), placed(101, 1, VmLabels.empty))
    val constraints = Vector(TopologySpreadConstraint("topology.aura.io/zone", maxSkew = 1))

    val ranked = ConstraintEvaluator.filterAndRank(hosts, constraints, placements)
    // Zone B has 0 VMs, Zone A has 2 — zone B should rank higher
    ranked.head._1 shouldBe HostId(2)
  }

  it should "score evenly when domains are balanced" in {
    val hosts       = Vector(host(1, zoneA), host(2, zoneB))
    val placements  = Vector(placed(100, 1, VmLabels.empty), placed(101, 2, VmLabels.empty))
    val constraints = Vector(TopologySpreadConstraint("topology.aura.io/zone"))

    val ranked = ConstraintEvaluator.filterAndRank(hosts, constraints, placements)
    // Both zones have 1 VM — scores should be equal
    ranked(0)._2 shouldBe ranked(1)._2 +- 0.001
  }

  // ── filterAndRank ───────────────────────────────────────────────────────

  "filterAndRank" should "combine hard and soft constraints" in {
    val hosts = Vector(
      host(1, zoneA + ("node.aura.io/gpu" -> "true")),
      host(2, zoneB + ("node.aura.io/gpu" -> "true")),
      host(3, zoneA) // no GPU
    )
    val constraints = Vector(
      ResourceTagRequired("node.aura.io/gpu", "true"),
      PreferredNodeAffinity(Map("topology.aura.io/zone" -> "us-east-1b"), weight = 80)
    )
    val ranked = ConstraintEvaluator.filterAndRank(hosts, constraints, Vector.empty)
    // Host 3 filtered out (no GPU). Host 2 preferred (zone B). Host 1 acceptable.
    ranked should have size 2
    ranked.head._1 shouldBe HostId(2)
  }

  it should "return empty when no hosts satisfy hard constraints" in {
    val hosts       = Vector(host(1, zoneA), host(2, zoneB))
    val constraints = Vector(ResourceTagRequired("node.aura.io/gpu", "true"))
    ConstraintEvaluator.filterAndRank(hosts, constraints, Vector.empty) shouldBe empty
  }

  // ── No constraints ──────────────────────────────────────────────────────

  "ConstraintEvaluator" should "pass all hosts when no constraints specified" in {
    val hosts  = Vector(host(1, zoneA), host(2, zoneB))
    val ranked = ConstraintEvaluator.filterAndRank(hosts, Vector.empty, Vector.empty)
    ranked should have size 2
    ranked.map(_._2).foreach(_ shouldBe 1.0)
  }

  it should "score 1.0 when only hard constraints present" in {
    val h           = host(1, zoneA)
    val constraints = Vector(RequiredNodeAffinity(Map("topology.aura.io/zone" -> "us-east-1a")))
    ConstraintEvaluator.scoreHost(h, constraints, Vector.empty) shouldBe 1.0
  }
