// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

/** Advanced scheduling constraints for VM and workload placement.
  *
  * Models K8s-style affinity/anti-affinity, topology spread constraints, and resource tagging — as immutable ADTs with
  * pure-function evaluators.
  */

/** Labels attached to a host for constraint matching. */
final case class HostLabels(tags: Map[String, String] = Map.empty):
  def get(key: String): Option[String] = tags.get(key)
  def has(key: String): Boolean        = tags.contains(key)
  def matches(required: Map[String, String]): Boolean =
    required.forall { case (k, v) => tags.get(k).contains(v) }
  def zone: Option[String]                       = tags.get("topology.aura.io/zone")
  def rack: Option[String]                       = tags.get("topology.aura.io/rack")
  def +(kv: (String, String)): HostLabels        = copy(tags = tags + kv)
  def ++(other: Map[String, String]): HostLabels = copy(tags = tags ++ other)

object HostLabels:
  val empty: HostLabels              = HostLabels()
  def zone(name: String): HostLabels = HostLabels(Map("topology.aura.io/zone" -> name))
  def rack(name: String): HostLabels = HostLabels(Map("topology.aura.io/rack" -> name))
  def withGpu: HostLabels            = HostLabels(Map("node.aura.io/gpu" -> "true"))
  def withSsd: HostLabels            = HostLabels(Map("node.aura.io/ssd" -> "true"))

/** Labels attached to a VM for affinity matching. */
final case class VmLabels(tags: Map[String, String] = Map.empty):
  def get(key: String): Option[String] = tags.get(key)
  def matches(selector: Map[String, String]): Boolean =
    selector.forall { case (k, v) => tags.get(k).contains(v) }
  def +(kv: (String, String)): VmLabels = copy(tags = tags + kv)

object VmLabels:
  val empty: VmLabels              = VmLabels()
  def app(name: String): VmLabels  = VmLabels(Map("app" -> name))
  def tier(name: String): VmLabels = VmLabels(Map("tier" -> name))

/** Scheduling constraint ADT. */
enum SchedulingConstraint:
  /** Host MUST match all specified labels. */
  case RequiredNodeAffinity(matchLabels: Map[String, String])

  /** Host is preferred if it matches labels; weight [1-100] for scoring. */
  case PreferredNodeAffinity(matchLabels: Map[String, String], weight: Int = 50)

  /** Co-locate with VMs matching the selector on the same topology domain. */
  case PodAffinity(topologyKey: String, matchLabels: Map[String, String])

  /** Spread away from VMs matching the selector across topology domains. */
  case PodAntiAffinity(topologyKey: String, matchLabels: Map[String, String])

  /** Distribute evenly across topology domains with maxSkew tolerance. */
  case TopologySpreadConstraint(topologyKey: String, maxSkew: Int = 1)

  /** Host MUST have a specific resource tag. */
  case ResourceTagRequired(key: String, value: String)

/** Information about an existing VM placement, used for affinity checks. */
final case class PlacedVm(
    vmId: VmId,
    hostId: HostId,
    labels: VmLabels
)

/** Host information for constraint evaluation. */
final case class ConstraintHost(
    hostId: HostId,
    labels: HostLabels,
    canFit: Boolean
)

/** Pure-function constraint evaluator. */
object ConstraintEvaluator:

  import SchedulingConstraint.*

  /** Check whether a host satisfies ALL hard constraints. */
  def satisfiesHard(
      host: ConstraintHost,
      constraints: Vector[SchedulingConstraint],
      placements: Vector[PlacedVm]
  ): Boolean =
    if !host.canFit then false
    else constraints.forall(c => checkHard(host, c, placements))

  private def checkHard(
      host: ConstraintHost,
      constraint: SchedulingConstraint,
      placements: Vector[PlacedVm]
  ): Boolean = constraint match
    case RequiredNodeAffinity(matchLabels) =>
      host.labels.matches(matchLabels)

    case ResourceTagRequired(key, value) =>
      host.labels.get(key).contains(value)

    case PodAffinity(topologyKey, matchLabels) =>
      // At least one matching VM must be on a host in the same topology domain
      val hostDomain = host.labels.get(topologyKey)
      hostDomain.isDefined && {
        val matchingVms = placements.filter(_.labels.matches(matchLabels))
        matchingVms.isEmpty || matchingVms.exists(p => hostDomainOf(p.hostId, topologyKey, host) == hostDomain)
      }

    case PodAntiAffinity(topologyKey, matchLabels) =>
      // No matching VM should be on a host in the same topology domain
      val hostDomain = host.labels.get(topologyKey)
      hostDomain match
        case None => true // No domain label => no conflict
        case Some(domain) =>
          val matchingVms = placements.filter(_.labels.matches(matchLabels))
          !matchingVms.exists(p => hostDomainOf(p.hostId, topologyKey, host) == Some(domain))

    // Soft constraints are not hard — always pass
    case _: PreferredNodeAffinity    => true
    case _: TopologySpreadConstraint => true

  /** Score a host based on soft constraints. Returns 0.0 (worst) to 1.0 (best). */
  def scoreHost(
      host: ConstraintHost,
      constraints: Vector[SchedulingConstraint],
      placements: Vector[PlacedVm],
      allHosts: Vector[ConstraintHost] = Vector.empty
  ): Double =
    val softConstraints = constraints.collect {
      case p: PreferredNodeAffinity    => p
      case t: TopologySpreadConstraint => t
    }
    if softConstraints.isEmpty then 1.0
    else
      val totalWeight = softConstraints.map {
        case p: PreferredNodeAffinity    => p.weight
        case _: TopologySpreadConstraint => 50
        case _                           => 0
      }.sum
      if totalWeight <= 0 then 1.0
      else
        val weightedScore = softConstraints.map { c =>
          val (score, weight) = scoreSoft(host, c, placements, allHosts)
          score * weight
        }.sum
        weightedScore / totalWeight

  private def scoreSoft(
      host: ConstraintHost,
      constraint: SchedulingConstraint,
      placements: Vector[PlacedVm],
      allHosts: Vector[ConstraintHost]
  ): (Double, Int) = constraint match
    case PreferredNodeAffinity(matchLabels, weight) =>
      val score = if host.labels.matches(matchLabels) then 1.0 else 0.0
      (score, weight)

    case TopologySpreadConstraint(topologyKey, maxSkew) =>
      val hostDomain   = host.labels.get(topologyKey).getOrElse("unknown")
      val domainCounts = countPerDomain(topologyKey, placements, allHosts)
      val currentCount = domainCounts.getOrElse(hostDomain, 0)
      val minCount     = if domainCounts.isEmpty then 0 else domainCounts.values.min
      // Lower count = better score (prefer underrepresented domains)
      val skew = currentCount - minCount
      val score =
        if skew >= maxSkew then 0.0
        else 1.0 - (skew.toDouble / math.max(1, maxSkew))
      (score, 50)

    case _ => (1.0, 0) // Hard constraints don't contribute to scoring

  /** Filter hosts by hard constraints and rank by soft constraint scores. */
  def filterAndRank(
      hosts: Vector[ConstraintHost],
      constraints: Vector[SchedulingConstraint],
      placements: Vector[PlacedVm]
  ): Vector[(HostId, Double)] =
    hosts
      .filter(h => satisfiesHard(h, constraints, placements))
      .map(h => (h.hostId, scoreHost(h, constraints, placements, hosts)))
      .sortBy(-_._2) // Highest score first

  /** Count VMs per topology domain. */
  private def countPerDomain(
      topologyKey: String,
      placements: Vector[PlacedVm],
      allHosts: Vector[ConstraintHost]
  ): Map[String, Int] =
    val hostDomainMap = allHosts.flatMap(h => h.labels.get(topologyKey).map(d => h.hostId -> d)).toMap
    placements
      .flatMap(p => hostDomainMap.get(p.hostId).map(d => d))
      .groupBy(identity)
      .map((d, vs) => d -> vs.size)

  /** Helper: find the topology domain of a host that contains a placed VM. */
  private def hostDomainOf(
      hostId: HostId,
      topologyKey: String,
      currentHost: ConstraintHost
  ): Option[String] =
    if hostId == currentHost.hostId then currentHost.labels.get(topologyKey)
    else None // We only know the current host's labels in this context

  /** Overload that uses a full host map for lookups. */
  def satisfiesHardWithHostMap(
      host: ConstraintHost,
      constraints: Vector[SchedulingConstraint],
      placements: Vector[PlacedVm],
      hostMap: Map[HostId, ConstraintHost]
  ): Boolean =
    if !host.canFit then false
    else constraints.forall(c => checkHardWithMap(host, c, placements, hostMap))

  private def checkHardWithMap(
      host: ConstraintHost,
      constraint: SchedulingConstraint,
      placements: Vector[PlacedVm],
      hostMap: Map[HostId, ConstraintHost]
  ): Boolean = constraint match
    case RequiredNodeAffinity(matchLabels) =>
      host.labels.matches(matchLabels)

    case ResourceTagRequired(key, value) =>
      host.labels.get(key).contains(value)

    case PodAffinity(topologyKey, matchLabels) =>
      val hostDomain = host.labels.get(topologyKey)
      hostDomain.isDefined && {
        val matchingVms = placements.filter(_.labels.matches(matchLabels))
        matchingVms.isEmpty || matchingVms.exists { p =>
          hostMap.get(p.hostId).flatMap(_.labels.get(topologyKey)) == hostDomain
        }
      }

    case PodAntiAffinity(topologyKey, matchLabels) =>
      val hostDomain = host.labels.get(topologyKey)
      hostDomain match
        case None => true
        case Some(domain) =>
          val matchingVms = placements.filter(_.labels.matches(matchLabels))
          !matchingVms.exists { p =>
            hostMap.get(p.hostId).flatMap(_.labels.get(topologyKey)).contains(domain)
          }

    case _: PreferredNodeAffinity    => true
    case _: TopologySpreadConstraint => true

  /** Filter and rank using full host map for accurate affinity checks. */
  def filterAndRankWithHostMap(
      hosts: Vector[ConstraintHost],
      constraints: Vector[SchedulingConstraint],
      placements: Vector[PlacedVm]
  ): Vector[(HostId, Double)] =
    val hostMap = hosts.map(h => h.hostId -> h).toMap
    hosts
      .filter(h => satisfiesHardWithHostMap(h, constraints, placements, hostMap))
      .map(h => (h.hostId, scoreHost(h, constraints, placements, hosts)))
      .sortBy(-_._2)
