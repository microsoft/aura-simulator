// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

/** Multi-tenancy and resource quota modeling.
  *
  * Supports tenant isolation, per-tenant resource limits, fair-share scheduling, and noisy-neighbor detection in shared
  * cloud environments.
  */

opaque type TenantId = Long
object TenantId:
  def apply(value: Long): TenantId         = value
  extension (id: TenantId) def value: Long = id
  given Ordering[TenantId] with
    def compare(x: TenantId, y: TenantId): Int = java.lang.Long.compare(x, y)

/** Resource quota for a tenant. */
final case class ResourceQuota(
    maxVms: Int = Int.MaxValue,
    maxPEs: PEs = PEs(Int.MaxValue),
    maxRamMB: MegaBytes = MegaBytes(Double.MaxValue),
    maxStorageMB: MegaBytes = MegaBytes(Double.MaxValue),
    maxBandwidthMbps: Mbps = Mbps(Double.MaxValue),
    priority: Int = 0 // higher = more priority in fair-share
)

object ResourceQuota:
  val unlimited: ResourceQuota = ResourceQuota()
  def small: ResourceQuota     = ResourceQuota(maxVms = 5, maxPEs = PEs(8), maxRamMB = MegaBytes(16384.0))
  def medium: ResourceQuota    = ResourceQuota(maxVms = 20, maxPEs = PEs(32), maxRamMB = MegaBytes(65536.0))
  def large: ResourceQuota     = ResourceQuota(maxVms = 100, maxPEs = PEs(128), maxRamMB = MegaBytes(262144.0))

/** Tracks per-tenant resource usage. */
final case class TenantUsage(
    tenantId: TenantId,
    vmCount: Int = 0,
    allocatedPEs: PEs = PEs(0),
    allocatedRamMB: MegaBytes = MegaBytes.Zero,
    allocatedStorageMB: MegaBytes = MegaBytes.Zero,
    allocatedBandwidthMbps: Mbps = Mbps.Zero
):
  def allocateVm(spec: ResourceSpec): TenantUsage = copy(
    vmCount = vmCount + 1,
    allocatedPEs = PEs(allocatedPEs.value + spec.pes.value),
    allocatedRamMB = allocatedRamMB + spec.ram,
    allocatedStorageMB = allocatedStorageMB + spec.storage,
    allocatedBandwidthMbps = Mbps(allocatedBandwidthMbps.value + spec.bw.value)
  )

  def releaseVm(spec: ResourceSpec): TenantUsage = copy(
    vmCount = math.max(0, vmCount - 1),
    allocatedPEs = PEs(math.max(0, allocatedPEs.value - spec.pes.value)),
    allocatedRamMB = MegaBytes(math.max(0.0, allocatedRamMB.value - spec.ram.value)),
    allocatedStorageMB = MegaBytes(math.max(0.0, allocatedStorageMB.value - spec.storage.value)),
    allocatedBandwidthMbps = Mbps(math.max(0.0, allocatedBandwidthMbps.value - spec.bw.value))
  )

  def withinQuota(quota: ResourceQuota, additionalSpec: ResourceSpec): Boolean =
    (vmCount + 1) <= quota.maxVms &&
      (allocatedPEs.value + additionalSpec.pes.value) <= quota.maxPEs.value &&
      (allocatedRamMB.value + additionalSpec.ram.value) <= quota.maxRamMB.value &&
      (allocatedStorageMB.value + additionalSpec.storage.value) <= quota.maxStorageMB.value &&
      (allocatedBandwidthMbps.value + additionalSpec.bw.value) <= quota.maxBandwidthMbps.value

/** Immutable multi-tenant resource manager. */
final case class TenantManager(
    quotas: Map[TenantId, ResourceQuota],
    usage: Map[TenantId, TenantUsage]
):
  def registerTenant(tenantId: TenantId, quota: ResourceQuota): TenantManager =
    copy(
      quotas = quotas + (tenantId -> quota),
      usage = usage + (tenantId   -> TenantUsage(tenantId))
    )

  def canAllocate(tenantId: TenantId, spec: ResourceSpec): Boolean =
    val tenantUsage = usage.getOrElse(tenantId, TenantUsage(tenantId))
    val tenantQuota = quotas.getOrElse(tenantId, ResourceQuota.unlimited)
    tenantUsage.withinQuota(tenantQuota, spec)

  def allocate(tenantId: TenantId, spec: ResourceSpec): TenantManager =
    val tenantUsage = usage.getOrElse(tenantId, TenantUsage(tenantId))
    copy(usage = usage + (tenantId -> tenantUsage.allocateVm(spec)))

  def release(tenantId: TenantId, spec: ResourceSpec): TenantManager =
    val tenantUsage = usage.getOrElse(tenantId, TenantUsage(tenantId))
    copy(usage = usage + (tenantId -> tenantUsage.releaseVm(spec)))

  def tenantUtilization(tenantId: TenantId): Double =
    val tenantUsage = usage.getOrElse(tenantId, TenantUsage(tenantId))
    val tenantQuota = quotas.getOrElse(tenantId, ResourceQuota.unlimited)
    if tenantQuota.maxPEs.value <= 0 || tenantQuota.maxPEs.value == Int.MaxValue then 0.0
    else tenantUsage.allocatedPEs.value.toDouble / tenantQuota.maxPEs.value

  /** Detect noisy neighbors: tenants using disproportionately more resources. */
  def noisyNeighbors(threshold: Double = 0.8): Vector[TenantId] =
    usage
      .collect {
        case (tid, u)
            if quotas
              .get(tid)
              .exists(q =>
                q.maxPEs.value < Int.MaxValue && u.allocatedPEs.value.toDouble / q.maxPEs.value > threshold
              ) =>
          tid
      }
      .toVector
      .sorted

  /** Fair-share weight for a tenant (based on priority). */
  def fairShareWeight(tenantId: TenantId): Double =
    val priority      = quotas.get(tenantId).map(_.priority).getOrElse(0)
    val totalPriority = quotas.values.map(q => math.max(1, q.priority)).sum
    if totalPriority <= 0 then 1.0 / math.max(1, quotas.size)
    else math.max(1, priority).toDouble / totalPriority

  def allTenants: Vector[TenantId] = quotas.keys.toVector.sorted

object TenantManager:
  val empty: TenantManager = TenantManager(Map.empty, Map.empty)
