// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.containers

import io.aura.core.types.*

/** Kubernetes-style Persistent Volume (PV) and Persistent Volume Claim (PVC) lifecycle modeling: storage classes,
  * access modes, volume binding, and reclaim policies — as immutable data and pure functions.
  */

// ── Access Modes ────────────────────────────────────────────────────

enum AccessMode:
  case ReadWriteOnce // Single node read-write
  case ReadOnlyMany  // Multiple nodes read-only
  case ReadWriteMany // Multiple nodes read-write

// ── Volume Phase ────────────────────────────────────────────────────

enum VolumePhase:
  case Available // Not yet bound to a PVC
  case Bound     // Bound to a PVC
  case Released  // PVC deleted, volume not yet reclaimed
  case Failed    // Reclamation failed

enum ClaimPhase:
  case Pending // Waiting for a matching PV
  case Bound   // Bound to a PV
  case Lost    // Bound PV no longer exists

// ── Reclaim Policy ──────────────────────────────────────────────────

enum ReclaimPolicy:
  case Retain  // Keep data, admin must manually clean up
  case Delete  // Delete the volume and data
  case Recycle // Basic scrub (rm -rf), make available again

// ── Volume Mode ─────────────────────────────────────────────────────

enum VolumeMode:
  case Filesystem // Volume is mounted as a directory
  case Block      // Volume is presented as a raw block device

// ── Storage Class ───────────────────────────────────────────────────

final case class StorageClass(
    name: String,
    provisioner: String,
    reclaimPolicy: ReclaimPolicy = ReclaimPolicy.Delete,
    volumeBindingMode: VolumeBindingMode = VolumeBindingMode.Immediate,
    allowVolumeExpansion: Boolean = false,
    storageTier: StorageTier = StorageTier.SSD,
    iopsPerGB: Long = 0L,
    throughputMBps: Double = 0.0
)

enum VolumeBindingMode:
  case Immediate       // Bind PV immediately when PVC is created
  case WaitForConsumer // Wait until a pod using the PVC is scheduled

object StorageClass:
  val gp3: StorageClass = StorageClass(
    "gp3",
    "ebs.csi.aws.com",
    ReclaimPolicy.Delete,
    storageTier = StorageTier.SSD,
    iopsPerGB = 3,
    throughputMBps = 125.0
  )
  val io2: StorageClass = StorageClass(
    "io2",
    "ebs.csi.aws.com",
    ReclaimPolicy.Delete,
    storageTier = StorageTier.NVMe,
    iopsPerGB = 64,
    throughputMBps = 1000.0,
    allowVolumeExpansion = true
  )
  val standard: StorageClass = StorageClass(
    "standard",
    "kubernetes.io/host-path",
    ReclaimPolicy.Retain,
    storageTier = StorageTier.HDD
  )
  val nfs: StorageClass = StorageClass(
    "nfs",
    "nfs.csi.k8s.io",
    ReclaimPolicy.Retain,
    volumeBindingMode = VolumeBindingMode.Immediate,
    storageTier = StorageTier.NetworkAttached,
    allowVolumeExpansion = true
  )

// ── Persistent Volume ───────────────────────────────────────────────

final case class PersistentVolume(
    name: String,
    capacityMB: MegaBytes,
    storageClass: StorageClass,
    accessModes: Set[AccessMode],
    reclaimPolicy: ReclaimPolicy,
    volumeMode: VolumeMode = VolumeMode.Filesystem,
    phase: VolumePhase = VolumePhase.Available,
    claimRef: Option[String] = None,    // PVC name bound to this PV
    nodeAffinity: Option[String] = None // Node constraint
):
  def isBound: Boolean     = phase == VolumePhase.Bound
  def isAvailable: Boolean = phase == VolumePhase.Available

// ── Persistent Volume Claim ─────────────────────────────────────────

final case class PersistentVolumeClaim(
    name: String,
    namespace: String = "default",
    requestedCapacityMB: MegaBytes,
    storageClassName: String,
    accessModes: Set[AccessMode],
    volumeMode: VolumeMode = VolumeMode.Filesystem,
    phase: ClaimPhase = ClaimPhase.Pending,
    boundVolumeName: Option[String] = None
):
  def isBound: Boolean   = phase == ClaimPhase.Bound
  def isPending: Boolean = phase == ClaimPhase.Pending

// ── Volume Mount ────────────────────────────────────────────────────

final case class VolumeMount(
    name: String,
    mountPath: String,
    readOnly: Boolean = false,
    claimName: String
)

// ── PV/PVC State ────────────────────────────────────────────────────

final case class VolumeState(
    volumes: Map[String, PersistentVolume] = Map.empty,
    claims: Map[String, PersistentVolumeClaim] = Map.empty,
    storageClasses: Map[String, StorageClass] = Map.empty
):
  def totalProvisionedMB: MegaBytes =
    MegaBytes(volumes.values.map(_.capacityMB.value).sum)

  def availableVolumes: Vector[PersistentVolume] =
    volumes.values.filter(_.isAvailable).toVector

  def boundVolumes: Vector[PersistentVolume] =
    volumes.values.filter(_.isBound).toVector

  def pendingClaims: Vector[PersistentVolumeClaim] =
    claims.values.filter(_.isPending).toVector

// ── Volume Manager (pure functions) ─────────────────────────────────

object VolumeManager:

  /** Register a storage class. */
  def registerStorageClass(state: VolumeState, sc: StorageClass): VolumeState =
    state.copy(storageClasses = state.storageClasses + (sc.name -> sc))

  /** Create a new persistent volume. */
  def createVolume(state: VolumeState, pv: PersistentVolume): VolumeState =
    state.copy(volumes = state.volumes + (pv.name -> pv))

  /** Submit a persistent volume claim. */
  def submitClaim(state: VolumeState, pvc: PersistentVolumeClaim): VolumeState =
    state.copy(claims = state.claims + (pvc.name -> pvc))

  /** Bind a PVC to a matching PV (immediate binding). Returns updated state with both PV and PVC bound.
    */
  def bindClaim(state: VolumeState, pvcName: String): VolumeState =
    state.claims.get(pvcName) match
      case None                     => state
      case Some(pvc) if pvc.isBound => state
      case Some(pvc) =>
        findMatchingVolume(state, pvc) match
          case None => state
          case Some(pv) =>
            val boundPv  = pv.copy(phase = VolumePhase.Bound, claimRef = Some(pvcName))
            val boundPvc = pvc.copy(phase = ClaimPhase.Bound, boundVolumeName = Some(pv.name))
            state.copy(
              volumes = state.volumes + (pv.name -> boundPv),
              claims = state.claims + (pvcName   -> boundPvc)
            )

  /** Auto-bind all pending claims that have matching volumes. */
  def bindAllPending(state: VolumeState): VolumeState =
    state.pendingClaims.foldLeft(state) { (s, pvc) =>
      bindClaim(s, pvc.name)
    }

  /** Dynamic provisioning: create a PV from a storage class to satisfy a PVC. */
  def dynamicProvision(state: VolumeState, pvcName: String): VolumeState =
    state.claims.get(pvcName) match
      case None                     => state
      case Some(pvc) if pvc.isBound => state
      case Some(pvc) =>
        state.storageClasses.get(pvc.storageClassName) match
          case None => state
          case Some(sc) =>
            val pvName = s"pv-${pvc.storageClassName}-${pvcName}"
            val pv = PersistentVolume(
              name = pvName,
              capacityMB = pvc.requestedCapacityMB,
              storageClass = sc,
              accessModes = pvc.accessModes,
              reclaimPolicy = sc.reclaimPolicy,
              volumeMode = pvc.volumeMode,
              phase = VolumePhase.Bound,
              claimRef = Some(pvcName)
            )
            val boundPvc = pvc.copy(phase = ClaimPhase.Bound, boundVolumeName = Some(pvName))
            state.copy(
              volumes = state.volumes + (pvName -> pv),
              claims = state.claims + (pvcName  -> boundPvc)
            )

  /** Release a PVC: unbind and apply reclaim policy. */
  def releaseClaim(state: VolumeState, pvcName: String): VolumeState =
    state.claims.get(pvcName) match
      case None => state
      case Some(pvc) =>
        val updatedClaims = state.claims - pvcName
        val updatedVolumes = pvc.boundVolumeName match
          case None => state.volumes
          case Some(pvName) =>
            state.volumes.get(pvName) match
              case None => state.volumes
              case Some(pv) =>
                val reclaimedPv = pv.reclaimPolicy match
                  case ReclaimPolicy.Delete  => None
                  case ReclaimPolicy.Retain  => Some(pv.copy(phase = VolumePhase.Released, claimRef = None))
                  case ReclaimPolicy.Recycle => Some(pv.copy(phase = VolumePhase.Available, claimRef = None))
                reclaimedPv match
                  case Some(v) => state.volumes + (pvName -> v)
                  case None    => state.volumes - pvName
        state.copy(volumes = updatedVolumes, claims = updatedClaims)

  /** Expand a bound volume (if storage class allows). */
  def expandVolume(state: VolumeState, pvName: String, newCapacityMB: MegaBytes): Either[String, VolumeState] =
    state.volumes.get(pvName) match
      case None => Left(s"Volume $pvName not found")
      case Some(pv) =>
        if !pv.storageClass.allowVolumeExpansion then Left("Storage class does not allow expansion")
        else if newCapacityMB.value <= pv.capacityMB.value then Left("New capacity must be larger")
        else
          val expanded = pv.copy(capacityMB = newCapacityMB)
          Right(state.copy(volumes = state.volumes + (pvName -> expanded)))

  /** Find a PV matching a PVC's requirements. */
  def findMatchingVolume(state: VolumeState, pvc: PersistentVolumeClaim): Option[PersistentVolume] =
    state.availableVolumes
      .filter(pv =>
        pv.storageClass.name == pvc.storageClassName &&
          pv.capacityMB.value >= pvc.requestedCapacityMB.value &&
          pvc.accessModes.subsetOf(pv.accessModes) &&
          pv.volumeMode == pvc.volumeMode
      )
      .sortBy(_.capacityMB.value) // best fit: smallest matching
      .headOption

  /** Estimate monthly storage cost for all provisioned volumes. */
  def totalMonthlyCost(state: VolumeState): Cost =
    val storage = state.volumes.values.map { pv =>
      val perf = storagePerformanceForClass(pv.storageClass)
      perf.monthlyCost(pv.capacityMB)
    }
    Cost(storage.map(_.value).sum)

  /** Map a StorageClass to StoragePerformance for I/O estimation. */
  def storagePerformanceForClass(sc: StorageClass): StoragePerformance =
    sc.storageTier match
      case StorageTier.NVMe            => StoragePerformance.nvme
      case StorageTier.SSD             => StoragePerformance.ssd
      case StorageTier.HDD             => StoragePerformance.hdd
      case StorageTier.NetworkAttached => StoragePerformance.networkAttached

  /** Compute effective write latency for a volume with replication. Wraps StorageReplicationEngine.writeLatency.
    */
  def writeLatencyWithReplication(
      pv: PersistentVolume,
      strategy: ReplicationStrategy,
      baseLatencyMs: Double = 1.0,
      networkLatencyMs: Double = 5.0
  ): SimTime =
    val perf = storagePerformanceForClass(pv.storageClass)
    StorageReplicationEngine.writeLatency(pv.capacityMB, perf, strategy, networkLatencyMs)

  /** Compute effective read latency for a volume with replication. */
  def readLatencyWithReplication(
      pv: PersistentVolume,
      strategy: ReplicationStrategy,
      baseLatencyMs: Double = 1.0,
      networkLatencyMs: Double = 5.0
  ): SimTime =
    val perf = storagePerformanceForClass(pv.storageClass)
    StorageReplicationEngine.readLatency(pv.capacityMB, perf, strategy, networkLatencyMs)

  /** Compute effective capacity with RAID applied. */
  def effectiveCapacityWithRaid(pv: PersistentVolume, raidLevel: RaidLevel, diskCount: Int): MegaBytes =
    val diskCapacity = MegaBytes(pv.capacityMB.value / diskCount)
    StorageReplicationEngine.raidEffectiveCapacity(diskCapacity, raidLevel)

  /** Compute data loss probability for a volume's replication strategy. */
  def dataLossProbability(strategy: ReplicationStrategy, diskFailureRate: Double = 0.02): Double =
    StorageReplicationEngine.dataLossProbability(diskFailureRate, strategy)
