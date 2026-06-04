// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.containers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.types.*

class PersistentVolumesSpec extends AnyFlatSpec with Matchers:

  private val gp3   = StorageClass.gp3
  private val io2   = StorageClass.io2
  private val nfsSc = StorageClass.nfs

  private def makePV(
      name: String,
      capacityMB: Double,
      sc: StorageClass = gp3,
      modes: Set[AccessMode] = Set(AccessMode.ReadWriteOnce)
  ) =
    PersistentVolume(name, MegaBytes(capacityMB), sc, modes, sc.reclaimPolicy)

  private def makePVC(
      name: String,
      capacityMB: Double,
      scName: String = "gp3",
      modes: Set[AccessMode] = Set(AccessMode.ReadWriteOnce)
  ) =
    PersistentVolumeClaim(name, "default", MegaBytes(capacityMB), scName, modes)

  private val baseState = VolumeState(
    storageClasses = Map("gp3" -> gp3, "io2" -> io2, "nfs" -> nfsSc)
  )

  // ── StorageClass presets ──────────────────────────────────────────

  "StorageClass" should "provide preset configurations" in {
    gp3.provisioner shouldBe "ebs.csi.aws.com"
    gp3.storageTier shouldBe StorageTier.SSD
    io2.iopsPerGB shouldBe 64
    nfsSc.allowVolumeExpansion shouldBe true
  }

  // ── PersistentVolume ──────────────────────────────────────────────

  "PersistentVolume" should "start as Available" in {
    val pv = makePV("pv-1", 10000)
    pv.isAvailable shouldBe true
    pv.isBound shouldBe false
  }

  // ── PersistentVolumeClaim ─────────────────────────────────────────

  "PersistentVolumeClaim" should "start as Pending" in {
    val pvc = makePVC("pvc-1", 5000)
    pvc.isPending shouldBe true
    pvc.isBound shouldBe false
  }

  // ── VolumeManager.createVolume ────────────────────────────────────

  "VolumeManager.createVolume" should "add volume to state" in {
    val pv    = makePV("pv-1", 10000)
    val state = VolumeManager.createVolume(baseState, pv)
    state.volumes should contain key "pv-1"
    state.totalProvisionedMB.value shouldBe 10000.0
  }

  // ── VolumeManager.bindClaim ───────────────────────────────────────

  "VolumeManager.bindClaim" should "bind PVC to matching PV" in {
    val pv  = makePV("pv-1", 10000)
    val pvc = makePVC("pvc-1", 5000)
    val state = VolumeManager.createVolume(
      VolumeManager.submitClaim(baseState, pvc),
      pv
    )

    val bound = VolumeManager.bindClaim(state, "pvc-1")
    bound.volumes("pv-1").isBound shouldBe true
    bound.volumes("pv-1").claimRef shouldBe Some("pvc-1")
    bound.claims("pvc-1").isBound shouldBe true
    bound.claims("pvc-1").boundVolumeName shouldBe Some("pv-1")
  }

  it should "not bind when no matching PV exists" in {
    val pvc    = makePVC("pvc-1", 5000, scName = "nonexistent-class")
    val state  = VolumeManager.submitClaim(baseState, pvc)
    val result = VolumeManager.bindClaim(state, "pvc-1")
    result.claims("pvc-1").isPending shouldBe true
  }

  it should "select best fit (smallest matching PV)" in {
    val smallPv = makePV("pv-small", 5000)
    val largePv = makePV("pv-large", 20000)
    val pvc     = makePVC("pvc-1", 4000)
    val state = VolumeManager.createVolume(
      VolumeManager.createVolume(VolumeManager.submitClaim(baseState, pvc), smallPv),
      largePv
    )

    val bound = VolumeManager.bindClaim(state, "pvc-1")
    bound.claims("pvc-1").boundVolumeName shouldBe Some("pv-small")
  }

  // ── VolumeManager.bindAllPending ──────────────────────────────────

  "VolumeManager.bindAllPending" should "bind all matching claims" in {
    val pv1  = makePV("pv-1", 10000)
    val pv2  = makePV("pv-2", 5000)
    val pvc1 = makePVC("pvc-1", 5000)
    val pvc2 = makePVC("pvc-2", 3000)
    val state = VolumeManager.createVolume(
      VolumeManager.createVolume(VolumeManager.submitClaim(VolumeManager.submitClaim(baseState, pvc1), pvc2), pv1),
      pv2
    )

    val bound = VolumeManager.bindAllPending(state)
    bound.pendingClaims shouldBe empty
    bound.boundVolumes should have size 2
  }

  // ── VolumeManager.dynamicProvision ────────────────────────────────

  "VolumeManager.dynamicProvision" should "create PV from storage class" in {
    val pvc   = makePVC("pvc-1", 8000)
    val state = VolumeManager.submitClaim(baseState, pvc)

    val provisioned = VolumeManager.dynamicProvision(state, "pvc-1")
    provisioned.claims("pvc-1").isBound shouldBe true
    val pvName = provisioned.claims("pvc-1").boundVolumeName.get
    provisioned.volumes(pvName).capacityMB.value shouldBe 8000.0
    provisioned.volumes(pvName).storageClass.name shouldBe "gp3"
  }

  it should "not provision without matching storage class" in {
    val pvc    = makePVC("pvc-1", 8000, scName = "missing")
    val state  = VolumeManager.submitClaim(baseState, pvc)
    val result = VolumeManager.dynamicProvision(state, "pvc-1")
    result.claims("pvc-1").isPending shouldBe true
  }

  // ── VolumeManager.releaseClaim ────────────────────────────────────

  "VolumeManager.releaseClaim" should "delete volume with Delete policy" in {
    val pvc = makePVC("pvc-1", 5000)
    val state = VolumeManager.dynamicProvision(
      VolumeManager.submitClaim(baseState, pvc),
      "pvc-1"
    )
    val pvName = state.claims("pvc-1").boundVolumeName.get

    val released = VolumeManager.releaseClaim(state, "pvc-1")
    released.claims should not contain key("pvc-1")
    released.volumes should not contain key(pvName) // Delete policy
  }

  it should "retain volume with Retain policy" in {
    val nfsPv = PersistentVolume("pv-nfs", MegaBytes(10000), nfsSc, Set(AccessMode.ReadWriteMany), ReclaimPolicy.Retain)
    val pvc   = PersistentVolumeClaim("pvc-nfs", "default", MegaBytes(5000), "nfs", Set(AccessMode.ReadWriteMany))
    val state = VolumeManager.bindClaim(
      VolumeManager.createVolume(VolumeManager.submitClaim(baseState, pvc), nfsPv),
      "pvc-nfs"
    )

    val released = VolumeManager.releaseClaim(state, "pvc-nfs")
    released.claims should not contain key("pvc-nfs")
    released.volumes should contain key "pv-nfs"
    released.volumes("pv-nfs").phase shouldBe VolumePhase.Released
  }

  it should "recycle volume with Recycle policy" in {
    val recyclePv =
      PersistentVolume("pv-recycle", MegaBytes(5000), gp3, Set(AccessMode.ReadWriteOnce), ReclaimPolicy.Recycle)
    val pvc = makePVC("pvc-1", 3000)
    val state = VolumeManager.bindClaim(
      VolumeManager.createVolume(VolumeManager.submitClaim(baseState, pvc), recyclePv),
      "pvc-1"
    )

    val released = VolumeManager.releaseClaim(state, "pvc-1")
    released.volumes should contain key "pv-recycle"
    released.volumes("pv-recycle").phase shouldBe VolumePhase.Available
    released.volumes("pv-recycle").claimRef shouldBe None
  }

  // ── VolumeManager.expandVolume ────────────────────────────────────

  "VolumeManager.expandVolume" should "expand when allowed by storage class" in {
    val pvc = makePVC("pvc-1", 8000, scName = "io2")
    val state = VolumeManager.dynamicProvision(
      VolumeManager.submitClaim(baseState, pvc),
      "pvc-1"
    )
    val pvName = state.claims("pvc-1").boundVolumeName.get

    val result = VolumeManager.expandVolume(state, pvName, MegaBytes(16000))
    result shouldBe a[Right[?, ?]]
    result.toOption.get.volumes(pvName).capacityMB.value shouldBe 16000.0
  }

  it should "reject expansion when not allowed" in {
    val pvc = makePVC("pvc-1", 8000, scName = "gp3")
    val state = VolumeManager.dynamicProvision(
      VolumeManager.submitClaim(baseState, pvc),
      "pvc-1"
    )
    val pvName = state.claims("pvc-1").boundVolumeName.get

    val result = VolumeManager.expandVolume(state, pvName, MegaBytes(16000))
    result shouldBe a[Left[?, ?]]
  }

  it should "reject shrinking" in {
    val pvc = makePVC("pvc-1", 8000, scName = "io2")
    val state = VolumeManager.dynamicProvision(
      VolumeManager.submitClaim(baseState, pvc),
      "pvc-1"
    )
    val pvName = state.claims("pvc-1").boundVolumeName.get

    val result = VolumeManager.expandVolume(state, pvName, MegaBytes(4000))
    result shouldBe a[Left[?, ?]]
  }

  // ── VolumeManager.totalMonthlyCost ────────────────────────────────

  "VolumeManager.totalMonthlyCost" should "sum cost across all volumes" in {
    val pv1 = makePV("pv-1", 10240) // 10 GB
    val pv2 = makePV("pv-2", 20480) // 20 GB
    val state = VolumeManager.createVolume(
      VolumeManager.createVolume(baseState, pv1),
      pv2
    )
    val cost = VolumeManager.totalMonthlyCost(state)
    cost.value should be > 0.0
  }

  // ── VolumeState ───────────────────────────────────────────────────

  "VolumeState" should "report available and bound volumes" in {
    val pv1   = makePV("pv-free", 5000)
    val pv2   = makePV("pv-bound", 5000).copy(phase = VolumePhase.Bound, claimRef = Some("pvc-x"))
    val state = baseState.copy(volumes = Map("pv-free" -> pv1, "pv-bound" -> pv2))

    state.availableVolumes should have size 1
    state.boundVolumes should have size 1
  }

  // ── Access Mode matching ──────────────────────────────────────────

  "VolumeManager.findMatchingVolume" should "respect access modes" in {
    val rwoPv = makePV("pv-rwo", 10000, modes = Set(AccessMode.ReadWriteOnce))
    val pvc   = makePVC("pvc-rwm", 5000, modes = Set(AccessMode.ReadWriteMany))
    val state = VolumeManager.createVolume(baseState, rwoPv)

    VolumeManager.findMatchingVolume(state, pvc) shouldBe None // RWM not subset of {RWO}
  }
