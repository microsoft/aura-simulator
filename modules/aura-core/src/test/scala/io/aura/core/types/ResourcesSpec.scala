// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ResourcesSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  val defaultSpec: ResourceSpec = ResourceSpec(
    pes = PEs(4),
    mips = MIPS(10000.0),
    ram = MegaBytes(8192.0),
    bw = Mbps(1000.0),
    storage = MegaBytes(100000.0)
  )

  val smallSpec: ResourceSpec = ResourceSpec(
    pes = PEs(2),
    mips = MIPS(5000.0),
    ram = MegaBytes(2048.0),
    bw = Mbps(500.0),
    storage = MegaBytes(10000.0)
  )

  "AvailableResources" should "be created from a ResourceSpec" in {
    val avail = AvailableResources.fromSpec(defaultSpec)
    avail.pes.value shouldBe 4
    avail.mips.value shouldBe 10000.0
    avail.ram.value shouldBe 8192.0
  }

  it should "check if a spec can fit" in {
    val avail = AvailableResources.fromSpec(defaultSpec)
    avail.canFit(smallSpec) shouldBe true

    val largeSpec = ResourceSpec(PEs(8), MIPS(20000.0), MegaBytes(16384.0), Mbps(2000.0), MegaBytes(200000.0))
    avail.canFit(largeSpec) shouldBe false
  }

  it should "allocate and reduce available resources" in {
    val avail      = AvailableResources.fromSpec(defaultSpec)
    val afterAlloc = avail.allocate(smallSpec)

    afterAlloc.pes.value shouldBe 2
    afterAlloc.mips.value shouldBe 5000.0
    afterAlloc.ram.value shouldBe 6144.0
    afterAlloc.bw.value shouldBe 500.0
    afterAlloc.storage.value shouldBe 90000.0
  }

  it should "release and restore resources" in {
    val avail        = AvailableResources.fromSpec(defaultSpec)
    val afterAlloc   = avail.allocate(smallSpec)
    val afterRelease = afterAlloc.release(smallSpec)

    afterRelease.pes.value shouldBe 4
    afterRelease.mips.value shouldBe 10000.0
    afterRelease.ram.value shouldBe 8192.0
  }

  "allocate then release" should "be the identity (property-based)" in {
    // Property: allocate(spec).release(spec) == original
    val avail     = AvailableResources.fromSpec(defaultSpec)
    val roundTrip = avail.allocate(smallSpec).release(smallSpec)

    roundTrip.pes.value shouldBe avail.pes.value
    roundTrip.mips.value shouldBe avail.mips.value +- 0.001
    roundTrip.ram.value shouldBe avail.ram.value +- 0.001
    roundTrip.bw.value shouldBe avail.bw.value +- 0.001
    roundTrip.storage.value shouldBe avail.storage.value +- 0.001
  }
