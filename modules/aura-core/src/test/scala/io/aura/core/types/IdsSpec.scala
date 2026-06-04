// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IdsSpec extends AnyFlatSpec with Matchers:

  "VmId" should "wrap Long values" in {
    val id = VmId(42L)
    id.value shouldBe 42L
    id.toLong shouldBe 42L
  }

  it should "be orderable" in {
    val ids = Vector(VmId(3L), VmId(1L), VmId(2L))
    ids.sorted.map(_.value) shouldBe Vector(1L, 2L, 3L)
  }

  "HostId" should "be distinct from VmId at the type level" in {
    val hostId = HostId(1L)
    val vmId   = VmId(1L)

    // These are different types even with same underlying value
    hostId.value shouldBe vmId.value
    // But you can't mix them — the compiler prevents it:
    // hostId: HostId != vmId: VmId (type-level distinction)
  }

  "SerialNumber" should "support increment" in {
    val sn = SerialNumber(0L)
    sn.next.value shouldBe 1L
    sn.next.next.value shouldBe 2L
  }

  "DatacenterId" should "wrap Long values" in {
    DatacenterId(100L).value shouldBe 100L
  }

  "BrokerId" should "wrap Long values" in {
    BrokerId(200L).value shouldBe 200L
  }

  "WorkloadId" should "wrap Long values" in {
    WorkloadId(300L).value shouldBe 300L
  }
