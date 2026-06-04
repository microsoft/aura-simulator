// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen

/** Property-based tests for resource invariants.
  *
  * These verify fundamental properties that must always hold:
  *   - Allocation never exceeds capacity
  *   - Release is the inverse of allocation
  *   - Resource arithmetic is consistent
  */
class ResourcePropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  val posDouble: Gen[Double] = Gen.choose(1.0, 100000.0)
  val posInt: Gen[Int]       = Gen.choose(1, 64)

  "ResourceSpec allocation" should "never produce negative available resources" in {
    forAll(posDouble, posDouble, posInt) { (hostMips, vmMips, pes) =>
      whenever(hostMips > 0 && vmMips > 0 && vmMips <= hostMips && pes > 0) {
        val hostSpec   = ResourceSpec(PEs(pes), MIPS(hostMips), MegaBytes(8192.0), Mbps(1000.0), MegaBytes(100000.0))
        val vmSpec     = ResourceSpec(PEs(1), MIPS(vmMips), MegaBytes(1024.0), Mbps(100.0), MegaBytes(1000.0))
        val avail      = AvailableResources.fromSpec(hostSpec)
        val afterAlloc = avail.allocate(vmSpec)

        afterAlloc.mips.value should be >= 0.0
        afterAlloc.ram.value should be >= 0.0
        afterAlloc.bw.value should be >= 0.0
        afterAlloc.storage.value should be >= 0.0
      }
    }
  }

  "Allocate then release" should "restore original resources (round-trip property)" in {
    forAll(posDouble, posDouble) { (hostMips, vmMips) =>
      whenever(hostMips > 0 && vmMips > 0 && vmMips <= hostMips) {
        val hostSpec = ResourceSpec(PEs(4), MIPS(hostMips), MegaBytes(8192.0), Mbps(1000.0), MegaBytes(100000.0))
        val vmSpec   = ResourceSpec(PEs(1), MIPS(vmMips), MegaBytes(1024.0), Mbps(100.0), MegaBytes(1000.0))
        val avail    = AvailableResources.fromSpec(hostSpec)

        val roundTrip = avail.allocate(vmSpec).release(vmSpec)

        roundTrip.mips.value shouldBe avail.mips.value +- 0.001
        roundTrip.ram.value shouldBe avail.ram.value +- 0.001
        roundTrip.bw.value shouldBe avail.bw.value +- 0.001
        roundTrip.storage.value shouldBe avail.storage.value +- 0.001
        roundTrip.pes.value shouldBe avail.pes.value
      }
    }
  }

  "canFit" should "be true when all resource dimensions are sufficient" in {
    forAll(posDouble, posDouble) { (hostMips, vmMips) =>
      whenever(hostMips > 0 && vmMips > 0) {
        val hostSpec = ResourceSpec(PEs(8), MIPS(hostMips), MegaBytes(16384.0), Mbps(10000.0), MegaBytes(1000000.0))
        val vmSpec   = ResourceSpec(PEs(1), MIPS(vmMips), MegaBytes(1024.0), Mbps(100.0), MegaBytes(1000.0))
        val avail    = AvailableResources.fromSpec(hostSpec)

        if vmMips <= hostMips then avail.canFit(vmSpec) shouldBe true
      }
    }
  }

  "SimTime arithmetic" should "be associative" in {
    forAll(posDouble, posDouble, posDouble) { (a, b, c) =>
      val t1 = SimTime(a)
      val t2 = SimTime(b)
      val t3 = SimTime(c)

      ((t1 + t2) + t3).value shouldBe (t1 + (t2 + t3)).value +- 0.001
    }
  }

  "SimTime ordering" should "be consistent with arithmetic" in {
    forAll(posDouble, posDouble) { (a, b) =>
      whenever(a != b) {
        val t1 = SimTime(a)
        val t2 = SimTime(b)

        if a < b then
          (t1 < t2) shouldBe true
          (t2 > t1) shouldBe true
        else
          (t1 > t2) shouldBe true
          (t2 < t1) shouldBe true
      }
    }
  }

  "Utilization" should "always be clamped to [0, 1]" in {
    forAll(Gen.choose(-10.0, 10.0)) { v =>
      val u = Utilization(v)
      u.value should be >= 0.0
      u.value should be <= 1.0
    }
  }

  "MI.executionTime" should "be inversely proportional to MIPS" in {
    forAll(posDouble, posDouble) { (miVal, mipsVal) =>
      whenever(mipsVal > 0) {
        val mi   = MI(miVal)
        val mips = MIPS(mipsVal)
        val time = mi.executionTime(mips)

        time.value shouldBe (miVal / mipsVal) +- 0.001
      }
    }
  }
