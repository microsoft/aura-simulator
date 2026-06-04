// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.serverless

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*

class ArrivalPatternSpec extends AnyFlatSpec with Matchers:

  "ArrivalPattern.uniform" should "space arrivals at fixed intervals" in {
    val pattern = ArrivalPattern.uniform(SimTime(2.0))
    val times   = pattern(5, SimTime(10.0))

    times should have size 5
    times(0).value shouldBe 10.0
    times(1).value shouldBe 12.0
    times(2).value shouldBe 14.0
    times(3).value shouldBe 16.0
    times(4).value shouldBe 18.0
  }

  it should "return a single arrival when count is 1" in {
    val pattern = ArrivalPattern.uniform(SimTime(5.0))
    val times   = pattern(1, SimTime(0.0))
    times should have size 1
    times(0).value shouldBe 0.0
  }

  "ArrivalPattern.poisson" should "generate the correct number of arrivals" in {
    val pattern = ArrivalPattern.poisson(rate = 10.0, seed = 42L)
    val times   = pattern(100, SimTime(0.0))
    times should have size 100
  }

  it should "produce monotonically increasing times" in {
    val pattern = ArrivalPattern.poisson(rate = 5.0, seed = 123L)
    val times   = pattern(50, SimTime(0.0))
    times.sliding(2).foreach { case Vector(a, b) =>
      b.value should be >= a.value
    }
  }

  it should "be deterministic with the same seed" in {
    val pattern1 = ArrivalPattern.poisson(rate = 5.0, seed = 42L)
    val pattern2 = ArrivalPattern.poisson(rate = 5.0, seed = 42L)
    val times1   = pattern1(20, SimTime(0.0))
    val times2   = pattern2(20, SimTime(0.0))
    times1.map(_.value) shouldBe times2.map(_.value)
  }

  "ArrivalPattern.burst" should "group arrivals into bursts" in {
    val pattern = ArrivalPattern.burst(batchSize = 3, interval = SimTime(10.0))
    val times   = pattern(9, SimTime(0.0))

    times should have size 9
    // First batch at t=0
    times(0).value shouldBe 0.0
    times(1).value shouldBe 0.0
    times(2).value shouldBe 0.0
    // Second batch at t=10
    times(3).value shouldBe 10.0
    times(4).value shouldBe 10.0
    times(5).value shouldBe 10.0
    // Third batch at t=20
    times(6).value shouldBe 20.0
    times(7).value shouldBe 20.0
    times(8).value shouldBe 20.0
  }

  it should "handle partial last batch" in {
    val pattern = ArrivalPattern.burst(batchSize = 3, interval = SimTime(5.0))
    val times   = pattern(5, SimTime(1.0))

    times should have size 5
    times(0).value shouldBe 1.0
    times(1).value shouldBe 1.0
    times(2).value shouldBe 1.0
    times(3).value shouldBe 6.0
    times(4).value shouldBe 6.0
  }

  "ArrivalPattern.trace" should "use explicit arrival times" in {
    val explicitTimes = Vector(SimTime(1.0), SimTime(3.0), SimTime(7.0), SimTime(15.0))
    val pattern       = ArrivalPattern.trace(explicitTimes)
    val times         = pattern(3, SimTime(0.0))

    times should have size 3
    times(0).value shouldBe 1.0
    times(1).value shouldBe 3.0
    times(2).value shouldBe 7.0
  }

  it should "limit to available trace data" in {
    val explicitTimes = Vector(SimTime(1.0), SimTime(2.0))
    val pattern       = ArrivalPattern.trace(explicitTimes)
    val times         = pattern(5, SimTime(0.0))
    times should have size 2
  }
