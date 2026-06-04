// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.types.*

class FaultInjectionSpec extends AnyFlatSpec with Matchers:

  "FaultInjection.generateFaults" should "generate faults within time bounds" in {
    val hosts = (0 until 5).map(i => HostId(i.toLong)).toVector
    val config = FaultInjectionConfig(
      faultArrivalDistribution = Distribution.exponential(10.0), // mean=0.1 hours=360s
      pesPerFault = Distribution.uniform(1.0, 3.0),
      seed = 42L
    )
    val faults = FaultInjection.generateFaults(hosts, config, SimTime(50000.0))

    faults should not be empty
    faults.foreach { f =>
      f.time.value should be > 0.0
      f.time.value should be <= 50000.0
    }
  }

  it should "generate faults with valid PE counts" in {
    val hosts = (0 until 5).map(i => HostId(i.toLong)).toVector
    val config = FaultInjectionConfig(
      faultArrivalDistribution = Distribution.exponential(10.0),
      pesPerFault = Distribution.uniform(1.0, 4.0),
      seed = 42L
    )
    val faults = FaultInjection.generateFaults(hosts, config, SimTime(50000.0))

    faults.foreach { f =>
      f.failedPEs.value should be >= 1
    }
  }

  it should "distribute faults across hosts" in {
    val hosts = (0 until 10).map(i => HostId(i.toLong)).toVector
    val config = FaultInjectionConfig(
      faultArrivalDistribution = Distribution.exponential(10.0),
      pesPerFault = Distribution.constant(1.0),
      seed = 42L
    )
    val faults = FaultInjection.generateFaults(hosts, config, SimTime(50000.0))

    // Should have faults on multiple hosts
    val affectedHosts = faults.map(_.hostId).distinct
    affectedHosts.size should be > 1
  }

  it should "respect maxFaultTime" in {
    val hosts = Vector(HostId(0))
    val config = FaultInjectionConfig(
      faultArrivalDistribution = Distribution.exponential(10.0),
      pesPerFault = Distribution.constant(1.0),
      seed = 42L,
      maxFaultTime = Some(SimTime(5000.0))
    )
    val faults = FaultInjection.generateFaults(hosts, config, SimTime(100000.0))

    faults.foreach { f =>
      f.time.value should be <= 5000.0
    }
  }

  it should "return empty for no hosts" in {
    val config = FaultInjectionConfig(
      faultArrivalDistribution = Distribution.exponential(0.5),
      pesPerFault = Distribution.constant(1.0),
      seed = 42L
    )
    FaultInjection.generateFaults(Vector.empty, config, SimTime(5000.0)) shouldBe empty
  }

  it should "be reproducible from the same seed" in {
    val hosts = (0 until 5).map(i => HostId(i.toLong)).toVector
    val config = FaultInjectionConfig(
      faultArrivalDistribution = Distribution.exponential(10.0),
      pesPerFault = Distribution.uniform(1.0, 3.0),
      seed = 42L
    )
    val faults1 = FaultInjection.generateFaults(hosts, config, SimTime(50000.0))
    val faults2 = FaultInjection.generateFaults(hosts, config, SimTime(50000.0))

    faults1.size shouldBe faults2.size
    faults1.zip(faults2).foreach { case (a, b) =>
      a.hostId shouldBe b.hostId
      a.time.value shouldBe b.time.value
      a.failedPEs.value shouldBe b.failedPEs.value
    }
  }
