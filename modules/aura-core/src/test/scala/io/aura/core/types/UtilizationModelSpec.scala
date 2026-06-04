// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UtilizationModelSpec extends AnyFlatSpec with Matchers:

  "UtilizationModel.full" should "always return 1.0" in {
    UtilizationModel.full(SimTime.Zero).value shouldBe 1.0
    UtilizationModel.full(SimTime(100.0)).value shouldBe 1.0
    UtilizationModel.full(SimTime(999.0)).value shouldBe 1.0
  }

  "UtilizationModel.constant" should "return the specified level" in {
    val model = UtilizationModel.constant(0.7)
    model(SimTime.Zero).value shouldBe 0.7
    model(SimTime(100.0)).value shouldBe 0.7
  }

  it should "clamp values to [0, 1]" in {
    UtilizationModel.constant(1.5)(SimTime.Zero).value shouldBe 1.0
    UtilizationModel.constant(-0.5)(SimTime.Zero).value shouldBe 0.0
  }

  "UtilizationModel.dynamic" should "increase over time" in {
    val model = UtilizationModel.dynamic(initial = 0.1, increment = 0.01)
    model(SimTime.Zero).value shouldBe 0.1 +- 0.001
    model(SimTime(10.0)).value shouldBe 0.2 +- 0.001
    model(SimTime(50.0)).value shouldBe 0.6 +- 0.001
  }

  it should "cap at maxUtilization" in {
    val model = UtilizationModel.dynamic(initial = 0.5, increment = 0.1, maxUtilization = 0.8)
    model(SimTime(100.0)).value shouldBe 0.8 +- 0.001
  }

  "UtilizationModel.stochastic" should "return values in [0, 1]" in {
    val model = UtilizationModel.stochastic(Distribution.uniform(), seed = 42L)
    for _ <- 0 until 100 do
      val u = model(SimTime.Zero)
      u.value should be >= 0.0
      u.value should be <= 1.0
  }

  "UtilizationModel.planetLab" should "interpolate trace data" in {
    val trace = Vector(20.0, 40.0, 60.0, 80.0)
    val model = UtilizationModel.planetLab(trace, intervalSeconds = 100.0)

    // At time 0: index 0 = 20% -> 0.2
    model(SimTime.Zero).value shouldBe 0.2 +- 0.01

    // At time 100: index 1 = 40% -> 0.4
    model(SimTime(100.0)).value shouldBe 0.4 +- 0.01
  }

  it should "return Zero for empty trace data" in {
    val model = UtilizationModel.planetLab(Vector.empty)
    model(SimTime(100.0)).value shouldBe 0.0
  }

  "UtilizationModel.composite" should "combine cpu, ram, bw models" in {
    val comp = UtilizationModel.composite(
      cpu = UtilizationModel.full,
      ram = UtilizationModel.constant(0.5),
      bw = UtilizationModel.constant(0.3)
    )

    comp.cpu(SimTime.Zero).value shouldBe 1.0
    comp.ram(SimTime.Zero).value shouldBe 0.5
    comp.bw(SimTime.Zero).value shouldBe 0.3
  }
