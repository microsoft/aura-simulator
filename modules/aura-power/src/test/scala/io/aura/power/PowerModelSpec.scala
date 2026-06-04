// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.power

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.{Utilization, Watts}

class PowerModelSpec extends AnyFlatSpec with Matchers:

  "Linear power model" should "return idle power at 0% utilization" in {
    val model = PowerModel.linear(Watts(120.0), Watts(80.0))
    model(Utilization(0.01)).value shouldBe 80.4 +- 0.1
  }

  it should "return max power at 100% utilization" in {
    val model = PowerModel.linear(Watts(120.0), Watts(80.0))
    model(Utilization.Full).value shouldBe 120.0 +- 0.001
  }

  it should "interpolate linearly" in {
    val model = PowerModel.linear(Watts(120.0), Watts(80.0))
    // At 50%: 80 + (120 - 80) * 0.5 = 100
    model(Utilization(0.5)).value shouldBe 100.0 +- 0.001
  }

  it should "return zero for zero utilization" in {
    val model = PowerModel.linear(Watts(120.0), Watts(80.0))
    model(Utilization.Zero).value shouldBe 0.0
  }

  "Cubic power model" should "grow cubically with utilization" in {
    val model = PowerModel.cubic(Watts(120.0), Watts(80.0))
    // At 50%: 80 + (120 - 80) * 0.125 = 85
    model(Utilization(0.5)).value shouldBe 85.0 +- 0.001
  }

  it should "return max power at full utilization" in {
    val model = PowerModel.cubic(Watts(120.0), Watts(80.0))
    model(Utilization.Full).value shouldBe 120.0 +- 0.001
  }

  "Sqrt power model" should "grow sublinearly" in {
    val model = PowerModel.sqrt(Watts(120.0), Watts(80.0))
    // At 25%: 80 + 40 * sqrt(0.25) = 80 + 20 = 100
    model(Utilization(0.25)).value shouldBe 100.0 +- 0.001
  }

  "SPECpower-based model" should "interpolate between data points" in {
    val model = PowerModel.specBased(
      Vector(
        (0.0, 80.0),
        (0.5, 100.0),
        (1.0, 120.0)
      )
    )

    model(Utilization(0.25)).value shouldBe 90.0 +- 0.001
    model(Utilization(0.75)).value shouldBe 110.0 +- 0.001
  }

  "HP ProLiant G4 model" should "produce reasonable power values" in {
    val model = PowerModel.hpProLiantG4
    model(Utilization(0.5)).value shouldBe 102.0 +- 1.0
    model(Utilization.Full).value shouldBe 117.0 +- 1.0
  }

  "HP ProLiant G5 model" should "produce reasonable power values" in {
    val model = PowerModel.hpProLiantG5
    model(Utilization(0.5)).value shouldBe 116.0 +- 1.0
    model(Utilization.Full).value shouldBe 135.0 +- 1.0
  }

  "Constant power model" should "always return the same value" in {
    val model = PowerModel.constant(Watts(100.0))
    model(Utilization.Zero).value shouldBe 100.0
    model(Utilization.Full).value shouldBe 100.0
    model(Utilization(0.5)).value shouldBe 100.0
  }

  "Zero power model" should "always return zero" in {
    PowerModel.zero(Utilization.Full).value shouldBe 0.0
    PowerModel.zero(Utilization.Zero).value shouldBe 0.0
  }

  "Custom power model" should "accept any function" in {
    val model = PowerModel.custom(u => u * u * 200.0)
    model(Utilization(0.5)).value shouldBe 50.0 +- 0.001
    model(Utilization.Full).value shouldBe 200.0 +- 0.001
  }
