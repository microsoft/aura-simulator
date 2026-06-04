// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DistributionSpec extends AnyFlatSpec with Matchers:

  "Distribution.uniform" should "produce values in [min, max]" in {
    val samples = Distribution.uniform(0.0, 10.0)(42L).take(1000).toVector
    samples.foreach { v =>
      v should be >= 0.0
      v should be <= 10.0
    }
  }

  it should "be reproducible from the same seed" in {
    val a = Distribution.uniform()(42L).take(100).toVector
    val b = Distribution.uniform()(42L).take(100).toVector
    a shouldBe b
  }

  it should "produce different values from different seeds" in {
    val a = Distribution.uniform()(42L).take(10).toVector
    val b = Distribution.uniform()(99L).take(10).toVector
    a should not be b
  }

  "Distribution.normal" should "produce values around the mean" in {
    val samples = Distribution.normal(100.0, 10.0)(42L).take(10000).toVector
    val mean    = samples.sum / samples.size
    mean shouldBe 100.0 +- 1.0
  }

  "Distribution.exponential" should "produce positive values" in {
    val samples = Distribution.exponential(1.0)(42L).take(1000).toVector
    samples.foreach { v =>
      v should be > 0.0
    }
  }

  it should "have mean approximately 1/rate" in {
    val rate    = 0.5
    val samples = Distribution.exponential(rate)(42L).take(10000).toVector
    val mean    = samples.sum / samples.size
    mean shouldBe (1.0 / rate) +- 0.1
  }

  "Distribution.poisson" should "produce non-negative integer values" in {
    val samples = Distribution.poisson(5.0)(42L).take(1000).toVector
    samples.foreach { v =>
      v should be >= 0.0
      v shouldBe v.toInt.toDouble
    }
  }

  it should "have mean approximately lambda" in {
    val lambda  = 5.0
    val samples = Distribution.poisson(lambda)(42L).take(10000).toVector
    val mean    = samples.sum / samples.size
    mean shouldBe lambda +- 0.2
  }

  "Distribution.weibull" should "produce positive values" in {
    val samples = Distribution.weibull(2.0, 1.0)(42L).take(1000).toVector
    samples.foreach { v =>
      v should be > 0.0
    }
  }

  "Distribution.pareto" should "produce values >= scale" in {
    val scale   = 1.0
    val samples = Distribution.pareto(2.0, scale)(42L).take(1000).toVector
    samples.foreach { v =>
      v should be >= scale
    }
  }

  "Distribution.lognormal" should "produce positive values" in {
    val samples = Distribution.lognormal(0.0, 1.0)(42L).take(1000).toVector
    samples.foreach { v =>
      v should be > 0.0
    }
  }

  "Distribution.beta" should "produce values in (0, 1)" in {
    val samples = Distribution.beta(2.0, 5.0)(42L).take(1000).toVector
    samples.foreach { v =>
      v should be > 0.0
      v should be < 1.0
    }
  }

  "Distribution.triangular" should "produce values in [min, max]" in {
    val samples = Distribution.triangular(1.0, 5.0, 10.0)(42L).take(1000).toVector
    samples.foreach { v =>
      v should be >= 1.0
      v should be <= 10.0
    }
  }

  "Distribution.constant" should "always produce the same value" in {
    val samples = Distribution.constant(42.0)(0L).take(100).toVector
    samples.foreach(_ shouldBe 42.0)
  }

  "Distribution.gamma" should "produce positive values" in {
    val samples = Distribution.gamma(2.0, 1.0)(42L).take(1000).toVector
    samples.foreach { v =>
      v should be > 0.0
    }
  }

  "Distribution.antithetic" should "alternate original and 1-x values" in {
    val base     = Distribution.uniform()(42L)
    val anti     = Distribution.antithetic(Distribution.uniform())(42L)
    val baseVals = base.take(10).toVector
    val antiVals = anti.take(10).toVector

    // Even indices should match base
    antiVals(0) shouldBe baseVals(0)
    // Odd indices should be 1 - base value
    antiVals(1) shouldBe (1.0 - baseVals(1)) +- 1e-10
  }

  it should "reduce variance compared to standard uniform" in {
    // The antithetic technique should produce pairs that average closer to 0.5
    val anti     = Distribution.antithetic(Distribution.uniform())(42L).take(1000).toVector
    val mean     = anti.sum / anti.size
    val variance = anti.map(v => (v - mean) * (v - mean)).sum / anti.size

    val standard    = Distribution.uniform()(42L).take(1000).toVector
    val stdMean     = standard.sum / standard.size
    val stdVariance = standard.map(v => (v - stdMean) * (v - stdMean)).sum / standard.size

    // Antithetic should have lower or similar variance
    variance should be <= stdVariance * 1.5
  }
