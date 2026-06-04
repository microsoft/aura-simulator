// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

/** A distribution is a pure function: seed -> stream of samples.
  *
  * Distributions are pure functions returning lazy streams. No shared mutable state, fully reproducible from seed.
  */
type Distribution = Long => LazyList[Double]

object Distribution:
  import java.util.Random

  def uniform(min: Double = 0.0, max: Double = 1.0): Distribution =
    seed =>
      val rng = new Random(seed)
      LazyList.continually(min + (max - min) * rng.nextDouble())

  def normal(mean: Double, stdDev: Double): Distribution =
    seed =>
      val rng = new Random(seed)
      LazyList.continually(mean + stdDev * rng.nextGaussian())

  def exponential(rate: Double): Distribution =
    seed =>
      val rng = new Random(seed)
      LazyList.continually(-math.log(1.0 - rng.nextDouble()) / rate)

  def poisson(lambda: Double): Distribution =
    seed =>
      val rng = new Random(seed)
      LazyList.continually {
        val L = math.exp(-lambda)
        @annotation.tailrec
        def poissonSample(k: Int, p: Double): Int =
          if p <= L then k - 1 else poissonSample(k + 1, p * rng.nextDouble())
        poissonSample(0, 1.0).toDouble
      }

  def weibull(shape: Double, scale: Double): Distribution =
    seed =>
      val rng = new Random(seed)
      LazyList.continually(scale * math.pow(-math.log(1.0 - rng.nextDouble()), 1.0 / shape))

  def gamma(shape: Double, scale: Double): Distribution =
    seed =>
      val rng = new Random(seed)
      // Marsaglia and Tsang's method for shape >= 1
      LazyList.continually {
        if shape >= 1.0 then
          val d = shape - 1.0 / 3.0
          val c = 1.0 / math.sqrt(9.0 * d)
          @annotation.tailrec
          def marsagliaTsang(): Double =
            val x    = rng.nextGaussian()
            val rawV = 1.0 + c * x
            if rawV <= 0 then marsagliaTsang()
            else
              val v = rawV * rawV * rawV
              val u = rng.nextDouble()
              if u < 1.0 - 0.0331 * (x * x) * (x * x) then d * v * scale
              else if math.log(u) < 0.5 * x * x + d * (1.0 - v + math.log(v)) then d * v * scale
              else marsagliaTsang()
          marsagliaTsang()
        else
          // For shape < 1, use shape+1 and apply correction
          val d          = shape + 1.0 - 1.0 / 3.0
          val c          = 1.0 / math.sqrt(9.0 * d)
          val correction = math.pow(rng.nextDouble(), 1.0 / shape)
          @annotation.tailrec
          def marsagliaTsang(): Double =
            val x    = rng.nextGaussian()
            val rawV = 1.0 + c * x
            if rawV <= 0 then marsagliaTsang()
            else
              val v = rawV * rawV * rawV
              val u = rng.nextDouble()
              if u < 1.0 - 0.0331 * (x * x) * (x * x) then d * v * scale * correction
              else if math.log(u) < 0.5 * x * x + d * (1.0 - v + math.log(v)) then d * v * scale * correction
              else marsagliaTsang()
          marsagliaTsang()
      }

  def pareto(shape: Double, scale: Double): Distribution =
    seed =>
      val rng = new Random(seed)
      LazyList.continually(scale / math.pow(rng.nextDouble(), 1.0 / shape))

  def lognormal(logMean: Double, logStdDev: Double): Distribution =
    seed =>
      val rng = new Random(seed)
      LazyList.continually(math.exp(logMean + logStdDev * rng.nextGaussian()))

  def beta(alpha: Double, beta: Double): Distribution =
    seed =>
      // Beta via ratio of Gamma samples
      val gammaA = gamma(alpha, 1.0)(seed)
      val gammaB = gamma(beta, 1.0)(seed + 7919L) // offset seed for independence
      gammaA.zip(gammaB).map { case (a, b) => a / (a + b) }

  def triangular(min: Double, mode: Double, max: Double): Distribution =
    seed =>
      val rng = new Random(seed)
      LazyList.continually {
        val u  = rng.nextDouble()
        val fc = (mode - min) / (max - min)
        if u < fc then min + math.sqrt(u * (max - min) * (mode - min))
        else max - math.sqrt((1.0 - u) * (max - min) * (max - mode))
      }

  def constant(value: Double): Distribution = _ => LazyList.continually(value)

  /** Antithetic variates: variance reduction for Monte Carlo. */
  def antithetic(base: Distribution): Distribution =
    seed =>
      val stream = base(seed)
      stream.zipWithIndex.map { case (v, i) => if i % 2 == 0 then v else 1.0 - v }
