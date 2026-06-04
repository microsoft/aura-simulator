// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.power

import io.aura.core.types.{Utilization, Watts}

/** Compositional power models as pure functions.
  *
  * Implemented as simple functions: Utilization => Watts. They compose naturally using standard function composition.
  */
type PowerModel = Utilization => Watts

object PowerModel:

  /** Linear power model: P(u) = idle + (max - idle) * u
    *
    * @param maxPower
    *   Power at 100% utilization (watts)
    * @param idlePower
    *   Power at 0% utilization (watts) — typically 60-70% of max
    */
  def linear(maxPower: Watts, idlePower: Watts): PowerModel =
    (utilization: Utilization) =>
      if utilization.value <= 0.0 then Watts.Zero
      else Watts(idlePower.value + (maxPower.value - idlePower.value) * utilization.value)

  /** Cubic power model: P(u) = idle + (max - idle) * u^3
    *
    * Models servers where power scales cubically with utilization.
    */
  def cubic(maxPower: Watts, idlePower: Watts): PowerModel =
    (utilization: Utilization) =>
      if utilization.value <= 0.0 then Watts.Zero
      else
        val u3 = utilization.value * utilization.value * utilization.value
        Watts(idlePower.value + (maxPower.value - idlePower.value) * u3)

  /** Square root power model: P(u) = idle + (max - idle) * sqrt(u)
    *
    * Models servers with diminishing power increase at higher utilization.
    */
  def sqrt(maxPower: Watts, idlePower: Watts): PowerModel =
    (utilization: Utilization) =>
      if utilization.value <= 0.0 then Watts.Zero
      else Watts(idlePower.value + (maxPower.value - idlePower.value) * math.sqrt(utilization.value))

  /** SPECpower-based interpolation model.
    *
    * Uses real SPECpower benchmark data points (utilization -> watts) and interpolates linearly between them. This is
    * the most accurate model for real server hardware.
    *
    * @param dataPoints
    *   Sorted pairs of (utilization, watts) from SPECpower benchmarks
    */
  /** SPECpower-based interpolation model. Returns None if dataPoints is empty.
    */
  def specBasedOpt(dataPoints: Vector[(Double, Double)]): Option[PowerModel] =
    if dataPoints.isEmpty then None
    else Some(specBased(dataPoints))

  def specBased(dataPoints: Vector[(Double, Double)]): PowerModel =
    val sorted =
      if dataPoints.nonEmpty then dataPoints.sortBy(_._1)
      else Vector((0.0, 0.0)) // fallback: zero power if empty (defensive)

    (utilization: Utilization) =>
      if utilization.value <= 0.0 then Watts.Zero
      else if sorted.size == 1 then Watts(sorted.head._2)
      else
        val u = utilization.value
        // Find the two data points to interpolate between
        val idx = sorted.indexWhere(_._1 >= u)
        if idx <= 0 then Watts(sorted.head._2)
        else if idx >= sorted.size then Watts(sorted.last._2)
        else
          val (u0, p0) = sorted(idx - 1)
          val (u1, p1) = sorted(idx)
          if u1 == u0 then Watts(p0)
          else
            val ratio = (u - u0) / (u1 - u0)
            Watts(p0 + (p1 - p0) * ratio)

  /** Custom power model from any function. */
  def custom(f: Double => Double): PowerModel =
    (utilization: Utilization) => Watts(f(utilization.value))

  /** Constant power model (for testing or simple scenarios). */
  def constant(watts: Watts): PowerModel =
    (_: Utilization) => watts

  /** Zero power model (host is off). */
  val zero: PowerModel = (_: Utilization) => Watts.Zero

  /** Standard SPECpower data for HP ProLiant ML110 G4 (common benchmark reference). */
  val hpProLiantG4: PowerModel = specBased(
    Vector(
      (0.0, 86.0),
      (0.1, 89.4),
      (0.2, 92.6),
      (0.3, 96.0),
      (0.4, 99.5),
      (0.5, 102.0),
      (0.6, 106.0),
      (0.7, 108.0),
      (0.8, 112.0),
      (0.9, 114.0),
      (1.0, 117.0)
    )
  )

  /** Standard SPECpower data for HP ProLiant ML110 G5 (common benchmark reference). */
  val hpProLiantG5: PowerModel = specBased(
    Vector(
      (0.0, 93.7),
      (0.1, 97.0),
      (0.2, 101.0),
      (0.3, 105.0),
      (0.4, 110.0),
      (0.5, 116.0),
      (0.6, 121.0),
      (0.7, 125.0),
      (0.8, 129.0),
      (0.9, 133.0),
      (1.0, 135.0)
    )
  )
