// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

/** A utilization model is a pure function: SimTime -> Utilization [0.0, 1.0].
  *
  * Utilization models are simple function types that compose naturally.
  */
type UtilizationModel = SimTime => Utilization

object UtilizationModel:
  val full: UtilizationModel = _ => Utilization.Full

  def constant(level: Double): UtilizationModel =
    _ => Utilization(math.max(0.0, math.min(1.0, level)))

  def dynamic(
      initial: Double = 0.0,
      increment: Double = 0.1,
      maxUtilization: Double = 1.0
  ): UtilizationModel =
    time =>
      val u = math.min(initial + increment * time.value, maxUtilization)
      Utilization(math.max(0.0, u))

  def stochastic(distribution: Distribution, seed: Long = 42L): UtilizationModel =
    val samples = distribution(seed).iterator
    _ => Utilization(math.max(0.0, math.min(1.0, samples.next())))

  def planetLab(traceData: Vector[Double], intervalSeconds: Double = 300.0): UtilizationModel =
    time =>
      if traceData.isEmpty then Utilization.Zero
      else
        val idx          = ((time.value / intervalSeconds) % traceData.size).toInt
        val nextIdx      = (idx + 1)                       % traceData.size
        val fraction     = (time.value / intervalSeconds) - idx.toDouble
        val interpolated = traceData(idx) * (1.0 - fraction) + traceData(nextIdx) * fraction
        Utilization(math.max(0.0, math.min(1.0, interpolated / 100.0)))

  def composite(cpu: UtilizationModel, ram: UtilizationModel, bw: UtilizationModel): CompositeUtilization =
    CompositeUtilization(cpu, ram, bw)

final case class CompositeUtilization(
    cpu: UtilizationModel,
    ram: UtilizationModel,
    bw: UtilizationModel
)
