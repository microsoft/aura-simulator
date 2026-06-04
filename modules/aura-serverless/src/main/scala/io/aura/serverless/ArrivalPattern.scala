// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.serverless

import io.aura.core.types.*
import scala.util.Random

/** Pure function: (count, startTime) => Vector[SimTime] of arrival times.
  *
  * Generates deterministic arrival patterns for function invocations.
  */
object ArrivalPattern:

  type ArrivalPattern = (Int, SimTime) => Vector[SimTime]

  /** Fixed intervals between arrivals. */
  def uniform(interval: SimTime): ArrivalPattern =
    (count, startTime) => (0 until count).map(i => startTime + interval * i.toDouble).toVector

  /** Poisson process — exponential inter-arrival times. Uses a deterministic seed for reproducibility across runs.
    */
  def poisson(rate: Double, seed: Long = 42L): ArrivalPattern =
    (count, startTime) =>
      val rng           = new Random(seed)
      val interArrivals = (0 until count).map(_ => -math.log(1.0 - rng.nextDouble()) / rate)
      interArrivals.scanLeft(startTime.value)((acc, ia) => acc + ia).init.map(SimTime(_)).toVector

  /** Periodic bursts: batchSize invocations arrive simultaneously, repeated at interval. */
  def burst(batchSize: Int, interval: SimTime): ArrivalPattern =
    (count, startTime) =>
      (0 until count).map { i =>
        val batchIndex = i / batchSize
        startTime + interval * batchIndex.toDouble
      }.toVector

  /** Explicit arrival times — uses provided times, limited to count. */
  def trace(times: Vector[SimTime]): ArrivalPattern =
    (count, _) => times.take(count)
