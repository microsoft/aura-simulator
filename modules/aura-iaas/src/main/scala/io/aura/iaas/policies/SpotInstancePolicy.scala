// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import io.aura.core.types.*
import io.aura.core.events.*

/** Configuration for spot instance behavior.
  *
  * @param interruptionTimes
  *   explicit times at which spot interruptions occur
  * @param noticePeriod
  *   warning period before VM termination (AWS gives ~2 min)
  * @param fallbackToOnDemand
  *   if true, broker automatically creates on-demand replacement
  * @param onDemandRates
  *   cost rates for fallback on-demand VMs (if fallback enabled)
  */
final case class SpotInstanceConfig(
    interruptionTimes: Vector[SimTime] = Vector.empty,
    noticePeriod: SimTime = SimTime(120.0),
    fallbackToOnDemand: Boolean = true,
    onDemandRates: Option[CostRates] = None
)

object SpotInstanceConfig:
  /** Generate periodic interruptions at a fixed interval. */
  def periodic(
      interval: SimTime,
      startAfter: SimTime = SimTime(300.0),
      endBefore: SimTime = SimTime(100000.0),
      noticePeriod: SimTime = SimTime(120.0),
      fallbackToOnDemand: Boolean = true,
      onDemandRates: Option[CostRates] = None
  ): SpotInstanceConfig =
    val times = Iterator
      .iterate(startAfter.value)(_ + interval.value)
      .takeWhile(_ < endBefore.value)
      .map(SimTime(_))
      .toVector
    SpotInstanceConfig(times, noticePeriod, fallbackToOnDemand, onDemandRates)

object SpotInterruptionScheduler:
  /** Generate SpotInterruption events for all spot VMs at configured times. */
  def generateEvents(
      brokerRef: EntityRef,
      spotVmIds: Vector[VmId],
      config: SpotInstanceConfig
  ): Vector[SimEvent] =
    if spotVmIds.isEmpty || config.interruptionTimes.isEmpty then Vector.empty
    else
      config.interruptionTimes.flatMap { time =>
        spotVmIds.map { vmId =>
          SimEvent(
            time = time,
            source = brokerRef,
            destination = brokerRef,
            payload = SimEventPayload.SpotInterruption(vmId, config.noticePeriod),
            serial = SerialNumber.Zero
          )
        }
      }
