// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.gpu.actors

import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}

import io.aura.core.types.*
import io.aura.core.events.*
import io.aura.core.events.SimEventPayload.*
import io.aura.core.engine.TimeCoordinator
import io.aura.core.engine.TimeCoordinator.{EntityCommand, ProcessEvents, StepComplete}
import io.aura.gpu.*
import io.aura.gpu.state.*

/** Actor representing a single GPU server node (e.g., DGX H100).
  *
  * Follows the immutable accumulator pattern established by DatacenterActor:
  *   - Registers with TimeCoordinator
  *   - Receives ProcessEvents, foldLeft over events
  *   - Emits events via ScheduleEvents, replies StepComplete
  *
  * Manages GPU device states, DVFS transitions, and power reporting.
  */
object GpuNodeActor:

  final case class Config(
      nodeId: GpuNodeId,
      serverConfig: GpuServerConfig,
      dvfsPolicy: DvfsPolicy,
      powerBudget: Watts,
      energySampleInterval: SimTime,
      coordinator: ActorRef[TimeCoordinator.Command]
  )

  private case class NodeAccumulator(
      state: GpuNodeState,
      emittedEvents: Vector[SimEvent]
  )

  def apply(config: Config): Behavior[EntityCommand] =
    Behaviors.setup { context =>
      val entityRef = EntityRef(s"gpu-node-${config.nodeId.value}", EntityType.GpuNode)
      config.coordinator ! TimeCoordinator.RegisterEntity(entityRef, context.self)

      val initialState = GpuNodeState.initial(
        config.nodeId,
        config.serverConfig.gpuCount,
        config.serverConfig.deviceSpec.frequencyMaxMHz
      )

      active(config, initialState, entityRef, context, SimTime.Zero)
    }

  private def active(
      config: Config,
      state: GpuNodeState,
      entityRef: EntityRef,
      context: ActorContext[EntityCommand],
      lastEnergyReportTime: SimTime
  ): Behavior[EntityCommand] =
    Behaviors.receiveMessage {
      case ProcessEvents(events, replyTo) =>
        val initial = NodeAccumulator(state, Vector.empty)

        val acc = events.foldLeft(initial) { (acc, event) =>
          event.payload match
            case GpuPowerReport(nodeId, deviceId, watts, fromTime, toTime, energyWh, computeW, memW) =>
              // Forward power reports (generated internally)
              acc

            case GpuDvfsChange(nodeId, deviceId, oldFreq, newFreq, reason) =>
              // Apply DVFS change to device state
              val updatedState = acc.state.updateFrequency(deviceId, newFreq)
              acc.copy(state = updatedState)

            case EnergySample(currentTime) =>
              // Apply DVFS policy and generate power reports for all devices
              val spec     = config.serverConfig.deviceSpec
              val duration = (currentTime - lastEnergyReportTime).value

              val (updatedState, powerEvents) = acc.state.devices.toVector.foldLeft(
                (acc.state, Vector.empty[SimEvent])
              ) { case ((nodeState, events), (devId, devState)) =>
                // Invoke DVFS policy to compute target frequency
                val targetFreq = config.dvfsPolicy(spec, devState.activity, config.powerBudget)
                val stateAfterDvfs =
                  if targetFreq != devState.currentFreqMHz then nodeState.updateFrequency(devId, targetFreq)
                  else nodeState

                // Calculate power with updated frequency
                val activityAtFreq = devState.activity.copy(currentFreqMHz = targetFreq)
                val components     = MultiComponentGpuPower.calculate(spec, activityAtFreq)
                val energyWh       = WattHours(components.total.value * duration / 3600.0)

                val dvfsEvents =
                  if targetFreq != devState.currentFreqMHz then
                    Vector(
                      SimEvent(
                        time = currentTime,
                        source = entityRef,
                        destination = entityRef,
                        payload =
                          GpuDvfsChange(config.nodeId, devId, devState.currentFreqMHz, targetFreq, "dvfs-policy"),
                        serial = SerialNumber.Zero
                      )
                    )
                  else Vector.empty

                val powerEvent = SimEvent(
                  time = currentTime,
                  source = entityRef,
                  destination = entityRef,
                  payload = GpuPowerReport(
                    config.nodeId,
                    devId,
                    components.total,
                    lastEnergyReportTime,
                    currentTime,
                    energyWh,
                    components.computeWatts,
                    components.hbmWatts
                  ),
                  serial = SerialNumber.Zero
                )

                (stateAfterDvfs, events ++ dvfsEvents :+ powerEvent)
              }
              acc.copy(state = updatedState, emittedEvents = acc.emittedEvents ++ powerEvents)

            case _ =>
              context.log.debug("GPU node {} ignoring event: {}", config.nodeId.value, event.payload)
              acc
        }

        if acc.emittedEvents.nonEmpty then config.coordinator ! TimeCoordinator.ScheduleEvents(acc.emittedEvents)

        val newLastReport = events.lastOption.map(_.time).getOrElse(lastEnergyReportTime)
        replyTo ! StepComplete(entityRef)
        active(config, acc.state, entityRef, context, newLastReport)

      case TimeCoordinator.FinalSnapshot(_, replyTo) =>
        // No end-of-sim bookkeeping; reply immediately so the coordinator
        // can proceed with shutdown.
        replyTo ! TimeCoordinator.FinalSnapshotComplete(entityRef)
        Behaviors.same
    }
