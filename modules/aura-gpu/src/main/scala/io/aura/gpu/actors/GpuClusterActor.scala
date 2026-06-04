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

/** Actor representing a cluster of GPU nodes.
  *
  * Manages cluster-level state and routes events to individual GPU nodes. Follows the standard Aura actor pattern with
  * immutable accumulators.
  */
object GpuClusterActor:

  final case class Config(
      clusterId: GpuClusterId,
      serverConfig: GpuServerConfig,
      nodeCount: Int,
      dvfsPolicy: DvfsPolicy,
      powerBudget: Watts,
      energySampleInterval: SimTime,
      coordinator: ActorRef[TimeCoordinator.Command]
  )

  private case class ClusterAccumulator(
      state: GpuClusterState,
      emittedEvents: Vector[SimEvent],
      totalEnergyWh: WattHours,
      devicePowerSnapshots: Map[GpuDeviceId, Watts]
  )

  def apply(config: Config): Behavior[EntityCommand] =
    Behaviors.setup { context =>
      val entityRef = EntityRef(s"gpu-cluster-${config.clusterId.value}", EntityType.GpuCluster)
      config.coordinator ! TimeCoordinator.RegisterEntity(entityRef, context.self)

      val initialState = GpuClusterState.initial(
        config.clusterId,
        config.serverConfig,
        config.nodeCount
      )

      active(config, initialState, entityRef, context)
    }

  private def active(
      config: Config,
      state: GpuClusterState,
      entityRef: EntityRef,
      context: ActorContext[EntityCommand]
  ): Behavior[EntityCommand] =
    Behaviors.receiveMessage {
      case ProcessEvents(events, replyTo) =>
        val initial = ClusterAccumulator(state, Vector.empty, WattHours.Zero, Map.empty)

        val acc = events.foldLeft(initial) { (acc, event) =>
          event.payload match
            case GpuNodeRegister(nodeId, gpuCount, gpuMemoryMB) =>
              // Node registration — state already initialized
              acc

            case GpuPowerReport(nodeId, deviceId, watts, fromTime, toTime, energyWh, computeW, memW) =>
              // Aggregate power reports: accumulate energy and track latest power per device
              acc.copy(
                totalEnergyWh = WattHours(acc.totalEnergyWh.value + energyWh.value),
                devicePowerSnapshots = acc.devicePowerSnapshots + (deviceId -> watts)
              )

            case _ =>
              context.log.debug("GPU cluster {} ignoring event: {}", config.clusterId.value, event.payload)
              acc
        }

        if acc.emittedEvents.nonEmpty then config.coordinator ! TimeCoordinator.ScheduleEvents(acc.emittedEvents)

        replyTo ! StepComplete(entityRef)
        active(config, acc.state, entityRef, context)

      case TimeCoordinator.FinalSnapshot(_, replyTo) =>
        // No end-of-sim bookkeeping; reply immediately so the coordinator
        // can proceed with shutdown.
        replyTo ! TimeCoordinator.FinalSnapshotComplete(entityRef)
        Behaviors.same
    }
