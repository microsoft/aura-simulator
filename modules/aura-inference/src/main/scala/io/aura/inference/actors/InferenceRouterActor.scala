// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.inference.actors

import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}

import io.aura.core.types.*
import io.aura.core.events.*
import io.aura.core.events.SimEventPayload.*
import io.aura.core.engine.TimeCoordinator
import io.aura.core.engine.TimeCoordinator.{EntityCommand, ProcessEvents, StepComplete}
import io.aura.inference.scheduling.EnergyAwareScheduler
import io.aura.inference.scheduling.EnergyAwareScheduler.{CagrConfig, RegionInfo}

/** Actor that routes inference requests across multiple engine replicas.
  *
  * Supports multiple routing strategies:
  *   - Round-robin: cycle through engines
  *   - Least-loaded: route to engine with lowest batch occupancy
  *   - Carbon-aware (CAGR): minimize CO2 subject to latency constraints
  *
  * Follows the standard Aura actor pattern: immutable state, foldLeft over events.
  */
object InferenceRouterActor:

  /** Routing strategy. */
  enum RoutingStrategy:
    case RoundRobin
    case LeastLoaded
    case CarbonAware(regions: Vector[RegionInfo], config: CagrConfig)

  /** Per-engine metadata tracked by the router. */
  final case class EngineInfo(
      engineRef: EntityRef,
      regionId: String,
      currentBatchSize: Int,
      maxBatchSize: Int,
      completedCount: Long
  ):
    def loadFraction: Double = currentBatchSize.toDouble / maxBatchSize.max(1)

  final case class Config(
      routerName: String,
      engines: Vector[EngineInfo],
      routingStrategy: RoutingStrategy,
      coordinator: ActorRef[TimeCoordinator.Command]
  )

  private case class RouterState(
      engines: Vector[EngineInfo],
      requestCount: Long,
      routingDecisions: Map[InferenceRequestId, Int] // requestId → engine index
  )

  def apply(config: Config): Behavior[EntityCommand] =
    Behaviors.setup { context =>
      val entityRef = EntityRef(config.routerName, EntityType.InferenceRouter)
      config.coordinator ! TimeCoordinator.RegisterEntity(entityRef, context.self)

      val initialState = RouterState(
        engines = config.engines,
        requestCount = 0,
        routingDecisions = Map.empty
      )

      active(config, initialState, entityRef, context)
    }

  private def active(
      config: Config,
      state: RouterState,
      entityRef: EntityRef,
      context: ActorContext[EntityCommand]
  ): Behavior[EntityCommand] =
    Behaviors.receiveMessage {
      case ProcessEvents(events, replyTo) =>
        val (newState, emittedEvents) = events.foldLeft((state, Vector.empty[SimEvent])) {
          case ((st, emitted), event) =>
            event.payload match

              case submit @ InferenceRequestSubmit(
                    requestId,
                    modelId,
                    promptTokens,
                    maxOutputTokens,
                    sloTtft,
                    sloTpot
                  ) =>
                // Route request to an engine
                val engineIdx    = selectEngine(st, config.routingStrategy)
                val targetEngine = st.engines(engineIdx)

                // Forward the request to the selected engine
                val forwardEvent = SimEvent(
                  time = event.time,
                  source = entityRef,
                  destination = targetEngine.engineRef,
                  payload = submit,
                  serial = SerialNumber.Zero
                )

                // Update engine batch estimate
                val updatedEngine  = targetEngine.copy(currentBatchSize = targetEngine.currentBatchSize + 1)
                val updatedEngines = st.engines.updated(engineIdx, updatedEngine)
                val updatedState = st.copy(
                  engines = updatedEngines,
                  requestCount = st.requestCount + 1,
                  routingDecisions = st.routingDecisions + (requestId -> engineIdx)
                )

                (updatedState, emitted :+ forwardEvent)

              case InferenceRequestComplete(reqId, _, _, _, _, _, _, _, _) =>
                // Update batch count for the engine that completed
                val engineIdx = st.routingDecisions.getOrElse(reqId, 0)
                val updatedEngines = if engineIdx < st.engines.size then
                  val eng = st.engines(engineIdx)
                  st.engines.updated(
                    engineIdx,
                    eng.copy(
                      currentBatchSize = (eng.currentBatchSize - 1).max(0),
                      completedCount = eng.completedCount + 1
                    )
                  )
                else st.engines

                val updatedState = st.copy(
                  engines = updatedEngines,
                  routingDecisions = st.routingDecisions - reqId
                )
                (updatedState, emitted)

              case _ =>
                (st, emitted)
        }

        if emittedEvents.nonEmpty then config.coordinator ! TimeCoordinator.ScheduleEvents(emittedEvents)

        replyTo ! StepComplete(entityRef)
        active(config, newState, entityRef, context)

      case TimeCoordinator.FinalSnapshot(_, replyTo) =>
        // No end-of-sim bookkeeping; reply immediately so the coordinator
        // can proceed with shutdown.
        replyTo ! TimeCoordinator.FinalSnapshotComplete(entityRef)
        Behaviors.same
    }

  /** Select engine index based on routing strategy. */
  private def selectEngine(state: RouterState, strategy: RoutingStrategy): Int =
    val engines = state.engines
    if engines.isEmpty then 0
    else
      strategy match
        case RoutingStrategy.RoundRobin =>
          (state.requestCount % engines.size).toInt

        case RoutingStrategy.LeastLoaded =>
          engines.zipWithIndex.minBy(_._1.loadFraction)._2

        case RoutingStrategy.CarbonAware(regions, config) =>
          // First use CAGR to pick the best region
          val regionIdx    = EnergyAwareScheduler.cagrRouteRequest(regions, config)
          val targetRegion = regions(regionIdx).regionId

          // Then pick the least-loaded engine in that region
          val regionEngines = engines.zipWithIndex.filter(_._1.regionId == targetRegion)
          if regionEngines.nonEmpty then regionEngines.minBy(_._1.loadFraction)._2
          else
            // Fallback: least loaded across all engines
            engines.zipWithIndex.minBy(_._1.loadFraction)._2
