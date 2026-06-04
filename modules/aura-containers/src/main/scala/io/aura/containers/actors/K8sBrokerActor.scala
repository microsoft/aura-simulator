// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.containers.actors

import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}

import io.aura.core.types.*
import io.aura.core.events.*
import io.aura.core.events.SimEventPayload.*
import io.aura.core.engine.TimeCoordinator
import io.aura.core.engine.TimeCoordinator.{EntityCommand, ProcessEvents, StepComplete}
import io.aura.containers.*
import io.aura.containers.state.*

/** Actor representing a K8s workload broker.
  *
  * Follows the ServerlessBrokerActor pattern:
  *   - Generates PodScheduleRequest events for all deployment replicas
  *   - Tracks pod lifecycle (scheduled, started, completed, failed, unschedulable)
  *   - Coordinator terminates on quiescence (event-queue empty + FinalSnapshot handshake)
  */
object K8sBrokerActor:

  final case class DeploymentBatch(
      deploymentId: DeploymentId,
      name: String,
      podSpec: PodSpec,
      replicas: Int,
      workloadConfig: Option[WorkloadConfig],
      submitDelay: SimTime = SimTime.Zero
  )

  final case class Config(
      brokerId: String,
      clusterRef: EntityRef,
      deployments: Vector[DeploymentBatch],
      coordinator: ActorRef[TimeCoordinator.Command]
  )

  /** Pre-generate PodScheduleRequest events on the caller thread. Returns (events, pendingPodIds) for scheduling before
    * actor startup.
    */
  def generateEvents(
      config: Config,
      nextPodId: Long
  ): (Vector[SimEvent], Set[PodId]) =
    val entityRef         = EntityRef(config.brokerId, EntityType.K8sCluster)
    val eventsBuilder     = Vector.newBuilder[SimEvent]
    val pendingIdsBuilder = Set.newBuilder[PodId]
    val _ = config.deployments.foldLeft(nextPodId) { case (currentId, batch) =>
      (0 until batch.replicas).foldLeft(currentId) { case (podIdCounter, i) =>
        val podId   = PodId(podIdCounter)
        val podName = s"${batch.name}-$i"
        val spec = PodDeploySpec(
          name = podName,
          namespace = "default",
          containers = batch.podSpec.containers.map(c =>
            ContainerResourceSpec(c.name, c.image, c.cpuRequest, c.cpuLimit, c.memoryRequest, c.memoryLimit)
          ),
          restartPolicy = batch.podSpec.restartPolicy.toString,
          priority = batch.podSpec.priority,
          nodeSelector = batch.podSpec.nodeSelector
        )
        eventsBuilder += SimEvent(
          time = batch.submitDelay,
          source = entityRef,
          destination = config.clusterRef,
          payload = PodScheduleRequest(podId, batch.deploymentId, spec),
          serial = SerialNumber.Zero
        )
        pendingIdsBuilder += podId
        podIdCounter + 1
      }
    }
    (eventsBuilder.result(), pendingIdsBuilder.result())

  private final case class BrokerState(
      totalPods: Int,
      pendingPods: Set[PodId],
      runningPods: Set[PodId],
      completedPods: Vector[PodId],
      failedPods: Vector[PodId],
      unschedulablePods: Vector[PodId]
  ):
    def totalResolved: Int =
      completedPods.size + failedPods.size + unschedulablePods.size

  /** Immutable accumulator for broker event processing. */
  private case class BrokerAccumulator(
      state: BrokerState,
      emittedEvents: Vector[SimEvent]
  )

  /** Create broker with pre-generated pending IDs (events already scheduled by caller). */
  def apply(config: Config, pendingPodIds: Set[PodId]): Behavior[EntityCommand] =
    Behaviors.setup { context =>
      val entityRef = EntityRef(config.brokerId, EntityType.K8sCluster)
      config.coordinator ! TimeCoordinator.RegisterEntity(entityRef, context.self)

      val initialState = BrokerState(
        totalPods = pendingPodIds.size,
        pendingPods = pendingPodIds,
        runningPods = Set.empty,
        completedPods = Vector.empty,
        failedPods = Vector.empty,
        unschedulablePods = Vector.empty
      )

      active(config, initialState, entityRef, context)
    }

  private def active(
      config: Config,
      state: BrokerState,
      entityRef: EntityRef,
      context: ActorContext[EntityCommand]
  ): Behavior[EntityCommand] =
    Behaviors.receiveMessage {
      case ProcessEvents(events, replyTo) =>
        val initial = BrokerAccumulator(state, Vector.empty)

        val acc = events.foldLeft(initial) { (acc, event) =>
          event.payload match
            case PodScheduled(podId, _, _) =>
              acc.copy(
                state = acc.state.copy(
                  pendingPods = acc.state.pendingPods - podId,
                  runningPods = acc.state.runningPods + podId
                )
              )

            case PodStarted(_, _, _) =>
              acc

            case PodCompleted(podId, _, _) =>
              acc.copy(state =
                acc.state.copy(
                  runningPods = acc.state.runningPods - podId,
                  completedPods = acc.state.completedPods :+ podId
                )
              )

            case PodFailed(podId, _, _) =>
              acc.copy(state =
                acc.state.copy(
                  pendingPods = acc.state.pendingPods - podId,
                  runningPods = acc.state.runningPods - podId,
                  failedPods = acc.state.failedPods :+ podId
                )
              )

            case PodUnschedulable(podId, _) =>
              acc.copy(state =
                acc.state.copy(
                  pendingPods = acc.state.pendingPods - podId,
                  unschedulablePods = acc.state.unschedulablePods :+ podId
                )
              )

            case _ =>
              context.log.debug("K8s broker {} ignoring event: {}", config.brokerId, event.payload)
              acc
        }

        if acc.emittedEvents.nonEmpty then config.coordinator ! TimeCoordinator.ScheduleEvents(acc.emittedEvents)

        replyTo ! StepComplete(entityRef)
        active(config, acc.state, entityRef, context)

      case TimeCoordinator.FinalSnapshot(_, replyTo) =>
        // Pod lifecycle events are emitted by the cluster as they happen;
        // no end-of-sim bookkeeping required.
        replyTo ! TimeCoordinator.FinalSnapshotComplete(entityRef)
        Behaviors.same
    }
