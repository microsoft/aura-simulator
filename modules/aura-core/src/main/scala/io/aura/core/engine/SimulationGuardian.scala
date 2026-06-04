// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.engine

import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}

import io.aura.core.types.*
import io.aura.core.events.*
import io.aura.core.engine.TimeCoordinator.{
  EntityCommand,
  FinalSnapshot,
  FinalSnapshotComplete,
  ProcessEvents,
  StepComplete
}

/** Top-level guardian actor that manages the simulation lifecycle.
  *
  * Responsibilities are purely setup:
  *   1. Spawn the TimeCoordinator 2. Register itself as a sim-guardian entity (so brokers can route metrics-bearing
  *      events here and they flow through the coordinator's MetricsCollector recording path) 3. Hand the simulation off
  *      to `config.spawnEntities` to bring up the datacenters / brokers / clusters 4. Send `StartSimulation` to begin
  *      event dispatch
  *
  * Termination is owned by the TimeCoordinator (quiescence-based, see `TimeCoordinator.beginFinalSnapshot`). The
  * guardian does not count broker-end signals and does not initiate shutdown.
  */
object SimulationGuardian:

  sealed trait Command
  case class RunSimulation(
      config: SimulationConfig,
      replyTo: ActorRef[TimeCoordinator.SimulationStatus]
  ) extends Command

  private case class WrappedEntityCommand(cmd: EntityCommand) extends Command

  def apply(): Behavior[Command] =
    Behaviors.setup { context =>
      idle(context)
    }

  private def idle(context: ActorContext[Command]): Behavior[Command] =
    Behaviors.receiveMessage {
      case RunSimulation(config, replyTo) =>
        val coordinator = context.spawn(
          TimeCoordinator(config.endTime),
          "time-coordinator"
        )

        val entityRef     = EntityRef("sim-guardian", EntityType.SimGuardian)
        val entityAdapter = context.messageAdapter[EntityCommand](WrappedEntityCommand.apply)
        coordinator ! TimeCoordinator.RegisterEntity(entityRef, entityAdapter)

        config.spawnEntities(context, coordinator)

        coordinator ! TimeCoordinator.StartSimulation(replyTo)

        running(entityRef)

      case _ => Behaviors.same
    }

  private def running(entityRef: EntityRef): Behavior[Command] =
    Behaviors.receiveMessage {
      // Guardian is a passive sink for metrics-bearing events routed to
      // "sim-guardian". They're recorded implicitly via the coordinator's
      // `recordEvent` dispatch (see TimeCoordinator.running); the only work
      // here is to ack the barrier so the coordinator can advance time.
      case WrappedEntityCommand(ProcessEvents(_, replyTo)) =>
        replyTo ! StepComplete(entityRef)
        Behaviors.same

      // Final-snapshot handshake — guardian has no end-of-sim bookkeeping
      // of its own; ack immediately so the coordinator can move on.
      case WrappedEntityCommand(FinalSnapshot(_, replyTo)) =>
        replyTo ! FinalSnapshotComplete(entityRef)
        Behaviors.same

      case _ => Behaviors.same
    }

/** Configuration passed to SimulationGuardian to set up a simulation.
  *
  * `brokerCount` is retained for DSL configuration assertions in tests; it is not consulted by the termination
  * machinery.
  */
trait SimulationConfig:
  def endTime: SimTime
  def brokerCount: Int = 1
  def spawnEntities(
      context: ActorContext[SimulationGuardian.Command],
      coordinator: ActorRef[TimeCoordinator.Command]
  ): Unit
