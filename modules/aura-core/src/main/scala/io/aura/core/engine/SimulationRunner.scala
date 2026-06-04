// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.engine

import org.apache.pekko.actor.typed.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.AskPattern.*
import org.apache.pekko.util.Timeout

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.*
import io.aura.core.types.*

/** Entry point for running simulations.
  *
  * Provides a simple `run(config): Either[SimulationError, SimulationResults]` API that hides the actor system
  * complexity from users.
  */
object SimulationRunner:

  /** Run a simulation to completion and return results.
    *
    * This is the primary user-facing API. It:
    *   1. Creates an ActorSystem with a SimulationGuardian 2. Sends the configuration to the guardian 3. Waits for
    *      completion 4. Returns immutable SimulationResults wrapped in Either
    */
  def run(
      config: SimulationConfig,
      timeout: FiniteDuration = 10.minutes
  ): Either[SimulationError, SimulationResults] =
    given Timeout = Timeout(timeout)

    val systemName = s"aura-sim-${java.util.concurrent.ThreadLocalRandom.current().nextLong(Long.MaxValue)}"
    val system     = ActorSystem(SimulationGuardian(), systemName)
    given ActorSystem[SimulationGuardian.Command] = system

    try
      val resultFuture: Future[TimeCoordinator.SimulationStatus] =
        system.ask(ref => SimulationGuardian.RunSimulation(config, ref))

      val status = Await.result(resultFuture, timeout)

      status match
        case TimeCoordinator.Completed(_, _, results) =>
          Right(results)
        case TimeCoordinator.Failed(reason) =>
          Left(SimulationError.ExecutionFailed(reason))
        case TimeCoordinator.Running(_, _) =>
          Left(SimulationError.Timeout("Simulation still running after timeout"))
    catch
      case e: java.util.concurrent.TimeoutException =>
        Left(SimulationError.Timeout(s"Simulation timed out after $timeout"))
      case e: Exception =>
        Left(SimulationError.ExecutionFailed(e.getMessage))
    finally
      system.terminate()
      Await.result(system.whenTerminated, 30.seconds)

  /** Run a simulation asynchronously. */
  def runAsync(
      config: SimulationConfig,
      timeout: FiniteDuration = 10.minutes
  ): Future[Either[SimulationError, SimulationResults]] =
    given Timeout = Timeout(timeout)

    val asyncName = s"aura-sim-async-${java.util.concurrent.ThreadLocalRandom.current().nextLong(Long.MaxValue)}"
    val system    = ActorSystem(SimulationGuardian(), asyncName)
    given ActorSystem[SimulationGuardian.Command] = system

    import system.executionContext

    val resultFuture: Future[TimeCoordinator.SimulationStatus] =
      system.ask(ref => SimulationGuardian.RunSimulation(config, ref))

    resultFuture
      .map {
        case TimeCoordinator.Completed(_, _, results) =>
          Right(results)
        case TimeCoordinator.Failed(reason) =>
          Left(SimulationError.ExecutionFailed(reason))
        case _ =>
          Left(SimulationError.Timeout("Unexpected simulation status"))
      }
      .recover {
        case e: java.util.concurrent.TimeoutException =>
          Left(SimulationError.Timeout(s"Simulation timed out after $timeout"))
        case e: Exception =>
          Left(SimulationError.ExecutionFailed(e.getMessage))
      }
      .andThen { case _ =>
        system.terminate()
      }

/** Typed simulation errors. */
enum SimulationError:
  case ExecutionFailed(reason: String)
  case Timeout(reason: String)
