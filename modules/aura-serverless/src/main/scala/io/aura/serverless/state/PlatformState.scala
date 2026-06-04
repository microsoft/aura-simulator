// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.serverless.state

import io.aura.core.types.*
import io.aura.serverless.Runtime

/** Container lifecycle status. */
enum ContainerStatus:
  case Idle, Executing, Evicted

/** Immutable state of a single container instance. */
final case class ContainerState(
    containerId: ContainerId,
    functionId: FunctionId,
    status: ContainerStatus,
    createdAt: SimTime,
    lastUsedAt: SimTime,
    activeInvocations: Int
)

/** Configuration for a deployed function. */
final case class FunctionConfig(
    functionId: FunctionId,
    name: String,
    memoryMB: MegaBytes,
    timeout: SimTime,
    runtime: Runtime,
    concurrencyLimit: Int,
    reservedConcurrency: Option[Int]
)

/** Tracking state for an active invocation. */
final case class ActiveInvocation(
    invocationId: InvocationId,
    functionId: FunctionId,
    containerId: ContainerId,
    startTime: SimTime,
    expectedFinishTime: SimTime,
    coldStart: Boolean
)

/** Immutable platform state for the FaaS engine. */
final case class PlatformState(
    functions: Map[FunctionId, FunctionConfig],
    containers: Map[ContainerId, ContainerState],
    activeInvocations: Map[InvocationId, ActiveInvocation],
    nextContainerId: Long,
    activeConcurrency: Map[FunctionId, Int]
):
  /** Find a warm (idle) container for the given function within TTL. */
  def findWarmContainer(functionId: FunctionId, currentTime: SimTime, ttl: SimTime): Option[ContainerId] =
    containers.collectFirst {
      case (cid, cs)
          if cs.functionId == functionId &&
            cs.status == ContainerStatus.Idle &&
            (currentTime - cs.lastUsedAt) < ttl =>
        cid
    }

  /** Get current concurrency for a function. */
  def currentConcurrency(functionId: FunctionId): Int =
    activeConcurrency.getOrElse(functionId, 0)

object PlatformState:
  val empty: PlatformState = PlatformState(
    functions = Map.empty,
    containers = Map.empty,
    activeInvocations = Map.empty,
    nextContainerId = 0L,
    activeConcurrency = Map.empty
  )
