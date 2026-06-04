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
import io.aura.inference.model.*
import io.aura.inference.state.*
import io.aura.inference.scheduling.BatchScheduler as InferenceBatchScheduler
import io.aura.inference.energy.InferencePowerModel
import io.aura.gpu.*

/** Actor representing an LLM inference engine (like a vLLM instance).
  *
  * Follows the standard Aura actor pattern with immutable accumulators:
  *   - Registers with TimeCoordinator
  *   - Receives ProcessEvents, foldLeft over events
  *   - Emits events via ScheduleEvents, replies StepComplete
  *
  * Models the full inference lifecycle:
  *   1. Request admission (batch scheduler) 2. Prefill phase (compute TTFT) 3. Decode iterations (compute TPOT) 4.
  *      Completion + energy tracking
  */
object InferenceEngineActor:

  /** Scheduling strategy for the engine. */
  enum SchedulingStrategy:
    case ContinuousBatching
    case ChunkedPrefill(chunkSize: Int = 512)
    case PowerCapped
    case Priority

  final case class Config(
      engineName: String,
      model: LlmModelSpec,
      deviceSpec: GpuDeviceSpec,
      tpDegree: Int = 1,
      ppStages: Int = 1,
      epDegree: Int = 1,
      maxBatchSize: Int = 256,
      iterationInterval: SimTime = SimTime(0.01),
      dvfsPolicy: DvfsPolicy = DvfsPolicy.maxPerformance,
      powerBudget: Watts = Watts(700.0),
      schedulingStrategy: SchedulingStrategy = SchedulingStrategy.ContinuousBatching,
      coordinator: ActorRef[TimeCoordinator.Command]
  )

  private case class EngineAccumulator(
      state: EngineState,
      emittedEvents: Vector[SimEvent]
  )

  def apply(config: Config): Behavior[EntityCommand] =
    Behaviors.setup { context =>
      val entityRef = EntityRef(config.engineName, EntityType.InferenceEngine)
      config.coordinator ! TimeCoordinator.RegisterEntity(entityRef, context.self)

      // Initialize KV cache from GPU memory
      // Total memory across all GPUs in the TP group (PP stages have separate memory)
      val totalGpuMemoryMB = MegaBytes(config.deviceSpec.gpuMemoryMB.value * config.tpDegree)
      // Model size per GPU: divided by TP (sharding), PP (layer splitting), and EP (expert distribution)
      val totalParallelism = config.tpDegree * config.ppStages * (if config.model.isMoE then config.epDegree else 1)
      val modelSizeMB      = MegaBytes(config.model.modelSizeMB.value / totalParallelism.max(1))
      val kvCache = KvCacheState.fromGpuMemory(
        totalGpuMemoryMB,
        modelSizeMB,
        config.model.kvCacheMBPerToken
      )

      val initialState = EngineState(
        model = config.model,
        kvCache = kvCache,
        waitingQueue = Vector.empty,
        runningBatch = Vector.empty,
        completedCount = 0,
        failedCount = 0,
        maxBatchSize = config.maxBatchSize,
        tpDegree = config.tpDegree,
        ppStages = config.ppStages,
        epDegree = config.epDegree
      )

      active(config, initialState, entityRef, context)
    }

  private def active(
      config: Config,
      state: EngineState,
      entityRef: EntityRef,
      context: ActorContext[EntityCommand]
  ): Behavior[EntityCommand] =
    Behaviors.receiveMessage {
      case ProcessEvents(events, replyTo) =>
        val initial = EngineAccumulator(state, Vector.empty)

        val acc = events.foldLeft(initial) { (acc, event) =>
          event.payload match

            case InferenceRequestSubmit(requestId, modelId, promptTokens, maxOutputTokens, sloTtft, sloTpot) =>
              // Enqueue request
              val pending = PendingRequest(
                requestId,
                modelId,
                promptTokens,
                maxOutputTokens,
                sloTtft,
                sloTpot,
                event.time,
                priority = 0
              )
              val updatedState = acc.state.enqueue(pending)

              // Try to admit immediately using configured strategy
              val scheduleResult = runScheduler(updatedState, config)
              val preemptedState = scheduleResult.toPreempt.foldLeft(updatedState)(_.preemptRequest(_))
              val (finalState, admitEvents) = processAdmissions(
                preemptedState,
                scheduleResult.toAdmit,
                event.time,
                entityRef,
                event.source,
                config
              )

              // Schedule first batch tick if this is the first request
              val tickEvents =
                if acc.state.runningBatch.isEmpty && finalState.runningBatch.nonEmpty then
                  Vector(
                    SimEvent(
                      time = event.time + config.iterationInterval,
                      source = entityRef,
                      destination = entityRef,
                      payload = InferenceBatchTick(config.engineName, event.time + config.iterationInterval),
                      serial = SerialNumber.Zero
                    )
                  )
                else Vector.empty

              acc.copy(
                state = finalState,
                emittedEvents = acc.emittedEvents ++ admitEvents ++ tickEvents
              )

            case InferencePrefillComplete(reqId, prefillTime, kvCachePages, powerWatts) =>
              // Mark prefill as complete — request can now enter decode phase
              val updatedState = acc.state.markPrefillComplete(reqId, event.time)
              acc.copy(state = updatedState)

            case InferenceBatchTick(_, currentTime) =>
              // Process one decode iteration for the running batch
              val (updatedState, decodeEvents, completionEvents, iterationTime) = processBatchIteration(
                acc.state,
                event.time,
                entityRef,
                config
              )

              // Try to admit new requests using configured strategy
              val schedResult    = runScheduler(updatedState, config)
              val preemptedState = schedResult.toPreempt.foldLeft(updatedState)(_.preemptRequest(_))
              val (finalState, admitEvents) = processAdmissions(
                preemptedState,
                schedResult.toAdmit,
                event.time,
                entityRef,
                entityRef,
                config
              )

              // Schedule next tick using model-predicted decode time
              val tickInterval = iterationTime
              val nextTickEvents =
                if finalState.runningBatch.nonEmpty || finalState.waitingQueue.nonEmpty then
                  Vector(
                    SimEvent(
                      time = event.time + tickInterval,
                      source = entityRef,
                      destination = entityRef,
                      payload = InferenceBatchTick(config.engineName, event.time + tickInterval),
                      serial = SerialNumber.Zero
                    )
                  )
                else Vector.empty

              acc.copy(
                state = finalState,
                emittedEvents = acc.emittedEvents ++ decodeEvents ++ completionEvents ++ admitEvents ++ nextTickEvents
              )

            case _ =>
              context.log.debug("Inference engine {} ignoring event: {}", config.engineName, event.payload)
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

  /** Dispatch to the configured scheduling strategy. */
  private def runScheduler(
      state: EngineState,
      config: Config
  ): InferenceBatchScheduler.ScheduleResult =
    config.schedulingStrategy match
      case SchedulingStrategy.ContinuousBatching =>
        InferenceBatchScheduler.continuousBatching(state)
      case SchedulingStrategy.ChunkedPrefill(chunkSize) =>
        InferenceBatchScheduler.chunkedPrefill(state, chunkSize)
      case SchedulingStrategy.PowerCapped =>
        InferenceBatchScheduler.powerCappedBatching(state, config.deviceSpec, config.powerBudget)
      case SchedulingStrategy.Priority =>
        InferenceBatchScheduler.priorityBatching(state)

  /** Process request admissions. */
  private def processAdmissions(
      state: EngineState,
      toAdmit: Vector[InferenceRequestId],
      currentTime: SimTime,
      engineRef: EntityRef,
      brokerRef: EntityRef,
      config: Config
  ): (EngineState, Vector[SimEvent]) =
    toAdmit.foldLeft((state, Vector.empty[SimEvent])) { case ((st, events), reqId) =>
      st.admit(reqId, currentTime) match
        case Some(admitted) =>
          // Find the admitted request
          val req = admitted.runningBatch.find(_.requestId == reqId).get

          // Allocate KV cache
          val pagesNeeded = admitted.kvCache.pagesNeeded(req.promptTokens + req.maxOutputTokens)
          val (newKvCache, _) = admitted.kvCache
            .allocate(reqId, pagesNeeded)
            .getOrElse(
              (admitted.kvCache, Vector.empty)
            )

          // Calculate prefill time
          val prefillTime = InferenceProfile.prefillLatency(
            config.model,
            req.promptTokens,
            1,
            config.deviceSpec,
            config.tpDegree,
            config.ppStages,
            config.epDegree
          )

          // Emit admission event
          val admitEvent = SimEvent(
            time = currentTime,
            source = engineRef,
            destination = brokerRef,
            payload = InferenceRequestAdmitted(reqId, req.modelId, config.engineName, currentTime),
            serial = SerialNumber.Zero
          )

          // Emit prefill complete event
          val kvPages      = pagesNeeded
          val prefillPower = InferencePowerModel.prefillPower(config.model, 1, config.deviceSpec, config.tpDegree)
          val prefillCompleteEvent = SimEvent(
            time = currentTime + prefillTime,
            source = engineRef,
            destination = engineRef,
            payload = InferencePrefillComplete(reqId, prefillTime, kvPages, prefillPower.total),
            serial = SerialNumber.Zero
          )

          // Record prefill energy on the request
          val prefillEnergyWh = WattHours(prefillPower.total.value * prefillTime.value / 3600.0)
          val stateWithKv = admitted
            .copy(kvCache = newKvCache)
            .recordPrefillEnergy(reqId, prefillEnergyWh)
          (stateWithKv, events :+ admitEvent :+ prefillCompleteEvent)

        case None =>
          // Failed to admit (shouldn't happen if scheduler checked capacity)
          val failEvent = SimEvent(
            time = currentTime,
            source = engineRef,
            destination = brokerRef,
            payload = InferenceRequestFailed(reqId, "Failed to admit request"),
            serial = SerialNumber.Zero
          )
          (st.failRequest(reqId), events :+ failEvent)
    }

  /** Intermediate state for a decode iteration step. */
  private case class DecodeStepResult(
      state: EngineState,
      completionEvents: Vector[SimEvent]
  )

  /** Process one decode iteration for the running batch.
    *
    * @return
    *   (updatedState, decodeEvents, completionEvents, iterationTime) where iterationTime is the model-predicted TPOT
    *   for tick scheduling
    */
  private def processBatchIteration(
      state: EngineState,
      currentTime: SimTime,
      engineRef: EntityRef,
      config: Config
  ): (EngineState, Vector[SimEvent], Vector[SimEvent], SimTime) =
    // Only decode requests whose prefill is complete
    val decodingRequests = state.runningBatch.filter(r => r.prefillComplete && !r.isComplete)
    val batchSize        = decodingRequests.size

    if batchSize == 0 then (state, Vector.empty, Vector.empty, config.iterationInterval)
    else
      val avgSeqLen = decodingRequests.map(_.currentSeqLen).sum / batchSize

      // Calculate decode latency for this batch step
      val tpotPerToken = InferenceProfile.decodeLatencyPerToken(
        config.model,
        batchSize,
        avgSeqLen,
        config.deviceSpec,
        config.tpDegree,
        config.ppStages,
        config.epDegree
      )

      // Apply DVFS policy to determine operating frequency
      val baseActivity = InferenceProfile.decodeActivity(
        config.model,
        batchSize,
        avgSeqLen,
        config.deviceSpec,
        config.tpDegree
      )
      val targetFreq   = config.dvfsPolicy(config.deviceSpec, baseActivity, config.powerBudget)
      val dvfsActivity = baseActivity.copy(currentFreqMHz = targetFreq)

      // Calculate power at the DVFS-adjusted frequency
      val decodePower   = MultiComponentGpuPower.calculate(config.deviceSpec, dvfsActivity)
      val energyPerStep = WattHours(decodePower.total.value * tpotPerToken.value / 3600.0 / batchSize.max(1))

      // Fold over decoding requests: record decode step, emit completion if done
      val result = decodingRequests.foldLeft(DecodeStepResult(state, Vector.empty)) { (acc, req) =>
        val stepped = acc.state.recordDecodeStep(req.requestId, 1, energyPerStep)
        stepped.runningBatch.find(_.requestId == req.requestId) match
          case Some(r) if r.isComplete =>
            val ttft       = r.ttft
            val totalTime  = currentTime - r.admitTime
            val decodeTime = SimTime(totalTime.value - ttft.value)
            val tpot   = if r.tokensGenerated > 0 then SimTime(decodeTime.value / r.tokensGenerated) else SimTime.Zero
            val sloMet = ttft <= r.sloTtft && tpot <= r.sloTpot

            val completeEvent = SimEvent(
              time = currentTime,
              source = engineRef,
              destination = engineRef,
              payload = InferenceRequestComplete(
                r.requestId,
                r.modelId,
                r.promptTokens,
                r.tokensGenerated,
                ttft,
                tpot,
                totalTime,
                r.accumulatedEnergyWh,
                sloMet
              ),
              serial = SerialNumber.Zero
            )
            DecodeStepResult(stepped.removeRequest(r.requestId), acc.completionEvents :+ completeEvent)
          case _ =>
            acc.copy(state = stepped)
      }

      (result.state, Vector.empty, result.completionEvents, tpotPerToken)
