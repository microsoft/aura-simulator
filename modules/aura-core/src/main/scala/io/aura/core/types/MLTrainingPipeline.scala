// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

/** ML training pipeline simulation: distributed training strategies, scaling efficiency, checkpointing, and throughput
  * estimation — built on top of GpuModel, modeled as pure functions.
  */

/** Distributed training strategy. */
enum DistributedStrategy:
  /** Replicate model on each GPU, split data across GPUs. */
  case DataParallel

  /** Split model layers across GPUs. */
  case ModelParallel

  /** Pipeline stages across GPUs (micro-batching). */
  case PipelineParallel

  /** Combination of data + model parallelism. */
  case Hybrid(dataParallelDegree: Int, modelParallelDegree: Int)

/** ML model specification. */
final case class ModelSpec(
    name: String,
    parameterCount: Long,
    layerCount: Int,
    modelSizeGB: Double,
    precision: GpuPrecision = GpuPrecision.Float16
):
  /** Memory needed per GPU for data-parallel training (full model + optimizer). */
  def memoryPerGpuGB(strategy: DistributedStrategy): Double = strategy match
    case DistributedStrategy.DataParallel =>
      modelSizeGB * 4.0 // model + gradients + optimizer states (Adam ~4x)
    case DistributedStrategy.ModelParallel =>
      modelSizeGB * 4.0 / layerCount.toDouble // split across layers
    case DistributedStrategy.PipelineParallel =>
      modelSizeGB * 4.0 / layerCount.toDouble
    case DistributedStrategy.Hybrid(_, mp) =>
      modelSizeGB * 4.0 / mp.toDouble

object ModelSpec:
  val resnet50: ModelSpec = ModelSpec("ResNet-50", 25_600_000L, 50, 0.1)
  val bert: ModelSpec     = ModelSpec("BERT-Large", 340_000_000L, 24, 1.3)
  val gpt2: ModelSpec     = ModelSpec("GPT-2", 1_500_000_000L, 48, 6.0)
  val llama7B: ModelSpec  = ModelSpec("LLaMA-7B", 7_000_000_000L, 32, 14.0)
  val llama70B: ModelSpec = ModelSpec("LLaMA-70B", 70_000_000_000L, 80, 140.0)

/** A training job with full pipeline stages. */
final case class TrainingJob(
    model: ModelSpec,
    totalSamples: Long,
    batchSize: Int,
    epochs: Int,
    strategy: DistributedStrategy = DistributedStrategy.DataParallel,
    checkpointEveryEpochs: Int = 1,
    evaluateEveryEpochs: Int = 1
):
  def totalBatches: Long    = (totalSamples / batchSize) * epochs
  def batchesPerEpoch: Long = totalSamples / batchSize
  def totalCheckpoints: Int = epochs / math.max(1, checkpointEveryEpochs)
  def totalEvaluations: Int = epochs / math.max(1, evaluateEveryEpochs)

/** Training progress snapshot. */
final case class TrainingProgress(
    currentEpoch: Int,
    currentBatch: Long,
    totalBatches: Long,
    loss: Double,
    throughputSamplesPerSec: Double,
    elapsedTime: SimTime
):
  def completionPct: Double =
    if totalBatches <= 0 then 100.0
    else currentBatch.toDouble / totalBatches * 100.0

  def estimatedTimeRemaining: SimTime =
    if currentBatch <= 0 || throughputSamplesPerSec <= 0.0 then SimTime.MaxValue
    else
      val remainingBatches = totalBatches - currentBatch
      SimTime(remainingBatches.toDouble / throughputSamplesPerSec * 1.0)

/** GPU cluster for distributed training. */
final case class GpuCluster(
    nodes: Vector[GpuClusterNode],
    interconnectBandwidthGbps: Double = 100.0
):
  def totalGpus: Int           = nodes.map(_.gpuSpec.gpuCount).sum
  def totalTflops: Double      = nodes.map(n => n.gpuSpec.tflopsFloat16 * n.gpuSpec.gpuCount).sum
  def totalGpuMemoryGB: Double = nodes.map(n => n.gpuSpec.totalGpuMemory.value / 1024.0).sum

  def allocate(gpuCount: Int): Option[GpuCluster] =
    val available      = nodes.filter(_.available)
    val totalAvailable = available.map(_.gpuSpec.gpuCount).sum
    if totalAvailable < gpuCount then None
    else
      val (updated, _) = nodes.foldLeft((Vector.empty[GpuClusterNode], gpuCount)) { case ((acc, remaining), node) =>
        if remaining > 0 && node.available then (acc :+ node.copy(available = false), remaining - node.gpuSpec.gpuCount)
        else (acc :+ node, remaining)
      }
      Some(copy(nodes = updated))

final case class GpuClusterNode(
    nodeId: String,
    gpuSpec: GpuSpec,
    available: Boolean = true
)

/** Pure-function ML pipeline engine. */
object MLPipelineEngine:

  /** Estimate total training time for a job on a GPU cluster. */
  def estimateTrainingTime(
      job: TrainingJob,
      gpuSpec: GpuSpec,
      gpuCount: Int
  ): SimTime =
    val singleGpuTflops = gpuSpec.tflopsFloat16 * gpuSpec.gpuCount
    if singleGpuTflops <= 0 then SimTime.MaxValue
    else
      // Estimate FLOPs per sample (rough: 6 * params for forward + backward)
      val flopsPerSample  = job.model.parameterCount * 6.0 / 1e12 // in TFLOPs
      val totalFlops      = flopsPerSample * job.totalSamples * job.epochs
      val effectiveTflops = singleGpuTflops * gpuCount * scalingEfficiency(gpuCount, job.strategy)
      val computeTime     = totalFlops / effectiveTflops

      // Add checkpoint and evaluation overhead
      val checkpointTime = job.totalCheckpoints * checkpointOverhead(job.model.modelSizeGB, 2.0).value
      val evalTime       = job.totalEvaluations * (computeTime / job.epochs * 0.1) // eval ~10% of epoch

      SimTime(computeTime + checkpointTime + evalTime)

  /** Scaling efficiency for multi-GPU training (accounts for communication overhead). */
  def scalingEfficiency(gpuCount: Int, strategy: DistributedStrategy): Double =
    if gpuCount <= 1 then 1.0
    else
      strategy match
        case DistributedStrategy.DataParallel =>
          // AllReduce overhead grows with GPU count
          1.0 / (1.0 + 0.05 * math.log(gpuCount.toDouble))

        case DistributedStrategy.ModelParallel =>
          // Pipeline bubbles reduce efficiency
          1.0 / (1.0 + 0.1 * (gpuCount - 1))

        case DistributedStrategy.PipelineParallel =>
          // Better than model parallel due to micro-batching
          1.0 / (1.0 + 0.03 * (gpuCount - 1))

        case DistributedStrategy.Hybrid(dp, _) =>
          // Combine data parallel efficiency with model parallel penalty
          val dpEff   = scalingEfficiency(dp, DistributedStrategy.DataParallel)
          val mpCount = gpuCount / math.max(1, dp)
          val mpEff   = scalingEfficiency(mpCount, DistributedStrategy.ModelParallel)
          dpEff * mpEff

  /** Time to write a checkpoint to storage.
    * @param modelSizeGB
    *   model size in GB
    * @param storageSpeedGBps
    *   storage write speed in GB/s
    */
  def checkpointOverhead(modelSizeGB: Double, storageSpeedGBps: Double): SimTime =
    if storageSpeedGBps <= 0.0 then SimTime.MaxValue
    else SimTime(modelSizeGB / storageSpeedGBps)

  /** Estimate training throughput in samples per second. */
  def estimateThroughput(
      job: TrainingJob,
      gpuSpec: GpuSpec,
      gpuCount: Int
  ): Double =
    val singleGpuTflops = gpuSpec.tflopsFloat16 * gpuSpec.gpuCount
    if singleGpuTflops <= 0 then 0.0
    else
      val flopsPerSample = job.model.parameterCount * 6.0 / 1e12
      if flopsPerSample <= 0 then 0.0
      else
        val effectiveTflops = singleGpuTflops * gpuCount * scalingEfficiency(gpuCount, job.strategy)
        effectiveTflops / flopsPerSample

  /** Find optimal GPU count to meet a target training time. */
  def optimalGpuCount(
      job: TrainingJob,
      gpuSpec: GpuSpec,
      maxGpus: Int,
      targetTime: SimTime
  ): Int =
    (1 to maxGpus)
      .find { count =>
        estimateTrainingTime(job, gpuSpec, count).value <= targetTime.value
      }
      .getOrElse(maxGpus)

  /** Compute communication volume for all-reduce (bytes per step). */
  def allReduceVolume(model: ModelSpec, gpuCount: Int): Double =
    if gpuCount <= 1 then 0.0
    else
      // Ring all-reduce: 2 * (n-1)/n * model_size
      val bytesPerParam = model.precision match
        case GpuPrecision.Float32                         => 4.0
        case GpuPrecision.Float16 | GpuPrecision.BFloat16 => 2.0
        case GpuPrecision.Int8                            => 1.0
      val gradientBytes = model.parameterCount * bytesPerParam
      2.0 * (gpuCount - 1).toDouble / gpuCount * gradientBytes
