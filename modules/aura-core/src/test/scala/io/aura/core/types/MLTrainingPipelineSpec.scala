// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MLTrainingPipelineSpec extends AnyFlatSpec with Matchers:

  private val a100      = GpuSpec.nvidiaA100
  private val h100      = GpuSpec.nvidiaH100
  private val resnetJob = TrainingJob(ModelSpec.resnet50, totalSamples = 1_000_000, batchSize = 256, epochs = 10)
  private val bertJob   = TrainingJob(ModelSpec.bert, totalSamples = 100_000, batchSize = 32, epochs = 3)
  // LLaMA job available for extended tests
  // private val llamaJob = TrainingJob(ModelSpec.llama7B, totalSamples = 50_000, batchSize = 8, epochs = 1,
  //   strategy = DistributedStrategy.DataParallel)

  // ── ModelSpec ───────────────────────────────────────────────────────────

  "ModelSpec" should "compute memory per GPU for data parallel" in {
    // ResNet-50 is 0.1 GB, data parallel needs ~4x for Adam
    ModelSpec.resnet50.memoryPerGpuGB(DistributedStrategy.DataParallel) shouldBe 0.4 +- 0.01
  }

  it should "reduce memory per GPU for model parallel" in {
    val dp = ModelSpec.llama7B.memoryPerGpuGB(DistributedStrategy.DataParallel)
    val mp = ModelSpec.llama7B.memoryPerGpuGB(DistributedStrategy.ModelParallel)
    mp should be < dp
  }

  it should "have correct presets" in {
    ModelSpec.resnet50.parameterCount shouldBe 25_600_000L
    ModelSpec.bert.layerCount shouldBe 24
    ModelSpec.llama70B.modelSizeGB shouldBe 140.0
  }

  // ── TrainingJob ─────────────────────────────────────────────────────────

  "TrainingJob" should "compute total batches" in {
    resnetJob.totalBatches shouldBe (1_000_000L / 256) * 10
  }

  it should "compute checkpoints and evaluations" in {
    val job = TrainingJob(ModelSpec.resnet50, 1000, 32, epochs = 10, checkpointEveryEpochs = 2, evaluateEveryEpochs = 5)
    job.totalCheckpoints shouldBe 5
    job.totalEvaluations shouldBe 2
  }

  // ── TrainingProgress ────────────────────────────────────────────────────

  "TrainingProgress" should "compute completion percentage" in {
    val progress = TrainingProgress(5, 500, 1000, 0.5, 100.0, SimTime(50.0))
    progress.completionPct shouldBe 50.0
  }

  // ── Scaling Efficiency ──────────────────────────────────────────────────

  "MLPipelineEngine.scalingEfficiency" should "return 1.0 for single GPU" in {
    MLPipelineEngine.scalingEfficiency(1, DistributedStrategy.DataParallel) shouldBe 1.0
  }

  it should "decrease with more GPUs for data parallel" in {
    val eff2  = MLPipelineEngine.scalingEfficiency(2, DistributedStrategy.DataParallel)
    val eff8  = MLPipelineEngine.scalingEfficiency(8, DistributedStrategy.DataParallel)
    val eff64 = MLPipelineEngine.scalingEfficiency(64, DistributedStrategy.DataParallel)
    eff2 should be > eff8
    eff8 should be > eff64
    eff2 should be > 0.9  // Good efficiency at 2 GPUs
    eff64 should be > 0.5 // Still usable at 64
  }

  it should "have pipeline parallel better than model parallel" in {
    val mp = MLPipelineEngine.scalingEfficiency(8, DistributedStrategy.ModelParallel)
    val pp = MLPipelineEngine.scalingEfficiency(8, DistributedStrategy.PipelineParallel)
    pp should be > mp
  }

  // ── Estimate Training Time ──────────────────────────────────────────────

  "MLPipelineEngine.estimateTrainingTime" should "decrease with more GPUs" in {
    val time1 = MLPipelineEngine.estimateTrainingTime(resnetJob, a100, 1)
    val time4 = MLPipelineEngine.estimateTrainingTime(resnetJob, a100, 4)
    val time8 = MLPipelineEngine.estimateTrainingTime(resnetJob, a100, 8)
    time4.value should be < time1.value
    time8.value should be < time4.value
  }

  it should "be faster on H100 than A100" in {
    val timeA100 = MLPipelineEngine.estimateTrainingTime(bertJob, a100, 1)
    val timeH100 = MLPipelineEngine.estimateTrainingTime(bertJob, h100, 1)
    timeH100.value should be < timeA100.value
  }

  // ── Throughput ──────────────────────────────────────────────────────────

  "MLPipelineEngine.estimateThroughput" should "increase with more GPUs" in {
    val tp1 = MLPipelineEngine.estimateThroughput(resnetJob, a100, 1)
    val tp4 = MLPipelineEngine.estimateThroughput(resnetJob, a100, 4)
    tp4 should be > tp1
    tp1 should be > 0.0
  }

  it should "return 0 for zero-tflops GPU" in {
    MLPipelineEngine.estimateThroughput(resnetJob, GpuSpec.none, 1) shouldBe 0.0
  }

  // ── Checkpoint Overhead ─────────────────────────────────────────────────

  "MLPipelineEngine.checkpointOverhead" should "scale with model size" in {
    val small = MLPipelineEngine.checkpointOverhead(1.0, 2.0)  // 1GB at 2GB/s
    val large = MLPipelineEngine.checkpointOverhead(10.0, 2.0) // 10GB at 2GB/s
    small.value shouldBe 0.5
    large.value shouldBe 5.0
  }

  // ── Optimal GPU Count ───────────────────────────────────────────────────

  "MLPipelineEngine.optimalGpuCount" should "find minimum GPUs for target time" in {
    val singleTime = MLPipelineEngine.estimateTrainingTime(resnetJob, a100, 1)
    val targetTime = SimTime(singleTime.value / 3.0) // Want 3x speedup
    val optimal    = MLPipelineEngine.optimalGpuCount(resnetJob, a100, 32, targetTime)
    optimal should be > 1
    optimal should be <= 32
    // Verify it actually meets the target
    MLPipelineEngine.estimateTrainingTime(resnetJob, a100, optimal).value should be <= targetTime.value
  }

  // ── AllReduce Volume ────────────────────────────────────────────────────

  "MLPipelineEngine.allReduceVolume" should "return 0 for single GPU" in {
    MLPipelineEngine.allReduceVolume(ModelSpec.bert, 1) shouldBe 0.0
  }

  it should "increase with GPU count" in {
    val vol2 = MLPipelineEngine.allReduceVolume(ModelSpec.bert, 2)
    val vol8 = MLPipelineEngine.allReduceVolume(ModelSpec.bert, 8)
    vol8 should be > vol2
    vol2 should be > 0.0
  }

  // ── GpuCluster ──────────────────────────────────────────────────────────

  "GpuCluster" should "compute total GPUs and memory" in {
    val cluster = GpuCluster(
      Vector(
        GpuClusterNode("n1", GpuSpec.scaled(a100, 8)),
        GpuClusterNode("n2", GpuSpec.scaled(a100, 8))
      )
    )
    cluster.totalGpus shouldBe 16
    cluster.totalGpuMemoryGB should be > 0.0
  }

  it should "track allocation" in {
    val cluster = GpuCluster(
      Vector(
        GpuClusterNode("n1", GpuSpec.scaled(a100, 8)),
        GpuClusterNode("n2", GpuSpec.scaled(a100, 8))
      )
    )
    val allocated = cluster.allocate(8)
    allocated shouldBe defined
    val remaining = allocated.get.nodes.count(_.available)
    remaining shouldBe 1
  }

  it should "reject over-allocation" in {
    val cluster = GpuCluster(Vector(GpuClusterNode("n1", GpuSpec.scaled(a100, 4))))
    cluster.allocate(8) shouldBe None
  }
