// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GpuModelSpec extends AnyFlatSpec with Matchers:

  // ─── GpuSpec ─────────────────────────────────────────────────────────

  "GpuSpec.nvidiaA100" should "have correct specs" in {
    val a100 = GpuSpec.nvidiaA100
    a100.gpuCount shouldBe 1
    a100.cudaCores shouldBe 6912
    a100.tensorCores shouldBe 432
    a100.tflopsFloat32 shouldBe 19.5
  }

  "GpuSpec.scaled" should "multiply GPU count" in {
    val multiGpu = GpuSpec.scaled(GpuSpec.nvidiaA100, 4)
    multiGpu.gpuCount shouldBe 4
    multiGpu.totalCudaCores shouldBe 6912 * 4
    multiGpu.totalGpuMemory.value shouldBe 81920.0 * 4
  }

  "GpuSpec.none" should "represent no GPU" in {
    GpuSpec.none.gpuCount shouldBe 0
    GpuSpec.none.totalCudaCores shouldBe 0
  }

  // ─── GpuWorkloadSpec ────────────────────────────────────────────────

  "GpuWorkloadSpec.estimateTime" should "compute execution time from TFLOPS" in {
    val workload = GpuWorkloadSpec(
      workloadType = GpuWorkloadType.Training,
      requiredGpuCount = 1,
      requiredGpuMemoryMB = MegaBytes(16000.0),
      computeOps = 100.0, // 100 TFLOP total
      precision = GpuPrecision.Float32
    )
    val time = workload.estimateTime(GpuSpec.nvidiaA100) // 19.5 TFLOPS
    // 100 / 19.5 ≈ 5.128 seconds
    time.value shouldBe 5.128 +- 0.01
  }

  it should "return MaxValue when GPU count insufficient" in {
    val workload = GpuWorkloadSpec(
      workloadType = GpuWorkloadType.Training,
      requiredGpuCount = 8,
      requiredGpuMemoryMB = MegaBytes(1024.0),
      computeOps = 100.0
    )
    workload.estimateTime(GpuSpec.nvidiaA100).value shouldBe Double.MaxValue
  }

  it should "return MaxValue when GPU memory insufficient" in {
    val workload = GpuWorkloadSpec(
      workloadType = GpuWorkloadType.Training,
      requiredGpuCount = 1,
      requiredGpuMemoryMB = MegaBytes(100000.0), // more than A100's 80GB
      computeOps = 100.0
    )
    workload.estimateTime(GpuSpec.nvidiaA100).value shouldBe Double.MaxValue
  }

  it should "use FP16 throughput for half-precision workloads" in {
    val fp32 = GpuWorkloadSpec(
      workloadType = GpuWorkloadType.Training,
      requiredGpuCount = 1,
      requiredGpuMemoryMB = MegaBytes(1024.0),
      computeOps = 100.0,
      precision = GpuPrecision.Float32
    )
    val fp16     = fp32.copy(precision = GpuPrecision.Float16)
    val timeFp32 = fp32.estimateTime(GpuSpec.nvidiaA100)
    val timeFp16 = fp16.estimateTime(GpuSpec.nvidiaA100)
    timeFp16.value should be < timeFp32.value // FP16 should be faster
  }

  it should "scale with multi-GPU" in {
    val workload = GpuWorkloadSpec(
      workloadType = GpuWorkloadType.Training,
      requiredGpuCount = 1,
      requiredGpuMemoryMB = MegaBytes(1024.0),
      computeOps = 100.0
    )
    val singleGpu = workload.estimateTime(GpuSpec.nvidiaA100)
    val quadGpu   = workload.estimateTime(GpuSpec.scaled(GpuSpec.nvidiaA100, 4))
    quadGpu.value shouldBe singleGpu.value / 4.0 +- 0.001
  }

  // ─── GpuPowerModel ──────────────────────────────────────────────────

  "GpuPowerModel.linear" should "return idle power at zero utilization" in {
    val model = GpuPowerModel.nvidiaA100
    model(GpuUtilization(Utilization.Zero, Utilization.Zero)).value shouldBe 50.0
  }

  it should "return max power at full utilization" in {
    val model = GpuPowerModel.nvidiaA100
    model(GpuUtilization(Utilization.Full, Utilization.Full)).value shouldBe 400.0
  }

  it should "interpolate linearly" in {
    val model    = GpuPowerModel.linear(Watts(400.0), Watts(50.0))
    val halfUtil = GpuUtilization(Utilization(0.5), Utilization(0.5))
    model(halfUtil).value shouldBe 225.0 // 50 + (400-50)*0.5
  }

  "GpuPowerModel presets" should "have H100 > A100 > V100 > T4 at full load" in {
    val full = GpuUtilization.full
    GpuPowerModel.nvidiaH100(full).value should be > GpuPowerModel.nvidiaA100(full).value
    GpuPowerModel.nvidiaA100(full).value should be > GpuPowerModel.nvidiaV100(full).value
    GpuPowerModel.nvidiaV100(full).value should be > GpuPowerModel.nvidiaT4(full).value
  }

  // ─── GpuScheduler ──────────────────────────────────────────────────

  "GpuScheduler.canAccommodate" should "check GPU count and memory" in {
    val workload = GpuWorkloadSpec(GpuWorkloadType.Training, 2, MegaBytes(32000.0), 100.0)
    GpuScheduler.canAccommodate(GpuSpec.scaled(GpuSpec.nvidiaA100, 4), workload) shouldBe true
    GpuScheduler.canAccommodate(GpuSpec.nvidiaA100, workload) shouldBe false // only 1 GPU
  }

  "GpuScheduler.bestFit" should "select GPU with fastest estimated time" in {
    val workload = GpuWorkloadSpec(GpuWorkloadType.Inference, 1, MegaBytes(8000.0), 50.0)
    val gpus = Vector(
      (0, GpuSpec.nvidiaT4),
      (1, GpuSpec.nvidiaV100),
      (2, GpuSpec.nvidiaA100)
    )
    GpuScheduler.bestFit(gpus, workload) shouldBe Some(2) // A100 is fastest
  }

  it should "return None when no GPU can accommodate" in {
    val workload = GpuWorkloadSpec(GpuWorkloadType.Training, 8, MegaBytes(1024.0), 100.0)
    val gpus     = Vector((0, GpuSpec.nvidiaA100)) // only 1 GPU
    GpuScheduler.bestFit(gpus, workload) shouldBe None
  }

  // ─── GPU Presets Ordering ────────────────────────────────────────────

  "GPU presets" should "have increasing performance T4 < V100 < A100 < H100" in {
    val workload = GpuWorkloadSpec(GpuWorkloadType.Training, 1, MegaBytes(1024.0), 100.0)
    val timeT4   = workload.estimateTime(GpuSpec.nvidiaT4)
    val timeV100 = workload.estimateTime(GpuSpec.nvidiaV100)
    val timeA100 = workload.estimateTime(GpuSpec.nvidiaA100)
    val timeH100 = workload.estimateTime(GpuSpec.nvidiaH100)
    timeH100.value should be < timeA100.value
    timeA100.value should be < timeV100.value
    timeV100.value should be < timeT4.value
  }
