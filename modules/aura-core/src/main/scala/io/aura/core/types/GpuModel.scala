// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

/** GPU specification for a host or VM. */
final case class GpuSpec(
    gpuCount: Int,
    gpuMemoryMB: MegaBytes,
    cudaCores: Int,
    tensorCores: Int = 0,
    tflopsFloat32: Double,
    tflopsFloat16: Double = 0.0
):
  def totalGpuMemory: MegaBytes = MegaBytes(gpuMemoryMB.value * gpuCount)
  def totalCudaCores: Int       = cudaCores * gpuCount
  def totalTensorCores: Int     = tensorCores * gpuCount

object GpuSpec:
  val none: GpuSpec = GpuSpec(0, MegaBytes.Zero, 0, 0, 0.0, 0.0)

  /** NVIDIA T4 — inference GPU. */
  val nvidiaT4: GpuSpec = GpuSpec(
    gpuCount = 1,
    gpuMemoryMB = MegaBytes(16384.0),
    cudaCores = 2560,
    tensorCores = 320,
    tflopsFloat32 = 8.1,
    tflopsFloat16 = 65.0
  )

  /** NVIDIA V100 — training GPU. */
  val nvidiaV100: GpuSpec = GpuSpec(
    gpuCount = 1,
    gpuMemoryMB = MegaBytes(32768.0),
    cudaCores = 5120,
    tensorCores = 640,
    tflopsFloat32 = 15.7,
    tflopsFloat16 = 125.0
  )

  /** NVIDIA A100 — data center GPU. */
  val nvidiaA100: GpuSpec = GpuSpec(
    gpuCount = 1,
    gpuMemoryMB = MegaBytes(81920.0),
    cudaCores = 6912,
    tensorCores = 432,
    tflopsFloat32 = 19.5,
    tflopsFloat16 = 312.0
  )

  /** NVIDIA H100 — next-gen data center GPU. */
  val nvidiaH100: GpuSpec = GpuSpec(
    gpuCount = 1,
    gpuMemoryMB = MegaBytes(81920.0),
    cudaCores = 14592,
    tensorCores = 456,
    tflopsFloat32 = 51.0,
    tflopsFloat16 = 989.0
  )

  /** Scale GPU count (e.g. for multi-GPU hosts). */
  def scaled(base: GpuSpec, count: Int): GpuSpec =
    base.copy(gpuCount = count)

/** GPU workload types. */
enum GpuWorkloadType:
  case Training   // Long-running, high memory, uses tensor cores
  case Inference  // Short, lower memory, may use tensor cores
  case Rendering  // Graphics, uses CUDA cores
  case Scientific // HPC, uses CUDA cores

/** GPU workload specification. */
final case class GpuWorkloadSpec(
    workloadType: GpuWorkloadType,
    requiredGpuCount: Int,
    requiredGpuMemoryMB: MegaBytes,
    computeOps: Double, // Total floating-point operations (in TFLOPs)
    precision: GpuPrecision = GpuPrecision.Float32,
    batchSize: Int = 1
):
  /** Estimate execution time on a given GPU spec. */
  def estimateTime(gpu: GpuSpec): SimTime =
    if gpu.gpuCount < requiredGpuCount then SimTime.MaxValue
    else if gpu.totalGpuMemory < requiredGpuMemoryMB then SimTime.MaxValue
    else
      val tflops = precision match
        case GpuPrecision.Float32 => gpu.tflopsFloat32 * gpu.gpuCount
        case GpuPrecision.Float16 | GpuPrecision.BFloat16 =>
          if gpu.tflopsFloat16 > 0 then gpu.tflopsFloat16 * gpu.gpuCount
          else gpu.tflopsFloat32 * gpu.gpuCount * 2.0 // approximate
        case GpuPrecision.Int8 =>
          gpu.tflopsFloat16 * gpu.gpuCount * 2.0 // INT8 ~2x FP16

      if tflops > 0 then SimTime(computeOps / tflops) // seconds
      else SimTime.MaxValue

/** GPU compute precision. */
enum GpuPrecision:
  case Float32, Float16, BFloat16, Int8

/** GPU utilization metrics. */
final case class GpuUtilization(
    computeUtilization: Utilization,
    memoryUtilization: Utilization
)

object GpuUtilization:
  val zero: GpuUtilization = GpuUtilization(Utilization.Zero, Utilization.Zero)
  val full: GpuUtilization = GpuUtilization(Utilization.Full, Utilization.Full)

/** GPU power model: maps GPU utilization to power consumption. */
type GpuPowerModel = GpuUtilization => Watts

object GpuPowerModel:

  /** Linear GPU power model.
    * @param maxPower
    *   TDP (e.g. 300W for A100)
    * @param idlePower
    *   Idle power (typically 30-50W)
    */
  def linear(maxPower: Watts, idlePower: Watts): GpuPowerModel =
    (util: GpuUtilization) =>
      if util.computeUtilization.value <= 0.0 then idlePower
      else Watts(idlePower.value + (maxPower.value - idlePower.value) * util.computeUtilization.value)

  /** Preset power models for common GPUs. */
  val nvidiaT4: GpuPowerModel   = linear(Watts(70.0), Watts(15.0))
  val nvidiaV100: GpuPowerModel = linear(Watts(300.0), Watts(40.0))
  val nvidiaA100: GpuPowerModel = linear(Watts(400.0), Watts(50.0))
  val nvidiaH100: GpuPowerModel = linear(Watts(700.0), Watts(75.0))

  val none: GpuPowerModel = (_: GpuUtilization) => Watts.Zero

// ─── Extended GPU Device Spec (for inference/DVFS modeling) ─────────────────

/** Extended GPU specification with HBM bandwidth, SM count, and DVFS parameters. Used by the aura-gpu and
  * aura-inference modules for detailed power/latency modeling.
  */
final case class GpuDeviceSpec(
    name: String,
    gpuMemoryMB: MegaBytes,
    hbmBandwidthGBps: Double,
    smCount: Int,
    cudaCores: Int,
    tensorCores: Int,
    tflopsFloat16: Double,
    tflopsFloat32: Double,
    frequencyMinMHz: Int,
    frequencyMaxMHz: Int,
    frequencyBaseMHz: Int,
    tdpWatts: Watts,
    idleWatts: Watts,
    hbmPowerPerGBps: Double = 0.5,
    nvlinkBandwidthGBps: Double = 0.0,
    nvlinkCount: Int = 0,
    pcieBandwidthGBps: Double = 32.0
)

object GpuDeviceSpec:
  val a100Sxm: GpuDeviceSpec = GpuDeviceSpec(
    name = "A100-SXM-80GB",
    gpuMemoryMB = MegaBytes(81920.0),
    hbmBandwidthGBps = 2039.0,
    smCount = 108,
    cudaCores = 6912,
    tensorCores = 432,
    tflopsFloat16 = 312.0,
    tflopsFloat32 = 19.5,
    frequencyMinMHz = 210,
    frequencyMaxMHz = 1410,
    frequencyBaseMHz = 1095,
    tdpWatts = Watts(400.0),
    idleWatts = Watts(50.0),
    hbmPowerPerGBps = 0.04,
    nvlinkBandwidthGBps = 600.0,
    nvlinkCount = 12,
    pcieBandwidthGBps = 64.0
  )

  val h100Sxm: GpuDeviceSpec = GpuDeviceSpec(
    name = "H100-SXM-80GB",
    gpuMemoryMB = MegaBytes(81920.0),
    hbmBandwidthGBps = 3352.0,
    smCount = 132,
    cudaCores = 14592,
    tensorCores = 456,
    tflopsFloat16 = 989.0,
    tflopsFloat32 = 51.0,
    frequencyMinMHz = 345,
    frequencyMaxMHz = 1980,
    frequencyBaseMHz = 1593,
    tdpWatts = Watts(700.0),
    idleWatts = Watts(75.0),
    hbmPowerPerGBps = 0.035,
    nvlinkBandwidthGBps = 900.0,
    nvlinkCount = 18,
    pcieBandwidthGBps = 128.0
  )

  val b200: GpuDeviceSpec = GpuDeviceSpec(
    name = "B200-192GB",
    gpuMemoryMB = MegaBytes(196608.0),
    hbmBandwidthGBps = 8000.0,
    smCount = 160,
    cudaCores = 18432,
    tensorCores = 512,
    tflopsFloat16 = 2250.0,
    tflopsFloat32 = 90.0,
    frequencyMinMHz = 400,
    frequencyMaxMHz = 2100,
    frequencyBaseMHz = 1700,
    tdpWatts = Watts(1000.0),
    idleWatts = Watts(100.0),
    hbmPowerPerGBps = 0.03,
    nvlinkBandwidthGBps = 1800.0,
    nvlinkCount = 18,
    pcieBandwidthGBps = 128.0
  )

/** Multi-component GPU power breakdown. */
final case class GpuPowerComponents(
    computeWatts: Watts,
    hbmWatts: Watts,
    nvlinkWatts: Watts,
    pcieWatts: Watts,
    idleWatts: Watts
):
  def total: Watts = Watts(computeWatts.value + hbmWatts.value + nvlinkWatts.value + pcieWatts.value + idleWatts.value)

/** GPU activity profile for DVFS and power calculation. */
final case class GpuActivityProfile(
    smUtilization: Utilization,
    hbmReadBWGBps: Double,
    hbmWriteBWGBps: Double,
    nvlinkUtilization: Utilization,
    currentFreqMHz: Int
):
  def hbmTotalBWGBps: Double = hbmReadBWGBps + hbmWriteBWGBps

object GpuActivityProfile:
  val idle: GpuActivityProfile = GpuActivityProfile(
    Utilization.Zero,
    0.0,
    0.0,
    Utilization.Zero,
    0
  )

/** Check if a GPU spec can satisfy a workload's requirements. */
object GpuScheduler:

  def canAccommodate(gpu: GpuSpec, workload: GpuWorkloadSpec): Boolean =
    gpu.gpuCount >= workload.requiredGpuCount &&
      gpu.totalGpuMemory >= workload.requiredGpuMemoryMB

  /** Find the best GPU from a set for a given workload (fastest estimated time). */
  def bestFit(gpus: Vector[(Int, GpuSpec)], workload: GpuWorkloadSpec): Option[Int] =
    gpus
      .filter((_, gpu) => canAccommodate(gpu, workload))
      .sortBy((_, gpu) => workload.estimateTime(gpu).value)
      .headOption
      .map(_._1)
