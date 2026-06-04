// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.gpu

import io.aura.core.types.*

/** GPU hardware configuration for a multi-GPU server node.
  *
  * Models server-level GPU configurations like DGX H100 (8×H100) or DGX A100 (8×A100) with interconnect topology.
  */
final case class GpuServerConfig(
    name: String,
    deviceSpec: GpuDeviceSpec,
    gpuCount: Int,
    interconnect: InterconnectType,
    hostCpuCores: Int = 128,
    hostRamGB: Int = 2048,
    networkBandwidthGbps: Double = 400.0
):
  def totalGpuMemoryMB: MegaBytes      = MegaBytes(deviceSpec.gpuMemoryMB.value * gpuCount)
  def totalHbmBandwidthGBps: Double    = deviceSpec.hbmBandwidthGBps * gpuCount
  def totalNvLinkBandwidthGBps: Double = deviceSpec.nvlinkBandwidthGBps * gpuCount

object GpuServerConfig:
  /** NVIDIA DGX A100 — 8× A100 SXM 80GB, NVSwitch. */
  val dgxA100: GpuServerConfig = GpuServerConfig(
    name = "DGX-A100",
    deviceSpec = GpuDeviceSpec.a100Sxm,
    gpuCount = 8,
    interconnect = InterconnectType.NVSwitch,
    hostCpuCores = 128,
    hostRamGB = 2048,
    networkBandwidthGbps = 200.0
  )

  /** NVIDIA DGX H100 — 8× H100 SXM 80GB, NVSwitch. */
  val dgxH100: GpuServerConfig = GpuServerConfig(
    name = "DGX-H100",
    deviceSpec = GpuDeviceSpec.h100Sxm,
    gpuCount = 8,
    interconnect = InterconnectType.NVSwitch,
    hostCpuCores = 112,
    hostRamGB = 2048,
    networkBandwidthGbps = 400.0
  )

/** GPU interconnect types within a node and across nodes. */
enum InterconnectType:
  case NVLink     // Direct NVLink (peer-to-peer, up to 18 links)
  case NVSwitch   // Full-bisection NVSwitch (all-to-all within node)
  case PCIe       // PCIe Gen4/Gen5 (lower bandwidth)
  case InfiniBand // Cross-node InfiniBand (HDR/NDR)

/** Interconnect bandwidth model. */
object InterconnectBandwidth:
  /** Effective all-reduce bandwidth for tensor parallelism (GB/s per link). */
  def allReduceBandwidth(interconnect: InterconnectType, deviceSpec: GpuDeviceSpec): Double =
    interconnect match
      case InterconnectType.NVSwitch =>
        deviceSpec.nvlinkBandwidthGBps * 0.9 // ~90% efficiency
      case InterconnectType.NVLink =>
        deviceSpec.nvlinkBandwidthGBps * 0.85 // ring all-reduce overhead
      case InterconnectType.PCIe =>
        deviceSpec.pcieBandwidthGBps * 0.7
      case InterconnectType.InfiniBand =>
        200.0 // NDR 400Gb/s ≈ 50GB/s per direction, bidirectional ≈ 200 effective
