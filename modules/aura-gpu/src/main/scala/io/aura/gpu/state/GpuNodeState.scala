// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.gpu.state

import io.aura.core.types.*

/** Immutable state for a single GPU device within a node. */
final case class GpuDeviceState(
    deviceId: GpuDeviceId,
    currentFreqMHz: Int,
    activity: GpuActivityProfile,
    allocatedMemoryMB: MegaBytes,
    lastPowerReport: Option[(SimTime, Watts)]
):
  def availableMemoryMB(totalMemoryMB: MegaBytes): MegaBytes =
    MegaBytes((totalMemoryMB.value - allocatedMemoryMB.value).max(0.0))

object GpuDeviceState:
  def initial(deviceId: GpuDeviceId, maxFreqMHz: Int): GpuDeviceState =
    GpuDeviceState(
      deviceId = deviceId,
      currentFreqMHz = maxFreqMHz,
      activity = GpuActivityProfile.idle,
      allocatedMemoryMB = MegaBytes.Zero,
      lastPowerReport = None
    )

/** Immutable state for a GPU node (multi-GPU server). */
final case class GpuNodeState(
    nodeId: GpuNodeId,
    devices: Map[GpuDeviceId, GpuDeviceState],
    totalDeviceCount: Int
):
  def totalAllocatedMemoryMB: MegaBytes =
    MegaBytes(devices.values.map(_.allocatedMemoryMB.value).sum)

  def allocateMemory(deviceId: GpuDeviceId, memoryMB: MegaBytes): GpuNodeState =
    devices.get(deviceId) match
      case Some(dev) =>
        val updated = dev.copy(allocatedMemoryMB = MegaBytes(dev.allocatedMemoryMB.value + memoryMB.value))
        copy(devices = devices + (deviceId -> updated))
      case None => this

  def releaseMemory(deviceId: GpuDeviceId, memoryMB: MegaBytes): GpuNodeState =
    devices.get(deviceId) match
      case Some(dev) =>
        val updated = dev.copy(allocatedMemoryMB = MegaBytes((dev.allocatedMemoryMB.value - memoryMB.value).max(0.0)))
        copy(devices = devices + (deviceId -> updated))
      case None => this

  def updateActivity(deviceId: GpuDeviceId, activity: GpuActivityProfile): GpuNodeState =
    devices.get(deviceId) match
      case Some(dev) =>
        copy(devices = devices + (deviceId -> dev.copy(activity = activity)))
      case None => this

  def updateFrequency(deviceId: GpuDeviceId, freqMHz: Int): GpuNodeState =
    devices.get(deviceId) match
      case Some(dev) =>
        copy(devices = devices + (deviceId -> dev.copy(currentFreqMHz = freqMHz)))
      case None => this

object GpuNodeState:
  def initial(nodeId: GpuNodeId, gpuCount: Int, maxFreqMHz: Int): GpuNodeState =
    val devices = (0 until gpuCount).map { i =>
      val devId = GpuDeviceId(nodeId.value * 100 + i)
      devId -> GpuDeviceState.initial(devId, maxFreqMHz)
    }.toMap
    GpuNodeState(nodeId, devices, gpuCount)
