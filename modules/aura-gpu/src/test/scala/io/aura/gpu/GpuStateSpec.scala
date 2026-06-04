// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.gpu

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.gpu.state.*

class GpuStateSpec extends AnyFlatSpec with Matchers:

  val nodeId       = GpuNodeId(0)
  val clusterId    = GpuClusterId(0)
  val serverConfig = GpuServerConfig.dgxA100

  "GpuDeviceState" should "initialize with max frequency and idle activity" in {
    val dev = GpuDeviceState.initial(GpuDeviceId(0), 1410)
    dev.currentFreqMHz shouldBe 1410
    dev.activity shouldBe GpuActivityProfile.idle
    dev.allocatedMemoryMB shouldBe MegaBytes.Zero
  }

  it should "calculate available memory" in {
    val dev = GpuDeviceState
      .initial(GpuDeviceId(0), 1410)
      .copy(allocatedMemoryMB = MegaBytes(10000.0))
    dev.availableMemoryMB(MegaBytes(81920.0)).value shouldBe 71920.0
  }

  "GpuNodeState" should "initialize with correct device count" in {
    val state = GpuNodeState.initial(nodeId, 8, 1410)
    state.devices.size shouldBe 8
    state.totalDeviceCount shouldBe 8
  }

  it should "allocate memory to a device" in {
    val state   = GpuNodeState.initial(nodeId, 2, 1410)
    val devId   = state.devices.keys.head
    val updated = state.allocateMemory(devId, MegaBytes(1024.0))
    updated.devices(devId).allocatedMemoryMB.value shouldBe 1024.0
  }

  it should "release memory from a device" in {
    val state     = GpuNodeState.initial(nodeId, 2, 1410)
    val devId     = state.devices.keys.head
    val allocated = state.allocateMemory(devId, MegaBytes(2048.0))
    val released  = allocated.releaseMemory(devId, MegaBytes(1024.0))
    released.devices(devId).allocatedMemoryMB.value shouldBe 1024.0
  }

  it should "not go below zero on release" in {
    val state    = GpuNodeState.initial(nodeId, 2, 1410)
    val devId    = state.devices.keys.head
    val released = state.releaseMemory(devId, MegaBytes(1000.0))
    released.devices(devId).allocatedMemoryMB.value shouldBe 0.0
  }

  it should "update device frequency" in {
    val state   = GpuNodeState.initial(nodeId, 2, 1410)
    val devId   = state.devices.keys.head
    val updated = state.updateFrequency(devId, 800)
    updated.devices(devId).currentFreqMHz shouldBe 800
  }

  it should "update device activity" in {
    val state    = GpuNodeState.initial(nodeId, 2, 1410)
    val devId    = state.devices.keys.head
    val activity = GpuActivityProfile(Utilization(0.9), 1000.0, 200.0, Utilization(0.5), 1410)
    val updated  = state.updateActivity(devId, activity)
    updated.devices(devId).activity.smUtilization.value shouldBe 0.9
  }

  it should "handle unknown device IDs gracefully" in {
    val state = GpuNodeState.initial(nodeId, 2, 1410)
    val bogus = GpuDeviceId(9999)
    state.allocateMemory(bogus, MegaBytes(100.0)) shouldBe state
    state.updateFrequency(bogus, 500) shouldBe state
  }

  it should "track total allocated memory across devices" in {
    val state  = GpuNodeState.initial(nodeId, 2, 1410)
    val devIds = state.devices.keys.toVector.sorted
    val updated = state
      .allocateMemory(devIds(0), MegaBytes(1000.0))
      .allocateMemory(devIds(1), MegaBytes(2000.0))
    updated.totalAllocatedMemoryMB.value shouldBe 3000.0
  }

  "GpuClusterState" should "initialize with correct node count" in {
    val state = GpuClusterState.initial(clusterId, serverConfig, 4)
    state.nodeCount shouldBe 4
    state.totalGpuCount shouldBe 4 * serverConfig.gpuCount
  }

  it should "get and update individual nodes" in {
    val state  = GpuClusterState.initial(clusterId, serverConfig, 2)
    val nodeId = state.nodes.keys.head
    val node   = state.getNode(nodeId)
    node shouldBe defined
    node.get.totalDeviceCount shouldBe serverConfig.gpuCount
  }

  it should "track total allocated memory across cluster" in {
    val state = GpuClusterState.initial(clusterId, serverConfig, 2)
    state.totalAllocatedMemoryMB.value shouldBe 0.0
  }
