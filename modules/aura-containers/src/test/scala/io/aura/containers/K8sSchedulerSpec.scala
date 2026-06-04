// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.containers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*

class K8sSchedulerSpec extends AnyFlatSpec with Matchers:

  private def makeNode(
      name: String,
      hostId: Long,
      totalCpu: Double = 10000.0,
      totalMem: Double = 16384.0,
      allocCpu: Double = 0.0,
      allocMem: Double = 0.0,
      labels: Map[String, String] = Map.empty,
      taints: Vector[Taint] = Vector.empty
  ): NodeState =
    NodeState(
      name = name,
      hostId = HostId(hostId),
      totalCpu = MIPS(totalCpu),
      totalMemory = MegaBytes(totalMem),
      allocatedCpu = MIPS(allocCpu),
      allocatedMemory = MegaBytes(allocMem),
      pods = Vector.empty,
      labels = labels,
      taints = taints
    )

  private def makePod(
      cpuReq: Double = 500.0,
      memReq: Double = 256.0,
      nodeSelector: Map[String, String] = Map.empty,
      tolerations: Vector[Toleration] = Vector.empty
  ): PodSpec =
    PodSpec(
      name = "test-pod",
      containers = Vector(
        ContainerSpec(
          name = "c1",
          image = "test:latest",
          cpuRequest = MIPS(cpuReq),
          cpuLimit = MIPS(cpuReq * 2),
          memoryRequest = MegaBytes(memReq),
          memoryLimit = MegaBytes(memReq * 2)
        )
      ),
      nodeSelector = nodeSelector,
      tolerations = tolerations
    )

  "leastRequested" should "pick the least utilized node" in {
    val nodes = Vector(
      makeNode("node-0", 0, allocCpu = 5000.0, allocMem = 8000.0), // 50% utilized
      makeNode("node-1", 1, allocCpu = 1000.0, allocMem = 2000.0), // 10% utilized
      makeNode("node-2", 2, allocCpu = 3000.0, allocMem = 5000.0)  // 25% utilized
    )
    val pod = makePod()
    K8sScheduler.leastRequested(nodes, pod) shouldBe Some("node-1")
  }

  "mostRequested" should "pick the most utilized node (bin packing)" in {
    val nodes = Vector(
      makeNode("node-0", 0, allocCpu = 5000.0, allocMem = 8000.0), // 50% utilized
      makeNode("node-1", 1, allocCpu = 1000.0, allocMem = 2000.0), // 10% utilized
      makeNode("node-2", 2, allocCpu = 3000.0, allocMem = 5000.0)  // 25% utilized
    )
    val pod = makePod()
    K8sScheduler.mostRequested(nodes, pod) shouldBe Some("node-0")
  }

  "balancedResource" should "minimize CPU/memory imbalance" in {
    val nodes = Vector(
      makeNode("node-0", 0, allocCpu = 5000.0, allocMem = 1000.0), // big imbalance
      makeNode("node-1", 1, allocCpu = 3000.0, allocMem = 3000.0), // small imbalance
      makeNode("node-2", 2, allocCpu = 1000.0, allocMem = 8000.0)  // big imbalance
    )
    val pod = makePod(cpuReq = 500.0, memReq = 500.0)
    K8sScheduler.balancedResource(nodes, pod) shouldBe Some("node-1")
  }

  "canSchedulePod" should "respect nodeSelector" in {
    val nodes = Vector(
      makeNode("node-0", 0, labels = Map("zone" -> "us-west")),
      makeNode("node-1", 1, labels = Map("zone" -> "us-east"))
    )
    val pod = makePod(nodeSelector = Map("zone" -> "us-east"))
    K8sScheduler.leastRequested(nodes, pod) shouldBe Some("node-1")
  }

  it should "respect tolerations" in {
    val nodes = Vector(
      makeNode("node-0", 0, taints = Vector(Taint("dedicated", "gpu", TaintEffect.NoSchedule))),
      makeNode("node-1", 1)
    )
    // Pod without toleration should not be scheduled on node-0
    val pod = makePod()
    K8sScheduler.leastRequested(nodes, pod) shouldBe Some("node-1")

    // Pod with toleration should be scheduled on node-0 (least utilized)
    val tolerantPod = makePod(tolerations =
      Vector(
        Toleration("dedicated", TolerationOperator.Equal, "gpu", TaintEffect.NoSchedule)
      )
    )
    K8sScheduler.leastRequested(nodes, tolerantPod) shouldBe Some("node-0")
  }

  it should "return None when no node can fit the pod" in {
    val nodes = Vector(
      makeNode("node-0", 0, totalCpu = 100.0, totalMem = 100.0),
      makeNode("node-1", 1, totalCpu = 100.0, totalMem = 100.0)
    )
    val pod = makePod(cpuReq = 500.0, memReq = 256.0)
    K8sScheduler.leastRequested(nodes, pod) shouldBe None
    K8sScheduler.mostRequested(nodes, pod) shouldBe None
    K8sScheduler.balancedResource(nodes, pod) shouldBe None
  }

  it should "return None when all resources are exhausted" in {
    val nodes = Vector(
      makeNode("node-0", 0, allocCpu = 10000.0, allocMem = 16384.0),
      makeNode("node-1", 1, allocCpu = 10000.0, allocMem = 16384.0)
    )
    val pod = makePod()
    K8sScheduler.leastRequested(nodes, pod) shouldBe None
  }

  "NodeState.schedulePod" should "track allocated resources" in {
    val node    = makeNode("node-0", 0)
    val pod     = makePod(cpuReq = 500.0, memReq = 256.0)
    val updated = node.schedulePod(pod, SimTime.Zero)
    updated.allocatedCpu.value shouldBe 500.0
    updated.allocatedMemory.value shouldBe 256.0
    updated.pods should have size 1
  }

  "NodeState.removePod" should "release allocated resources" in {
    val node    = makeNode("node-0", 0)
    val pod     = makePod(cpuReq = 500.0, memReq = 256.0)
    val withPod = node.schedulePod(pod, SimTime.Zero)
    val removed = withPod.removePod("test-pod")
    removed.allocatedCpu.value shouldBe 0.0
    removed.allocatedMemory.value shouldBe 0.0
    removed.pods should have size 0
  }

  "PodSpec" should "compute total resource requests from containers" in {
    val pod = PodSpec(
      name = "multi-container",
      containers = Vector(
        ContainerSpec("c1", "img1", MIPS(500.0), MIPS(1000.0), MegaBytes(256.0), MegaBytes(512.0)),
        ContainerSpec("c2", "img2", MIPS(300.0), MIPS(600.0), MegaBytes(128.0), MegaBytes(256.0))
      )
    )
    pod.totalCpuRequest.value shouldBe 800.0
    pod.totalMemoryRequest.value shouldBe 384.0
    pod.totalCpuLimit.value shouldBe 1600.0
    pod.totalMemoryLimit.value shouldBe 768.0
  }
