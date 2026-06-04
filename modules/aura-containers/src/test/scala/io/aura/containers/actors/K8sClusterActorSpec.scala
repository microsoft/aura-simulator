// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.containers.actors

import org.apache.pekko.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.core.events.*
import io.aura.core.events.SimEventPayload.*
import io.aura.core.engine.TimeCoordinator
import io.aura.core.engine.TimeCoordinator.{ProcessEvents, StepComplete}
import io.aura.containers.*
import io.aura.containers.state.*

class K8sClusterActorSpec extends ScalaTestWithActorTestKit with AnyFlatSpecLike with Matchers:

  private val coordinatorProbe = createTestProbe[TimeCoordinator.Command]()

  private def createClusterActor(
      deployments: Vector[DeploymentRecord] = Vector.empty
  ) =
    val config = K8sClusterActor.Config(
      clusterName = "test-cluster",
      schedulingPolicy = K8sScheduler.leastRequested,
      datacenterRef = EntityRef("dc-0", EntityType.Datacenter),
      hostConfigs = Vector(
        K8sClusterActor.HostNodeMapping("node-0", HostId(0L), MIPS(10000.0), MegaBytes(16384.0)),
        K8sClusterActor.HostNodeMapping("node-1", HostId(1L), MIPS(10000.0), MegaBytes(16384.0))
      ),
      coordinator = coordinatorProbe.ref,
      initialDeployments = deployments
    )
    spawn(K8sClusterActor(config, nextVmId = 100L))

  private def makePodScheduleRequest(podId: Long, deploymentId: Long = 0L): PodScheduleRequest =
    PodScheduleRequest(
      podId = PodId(podId),
      deploymentId = DeploymentId(deploymentId),
      spec = PodDeploySpec(
        name = s"pod-$podId",
        namespace = "default",
        containers = Vector(
          ContainerResourceSpec(
            name = "container",
            image = "test:latest",
            cpuRequest = MIPS(500.0),
            cpuLimit = MIPS(1000.0),
            memoryRequest = MegaBytes(256.0),
            memoryLimit = MegaBytes(512.0)
          )
        ),
        restartPolicy = "Never",
        priority = 0,
        nodeSelector = Map.empty
      )
    )

  "K8sClusterActor" should "register with coordinator on startup" in {
    createClusterActor()
    val msg = coordinatorProbe.receiveMessage()
    msg shouldBe a[TimeCoordinator.RegisterEntity]
    val reg = msg.asInstanceOf[TimeCoordinator.RegisterEntity]
    reg.entityRef.name shouldBe "test-cluster"
    reg.entityRef.entityType shouldBe EntityType.K8sCluster
  }

  it should "schedule a pod on an available node" in {
    val actor = createClusterActor()
    coordinatorProbe.receiveMessage() // RegisterEntity

    val event = SimEvent(
      time = SimTime(1.0),
      source = EntityRef("k8s-broker-test-cluster", EntityType.K8sCluster),
      destination = EntityRef("test-cluster", EntityType.K8sCluster),
      payload = makePodScheduleRequest(0),
      serial = SerialNumber.Zero
    )

    actor ! ProcessEvents(Vector(event), coordinatorProbe.ref)

    // Should emit ScheduleEvents (PodScheduled + VmCreateRequest)
    val scheduleMsg = coordinatorProbe.receiveMessage()
    scheduleMsg shouldBe a[TimeCoordinator.ScheduleEvents]
    val events = scheduleMsg.asInstanceOf[TimeCoordinator.ScheduleEvents].events
    events.exists(_.payload.isInstanceOf[PodScheduled]) shouldBe true
    events.exists(_.payload.isInstanceOf[VmCreateRequest]) shouldBe true

    // Should reply StepComplete
    val complete = coordinatorProbe.receiveMessage()
    complete shouldBe a[StepComplete]
  }

  it should "emit PodUnschedulable when resources exhausted" in {
    val actor = createClusterActor()
    coordinatorProbe.receiveMessage() // RegisterEntity

    // Create a pod that needs more CPU than any node has
    val bigPodRequest = PodScheduleRequest(
      podId = PodId(0),
      deploymentId = DeploymentId(0),
      spec = PodDeploySpec(
        name = "big-pod",
        namespace = "default",
        containers = Vector(
          ContainerResourceSpec(
            name = "big",
            image = "test:latest",
            cpuRequest = MIPS(99999.0),
            cpuLimit = MIPS(99999.0),
            memoryRequest = MegaBytes(256.0),
            memoryLimit = MegaBytes(512.0)
          )
        ),
        restartPolicy = "Never",
        priority = 0,
        nodeSelector = Map.empty
      )
    )

    val event = SimEvent(
      time = SimTime(1.0),
      source = EntityRef("k8s-broker-test-cluster", EntityType.K8sCluster),
      destination = EntityRef("test-cluster", EntityType.K8sCluster),
      payload = bigPodRequest,
      serial = SerialNumber.Zero
    )

    actor ! ProcessEvents(Vector(event), coordinatorProbe.ref)

    val scheduleMsg = coordinatorProbe.receiveMessage()
    scheduleMsg shouldBe a[TimeCoordinator.ScheduleEvents]
    val events = scheduleMsg.asInstanceOf[TimeCoordinator.ScheduleEvents].events
    events.exists(_.payload.isInstanceOf[PodUnschedulable]) shouldBe true

    coordinatorProbe.receiveMessage() // StepComplete
  }

  it should "schedule multiple pods across nodes" in {
    val actor = createClusterActor()
    coordinatorProbe.receiveMessage() // RegisterEntity

    // Schedule 2 pods
    val events = (0 to 1).map { i =>
      SimEvent(
        time = SimTime(1.0),
        source = EntityRef("k8s-broker-test-cluster", EntityType.K8sCluster),
        destination = EntityRef("test-cluster", EntityType.K8sCluster),
        payload = makePodScheduleRequest(i.toLong),
        serial = SerialNumber(i.toLong)
      )
    }.toVector

    actor ! ProcessEvents(events, coordinatorProbe.ref)

    val scheduleMsg = coordinatorProbe.receiveMessage()
    scheduleMsg shouldBe a[TimeCoordinator.ScheduleEvents]
    val emitted         = scheduleMsg.asInstanceOf[TimeCoordinator.ScheduleEvents].events
    val scheduledEvents = emitted.filter(_.payload.isInstanceOf[PodScheduled])
    scheduledEvents should have size 2

    coordinatorProbe.receiveMessage() // StepComplete
  }
