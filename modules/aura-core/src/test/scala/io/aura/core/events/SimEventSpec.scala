// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.events

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*

class SimEventSpec extends AnyFlatSpec with Matchers:

  val testSource = EntityRef("test-source", EntityType.Broker)
  val testDest   = EntityRef("test-dest", EntityType.Datacenter)

  "SimEvent" should "be ordered by time first, then serial" in {
    val e1 = SimEvent(SimTime(1.0), testSource, testDest, SimEventPayload.SimulationStart, SerialNumber(0L))
    val e2 = SimEvent(SimTime(2.0), testSource, testDest, SimEventPayload.SimulationEnd, SerialNumber(0L))
    val e3 = SimEvent(SimTime(1.0), testSource, testDest, SimEventPayload.SimulationEnd, SerialNumber(1L))

    val sorted = Vector(e2, e3, e1).sorted
    sorted(0) shouldBe e1 // time 1.0, serial 0
    sorted(1) shouldBe e3 // time 1.0, serial 1
    sorted(2) shouldBe e2 // time 2.0
  }

  "SimEventPayload" should "support exhaustive pattern matching" in {
    val payload: SimEventPayload = SimEventPayload.SimulationStart

    val description = payload match
      case SimEventPayload.SimulationStart                           => "start"
      case SimEventPayload.SimulationEnd                             => "end"
      case SimEventPayload.TimeAdvance(_)                            => "advance"
      case SimEventPayload.TimeStepComplete(_)                       => "step-complete"
      case SimEventPayload.VmCreateRequest(_, _, _)                  => "vm-create"
      case SimEventPayload.VmCreated(_, _, _)                        => "vm-created"
      case SimEventPayload.VmCreateFailed(_, _)                      => "vm-failed"
      case SimEventPayload.VmDestroyRequest(_)                       => "vm-destroy"
      case SimEventPayload.VmDestroyed(_, _)                         => "vm-destroyed"
      case SimEventPayload.WorkloadSubmit(_)                         => "wl-submit"
      case SimEventPayload.WorkloadAssign(_, _)                      => "wl-assign"
      case SimEventPayload.WorkloadStarted(_, _, _, _)               => "wl-started"
      case SimEventPayload.WorkloadFinished(_, _, _, _, _)           => "wl-finished"
      case SimEventPayload.WorkloadUpdate(_)                         => "wl-update"
      case SimEventPayload.WorkloadFailed(_, _)                      => "wl-failed"
      case SimEventPayload.WorkloadPreempted(_, _, _)                => "wl-preempted"
      case SimEventPayload.HostUtilizationUpdate(_, _, _)            => "host-util"
      case SimEventPayload.EnergyReport(_, _, _, _, _)               => "energy"
      case SimEventPayload.EnergySample(_)                           => "energy-sample"
      case SimEventPayload.VmMigrationRequest(_, _, _)               => "vm-mig-req"
      case SimEventPayload.VmMigrationStart(_, _, _, _, _)           => "vm-mig-start"
      case SimEventPayload.VmMigrationComplete(_, _, _)              => "vm-mig-complete"
      case SimEventPayload.VmMigrationFailed(_, _)                   => "vm-mig-failed"
      case SimEventPayload.VmScaleRequest(_, _, _, _)                => "vm-scale-req"
      case SimEventPayload.VmScaled(_, _, _, _)                      => "vm-scaled"
      case SimEventPayload.DatacenterRegistration(_, _)              => "dc-reg"
      case SimEventPayload.BrokerRegistration(_, _)                  => "broker-reg"
      case SimEventPayload.ScheduleWorkloads(_)                      => "schedule"
      case SimEventPayload.FunctionDeploy(_, _)                      => "fn-deploy"
      case SimEventPayload.FunctionInvoke(_, _, _, _)                => "fn-invoke"
      case SimEventPayload.InvocationStarted(_, _, _, _)             => "inv-started"
      case SimEventPayload.InvocationComplete(_, _, _, _, _, _)      => "inv-complete"
      case SimEventPayload.InvocationTimedOut(_, _, _)               => "inv-timeout"
      case SimEventPayload.InvocationThrottled(_, _, _)              => "inv-throttled"
      case SimEventPayload.ContainerCreated(_, _, _)                 => "container-created"
      case SimEventPayload.ContainerEvicted(_, _)                    => "container-evicted"
      case SimEventPayload.PodScheduleRequest(_, _, _)               => "pod-schedule-req"
      case SimEventPayload.PodScheduled(_, _, _)                     => "pod-scheduled"
      case SimEventPayload.PodStarted(_, _, _)                       => "pod-started"
      case SimEventPayload.PodCompleted(_, _, _)                     => "pod-completed"
      case SimEventPayload.PodFailed(_, _, _)                        => "pod-failed"
      case SimEventPayload.PodEvicted(_, _, _)                       => "pod-evicted"
      case SimEventPayload.PodUnschedulable(_, _)                    => "pod-unschedulable"
      case SimEventPayload.EdgeTaskSubmit(_, _, _, _, _, _)          => "edge-submit"
      case SimEventPayload.EdgeTaskStarted(_, _, _, _, _, _)         => "edge-started"
      case SimEventPayload.EdgeTaskCompleted(_, _, _, _, _, _, _, _) => "edge-completed"
      case SimEventPayload.EdgeTaskFailed(_, _)                      => "edge-failed"
      case SimEventPayload.FederatedTaskSubmit(_, _, _, _, _)        => "fed-submit"
      case SimEventPayload.FederatedTaskRouted(_, _, _)              => "fed-routed"
      case SimEventPayload.FederatedTaskCompleted(_, _, _, _, _, _)  => "fed-completed"
      case SimEventPayload.FederatedTaskFailed(_, _, _)              => "fed-failed"
      case SimEventPayload.SpotInterruption(_, _)                    => "spot-interruption"
      case SimEventPayload.ScalingCheck(_)                           => "scaling-check"
      case SimEventPayload.HostFaultEvent(_, _)                      => "host-fault"
      case SimEventPayload.VmCostReport(_, _, _, _, _, _)            => "vm-cost-report"

    description shouldBe "start"
  }

  "WorkloadSpec.simple" should "create a workload with sensible defaults" in {
    val wl = WorkloadSpec.simple(
      id = WorkloadId(1L),
      length = MI(10000.0),
      pes = PEs(2)
    )

    wl.id.value shouldBe 1L
    wl.length.value shouldBe 10000.0
    wl.pes.value shouldBe 2
    wl.requiredMips.value shouldBe 1000.0
    wl.utilizationCpu.value shouldBe 1.0
    wl.submissionDelay.isZero shouldBe true
  }
