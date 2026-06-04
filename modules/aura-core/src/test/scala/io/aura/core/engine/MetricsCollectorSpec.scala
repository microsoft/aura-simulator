// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.core.events.*
import io.aura.core.events.SimEventPayload.*

class MetricsCollectorSpec extends AnyFlatSpec with Matchers:

  private val src         = EntityRef("test-broker", EntityType.Broker)
  private val dst         = EntityRef("test-dc", EntityType.Datacenter)
  private val platformSrc = EntityRef("platform", EntityType.FaasPlatform)
  private val edgeSrc     = EntityRef("edge-broker", EntityType.EdgeEnvironment)

  private def event(
      payload: SimEventPayload,
      time: Double = 1.0,
      source: EntityRef = src,
      destination: EntityRef = dst
  ): SimEvent =
    SimEvent(SimTime(time), source, destination, payload, SerialNumber.Zero)

  // ─── Empty state ─────────────────────────────────────────────────────

  "MetricsCollector.empty" should "have zero event count" in {
    val mc = MetricsCollector.empty
    mc.eventCount shouldBe 0L
  }

  "MetricsCollector.empty snapshot" should "have all empty vectors" in {
    val snap = MetricsCollector.empty.snapshot
    snap.workloadResults shouldBe empty
    snap.failedWorkloads shouldBe empty
    snap.vmPlacements shouldBe empty
    snap.hostUtilizations shouldBe empty
    snap.energyRecords shouldBe empty
    snap.migrationRecords shouldBe empty
    snap.costRecords shouldBe empty
    snap.faultRecords shouldBe empty
    snap.vmDestroyedRecords shouldBe empty
    snap.invocationResults shouldBe empty
    snap.throttledInvocations shouldBe empty
    snap.timedOutInvocations shouldBe empty
    snap.podResults shouldBe empty
    snap.podSchedulingRecords shouldBe empty
    snap.unschedulablePods shouldBe empty
    snap.edgeTaskResults shouldBe empty
    snap.failedEdgeTasks shouldBe empty
    snap.federatedTaskResults shouldBe empty
    snap.failedFederatedTasks shouldBe empty
    snap.slaViolationRecords shouldBe empty
    snap.consolidationRecords shouldBe empty
    snap.batchJobRecords shouldBe empty
    snap.eventCount shouldBe 0L
  }

  // ─── Event count ─────────────────────────────────────────────────────

  "recordEvent" should "increment event count for every event including unmatched" in {
    val mc = MetricsCollector.empty
    mc.recordEvent(event(SimulationStart))
    mc.recordEvent(event(SimulationEnd))
    mc.eventCount shouldBe 2L
  }

  it should "return this (same reference) for mutation chaining" in {
    val mc       = MetricsCollector.empty
    val returned = mc.recordEvent(event(SimulationStart))
    returned shouldBe theSameInstanceAs(mc)
  }

  // ─── IaaS events ────────────────────────────────────────────────────

  "WorkloadFinished" should "record a workload result" in {
    val mc = MetricsCollector.empty
    mc.recordEvent(event(WorkloadFinished(WorkloadId(1), VmId(1), HostId(1), SimTime(10.0), MI(1000.0))))
    val snap = mc.snapshot
    snap.workloadResults should have size 1
    snap.workloadResults.head.workloadId shouldBe WorkloadId(1)
    snap.workloadResults.head.executedMI shouldBe MI(1000.0)
  }

  "WorkloadFailed" should "record a failed workload" in {
    val mc = MetricsCollector.empty
    mc.recordEvent(event(WorkloadFailed(WorkloadId(2), "timeout")))
    val snap = mc.snapshot
    snap.failedWorkloads should have size 1
    snap.failedWorkloads.head.reason shouldBe "timeout"
  }

  "VmCreated" should "record a VM placement" in {
    val mc = MetricsCollector.empty
    mc.recordEvent(event(VmCreated(VmId(1), HostId(1), DatacenterId(0))))
    val snap = mc.snapshot
    snap.vmPlacements should have size 1
    snap.vmPlacements.head.vmId shouldBe VmId(1)
  }

  "HostUtilizationUpdate" should "record utilization" in {
    val mc = MetricsCollector.empty
    mc.recordEvent(event(HostUtilizationUpdate(HostId(1), Utilization(0.5), SimTime(5.0))))
    val snap = mc.snapshot
    snap.hostUtilizations should have size 1
    snap.slaViolationRecords shouldBe empty
  }

  "HostUtilizationUpdate above 90%" should "also record SLA violation" in {
    val mc = MetricsCollector.empty
    mc.recordEvent(event(HostUtilizationUpdate(HostId(1), Utilization(0.95), SimTime(5.0))))
    val snap = mc.snapshot
    snap.hostUtilizations should have size 1
    snap.slaViolationRecords should have size 1
    snap.slaViolationRecords.head.reason should include("90%")
  }

  "EnergyReport" should "record energy data" in {
    val mc = MetricsCollector.empty
    mc.recordEvent(event(EnergyReport(HostId(1), Watts(100.0), SimTime(0.0), SimTime(1.0), WattHours(0.1))))
    mc.snapshot.energyRecords should have size 1
  }

  "VmMigrationStart" should "record migration" in {
    val mc = MetricsCollector.empty
    mc.recordEvent(event(VmMigrationStart(VmId(1), HostId(1), HostId(2), SimTime(5.0), MegaBytes(512.0))))
    mc.snapshot.migrationRecords should have size 1
  }

  "VmDestroyed" should "record VM destruction" in {
    val mc = MetricsCollector.empty
    mc.recordEvent(event(VmDestroyed(VmId(1), HostId(1))))
    mc.snapshot.vmDestroyedRecords should have size 1
  }

  "VmCostReport" should "record cost data" in {
    val mc = MetricsCollector.empty
    mc.recordEvent(event(VmCostReport(VmId(1), Cost(1.0), Cost(0.5), Cost(0.1), Cost(0.2), Cost(1.8))))
    mc.snapshot.costRecords should have size 1
  }

  "HostFaultEvent" should "record fault" in {
    val mc = MetricsCollector.empty
    mc.recordEvent(event(HostFaultEvent(HostId(1), PEs(2))))
    mc.snapshot.faultRecords should have size 1
  }

  // ─── Serverless events ──────────────────────────────────────────────

  "InvocationComplete" should "record result when destination is not FaasPlatform" in {
    val mc = MetricsCollector.empty
    val e = event(
      InvocationComplete(InvocationId(1), FunctionId(1), SimTime(0.0), SimTime(1.0), GBSeconds(0.5), true),
      source = platformSrc,
      destination = src
    )
    mc.recordEvent(e)
    mc.snapshot.invocationResults should have size 1
    mc.snapshot.invocationResults.head.coldStart shouldBe true
  }

  "InvocationComplete" should "NOT record when destination is FaasPlatform" in {
    val mc = MetricsCollector.empty
    val e = event(
      InvocationComplete(InvocationId(1), FunctionId(1), SimTime(0.0), SimTime(1.0), GBSeconds(0.5), false),
      source = src,
      destination = platformSrc
    )
    mc.recordEvent(e)
    mc.snapshot.invocationResults shouldBe empty
  }

  "InvocationThrottled" should "record throttled invocation" in {
    val mc = MetricsCollector.empty
    mc.recordEvent(event(InvocationThrottled(InvocationId(1), FunctionId(1), "concurrency limit")))
    mc.snapshot.throttledInvocations should have size 1
  }

  "InvocationTimedOut" should "record timed-out invocation" in {
    val mc = MetricsCollector.empty
    mc.recordEvent(event(InvocationTimedOut(InvocationId(1), FunctionId(1), "exceeded 300s")))
    mc.snapshot.timedOutInvocations should have size 1
  }

  // ─── Container events ───────────────────────────────────────────────

  "PodScheduled" should "record scheduling" in {
    val mc = MetricsCollector.empty
    mc.recordEvent(event(PodScheduled(PodId(1), HostId(1), "node-0")))
    mc.snapshot.podSchedulingRecords should have size 1
  }

  "PodStarted then PodCompleted" should "update pod result in place" in {
    val mc = MetricsCollector.empty
    mc.recordEvent(event(PodStarted(PodId(1), HostId(1), SimTime(1.0)), time = 1.0))
    mc.recordEvent(event(PodCompleted(PodId(1), HostId(1), SimTime(5.0)), time = 5.0))
    val snap = mc.snapshot
    snap.podResults should have size 1
    snap.podResults.head.phase shouldBe "Succeeded"
    snap.podResults.head.finishTime shouldBe Some(SimTime(5.0))
  }

  "PodCompleted without PodStarted" should "add a new result" in {
    val mc = MetricsCollector.empty
    mc.recordEvent(event(PodCompleted(PodId(99), HostId(1), SimTime(5.0))))
    mc.snapshot.podResults should have size 1
    mc.snapshot.podResults.head.phase shouldBe "Succeeded"
  }

  "PodFailed after PodStarted" should "update phase to Failed" in {
    val mc = MetricsCollector.empty
    mc.recordEvent(event(PodStarted(PodId(1), HostId(1), SimTime(1.0))))
    mc.recordEvent(event(PodFailed(PodId(1), HostId(1), "OOMKilled")))
    mc.snapshot.podResults should have size 1
    mc.snapshot.podResults.head.phase shouldBe "Failed"
  }

  "PodUnschedulable" should "record unschedulable pod" in {
    val mc = MetricsCollector.empty
    mc.recordEvent(event(PodUnschedulable(PodId(1), "Insufficient cpu")))
    mc.snapshot.unschedulablePods should have size 1
  }

  // ─── Edge events ────────────────────────────────────────────────────

  "EdgeTaskCompleted" should "record when source != destination" in {
    val mc = MetricsCollector.empty
    val e = event(
      EdgeTaskCompleted(EdgeTaskId(1), "node-a", "node-b", true, SimTime(0.01), SimTime(0.0), SimTime(1.0), "latency"),
      source = edgeSrc,
      destination = dst
    )
    mc.recordEvent(e)
    mc.snapshot.edgeTaskResults should have size 1
  }

  "EdgeTaskCompleted" should "NOT record when source == destination" in {
    val mc = MetricsCollector.empty
    val e = event(
      EdgeTaskCompleted(EdgeTaskId(1), "node-a", "node-b", true, SimTime(0.01), SimTime(0.0), SimTime(1.0), "latency"),
      source = edgeSrc,
      destination = edgeSrc
    )
    mc.recordEvent(e)
    mc.snapshot.edgeTaskResults shouldBe empty
  }

  "EdgeTaskFailed" should "record failed edge task" in {
    val mc = MetricsCollector.empty
    mc.recordEvent(event(EdgeTaskFailed(EdgeTaskId(1), "deadline exceeded")))
    mc.snapshot.failedEdgeTasks should have size 1
  }

  // ─── Federated events ───────────────────────────────────────────────

  "FederatedTaskCompleted" should "record result" in {
    val mc = MetricsCollector.empty
    mc.recordEvent(
      event(
        FederatedTaskCompleted(FederatedTaskId(1), ExecutionTier.Edge, ExecutionTier.K8s, 2, SimTime(0.0), SimTime(5.0))
      )
    )
    mc.snapshot.federatedTaskResults should have size 1
  }

  "FederatedTaskFailed" should "record failure" in {
    val mc = MetricsCollector.empty
    mc.recordEvent(event(FederatedTaskFailed(FederatedTaskId(1), "all tiers exhausted", 3)))
    mc.snapshot.failedFederatedTasks should have size 1
  }

  // ─── Batch events ───────────────────────────────────────────────────

  "BatchJobCompleted" should "record batch job" in {
    val mc = MetricsCollector.empty
    mc.recordEvent(event(BatchJobCompleted(JobId(1), SimTime(0.0), SimTime(10.0), SimTime(10.0))))
    mc.snapshot.batchJobRecords should have size 1
  }

  "ConsolidationCheck" should "record consolidation" in {
    val mc = MetricsCollector.empty
    mc.recordEvent(event(ConsolidationCheck(DatacenterId(0), SimTime(50.0))))
    mc.snapshot.consolidationRecords should have size 1
  }

  // ─── Dedicated methods ──────────────────────────────────────────────

  "recordEnergy" should "append energy record directly" in {
    val mc     = MetricsCollector.empty
    val record = EnergyRecord(HostId(1), Watts(200.0), SimTime(0.0), SimTime(1.0), WattHours(0.2))
    mc.recordEnergy(record)
    mc.snapshot.energyRecords should have size 1
  }

  "addCostRecord" should "append cost record directly" in {
    val mc     = MetricsCollector.empty
    val record = VmCostRecord(VmId(1), Cost(1.0), Cost(0.5), Cost(0.1), Cost(0.2), Cost(1.8), SimTime(10.0))
    mc.addCostRecord(record)
    mc.snapshot.costRecords should have size 1
  }

  // ─── Multiple events accumulation ───────────────────────────────────

  "MetricsCollector" should "accumulate many events correctly" in {
    val mc = MetricsCollector.empty
    (1 to 100).foreach { i =>
      mc.recordEvent(event(WorkloadFinished(WorkloadId(i), VmId(1), HostId(1), SimTime(i.toDouble), MI(100.0))))
    }
    (1 to 50).foreach { i =>
      mc.recordEvent(event(WorkloadFailed(WorkloadId(1000 + i), "fail")))
    }
    val snap = mc.snapshot
    snap.workloadResults should have size 100
    snap.failedWorkloads should have size 50
    snap.eventCount shouldBe 150L
  }

  "snapshot" should "produce independent immutable copies" in {
    val mc = MetricsCollector.empty
    mc.recordEvent(event(WorkloadFinished(WorkloadId(1), VmId(1), HostId(1), SimTime(1.0), MI(100.0))))
    val snap1 = mc.snapshot
    mc.recordEvent(event(WorkloadFinished(WorkloadId(2), VmId(1), HostId(1), SimTime(2.0), MI(200.0))))
    val snap2 = mc.snapshot

    snap1.workloadResults should have size 1
    snap2.workloadResults should have size 2
    snap1.eventCount shouldBe 1L
    snap2.eventCount shouldBe 2L
  }

  "foldLeft with recordEvent" should "work correctly (TimeCoordinator pattern)" in {
    val mc = MetricsCollector.empty
    val events = (1 to 10)
      .map(i => event(WorkloadFinished(WorkloadId(i), VmId(1), HostId(1), SimTime(i.toDouble), MI(100.0))))
      .toVector

    val result = events.foldLeft(mc)((collector, evt) => collector.recordEvent(evt))
    result shouldBe theSameInstanceAs(mc)
    mc.snapshot.workloadResults should have size 10
    mc.eventCount shouldBe 10L
  }
