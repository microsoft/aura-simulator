// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz.frontend.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import upickle.default.*

class SimulationDataSpec extends AnyFlatSpec with Matchers:

  val sampleJson: String = """{
  "simulationEndTime": 100.0,
  "totalEventsProcessed": 5000,
  "totalEnergyWh": 12.5,
  "avgCompletionTime": 45.3,
  "totalCost": 0.1234,
  "workloadResults": [
    {"workloadId":1,"vmId":0,"hostId":0,"finishTime":50.0,"executedMI":1000.0},
    {"workloadId":2,"vmId":1,"hostId":0,"finishTime":75.0,"executedMI":2000.0}
  ],
  "failedWorkloads": [
    {"workloadId":3,"reason":"Host failure","time":60.0}
  ],
  "vmPlacements": [
    {"vmId":0,"hostId":0,"datacenterId":0,"time":0.0}
  ],
  "energyRecords": [
    {"hostId":0,"watts":100.0,"fromTime":0.0,"toTime":50.0,"energyWh":5.0}
  ],
  "migrationRecords": [
    {"vmId":0,"sourceHostId":0,"targetHostId":1,"startTime":30.0,"duration":0.5,"dataTransferred":128.0}
  ],
  "invocationResults": [
    {"invocationId":1,"functionId":0,"startTime":10.0,"finishTime":10.5,"billedGBSeconds":0.005,"coldStart":true}
  ],
  "throttledInvocations": [
    {"invocationId":2,"functionId":0,"reason":"Concurrency limit","time":20.0}
  ],
  "timedOutInvocations": [
    {"invocationId":3,"functionId":1,"reason":"Timeout exceeded","time":30.0}
  ],
  "podResults": [
    {"podId":1,"hostId":0,"startTime":5.0,"finishTime":50.0,"phase":"Succeeded"}
  ],
  "podSchedulingRecords": [
    {"podId":1,"hostId":0,"nodeName":"node-1","time":5.0}
  ],
  "edgeTaskResults": [
    {"taskId":1,"sourceNodeName":"edge-1","executionNodeName":"cloud-1","offloaded":true,"networkLatency":0.01,"startTime":10.0,"finishTime":20.0,"offloadingReason":"High latency"}
  ],
  "failedEdgeTasks": [
    {"taskId":2,"reason":"Network failure","time":15.0}
  ],
  "federatedTaskResults": [
    {"taskId":1,"initialTier":"Edge","finalTier":"Cloud","escalations":2,"startTime":5.0,"finishTime":25.0}
  ],
  "failedFederatedTasks": [
    {"taskId":2,"reason":"All tiers exhausted","tiersAttempted":3,"time":30.0}
  ],
  "costRecords": [
    {"vmId":0,"cpuCost":0.05,"ramCost":0.02,"bwCost":0.01,"storageCost":0.005,"totalCost":0.085,"time":100.0}
  ],
  "faultRecords": [
    {"hostId":0,"failedPEs":2,"time":40.0}
  ],
  "vmDestroyedRecords": [
    {"vmId":0,"hostId":0,"time":40.0,"reason":"Host fault"}
  ]
}"""

  "SimulationData" should "parse a complete JSON document" in {
    val data = read[SimulationData](sampleJson)

    data.simulationEndTime shouldBe 100.0
    data.totalEventsProcessed shouldBe 5000L
    data.totalEnergyWh shouldBe 12.5
    data.avgCompletionTime shouldBe 45.3
    data.totalCost shouldBe 0.1234
  }

  it should "parse workload results" in {
    val data = read[SimulationData](sampleJson)

    data.workloadResults should have size 2
    data.workloadResults.head.workloadId shouldBe 1
    data.workloadResults.head.executedMI shouldBe 1000.0
    data.workloadResults(1).finishTime shouldBe 75.0
  }

  it should "parse failed workloads" in {
    val data = read[SimulationData](sampleJson)

    data.failedWorkloads should have size 1
    data.failedWorkloads.head.reason shouldBe "Host failure"
  }

  it should "parse energy records" in {
    val data = read[SimulationData](sampleJson)

    data.energyRecords should have size 1
    data.energyRecords.head.watts shouldBe 100.0
    data.energyRecords.head.energyWh shouldBe 5.0
  }

  it should "parse migration records" in {
    val data = read[SimulationData](sampleJson)

    data.migrationRecords should have size 1
    data.migrationRecords.head.dataTransferred shouldBe 128.0
  }

  it should "parse serverless records" in {
    val data = read[SimulationData](sampleJson)

    data.invocationResults should have size 1
    data.invocationResults.head.coldStart shouldBe true
    data.throttledInvocations should have size 1
    data.timedOutInvocations should have size 1
  }

  it should "parse container records" in {
    val data = read[SimulationData](sampleJson)

    data.podResults should have size 1
    data.podResults.head.phase shouldBe "Succeeded"
    data.podResults.head.finishTime shouldBe Some(50.0)
    data.podSchedulingRecords should have size 1
  }

  it should "parse edge records" in {
    val data = read[SimulationData](sampleJson)

    data.edgeTaskResults should have size 1
    data.edgeTaskResults.head.offloaded shouldBe true
    data.failedEdgeTasks should have size 1
  }

  it should "parse federated records" in {
    val data = read[SimulationData](sampleJson)

    data.federatedTaskResults should have size 1
    data.federatedTaskResults.head.escalations shouldBe 2
    data.failedFederatedTasks should have size 1
    data.failedFederatedTasks.head.tiersAttempted shouldBe 3
  }

  it should "parse cost and fault records" in {
    val data = read[SimulationData](sampleJson)

    data.costRecords should have size 1
    data.costRecords.head.totalCost shouldBe 0.085
    data.faultRecords should have size 1
    data.faultRecords.head.failedPEs shouldBe 2
    data.vmDestroyedRecords should have size 1
  }

  it should "detect data availability flags correctly" in {
    val data = read[SimulationData](sampleJson)

    data.hasServerlessData shouldBe true
    data.hasContainerData shouldBe true
    data.hasEdgeData shouldBe true
    data.hasFederatedData shouldBe true
  }

  it should "handle empty arrays" in {
    val emptyJson = """{
  "simulationEndTime": 0.0,
  "totalEventsProcessed": 0,
  "totalEnergyWh": 0.0,
  "avgCompletionTime": 0.0,
  "totalCost": 0.0,
  "workloadResults": [],
  "failedWorkloads": [],
  "vmPlacements": [],
  "energyRecords": [],
  "migrationRecords": [],
  "invocationResults": [],
  "throttledInvocations": [],
  "timedOutInvocations": [],
  "podResults": [],
  "podSchedulingRecords": [],
  "edgeTaskResults": [],
  "failedEdgeTasks": [],
  "federatedTaskResults": [],
  "failedFederatedTasks": [],
  "costRecords": [],
  "faultRecords": [],
  "vmDestroyedRecords": []
}"""
    val data      = read[SimulationData](emptyJson)

    data.workloadResults shouldBe empty
    data.hasServerlessData shouldBe false
    data.hasContainerData shouldBe false
    data.hasEdgeData shouldBe false
    data.hasFederatedData shouldBe false
  }

  "MetricsSnapshot" should "parse a snapshot JSON" in {
    val json     = """{
  "timestamp": 50.0,
  "completedWorkloads": 10,
  "failedWorkloads": 1,
  "activeVms": 5,
  "totalMigrations": 2,
  "totalFaults": 0,
  "totalEnergyWh": 6.5,
  "avgCompletionTime": 25.0,
  "throughput": 0.2,
  "currentUtilization": 0.75,
  "totalCost": 0.05,
  "eventCount": 1000
}"""
    val snapshot = read[MetricsSnapshot](json)

    snapshot.timestamp shouldBe 50.0
    snapshot.completedWorkloads shouldBe 10
    snapshot.currentUtilization shouldBe 0.75
    snapshot.eventCount shouldBe 1000L
  }
