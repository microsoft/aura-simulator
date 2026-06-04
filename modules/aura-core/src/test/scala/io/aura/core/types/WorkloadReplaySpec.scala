// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WorkloadReplaySpec extends AnyFlatSpec with Matchers:

  // ─── Google Trace Parser ────────────────────────────────────────────

  private val googleCsv =
    """time,jobId,taskIndex,eventType,cpuRequest,memoryRequest
      |0,100,0,0,0.5,0.25
      |0,100,1,0,1.0,0.5
      |1000000,101,0,0,2.0,1.0
      |5000000,100,0,3,0.5,0.25
      |5000000,100,1,3,1.0,0.5
      |8000000,101,0,3,2.0,1.0""".stripMargin

  "WorkloadReplay.parseGoogleTrace" should "parse valid Google trace CSV" in {
    val result = WorkloadReplay.parseGoogleTrace(googleCsv)
    result shouldBe a[Right[?, ?]]
    val trace = result.toOption.get
    trace.source shouldBe "google-cluster-trace-v3"
    trace.records should not be empty
  }

  it should "convert microsecond timestamps to seconds" in {
    val trace = WorkloadReplay.parseGoogleTrace(googleCsv).toOption.get
    // job 101 submits at 1000000 microseconds = 1 second
    trace.records.find(_.jobId == 101).get.submitTime shouldBe 1.0 +- 0.001
  }

  it should "infer durations from submit-finish pairs" in {
    val trace = WorkloadReplay.parseGoogleTrace(googleCsv).toOption.get
    // job 100 task 0: submit at 0, finish at 5000000us = 5s
    val job100task0 = trace.records.find(r => r.jobId == 100 && r.taskIndex == 0).get
    job100task0.duration shouldBe 5.0 +- 0.001
  }

  it should "scale CPU and memory" in {
    val trace = WorkloadReplay.parseGoogleTrace(googleCsv, cpuScale = 4.0, memoryScaleMB = 2048.0).toOption.get
    val r     = trace.records.find(r => r.jobId == 100 && r.taskIndex == 0).get
    r.cpuRequest shouldBe 2.0 +- 0.001    // 0.5 * 4.0
    r.memoryRequest shouldBe 512.0 +- 0.1 // 0.25 * 2048.0
  }

  it should "reject empty CSV" in {
    WorkloadReplay.parseGoogleTrace("header\n") shouldBe a[Left[?, ?]]
  }

  // ─── Azure Trace Parser ─────────────────────────────────────────────

  private val azureCsv =
    """vmId,subscriptionId,deploymentId,timestamp,cpuCores,memoryGB
      |1,sub1,dep1,0.0,4,8.0
      |2,sub1,dep1,10.0,2,4.0
      |3,sub2,dep2,20.0,8,16.0""".stripMargin

  "WorkloadReplay.parseAzureTrace" should "parse valid Azure trace CSV" in {
    val result = WorkloadReplay.parseAzureTrace(azureCsv)
    result shouldBe a[Right[?, ?]]
    val trace = result.toOption.get
    trace.records should have size 3
    trace.source shouldBe "azure-vm-trace-2019"
  }

  it should "convert memory from GB to MB" in {
    val trace = WorkloadReplay.parseAzureTrace(azureCsv).toOption.get
    val r     = trace.records.head
    r.memoryRequest shouldBe 8192.0 +- 0.1 // 8 GB * 1024
  }

  it should "use correct CPU cores" in {
    val trace = WorkloadReplay.parseAzureTrace(azureCsv).toOption.get
    trace.records.map(_.cpuRequest) shouldBe Vector(4.0, 2.0, 8.0)
  }

  it should "reject empty CSV" in {
    WorkloadReplay.parseAzureTrace("header\n") shouldBe a[Left[?, ?]]
  }

  // ─── Simple Trace Parser ────────────────────────────────────────────

  private val simpleCsv =
    """submitTime,duration,cpuCores,memoryMB,priority
      |0.0,10.0,2,4096,5
      |5.0,20.0,4,8192,3
      |15.0,5.0,1,2048,1""".stripMargin

  "WorkloadReplay.parseSimpleTrace" should "parse valid simple CSV" in {
    val result = WorkloadReplay.parseSimpleTrace(simpleCsv)
    result shouldBe a[Right[?, ?]]
    val trace = result.toOption.get
    trace.records should have size 3
    trace.source shouldBe "simple-csv"
  }

  it should "capture priority from 5th column" in {
    val trace = WorkloadReplay.parseSimpleTrace(simpleCsv).toOption.get
    trace.records.map(_.priority) shouldBe Vector(5, 3, 1)
  }

  it should "compute correct time span" in {
    val trace = WorkloadReplay.parseSimpleTrace(simpleCsv).toOption.get
    trace.timeSpanSeconds shouldBe 25.0 +- 0.001 // 5 + 20 = 25 (max of submit+duration)
  }

  // ─── ParsedTrace Operations ─────────────────────────────────────────

  "ParsedTrace.scaleTo" should "scale timestamps proportionally" in {
    val trace  = WorkloadReplay.parseSimpleTrace(simpleCsv).toOption.get
    val scaled = trace.scaleTo(100.0)
    scaled.timeSpanSeconds shouldBe 100.0 +- 0.001
    // Original: submitTime=5, span=25 → scaled: submitTime=5*(100/25)=20
    scaled.records(1).submitTime shouldBe 20.0 +- 0.001
  }

  "ParsedTrace.sample" should "reduce record count" in {
    val trace   = WorkloadReplay.parseSimpleTrace(simpleCsv).toOption.get
    val sampled = trace.sample(0.5, seed = 42L)
    sampled.records.size should be <= trace.records.size
  }

  "ParsedTrace.toWorkloadSpecs" should "convert to WorkloadSpec vector" in {
    val trace = WorkloadReplay.parseSimpleTrace(simpleCsv).toOption.get
    val specs = trace.toWorkloadSpecs()
    specs should have size 3
    specs.head.submissionDelay shouldBe SimTime(0.0)
    specs(1).submissionDelay shouldBe SimTime(5.0)
  }

  it should "compute MI from duration * MIPS" in {
    val trace = WorkloadReplay.parseSimpleTrace(simpleCsv).toOption.get
    val specs = trace.toWorkloadSpecs(baseMIPS = MIPS(1000.0))
    // First record: duration=10s, 1000 MIPS → 10000 MI
    specs.head.length.value shouldBe 10000.0 +- 0.001
  }

  it should "set PEs from CPU request" in {
    val trace = WorkloadReplay.parseSimpleTrace(simpleCsv).toOption.get
    val specs = trace.toWorkloadSpecs()
    specs(1).pes.value shouldBe 4 // 4 CPU cores
  }

  // ─── traceSummary ───────────────────────────────────────────────────

  "WorkloadReplay.traceSummary" should "produce readable output" in {
    val trace   = WorkloadReplay.parseSimpleTrace(simpleCsv).toOption.get
    val summary = WorkloadReplay.traceSummary(trace)
    summary should include("Records: 3")
    summary should include("simple-csv")
    summary should include("Avg CPU")
  }

  // ─── Edge Cases ─────────────────────────────────────────────────────

  "WorkloadReplay" should "handle CSV with only header" in {
    WorkloadReplay.parseSimpleTrace("submitTime,duration,cpu,memory") shouldBe a[Left[?, ?]]
  }

  it should "skip malformed rows" in {
    val csv = """submitTime,duration,cpuCores,memoryMB
      |0.0,10.0,2,4096
      |bad,data,here
      |5.0,20.0,4,8192""".stripMargin
    val trace = WorkloadReplay.parseSimpleTrace(csv).toOption.get
    trace.records should have size 2
  }

  "ParsedTrace.submitsOnly" should "filter to submit events" in {
    val trace   = WorkloadReplay.parseGoogleTrace(googleCsv).toOption.get
    val submits = trace.submitsOnly
    submits.records.forall(_.eventType == TraceEventType.Submit) shouldBe true
  }
