// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.traces

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.types.*
import io.aura.core.engine.{FaultRecord, MigrationRecord, SimulationResults, VmPlacement, WorkloadResult}

class OtelExporterSpec extends AnyFlatSpec with Matchers:

  private def sampleResults: SimulationResults =
    SimulationResults.empty.copy(
      simulationEndTime = SimTime(100.0),
      workloadResults = Vector(
        WorkloadResult(WorkloadId(0), VmId(0), HostId(0), SimTime(50.0), MI(10000.0)),
        WorkloadResult(WorkloadId(1), VmId(1), HostId(1), SimTime(80.0), MI(20000.0))
      ),
      migrationRecords = Vector(
        MigrationRecord(VmId(0), HostId(0), HostId(1), SimTime(30.0), SimTime(5.0), MegaBytes(1024.0))
      ),
      faultRecords = Vector(
        FaultRecord(HostId(0), PEs(4), SimTime(25.0))
      )
    )

  // ─── OTLP JSON Structure ────────────────────────────────────────────

  "OtelExporter.toOtlpJson" should "produce valid OTLP JSON with resourceSpans" in {
    val json = OtelExporter.toOtlpJson(sampleResults)
    json should include("\"resourceSpans\"")
    json should include("\"scopeSpans\"")
    json should include("\"spans\"")
  }

  it should "include service name in resource attributes" in {
    val json = OtelExporter.toOtlpJson(sampleResults, serviceName = "my-cloud-sim")
    json should include("service.name")
    json should include("my-cloud-sim")
  }

  it should "include scope information" in {
    val json = OtelExporter.toOtlpJson(sampleResults)
    json should include("aura-simulator")
    json should include("0.1.0")
  }

  // ─── Workload Spans ─────────────────────────────────────────────────

  it should "include workload spans" in {
    val json = OtelExporter.toOtlpJson(sampleResults)
    json should include("workload-0")
    json should include("workload-1")
  }

  it should "include workload attributes" in {
    val json = OtelExporter.toOtlpJson(sampleResults)
    json should include("workload.id")
    json should include("vm.id")
    json should include("host.id")
    json should include("mi.executed")
  }

  // ─── Migration Spans ────────────────────────────────────────────────

  it should "include migration spans" in {
    val json = OtelExporter.toOtlpJson(sampleResults)
    json should include("migration-vm-0")
    json should include("source.host")
    json should include("target.host")
    json should include("data.transferred.mb")
  }

  // ─── Fault Spans ────────────────────────────────────────────────────

  it should "include fault event spans with error status" in {
    val json = OtelExporter.toOtlpJson(sampleResults)
    json should include("fault-host-0")
    json should include("failed.pes")
    json should include("\"code\": 2") // STATUS_CODE_ERROR
  }

  // ─── Trace and Span IDs ────────────────────────────────────────────

  it should "produce 32-character trace ID" in {
    val json           = OtelExporter.toOtlpJson(sampleResults)
    val traceIdPattern = """"traceId": "([a-f0-9]{32})"""".r
    traceIdPattern.findFirstMatchIn(json) should not be empty
  }

  it should "produce 16-character span IDs" in {
    val json          = OtelExporter.toOtlpJson(sampleResults)
    val spanIdPattern = """"spanId": "([a-f0-9]{16})"""".r
    spanIdPattern.findAllMatchIn(json).toVector should not be empty
  }

  it should "use consistent trace ID across all spans" in {
    val json           = OtelExporter.toOtlpJson(sampleResults)
    val traceIdPattern = """"traceId": "([a-f0-9]{32})"""".r
    val traceIds       = traceIdPattern.findAllMatchIn(json).map(_.group(1)).toSet
    traceIds should have size 1
  }

  // ─── Edge Cases ─────────────────────────────────────────────────────

  it should "handle empty results" in {
    val json = OtelExporter.toOtlpJson(SimulationResults.empty)
    json should include("\"resourceSpans\"")
    json should include("\"spans\": []")
  }

  it should "use custom trace ID seed" in {
    val json1          = OtelExporter.toOtlpJson(sampleResults, traceIdSeed = 1L)
    val json2          = OtelExporter.toOtlpJson(sampleResults, traceIdSeed = 2L)
    val traceIdPattern = """"traceId": "([a-f0-9]{32})"""".r
    val tid1           = traceIdPattern.findFirstMatchIn(json1).map(_.group(1))
    val tid2           = traceIdPattern.findFirstMatchIn(json2).map(_.group(1))
    tid1 should not be tid2
  }

  it should "include time as nanosecond strings" in {
    val json = OtelExporter.toOtlpJson(sampleResults)
    json should include("startTimeUnixNano")
    json should include("endTimeUnixNano")
  }
