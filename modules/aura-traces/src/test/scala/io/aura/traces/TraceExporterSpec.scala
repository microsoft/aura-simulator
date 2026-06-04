// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.traces

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.types.*
import io.aura.core.engine.{
  EnergyRecord,
  FaultRecord,
  MigrationRecord,
  SimulationResults,
  VmDestroyedRecord,
  VmPlacement,
  WorkloadResult
}

class TraceExporterSpec extends AnyFlatSpec with Matchers:

  private def sampleResults: SimulationResults =
    SimulationResults.empty.copy(
      simulationEndTime = SimTime(100.0),
      workloadResults = Vector(
        WorkloadResult(WorkloadId(0), VmId(0), HostId(0), SimTime(50.0), MI(10000.0)),
        WorkloadResult(WorkloadId(1), VmId(1), HostId(0), SimTime(80.0), MI(20000.0))
      ),
      vmPlacements = Vector(
        VmPlacement(VmId(0), HostId(0), DatacenterId(0), SimTime(0.0)),
        VmPlacement(VmId(1), HostId(0), DatacenterId(0), SimTime(0.0))
      ),
      migrationRecords = Vector(
        MigrationRecord(VmId(0), HostId(0), HostId(1), SimTime(30.0), SimTime(5.0), MegaBytes(1024.0))
      ),
      faultRecords = Vector(
        FaultRecord(HostId(0), PEs(4), SimTime(25.0))
      )
    )

  // ─── Chrome Trace Format ──────────────────────────────────────────────

  "TraceExporter.toChromeTrace" should "produce valid JSON with traceEvents array" in {
    val json = TraceExporter.toChromeTrace(sampleResults)
    json should startWith("{\"traceEvents\":[")
    json should endWith("]}")
  }

  it should "include workload events" in {
    val json = TraceExporter.toChromeTrace(sampleResults)
    json should include("workload-0")
    json should include("workload-1")
    json should include("\"cat\":\"workload\"")
    json should include("\"ph\":\"X\"")
  }

  it should "include migration events" in {
    val json = TraceExporter.toChromeTrace(sampleResults)
    json should include("migrate-vm-0")
    json should include("\"cat\":\"migration\"")
  }

  it should "include fault events as instant markers" in {
    val json = TraceExporter.toChromeTrace(sampleResults)
    json should include("fault-host-0")
    json should include("\"ph\":\"i\"")
  }

  it should "include VM placement events" in {
    val json = TraceExporter.toChromeTrace(sampleResults)
    json should include("vm-placed-0")
    json should include("\"cat\":\"placement\"")
  }

  it should "include process metadata" in {
    val json = TraceExporter.toChromeTrace(sampleResults)
    json should include("process_name")
    json should include("Datacenter")
  }

  it should "handle empty results" in {
    val json = TraceExporter.toChromeTrace(SimulationResults.empty)
    json should startWith("{\"traceEvents\":[")
    json should include("process_name")
  }

  // ─── Jaeger Trace Format ──────────────────────────────────────────────

  "TraceExporter.toJaegerTrace" should "produce valid Jaeger JSON" in {
    val json = TraceExporter.toJaegerTrace(sampleResults)
    json should include("\"data\"")
    json should include("\"spans\"")
    json should include("\"processes\"")
  }

  it should "include workload spans" in {
    val json = TraceExporter.toJaegerTrace(sampleResults)
    json should include("workload-0")
    json should include("workload-1")
  }

  it should "use custom service name" in {
    val json = TraceExporter.toJaegerTrace(sampleResults, serviceName = "my-sim")
    json should include("my-sim")
  }

  it should "include span tags with VM and host info" in {
    val json = TraceExporter.toJaegerTrace(sampleResults)
    json should include("vm.id")
    json should include("host.id")
    json should include("mi.executed")
  }
