// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.traces

import io.aura.core.types.*
import io.aura.core.engine.{SimulationResults, WorkloadResult}

/** Export simulation results to OpenTelemetry Protocol (OTLP) JSON format.
  *
  * Produces JSON compatible with the OTLP/HTTP JSON protocol for traces. Can be sent to any OpenTelemetry collector or
  * backend (Jaeger, Zipkin, Tempo, etc.).
  *
  * Reference: https://opentelemetry.io/docs/specs/otlp/
  */
object OtelExporter:

  private def toHex(value: Long, length: Int): String =
    val hex = java.lang.Long.toHexString(value)
    "0" * (length - hex.length) + hex

  private def traceId(seed: Long): String = toHex(seed, 16) + toHex(seed ^ 0x5deece66dL, 16)
  private def spanId(value: Long): String = toHex(value + 1, 16)

  /** Export to OTLP JSON trace format.
    *
    * @param results
    *   Simulation results
    * @param serviceName
    *   Service name for the trace resource
    * @param traceIdSeed
    *   Seed for generating trace ID
    */
  def toOtlpJson(
      results: SimulationResults,
      serviceName: String = "aura-simulation",
      traceIdSeed: Long = 42L
  ): String =
    val tid = traceId(traceIdSeed)

    val workloadSpans = results.workloadResults.map { wr =>
      val startTime = estimateStartTimeNanos(wr)
      val endTime   = (wr.finishTime.value * 1_000_000_000L).toLong
      otelSpan(
        traceId = tid,
        spanId = spanId(wr.workloadId.value),
        name = s"workload-${wr.workloadId.value}",
        kind = 1, // SPAN_KIND_INTERNAL
        startTimeNanos = startTime,
        endTimeNanos = endTime,
        attributes = Vector(
          otelAttribute("workload.id", wr.workloadId.value),
          otelAttribute("vm.id", wr.vmId.value),
          otelAttribute("host.id", wr.hostId.value),
          otelAttribute("mi.executed", wr.executedMI.value)
        ),
        status = 1 // STATUS_CODE_OK
      )
    }

    val migrationSpans = results.migrationRecords.map { mr =>
      val startNanos = (mr.startTime.value * 1_000_000_000L).toLong
      val endNanos   = ((mr.startTime.value + mr.duration.value) * 1_000_000_000L).toLong
      otelSpan(
        traceId = tid,
        spanId = spanId(mr.vmId.value + 10000),
        name = s"migration-vm-${mr.vmId.value}",
        kind = 1,
        startTimeNanos = startNanos,
        endTimeNanos = endNanos,
        attributes = Vector(
          otelAttribute("vm.id", mr.vmId.value),
          otelAttribute("source.host", mr.sourceHostId.value),
          otelAttribute("target.host", mr.targetHostId.value),
          otelAttribute("data.transferred.mb", mr.dataTransferred.value)
        ),
        status = 1
      )
    }

    val faultEvents = results.faultRecords.map { fr =>
      val timeNanos = (fr.time.value * 1_000_000_000L).toLong
      otelSpan(
        traceId = tid,
        spanId = spanId(fr.hostId.value + 20000),
        name = s"fault-host-${fr.hostId.value}",
        kind = 1,
        startTimeNanos = timeNanos,
        endTimeNanos = timeNanos + 1_000_000L, // 1ms duration for instant events
        attributes = Vector(
          otelAttribute("host.id", fr.hostId.value),
          otelAttribute("failed.pes", fr.failedPEs.value.toLong)
        ),
        status = 2 // STATUS_CODE_ERROR
      )
    }

    val allSpans = workloadSpans ++ migrationSpans ++ faultEvents

    s"""{
  "resourceSpans": [{
    "resource": {
      "attributes": [
        ${otelAttribute("service.name", serviceName)}
      ]
    },
    "scopeSpans": [{
      "scope": {
        "name": "aura-simulator",
        "version": "0.1.0"
      },
      "spans": [${allSpans.mkString(",\n        ")}]
    }]
  }]
}"""

  private def otelSpan(
      traceId: String,
      spanId: String,
      name: String,
      kind: Int,
      startTimeNanos: Long,
      endTimeNanos: Long,
      attributes: Vector[String],
      status: Int
  ): String =
    s"""{
          "traceId": "$traceId",
          "spanId": "$spanId",
          "name": "$name",
          "kind": $kind,
          "startTimeUnixNano": "$startTimeNanos",
          "endTimeUnixNano": "$endTimeNanos",
          "attributes": [${attributes.mkString(", ")}],
          "status": {"code": $status}
        }"""

  private def otelAttribute(key: String, value: String): String =
    s"""{"key": "$key", "value": {"stringValue": "$value"}}"""

  private def otelAttribute(key: String, value: Long): String =
    s"""{"key": "$key", "value": {"intValue": "$value"}}"""

  private def otelAttribute(key: String, value: Double): String =
    s"""{"key": "$key", "value": {"doubleValue": $value}}"""

  private def estimateStartTimeNanos(wr: WorkloadResult): Long =
    val startSeconds = math.max(0.0, wr.finishTime.value * 0.1)
    (startSeconds * 1_000_000_000L).toLong
