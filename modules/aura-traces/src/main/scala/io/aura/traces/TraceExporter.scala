// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.traces

import io.aura.core.types.*
import io.aura.core.engine.{SimulationResults, WorkloadResult}

/** Export simulation results to Chrome Trace Event Format (CTF).
  *
  * The output JSON can be loaded into:
  *   - Chrome's chrome://tracing
  *   - Perfetto UI (https://ui.perfetto.dev)
  *   - Jaeger (with conversion)
  *
  * Reference: https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU
  */
object TraceExporter:

  /** A single trace event in Chrome Trace Format. */
  private case class TraceEvent(
      name: String,
      cat: String,                // category
      ph: String,                 // phase: B=begin, E=end, X=complete, i=instant
      ts: Double,                 // timestamp in microseconds
      dur: Option[Double] = None, // duration in microseconds (for X events)
      pid: Long = 0,              // process ID (datacenter)
      tid: Long = 0,              // thread ID (host/VM)
      args: Map[String, String] = Map.empty
  ):
    def toJSON: String =
      val base   = s"""{"name":"$name","cat":"$cat","ph":"$ph","ts":$ts,"pid":$pid,"tid":$tid"""
      val durStr = dur.map(d => s""","dur":$d""").getOrElse("")
      val argsStr =
        if args.isEmpty then ""
        else
          val fields = args.map { case (k, v) => s""""$k":"$v"""" }.mkString(",")
          s""","args":{$fields}"""
      base + durStr + argsStr + "}"

  /** Export simulation results to Chrome Trace Event Format JSON string.
    *
    * Maps simulation concepts to trace concepts:
    *   - Process = Datacenter
    *   - Thread = Host/VM
    *   - Complete events (X) = Workloads, Migrations
    *   - Instant events (i) = Faults, VM placements
    */
  def toChromeTrace(results: SimulationResults): String =
    val events = Vector.newBuilder[TraceEvent]

    // Workload execution spans (complete events)
    results.workloadResults.foreach { wr =>
      val startTime = estimateStartTime(wr)
      val duration  = wr.finishTime.value - startTime
      events += TraceEvent(
        name = s"workload-${wr.workloadId.value}",
        cat = "workload",
        ph = "X",
        ts = startTime * 1_000_000, // seconds to microseconds
        dur = Some(duration * 1_000_000),
        pid = 0,
        tid = wr.vmId.value,
        args = Map(
          "workloadId" -> wr.workloadId.value.toString,
          "vmId"       -> wr.vmId.value.toString,
          "hostId"     -> wr.hostId.value.toString,
          "executedMI" -> f"${wr.executedMI.value}%.0f"
        )
      )
    }

    // Migration spans
    results.migrationRecords.foreach { mr =>
      events += TraceEvent(
        name = s"migrate-vm-${mr.vmId.value}",
        cat = "migration",
        ph = "X",
        ts = mr.startTime.value * 1_000_000,
        dur = Some(mr.duration.value * 1_000_000),
        pid = 0,
        tid = mr.vmId.value + 1000, // offset to avoid tid collision
        args = Map(
          "vmId"            -> mr.vmId.value.toString,
          "sourceHost"      -> mr.sourceHostId.value.toString,
          "targetHost"      -> mr.targetHostId.value.toString,
          "dataTransferred" -> f"${mr.dataTransferred.value}%.2f MB"
        )
      )
    }

    // Fault instant events
    results.faultRecords.foreach { fr =>
      events += TraceEvent(
        name = s"fault-host-${fr.hostId.value}",
        cat = "fault",
        ph = "i",
        ts = fr.time.value * 1_000_000,
        pid = 0,
        tid = fr.hostId.value + 2000,
        args = Map(
          "hostId"    -> fr.hostId.value.toString,
          "failedPEs" -> fr.failedPEs.value.toString
        )
      )
    }

    // VM placement instant events
    results.vmPlacements.foreach { vp =>
      events += TraceEvent(
        name = s"vm-placed-${vp.vmId.value}",
        cat = "placement",
        ph = "i",
        ts = vp.time.value * 1_000_000,
        pid = vp.datacenterId.value,
        tid = vp.vmId.value,
        args = Map(
          "vmId"   -> vp.vmId.value.toString,
          "hostId" -> vp.hostId.value.toString
        )
      )
    }

    // Process/thread metadata
    val metaEvents = Vector.newBuilder[String]
    metaEvents += s"""{"name":"process_name","ph":"M","pid":0,"tid":0,"args":{"name":"Datacenter"}}"""

    val vmIds = results.workloadResults.map(_.vmId.value).distinct.sorted
    vmIds.foreach { vmId =>
      metaEvents += s"""{"name":"thread_name","ph":"M","pid":0,"tid":$vmId,"args":{"name":"VM-$vmId"}}"""
    }

    val allEvents = events.result().map(_.toJSON) ++ metaEvents.result()
    s"""{"traceEvents":[${allEvents.mkString(",\n")}]}"""

  /** Export to Jaeger-compatible trace JSON.
    *
    * Jaeger uses a different JSON format with spans grouped by process.
    */
  def toJaegerTrace(results: SimulationResults, serviceName: String = "aura-simulation"): String =
    val spans = results.workloadResults.map { wr =>
      val startTime = estimateStartTime(wr)
      val duration  = wr.finishTime.value - startTime
      val spanId    = f"${wr.workloadId.value}%016x"
      val traceId   = "0" * 32
      s"""{
        "traceID":"$traceId",
        "spanID":"$spanId",
        "operationName":"workload-${wr.workloadId.value}",
        "startTime":${(startTime * 1_000_000).toLong},
        "duration":${(duration * 1_000_000).toLong},
        "tags":[
          {"key":"vm.id","type":"int64","value":${wr.vmId.value}},
          {"key":"host.id","type":"int64","value":${wr.hostId.value}},
          {"key":"mi.executed","type":"float64","value":${wr.executedMI.value}}
        ],
        "logs":[],
        "processID":"p1"
      }"""
    }

    s"""{
  "data":[{
    "traceID":"${"0" * 32}",
    "spans":[${spans.mkString(",\n")}],
    "processes":{"p1":{"serviceName":"$serviceName","tags":[]}}
  }]
}"""

  /** Estimate workload start time from finish time and execution time. Uses a simple heuristic based on MI executed.
    */
  private def estimateStartTime(wr: WorkloadResult): Double =
    // For workloads, estimate start as some fraction before finish
    // This is approximate since we don't track exact start time in results
    math.max(0.0, wr.finishTime.value * 0.1) // conservative: assume started at 10% of finish
