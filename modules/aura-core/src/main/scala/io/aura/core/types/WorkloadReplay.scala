// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import io.aura.core.events.WorkloadSpec

/** Parsed trace record from a real-world workload trace.
  *
  * Agnostic to trace format — Google, Azure, Alibaba traces all get normalized to this common representation.
  */
final case class TraceRecord(
    jobId: Long,
    taskIndex: Int,
    submitTime: Double,    // seconds
    duration: Double,      // seconds
    cpuRequest: Double,    // cores
    memoryRequest: Double, // MB
    priority: Int = 0,
    eventType: TraceEventType = TraceEventType.Submit
)

enum TraceEventType:
  case Submit, Schedule, Evict, Finish, Fail, Kill

/** Parsed trace with metadata. */
final case class ParsedTrace(
    records: Vector[TraceRecord],
    source: String,
    totalJobs: Int,
    timeSpanSeconds: Double
):
  /** Scale trace time to fit within a simulation window. */
  def scaleTo(targetDurationSeconds: Double): ParsedTrace =
    if timeSpanSeconds <= 0 then this
    else
      val factor = targetDurationSeconds / timeSpanSeconds
      copy(
        records = records.map(r =>
          r.copy(
            submitTime = r.submitTime * factor,
            duration = r.duration * factor
          )
        ),
        timeSpanSeconds = targetDurationSeconds
      )

  /** Sample a fraction of records (for large traces). */
  def sample(fraction: Double, seed: Long = 42L): ParsedTrace =
    val rng     = new java.util.Random(seed)
    val sampled = records.filter(_ => rng.nextDouble() < fraction)
    copy(records = sampled, totalJobs = sampled.map(_.jobId).distinct.size)

  /** Filter to only submission events. */
  def submitsOnly: ParsedTrace =
    copy(records = records.filter(_.eventType == TraceEventType.Submit))

  /** Convert trace records to WorkloadSpecs for simulation. */
  def toWorkloadSpecs(
      baseMIPS: MIPS = MIPS(1000.0),
      defaultFileSizeMB: MegaBytes = MegaBytes(300.0)
  ): Vector[WorkloadSpec] =
    records.zipWithIndex.map { (rec, idx) =>
      val mi  = MI(rec.duration * baseMIPS.value) // duration * MIPS = MI
      val pes = PEs(math.max(1, math.ceil(rec.cpuRequest).toInt))
      WorkloadSpec(
        id = WorkloadId(idx.toLong),
        length = mi,
        pes = pes,
        requiredMips = baseMIPS,
        fileSize = defaultFileSizeMB,
        outputSize = defaultFileSizeMB,
        utilizationCpu = Utilization(math.min(1.0, rec.cpuRequest / math.ceil(rec.cpuRequest))),
        utilizationRam = Utilization(0.5),
        utilizationBw = Utilization(0.1),
        submissionDelay = SimTime(rec.submitTime),
        weight = rec.priority
      )
    }

/** Parsers for real-world workload trace formats. */
object WorkloadReplay:

  /** Parse Google Cluster Trace v3 format (2019).
    *
    * Expected CSV columns: time, jobId, taskIndex, eventType, cpuRequest, memoryRequest, ... Fields are microsecond
    * timestamps and normalized resource requests.
    *
    * @param csvContent
    *   CSV content with header row
    * @param cpuScale
    *   Multiplier for CPU (Google uses normalized values, typically 0-1 per core)
    * @param memoryScaleMB
    *   Multiplier to convert memory to MB (Google uses normalized values)
    */
  def parseGoogleTrace(
      csvContent: String,
      cpuScale: Double = 1.0,
      memoryScaleMB: Double = 1024.0
  ): Either[String, ParsedTrace] =
    val lines = csvContent.split('\n').toVector
    if lines.size < 2 then Left("CSV must have header and at least one data row")
    else
      val records = lines.drop(1).flatMap { line =>
        val fields = line.split(',').map(_.trim)
        if fields.length < 6 then None
        else
          for
            time      <- fields(0).toDoubleOption
            jobId     <- fields(1).toLongOption
            taskIndex <- fields(2).toIntOption
            eventType <- parseGoogleEventType(fields(3))
            cpu       <- fields(4).toDoubleOption
            memory    <- fields(5).toDoubleOption
          yield TraceRecord(
            jobId = jobId,
            taskIndex = taskIndex,
            submitTime = time / 1_000_000.0, // microseconds to seconds
            duration = 0.0,                  // will be computed from event pairs
            cpuRequest = cpu * cpuScale,
            memoryRequest = memory * memoryScaleMB,
            eventType = eventType
          )
      }

      if records.isEmpty then Left("No valid records parsed from Google trace")
      else
        val withDurations = inferDurations(records)
        val timeSpan =
          if withDurations.nonEmpty then withDurations.map(r => r.submitTime + r.duration).max
          else 0.0
        Right(
          ParsedTrace(
            records = withDurations,
            source = "google-cluster-trace-v3",
            totalJobs = withDurations.map(_.jobId).distinct.size,
            timeSpanSeconds = timeSpan
          )
        )

  /** Parse Azure VM Trace format (2019).
    *
    * Expected CSV columns: vmId, subscriptionId, deploymentId, timestamp, cpuCores, memoryGB, ...
    *
    * @param csvContent
    *   CSV content with header row
    */
  def parseAzureTrace(
      csvContent: String
  ): Either[String, ParsedTrace] =
    val lines = csvContent.split('\n').toVector
    if lines.size < 2 then Left("CSV must have header and at least one data row")
    else
      val records = lines.drop(1).flatMap { line =>
        val fields = line.split(',').map(_.trim)
        if fields.length < 5 then None
        else
          for
            vmId      <- fields(0).toLongOption
            timestamp <- fields(3).toDoubleOption
            cpuCores  <- fields(4).toDoubleOption
          yield
            val memoryGB = fields.lift(5).flatMap(_.toDoubleOption).getOrElse(4.0)
            TraceRecord(
              jobId = vmId,
              taskIndex = 0,
              submitTime = timestamp,
              duration = 3600.0, // default 1 hour per VM
              cpuRequest = cpuCores,
              memoryRequest = memoryGB * 1024.0,
              priority = 0
            )
      }

      if records.isEmpty then Left("No valid records parsed from Azure trace")
      else
        val timeSpan =
          if records.nonEmpty then records.map(r => r.submitTime + r.duration).max
          else 0.0
        Right(
          ParsedTrace(
            records = records,
            source = "azure-vm-trace-2019",
            totalJobs = records.map(_.jobId).distinct.size,
            timeSpanSeconds = timeSpan
          )
        )

  /** Parse a simple generic CSV trace format.
    *
    * Expected columns: submitTime, duration, cpuCores, memoryMB Simplest format for custom workload traces.
    */
  def parseSimpleTrace(csvContent: String): Either[String, ParsedTrace] =
    val lines = csvContent.split('\n').toVector
    if lines.size < 2 then Left("CSV must have header and at least one data row")
    else
      val records = lines.drop(1).zipWithIndex.flatMap { (line, idx) =>
        val fields = line.split(',').map(_.trim)
        if fields.length < 4 then None
        else
          for
            submitTime <- fields(0).toDoubleOption
            duration   <- fields(1).toDoubleOption
            cpu        <- fields(2).toDoubleOption
            memory     <- fields(3).toDoubleOption
          yield TraceRecord(
            jobId = idx.toLong,
            taskIndex = 0,
            submitTime = submitTime,
            duration = duration,
            cpuRequest = cpu,
            memoryRequest = memory,
            priority = fields.lift(4).flatMap(_.toIntOption).getOrElse(0)
          )
      }

      if records.isEmpty then Left("No valid records parsed")
      else
        val timeSpan = records.map(r => r.submitTime + r.duration).max
        Right(
          ParsedTrace(
            records = records,
            source = "simple-csv",
            totalJobs = records.size,
            timeSpanSeconds = timeSpan
          )
        )

  /** Infer durations from event pairs (submit → finish). */
  private def inferDurations(records: Vector[TraceRecord]): Vector[TraceRecord] =
    val finishTimes = records
      .filter(r => r.eventType == TraceEventType.Finish || r.eventType == TraceEventType.Kill)
      .groupBy(r => (r.jobId, r.taskIndex))
      .view
      .mapValues(_.head.submitTime)
      .toMap

    records
      .filter(_.eventType == TraceEventType.Submit)
      .map { r =>
        val key = (r.jobId, r.taskIndex)
        val duration = finishTimes.get(key) match
          case Some(endTime) => math.max(0.0, endTime - r.submitTime)
          case None          => r.cpuRequest * 60.0 // estimate: 1 min per core
        r.copy(duration = duration)
      }

  private def parseGoogleEventType(s: String): Option[TraceEventType] =
    s.trim.toLowerCase match
      case "0" | "submit"   => Some(TraceEventType.Submit)
      case "1" | "schedule" => Some(TraceEventType.Schedule)
      case "2" | "evict"    => Some(TraceEventType.Evict)
      case "3" | "finish"   => Some(TraceEventType.Finish)
      case "4" | "fail"     => Some(TraceEventType.Fail)
      case "5" | "kill"     => Some(TraceEventType.Kill)
      case _                => Some(TraceEventType.Submit)

  /** Generate a summary of parsed trace for logging. */
  def traceSummary(trace: ParsedTrace): String =
    val avgCpu = if trace.records.nonEmpty then trace.records.map(_.cpuRequest).sum / trace.records.size else 0.0
    val avgMem = if trace.records.nonEmpty then trace.records.map(_.memoryRequest).sum / trace.records.size else 0.0
    val avgDur = if trace.records.nonEmpty then trace.records.map(_.duration).sum / trace.records.size else 0.0
    f"""Trace Summary (${trace.source}):
  Records: ${trace.records.size}
  Unique jobs: ${trace.totalJobs}
  Time span: ${trace.timeSpanSeconds}%.1f seconds
  Avg CPU: ${avgCpu}%.2f cores
  Avg Memory: ${avgMem}%.1f MB
  Avg Duration: ${avgDur}%.1f seconds"""
