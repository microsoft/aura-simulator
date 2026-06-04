// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.traces

import io.aura.core.types.*
import io.aura.core.events.WorkloadSpec

import scala.io.Source
import scala.util.Using

/** Trace readers for real-world cloud workload datasets.
  *
  * Supports:
  *   - Google Cluster Data 2011 (task events + machine events)
  *   - Azure VM Traces 2019
  *
  * All read methods return Either[TraceReadError, Vector[...]] for safe error handling.
  */

/** Typed errors for trace reading operations. */
enum TraceReadError:
  case FileNotFound(path: String, cause: String)
  case ParseError(path: String, cause: String)

// ─── Google Cluster Data 2011 ──────────────────────────────────────────

object GoogleClusterTrace:

  /** A task event from the Google Cluster Data trace. */
  final case class TaskEvent(
      timestamp: SimTime, // microseconds in original, converted to seconds
      jobId: Long,
      taskIndex: Int,
      machineId: Option[Long],
      eventType: TaskEventType,
      cpuRequest: Double,    // normalized to machine capacity [0, 1]
      memoryRequest: Double, // normalized
      diskRequest: Double,   // normalized
      priority: Int
  )

  enum TaskEventType(val code: Int):
    case Submit        extends TaskEventType(0)
    case Schedule      extends TaskEventType(1)
    case Evict         extends TaskEventType(2)
    case Fail          extends TaskEventType(3)
    case Finish        extends TaskEventType(4)
    case Kill          extends TaskEventType(5)
    case Lost          extends TaskEventType(6)
    case UpdatePending extends TaskEventType(7)
    case UpdateRunning extends TaskEventType(8)

  object TaskEventType:
    def fromCode(code: Int): Option[TaskEventType] =
      TaskEventType.values.find(_.code == code)

  /** A machine event from the trace. */
  final case class MachineEvent(
      timestamp: SimTime,
      machineId: Long,
      eventType: MachineEventType,
      cpuCapacity: Double,   // normalized
      memoryCapacity: Double // normalized
  )

  enum MachineEventType(val code: Int):
    case Add    extends MachineEventType(0)
    case Remove extends MachineEventType(1)
    case Update extends MachineEventType(2)

  object MachineEventType:
    def fromCode(code: Int): Option[MachineEventType] =
      MachineEventType.values.find(_.code == code)

  /** Read task events from a CSV file. Format: timestamp,missing_info,job_id,task_index,machine_id,event_type,user,
    * scheduling_class,priority,cpu_request,memory_request,disk_request,constraint
    */
  def readTaskEvents(path: String, limit: Int = Int.MaxValue): Either[TraceReadError, Vector[TaskEvent]] =
    Using(Source.fromFile(path)) { source =>
      source
        .getLines()
        .take(limit)
        .flatMap(parseLine)
        .toVector
    }.toEither.left.map(e => TraceReadError.FileNotFound(path, e.getMessage))

  private def parseLine(line: String): Option[TaskEvent] =
    val fields = line.split(",", -1)
    if fields.length < 10 then None
    else
      for
        timestamp <- parseDouble(fields(0).trim).map(v => SimTime(v / 1_000_000.0))
        jobId     <- parseLong(fields(2).trim)
        taskIndex <- parseInt(fields(3).trim)
        eventCode <- parseInt(fields(5).trim)
        eventType <- TaskEventType.fromCode(eventCode)
      yield TaskEvent(
        timestamp = timestamp,
        jobId = jobId,
        taskIndex = taskIndex,
        machineId = parseLong(fields(4).trim),
        eventType = eventType,
        cpuRequest = parseDouble(fields(9).trim).getOrElse(0.0),
        memoryRequest = if fields.length > 10 then parseDouble(fields(10).trim).getOrElse(0.0) else 0.0,
        diskRequest = if fields.length > 11 then parseDouble(fields(11).trim).getOrElse(0.0) else 0.0,
        priority = if fields.length > 8 then parseInt(fields(8).trim).getOrElse(0) else 0
      )

  /** Read machine events from a CSV file. Format:
    * timestamp,machine_id,event_type,platform_id,cpu_capacity,memory_capacity
    */
  def readMachineEvents(path: String, limit: Int = Int.MaxValue): Either[TraceReadError, Vector[MachineEvent]] =
    Using(Source.fromFile(path)) { source =>
      source
        .getLines()
        .take(limit)
        .flatMap(parseMachineLine)
        .toVector
    }.toEither.left.map(e => TraceReadError.FileNotFound(path, e.getMessage))

  private def parseMachineLine(line: String): Option[MachineEvent] =
    val fields = line.split(",", -1)
    if fields.length < 4 then None
    else
      for
        timestamp <- parseDouble(fields(0).trim).map(v => SimTime(v / 1_000_000.0))
        machineId <- parseLong(fields(1).trim)
        eventCode <- parseInt(fields(2).trim)
        eventType <- MachineEventType.fromCode(eventCode)
      yield MachineEvent(
        timestamp = timestamp,
        machineId = machineId,
        eventType = eventType,
        cpuCapacity = if fields.length > 4 then parseDouble(fields(4).trim).getOrElse(0.0) else 0.0,
        memoryCapacity = if fields.length > 5 then parseDouble(fields(5).trim).getOrElse(0.0) else 0.0
      )

  /** Convert task events to Aura WorkloadSpecs.
    *
    * @param tasks
    *   Task events to convert
    * @param mipsPerUnit
    *   MIPS for a normalized CPU unit of 1.0
    * @param avgTaskLengthMI
    *   Average task length in MI (since trace has no duration)
    */
  def toWorkloads(
      tasks: Vector[TaskEvent],
      mipsPerUnit: Double = 10000.0,
      avgTaskLengthMI: Double = 50000.0
  ): Vector[WorkloadSpec] =
    tasks
      .filter(_.eventType == TaskEventType.Submit)
      .zipWithIndex
      .map { case (task, idx) =>
        WorkloadSpec(
          id = WorkloadId(idx.toLong),
          length = MI(avgTaskLengthMI * math.max(task.cpuRequest, 0.01)),
          pes = PEs(math.max(1, math.ceil(task.cpuRequest * 4).toInt)),
          requiredMips = MIPS(task.cpuRequest * mipsPerUnit),
          fileSize = MegaBytes(300.0),
          outputSize = MegaBytes(300.0),
          utilizationCpu = Utilization(task.cpuRequest),
          utilizationRam = Utilization(task.memoryRequest),
          utilizationBw = Utilization(0.1),
          submissionDelay = task.timestamp
        )
      }

// ─── Azure VM Traces 2019 ──────────────────────────────────────────────

object AzureVmTrace:

  /** A VM creation/deletion event from the Azure traces. */
  final case class VmEvent(
      vmId: String,
      subscriptionId: String,
      deploymentId: String,
      timestamp: SimTime,
      vmCategory: VmCategory,
      coreCount: Int,
      memoryGb: Double
  )

  enum VmCategory:
    case Delay_Insensitive, Interactive, Unknown

  /** Read VM events from an Azure trace CSV file. Format:
    * vm_id,subscription_id,deployment_id,timestamp,vm_category,core_count,memory_gb
    */
  def readVmEvents(path: String, limit: Int = Int.MaxValue): Either[TraceReadError, Vector[VmEvent]] =
    Using(Source.fromFile(path)) { source =>
      source
        .getLines()
        .drop(1) // skip header
        .take(limit)
        .flatMap(parseVmLine)
        .toVector
    }.toEither.left.map(e => TraceReadError.FileNotFound(path, e.getMessage))

  private def parseVmLine(line: String): Option[VmEvent] =
    val fields = line.split(",", -1)
    if fields.length < 7 then None
    else
      for
        timestamp <- parseDouble(fields(3).trim).map(SimTime(_))
        coreCount <- parseInt(fields(5).trim)
        memoryGb  <- parseDouble(fields(6).trim)
      yield
        val category = fields(4).trim.toLowerCase match
          case "delay-insensitive" => VmCategory.Delay_Insensitive
          case "interactive"       => VmCategory.Interactive
          case _                   => VmCategory.Unknown
        VmEvent(
          vmId = fields(0).trim,
          subscriptionId = fields(1).trim,
          deploymentId = fields(2).trim,
          timestamp = timestamp,
          vmCategory = category,
          coreCount = coreCount,
          memoryGb = memoryGb
        )

// ─── Azure Functions Traces 2019 ──────────────────────────────────────

object AzureFunctionsTrace:

  enum TriggerType:
    case Http, Timer, Queue, Event, Orchestration, Storage, Other

  object TriggerType:
    def fromString(s: String): TriggerType = s.trim.toLowerCase match
      case "http"          => TriggerType.Http
      case "timer"         => TriggerType.Timer
      case "queue"         => TriggerType.Queue
      case "event"         => TriggerType.Event
      case "orchestration" => TriggerType.Orchestration
      case "storage"       => TriggerType.Storage
      case _               => TriggerType.Other

  /** Per-minute invocation counts for a single function over 24 hours. */
  final case class FunctionInvocationProfile(
      hashOwner: String,
      hashApp: String,
      hashFunction: String,
      trigger: TriggerType,
      minuteCounts: Vector[Int] // 1440 per-minute invocation counts
  ):
    def totalInvocations: Int = minuteCounts.sum
    def activeMinutes: Int    = minuteCounts.count(_ > 0)
    def functionKey: String   = s"$hashOwner/$hashApp/$hashFunction"

  /** Duration statistics for a single function. */
  final case class FunctionDurationProfile(
      hashOwner: String,
      hashApp: String,
      hashFunction: String,
      average: Double,
      count: Int,
      minimum: Double,
      maximum: Double,
      percentile1: Double,
      percentile25: Double,
      percentile50: Double,
      percentile75: Double,
      percentile99: Double,
      percentile100: Double
  )

  /** Read invocation profiles from an Azure Functions invocation CSV. Format:
    * HashOwner,HashApp,HashFunction,Trigger,1,2,...,1440
    */
  def readInvocationProfiles(
      path: String,
      limit: Int = Int.MaxValue
  ): Either[TraceReadError, Vector[FunctionInvocationProfile]] =
    Using(Source.fromFile(path)) { source =>
      source
        .getLines()
        .drop(1) // skip header
        .take(limit)
        .flatMap(parseInvocationLine)
        .toVector
    }.toEither.left.map(e => TraceReadError.FileNotFound(path, e.getMessage))

  private def parseInvocationLine(line: String): Option[FunctionInvocationProfile] =
    val fields = line.split(",", -1)
    if fields.length < 1444 then None
    else
      val counts = (4 until 1444).map { i =>
        parseInt(fields(i).trim).getOrElse(0)
      }.toVector
      Some(
        FunctionInvocationProfile(
          hashOwner = fields(0).trim,
          hashApp = fields(1).trim,
          hashFunction = fields(2).trim,
          trigger = TriggerType.fromString(fields(3)),
          minuteCounts = counts
        )
      )

  /** Read duration profiles from an Azure Functions duration CSV. Format:
    * HashOwner,HashApp,HashFunction,Average,Count,Minimum,Maximum, percentile_Average_1,...,percentile_Average_100
    */
  def readDurationProfiles(
      path: String,
      limit: Int = Int.MaxValue
  ): Either[TraceReadError, Vector[FunctionDurationProfile]] =
    Using(Source.fromFile(path)) { source =>
      source
        .getLines()
        .drop(1)
        .take(limit)
        .flatMap(parseDurationLine)
        .toVector
    }.toEither.left.map(e => TraceReadError.FileNotFound(path, e.getMessage))

  private def parseDurationLine(line: String): Option[FunctionDurationProfile] =
    val fields = line.split(",", -1)
    if fields.length < 13 then None
    else
      for
        avg   <- parseDouble(fields(3).trim)
        count <- parseInt(fields(4).trim)
        min   <- parseDouble(fields(5).trim)
        max   <- parseDouble(fields(6).trim)
      yield FunctionDurationProfile(
        hashOwner = fields(0).trim,
        hashApp = fields(1).trim,
        hashFunction = fields(2).trim,
        average = avg,
        count = count,
        minimum = min,
        maximum = max,
        percentile1 = parseDouble(fields(7).trim).getOrElse(avg),
        percentile25 = parseDouble(fields(8).trim).getOrElse(avg),
        percentile50 = parseDouble(fields(9).trim).getOrElse(avg),
        percentile75 = parseDouble(fields(10).trim).getOrElse(avg),
        percentile99 = parseDouble(fields(11).trim).getOrElse(avg),
        percentile100 = parseDouble(fields(12).trim).getOrElse(avg)
      )

  /** Convert per-minute invocation counts to sorted arrival times.
    *
    * For each minute with N invocations, generates N uniformly-distributed random arrival times within that 60-second
    * window.
    */
  def toArrivalTimes(profile: FunctionInvocationProfile, seed: Long = 42L): Vector[SimTime] =
    val rng = new scala.util.Random(seed)
    profile.minuteCounts.zipWithIndex
      .flatMap { case (count, minute) =>
        if count <= 0 then Vector.empty
        else
          val windowStart = minute * 60.0
          (0 until count).map { _ =>
            SimTime(windowStart + rng.nextDouble() * 60.0)
          }
      }
      .sortBy(_.value)

// ─── Safe parsing helpers ──────────────────────────────────────────────

private def parseDouble(s: String): Option[Double] =
  if s.isEmpty then None
  else scala.util.Try(s.toDouble).toOption

private def parseLong(s: String): Option[Long] =
  if s.isEmpty then None
  else scala.util.Try(s.toLong).toOption

private def parseInt(s: String): Option[Int] =
  if s.isEmpty then None
  else scala.util.Try(s.toInt).toOption
