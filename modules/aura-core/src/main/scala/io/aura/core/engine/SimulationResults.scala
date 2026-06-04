// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.engine

import io.aura.core.types.*

/** Immutable simulation results returned after a simulation completes. */
final case class SimulationResults(
    simulationEndTime: SimTime,
    workloadResults: Vector[WorkloadResult],
    failedWorkloads: Vector[FailedWorkload],
    vmPlacements: Vector[VmPlacement],
    hostUtilizations: Vector[HostUtilizationRecord],
    energyRecords: Vector[EnergyRecord],
    migrationRecords: Vector[MigrationRecord],
    costRecords: Vector[VmCostRecord],
    faultRecords: Vector[FaultRecord],
    vmDestroyedRecords: Vector[VmDestroyedRecord],
    invocationResults: Vector[InvocationResult],
    throttledInvocations: Vector[ThrottledInvocation],
    timedOutInvocations: Vector[TimedOutInvocation],
    podResults: Vector[PodResult],
    podSchedulingRecords: Vector[PodSchedulingRecord],
    unschedulablePods: Vector[UnschedulablePod],
    edgeTaskResults: Vector[EdgeTaskResult],
    failedEdgeTasks: Vector[FailedEdgeTask],
    federatedTaskResults: Vector[FederatedTaskResult],
    failedFederatedTasks: Vector[FailedFederatedTask],
    slaViolationRecords: Vector[SlaViolationRecord],
    consolidationRecords: Vector[ConsolidationRecord],
    batchJobRecords: Vector[BatchJobRecord],
    inferenceResults: Vector[InferenceResult],
    failedInferenceRequests: Vector[FailedInferenceRequest],
    gpuEnergyRecords: Vector[GpuEnergyRecord],
    dvfsRecords: Vector[DvfsRecord],
    totalEventsProcessed: Long
):

  /** Average workload completion time. */
  def avgCompletionTime: SimTime =
    if workloadResults.isEmpty then SimTime.Zero
    else
      val total = workloadResults.map(_.finishTime.value).sum
      SimTime(total / workloadResults.size)

  /** Total energy consumed across all hosts in watt-hours. */
  def totalEnergyWh: WattHours =
    energyRecords.foldLeft(WattHours.Zero)((acc, r) => acc + r.energyWh)

  /** One-line summary of simulation results. */
  def formatSummary: String =
    val avgTime    = if workloadResults.isEmpty then 0.0 else avgCompletionTime.value
    val energy     = totalEnergyWh.value
    val migrations = migrationRecords.size
    val failed     = failedWorkloads.size
    val failedStr  = if failed > 0 then s" | Failed: $failed" else ""
    f"Completed: ${workloadResults.size} workloads | Avg time: ${avgTime}%.1fs | Energy: ${energy}%.2f Wh | Migrations: $migrations" + failedStr

  /** Format results as a readable table. */
  def formatWorkloadTable: String =
    val header    = f"${"Workload"}%-12s ${"VM"}%-8s ${"Host"}%-8s ${"Finish Time"}%-14s ${"MI Executed"}%-12s"
    val separator = "-" * header.length
    val rows = workloadResults.sortBy(_.workloadId).map { r =>
      f"${r.workloadId.value}%-12d ${r.vmId.value}%-8d ${r.hostId.value}%-8d ${r.finishTime.value}%-14.2f ${r.executedMI.value}%-12.0f"
    }
    (header +: separator +: rows).mkString("\n")

  /** Per-host energy consumption table. */
  def formatEnergyReport: String =
    if energyRecords.isEmpty then "No energy data recorded."
    else
      val byHost    = energyRecords.groupBy(_.hostId)
      val header    = f"${"Host"}%-8s ${"Total Energy (Wh)"}%-20s ${"Avg Power (W)"}%-16s ${"Samples"}%-8s"
      val separator = "-" * header.length
      val rows = byHost.toVector.sortBy(_._1).map { case (hostId, records) =>
        val totalEnergy = records.map(_.energyWh.value).sum
        val avgPower    = if records.nonEmpty then records.map(_.watts.value).sum / records.size else 0.0
        f"${hostId.value}%-8d ${totalEnergy}%-20.4f ${avgPower}%-16.2f ${records.size}%-8d"
      }
      (header +: separator +: rows).mkString("\n")

  /** Migration event log table. */
  def formatMigrationLog: String =
    if migrationRecords.isEmpty then "No migrations recorded."
    else
      val header    = f"${"VM"}%-8s ${"Source"}%-8s ${"Target"}%-8s ${"Start Time"}%-14s ${"Duration"}%-12s"
      val separator = "-" * header.length
      val rows = migrationRecords.sortBy(_.startTime).map { r =>
        f"${r.vmId.value}%-8d ${r.sourceHostId.value}%-8d ${r.targetHostId.value}%-8d ${r.startTime.value}%-14.2f ${r.duration.value}%-12.4f"
      }
      (header +: separator +: rows).mkString("\n")

  /** Per-host utilization history table. */
  def formatHostUtilization: String =
    if hostUtilizations.isEmpty then "No utilization data recorded."
    else
      val header    = f"${"Host"}%-8s ${"Time"}%-14s ${"Utilization %"}%-14s"
      val separator = "-" * header.length
      val rows = hostUtilizations.sortBy(r => (r.hostId, r.time)).map { r =>
        f"${r.hostId.value}%-8d ${r.time.value}%-14.2f ${r.utilization.value * 100}%-14.1f"
      }
      (header +: separator +: rows).mkString("\n")

  // ─── Container Orchestration Results ────────────────────────────────

  /** One-line pod summary. */
  def formatPodSummary: String =
    if podResults.isEmpty && unschedulablePods.isEmpty then "No pod results recorded."
    else
      val running       = podResults.count(_.phase == "Running")
      val completed     = podResults.count(_.phase == "Succeeded")
      val failed        = podResults.count(_.phase == "Failed")
      val unschedulable = unschedulablePods.size
      val total         = podResults.size + unschedulable
      s"Pods: $total | Running: $running | Completed: $completed | Failed: $failed | Unschedulable: $unschedulable"

  /** Per-pod detail table. */
  def formatPodTable: String =
    if podResults.isEmpty then "No pod results recorded."
    else
      val header    = f"${"Pod ID"}%-10s ${"Host"}%-8s ${"Start Time"}%-14s ${"Finish Time"}%-14s ${"Phase"}%-12s"
      val separator = "-" * header.length
      val rows = podResults.sortBy(_.podId).map { r =>
        val finish = r.finishTime.map(t => f"${t.value}%-14.2f").getOrElse("--            ")
        f"${r.podId.value}%-10d ${r.hostId.value}%-8d ${r.startTime.value}%-14.2f $finish ${r.phase}%-12s"
      }
      (header +: separator +: rows).mkString("\n")

  /** Per-node scheduling report. */
  def formatSchedulingReport: String =
    if podSchedulingRecords.isEmpty then "No scheduling data recorded."
    else
      val byNode    = podSchedulingRecords.groupBy(_.nodeName)
      val header    = f"${"Node"}%-20s ${"Pods Scheduled"}%-16s ${"Host ID"}%-10s"
      val separator = "-" * header.length
      val rows = byNode.toVector.sortBy(_._1).map { case (nodeName, records) =>
        val hostId = records.head.hostId.value
        f"$nodeName%-20s ${records.size}%-16d ${hostId}%-10d"
      }
      (header +: separator +: rows).mkString("\n")

  // ─── Serverless Results ──────────────────────────────────────────────

  /** One-line serverless invocation summary. */
  def formatInvocationSummary: String =
    if invocationResults.isEmpty && throttledInvocations.isEmpty && timedOutInvocations.isEmpty then
      "No serverless invocations recorded."
    else
      val functionCount    = invocationResults.map(_.functionId).distinct.size
      val totalInvocations = invocationResults.size + throttledInvocations.size + timedOutInvocations.size
      val coldStarts       = invocationResults.count(_.coldStart)
      val coldPct   = if invocationResults.nonEmpty then coldStarts.toDouble / invocationResults.size * 100 else 0.0
      val totalCost = invocationResults.foldLeft(GBSeconds.Zero)((acc, r) => acc + r.billedGBSeconds)
      val timeouts  = timedOutInvocations.size
      val throttled = throttledInvocations.size
      f"Functions: $functionCount | Invocations: $totalInvocations | Cold starts: $coldStarts ($coldPct%.1f%%) | Timeouts: $timeouts | Throttled: $throttled | Cost: ${totalCost.value}%.4f GB-s"

  /** Per-invocation detail table. */
  def formatInvocationTable: String =
    if invocationResults.isEmpty then "No invocation results recorded."
    else
      val header =
        f"${"Invocation"}%-12s ${"Function"}%-10s ${"Start"}%-10s ${"Finish"}%-10s ${"GB-s"}%-10s ${"Cold?"}%-6s"
      val separator = "-" * header.length
      val rows = invocationResults.sortBy(_.invocationId).map { r =>
        f"${r.invocationId.value}%-12d ${r.functionId.value}%-10d ${r.startTime.value}%-10.3f ${r.finishTime.value}%-10.3f ${r.billedGBSeconds.value}%-10.4f ${r.coldStart}%-6s"
      }
      (header +: separator +: rows).mkString("\n")

  /** Per-function cold start statistics. */
  def formatColdStartReport: String =
    if invocationResults.isEmpty then "No invocation results recorded."
    else
      val byFunction = invocationResults.groupBy(_.functionId)
      val header     = f"${"Function"}%-10s ${"Cold Starts"}%-14s ${"Total"}%-10s ${"Cold Start %"}%-14s"
      val separator  = "-" * header.length
      val rows = byFunction.toVector.sortBy(_._1).map { case (funcId, results) =>
        val coldCount = results.count(_.coldStart)
        val total     = results.size
        val pct       = if total > 0 then coldCount.toDouble / total * 100 else 0.0
        f"${funcId.value}%-10d ${coldCount}%-14d ${total}%-10d ${pct}%-14.1f"
      }
      (header +: separator +: rows).mkString("\n")

  /** Per-function billing breakdown. */
  def formatBillingReport: String =
    if invocationResults.isEmpty then "No invocation results recorded."
    else
      val byFunction = invocationResults.groupBy(_.functionId)
      val header     = f"${"Function"}%-10s ${"Invocations"}%-14s ${"GB-Seconds"}%-14s ${"Avg GB-s"}%-14s"
      val separator  = "-" * header.length
      val rows = byFunction.toVector.sortBy(_._1).map { case (funcId, results) =>
        val totalGBs = results.foldLeft(GBSeconds.Zero)((acc, r) => acc + r.billedGBSeconds)
        val avgGBs   = if results.nonEmpty then totalGBs.value / results.size else 0.0
        f"${funcId.value}%-10d ${results.size}%-14d ${totalGBs.value}%-14.4f ${avgGBs}%-14.4f"
      }
      (header +: separator +: rows).mkString("\n")

  /** Per-function concurrency statistics (reconstructed from start/finish times). */
  def formatConcurrencyReport: String =
    if invocationResults.isEmpty then "No invocation results recorded."
    else
      val byFunction = invocationResults.groupBy(_.functionId)
      val header     = f"${"Function"}%-10s ${"Peak Concurrency"}%-18s ${"Avg Concurrency"}%-18s"
      val separator  = "-" * header.length
      val rows = byFunction.toVector.sortBy(_._1).map { case (funcId, results) =>
        // Reconstruct concurrency timeline using +1 at start, -1 at finish
        val events = results
          .flatMap { r =>
            Vector((r.startTime.value, 1), (r.finishTime.value, -1))
          }
          .sortBy(_._1)
        val (_, peak, totalConcTime, _) = events.foldLeft((0, 0, 0.0, events.headOption.map(_._1).getOrElse(0.0))) {
          case ((current, peak, totalConcTime, lastTime), (time, delta)) =>
            val newConcTime = totalConcTime + current * (time - lastTime)
            val newCurrent  = current + delta
            val newPeak     = math.max(peak, newCurrent)
            (newCurrent, newPeak, newConcTime, time)
        }
        val duration = if events.nonEmpty then events.last._1 - events.head._1 else 1.0
        val avgConc  = if duration > 0 then totalConcTime / duration else 0.0
        f"${funcId.value}%-10d ${peak}%-18d ${avgConc}%-18.2f"
      }
      (header +: separator +: rows).mkString("\n")

  // ─── Edge Computing Results ─────────────────────────────────────────

  /** One-line edge computing summary. */
  def formatEdgeSummary: String =
    if edgeTaskResults.isEmpty && failedEdgeTasks.isEmpty then "No edge tasks recorded."
    else
      val total     = edgeTaskResults.size + failedEdgeTasks.size
      val completed = edgeTaskResults.size
      val failed    = failedEdgeTasks.size
      val offloaded = edgeTaskResults.count(_.offloaded)
      s"Tasks: $total | Completed: $completed | Failed: $failed | Offloaded: $offloaded"

  /** Per-task edge detail table. */
  def formatEdgeTable: String =
    if edgeTaskResults.isEmpty then "No edge task results recorded."
    else
      val header =
        f"${"Task ID"}%-10s ${"Source"}%-16s ${"Exec Node"}%-16s ${"Latency"}%-12s ${"Start"}%-10s ${"Finish"}%-10s ${"Offloaded"}%-10s"
      val separator = "-" * header.length
      val rows = edgeTaskResults.sortBy(_.taskId).map { r =>
        f"${r.taskId.value}%-10d ${r.sourceNodeName}%-16s ${r.executionNodeName}%-16s ${r.networkLatency.value}%-12.4f ${r.startTime.value}%-10.3f ${r.finishTime.value}%-10.3f ${r.offloaded}%-10s"
      }
      (header +: separator +: rows).mkString("\n")

  /** Per-node offloading report. */
  def formatOffloadingReport: String =
    if edgeTaskResults.isEmpty then "No edge task results recorded."
    else
      val allNodes = (edgeTaskResults.map(_.sourceNodeName) ++ edgeTaskResults.map(_.executionNodeName)).distinct.sorted
      val header   = f"${"Node"}%-20s ${"Tasks Sourced"}%-16s ${"Tasks Executed"}%-16s ${"Tasks Offloaded"}%-16s"
      val separator = "-" * header.length
      val rows = allNodes.map { nodeName =>
        val sourced   = edgeTaskResults.count(_.sourceNodeName == nodeName)
        val executed  = edgeTaskResults.count(_.executionNodeName == nodeName)
        val offloaded = edgeTaskResults.count(r => r.sourceNodeName == nodeName && r.offloaded)
        f"$nodeName%-20s ${sourced}%-16d ${executed}%-16d ${offloaded}%-16d"
      }
      (header +: separator +: rows).mkString("\n")

  // ─── Cost Results ──────────────────────────────────────────────────

  /** Total cost across all VMs. */
  def totalCost: Cost =
    costRecords.foldLeft(Cost.Zero)((acc, r) => acc + r.totalCost)

  /** Total data transferred during all migrations. */
  def totalMigrationDataTransferred: MegaBytes =
    migrationRecords.foldLeft(MegaBytes.Zero)((acc, r) => acc + r.dataTransferred)

  /** Per-VM cost breakdown table. */
  def formatCostReport: String =
    if costRecords.isEmpty then "No cost data recorded."
    else
      val header =
        f"${"VM"}%-8s ${"CPU Cost"}%-12s ${"RAM Cost"}%-12s ${"BW Cost"}%-12s ${"Storage"}%-12s ${"Total"}%-12s"
      val separator = "-" * header.length
      val rows = costRecords.sortBy(_.vmId).map { r =>
        f"${r.vmId.value}%-8d ${r.cpuCost.value}%-12.4f ${r.ramCost.value}%-12.4f ${r.bwCost.value}%-12.4f ${r.storageCost.value}%-12.4f ${r.totalCost.value}%-12.4f"
      }
      val totalLine = f"${"TOTAL"}%-8s ${""}%-12s ${""}%-12s ${""}%-12s ${""}%-12s ${totalCost.value}%-12.4f"
      (header +: separator +: rows :+ separator :+ totalLine).mkString("\n")

  /** Unified cost report aggregating VM costs, migration bandwidth costs, and FaaS costs. */
  def formatUnifiedCostReport(perTransferMB: Cost = Cost.Zero, faasRatePerGBs: Cost = Cost.Zero): String =
    val vmCost        = totalCost
    val migrationCost = Cost(totalMigrationDataTransferred.value * perTransferMB.value)
    val faasCost = Cost(
      invocationResults.foldLeft(0.0)((acc, r) => acc + r.billedGBSeconds.value) * faasRatePerGBs.value
    )
    val grandTotal = vmCost + migrationCost + faasCost
    val lines = Vector(
      f"VM Resource Costs:      ${vmCost.value}%.6f",
      f"Migration BW Costs:     ${migrationCost.value}%.6f",
      f"FaaS Invocation Costs:  ${faasCost.value}%.6f",
      f"Grand Total:            ${grandTotal.value}%.6f"
    )
    lines.mkString("\n")

  // ─── Fault Results ────────────────────────────────────────────────

  /** Fault injection report. */
  def formatFaultReport: String =
    if faultRecords.isEmpty then "No fault data recorded."
    else
      val header    = f"${"Host"}%-8s ${"Failed PEs"}%-12s ${"Time"}%-14s"
      val separator = "-" * header.length
      val rows = faultRecords.sortBy(_.time).map { r =>
        f"${r.hostId.value}%-8d ${r.failedPEs.value}%-12d ${r.time.value}%-14.2f"
      }
      val summary = s"\nTotal faults: ${faultRecords.size} | VMs destroyed: ${vmDestroyedRecords.size}"
      (header +: separator +: rows :+ summary).mkString("\n")

  // ─── Batch Scheduling Results ─────────────────────────────────────

  /** One-line batch scheduling summary. */
  def formatBatchSummary: String =
    if batchJobRecords.isEmpty then "No batch jobs recorded."
    else
      val avgTurnaround = batchJobRecords.map(_.turnaroundTime.value).sum / batchJobRecords.size
      f"Batch Jobs: ${batchJobRecords.size} completed | Avg turnaround: ${avgTurnaround}%.2fs"

  /** Per-job batch detail table. */
  def formatBatchTable: String =
    if batchJobRecords.isEmpty then "No batch job results recorded."
    else
      val header    = f"${"Job ID"}%-10s ${"Start"}%-14s ${"Finish"}%-14s ${"Turnaround"}%-14s"
      val separator = "-" * header.length
      val rows = batchJobRecords.sortBy(_.jobId).map { r =>
        f"${r.jobId.value}%-10d ${r.startTime.value}%-14.2f ${r.finishTime.value}%-14.2f ${r.turnaroundTime.value}%-14.2f"
      }
      (header +: separator +: rows).mkString("\n")

  // ─── Real-time SLA Violations ────────────────────────────────────

  /** Format real-time SLA violation report. */
  def formatSlaViolationReport: String =
    if slaViolationRecords.isEmpty then "No real-time SLA violations recorded."
    else
      val header    = f"${"Host"}%-8s ${"Utilization %"}%-16s ${"Time"}%-14s ${"Reason"}%-30s"
      val separator = "-" * header.length
      val rows = slaViolationRecords.sortBy(_.time).map { r =>
        f"${r.hostId.value}%-8d ${r.utilization.value * 100}%-16.1f ${r.time.value}%-14.2f ${r.reason}%-30s"
      }
      val summary = s"\nTotal violations: ${slaViolationRecords.size}"
      (header +: separator +: rows :+ summary).mkString("\n")

  /** SLA violation rate: fraction of utilization samples that violated SLA. */
  def slaViolationRate: Double =
    if hostUtilizations.isEmpty then 0.0
    else slaViolationRecords.size.toDouble / hostUtilizations.size

  // ─── SLA Evaluation ───────────────────────────────────────────────

  /** Evaluate SLA contract against these results. */
  def evaluateSla(contract: SlaContract): Vector[SlaViolation] =
    SlaEvaluator.evaluate(this, contract)

  /** Format SLA evaluation results. */
  def formatSlaReport(contract: SlaContract): String =
    val violations = evaluateSla(contract)
    if violations.isEmpty then "SLA: All constraints satisfied."
    else
      val header    = f"${"Metric"}%-24s ${"Constraint"}%-14s ${"Actual"}%-14s ${"Entity"}%-20s ${"Time"}%-14s"
      val separator = "-" * header.length
      val rows = violations.map { v =>
        f"${v.metric}%-24s ${v.constraintValue}%-14.4f ${v.actualValue}%-14.4f ${v.entityId}%-20s ${v.time.value}%-14.2f"
      }
      val summary = s"\nSLA: ${violations.size} violations detected."
      (header +: separator +: rows :+ summary).mkString("\n")

  // ─── LLM Inference Results ──────────────────────────────────────────

  /** One-line inference summary. */
  def formatInferenceSummary: String =
    if inferenceResults.isEmpty && failedInferenceRequests.isEmpty then "No inference requests recorded."
    else
      val total       = inferenceResults.size + failedInferenceRequests.size
      val completed   = inferenceResults.size
      val failed      = failedInferenceRequests.size
      val sloMet      = inferenceResults.count(_.sloMet)
      val sloPct      = if completed > 0 then sloMet.toDouble / completed * 100 else 0.0
      val avgTtft     = if completed > 0 then inferenceResults.map(_.ttft.value).sum / completed else 0.0
      val avgTpot     = if completed > 0 then inferenceResults.map(_.tpot.value).sum / completed else 0.0
      val totalEnergy = inferenceResults.foldLeft(WattHours.Zero)((acc, r) => acc + r.totalEnergyWh)
      f"Requests: $total | Completed: $completed | Failed: $failed | SLO met: $sloMet ($sloPct%.1f%%) | Avg TTFT: ${avgTtft * 1000}%.1f ms | Avg TPOT: ${avgTpot * 1000}%.1f ms | Energy: ${totalEnergy.value}%.4f Wh"

  /** Per-request inference detail table. */
  def formatInferenceTable: String =
    if inferenceResults.isEmpty then "No inference results recorded."
    else
      val header =
        f"${"Request"}%-10s ${"Model"}%-8s ${"Prompt"}%-8s ${"Output"}%-8s ${"TTFT(ms)"}%-10s ${"TPOT(ms)"}%-10s ${"Total(s)"}%-10s ${"Energy(Wh)"}%-12s ${"SLO"}%-5s"
      val separator = "-" * header.length
      val rows = inferenceResults.sortBy(_.requestId).map { r =>
        f"${r.requestId.value}%-10d ${r.modelId.value}%-8d ${r.promptTokens}%-8d ${r.outputTokens}%-8d ${r.ttft.value * 1000}%-10.1f ${r.tpot.value * 1000}%-10.1f ${r.totalTime.value}%-10.3f ${r.totalEnergyWh.value}%-12.6f ${r.sloMet}%-5s"
      }
      (header +: separator +: rows).mkString("\n")

  /** GPU energy breakdown report. */
  def formatGpuEnergyReport: String =
    if gpuEnergyRecords.isEmpty then "No GPU energy data recorded."
    else
      val byDevice = gpuEnergyRecords.groupBy(r => (r.nodeId, r.deviceId))
      val header =
        f"${"Node"}%-8s ${"Device"}%-8s ${"Total(Wh)"}%-12s ${"Avg Power(W)"}%-14s ${"Avg Compute(W)"}%-16s ${"Avg Memory(W)"}%-14s ${"Samples"}%-8s"
      val separator = "-" * header.length
      val rows = byDevice.toVector.sortBy(_._1).map { case ((nodeId, devId), records) =>
        val totalE = records.map(_.energyWh.value).sum
        val avgP   = records.map(_.watts.value).sum / records.size
        val avgC   = records.map(_.computeWatts.value).sum / records.size
        val avgM   = records.map(_.memoryWatts.value).sum / records.size
        f"${nodeId.value}%-8d ${devId.value}%-8d ${totalE}%-12.4f ${avgP}%-14.2f ${avgC}%-16.2f ${avgM}%-14.2f ${records.size}%-8d"
      }
      (header +: separator +: rows).mkString("\n")

  /** Total GPU energy across all devices. */
  def totalGpuEnergyWh: WattHours =
    gpuEnergyRecords.foldLeft(WattHours.Zero)((acc, r) => acc + r.energyWh)

  /** Total inference energy across all requests. */
  def totalInferenceEnergyWh: WattHours =
    inferenceResults.foldLeft(WattHours.Zero)((acc, r) => acc + r.totalEnergyWh)

  /** DVFS transition log. */
  def formatDvfsReport: String =
    if dvfsRecords.isEmpty then "No DVFS transitions recorded."
    else
      val header = f"${"Node"}%-8s ${"Device"}%-8s ${"Time"}%-12s ${"Old MHz"}%-10s ${"New MHz"}%-10s ${"Reason"}%-20s"
      val separator = "-" * header.length
      val rows = dvfsRecords.sortBy(_.time).map { r =>
        f"${r.nodeId.value}%-8d ${r.deviceId.value}%-8d ${r.time.value}%-12.3f ${r.oldFreqMHz}%-10d ${r.newFreqMHz}%-10d ${r.reason}%-20s"
      }
      (header +: separator +: rows).mkString("\n")

  // ─── Multi-format Export ──────────────────────────────────────────

  /** Export workload results in the specified format. */
  def exportAs(format: OutputFormat): String =
    val columns = Vector(
      Column[WorkloadResult]("Workload", format = r => r.workloadId.value.toString),
      Column[WorkloadResult]("VM", format = r => r.vmId.value.toString),
      Column[WorkloadResult]("Host", format = r => r.hostId.value.toString),
      Column[WorkloadResult]("Finish Time", align = Align.Right, format = r => f"${r.finishTime.value}%.2f"),
      Column[WorkloadResult]("MI Executed", align = Align.Right, format = r => f"${r.executedMI.value}%.0f")
    )
    TableFormatter.format("Workload Results", workloadResults.sortBy(_.workloadId), columns, format)

  // ─── Federated Orchestration Results ───────────────────────────────

  /** One-line federated orchestration summary. */
  def formatFederatedSummary: String =
    if federatedTaskResults.isEmpty && failedFederatedTasks.isEmpty then "No federated tasks recorded."
    else
      val total     = federatedTaskResults.size + failedFederatedTasks.size
      val succeeded = federatedTaskResults.size
      val failed    = failedFederatedTasks.size
      val escalated = federatedTaskResults.count(_.escalations > 0)
      s"Tasks: $total | Succeeded: $succeeded | Failed: $failed | Escalated: $escalated"

  /** Per-task federated detail table. */
  def formatFederatedTable: String =
    if federatedTaskResults.isEmpty then "No federated task results recorded."
    else
      val header =
        f"${"Task ID"}%-10s ${"Initial Tier"}%-16s ${"Final Tier"}%-16s ${"Escalations"}%-14s ${"Start"}%-10s ${"Finish"}%-10s"
      val separator = "-" * header.length
      val rows = federatedTaskResults.sortBy(_.taskId).map { r =>
        f"${r.taskId.value}%-10d ${r.initialTier}%-16s ${r.finalTier}%-16s ${r.escalations}%-14d ${r.startTime.value}%-10.3f ${r.finishTime.value}%-10.3f"
      }
      (header +: separator +: rows).mkString("\n")

  /** Per-tier distribution breakdown. */
  def formatTierDistribution: String =
    if federatedTaskResults.isEmpty && failedFederatedTasks.isEmpty then "No federated tasks recorded."
    else
      import io.aura.core.types.ExecutionTier
      val tiers     = Vector(ExecutionTier.Edge, ExecutionTier.Serverless, ExecutionTier.K8s)
      val header    = f"${"Tier"}%-16s ${"Attempted"}%-12s ${"Succeeded"}%-12s ${"Failed"}%-12s ${"Avg Latency"}%-14s"
      val separator = "-" * header.length
      val rows = tiers.map { tier =>
        val succeeded = federatedTaskResults.count(_.finalTier == tier)
        val attempted = federatedTaskResults.count(_.initialTier == tier) +
          federatedTaskResults.count(r => r.finalTier == tier && r.initialTier != tier)
        val tierResults = federatedTaskResults.filter(_.finalTier == tier)
        val avgLatency =
          if tierResults.nonEmpty then
            tierResults.map(r => r.finishTime.value - r.startTime.value).sum / tierResults.size
          else 0.0
        val tierFailed = failedFederatedTasks.count(_.tiersAttempted > 0)
        f"${tier}%-16s ${attempted}%-12d ${succeeded}%-12d ${tierFailed}%-12d ${avgLatency}%-14.3f"
      }
      (header +: separator +: rows).mkString("\n")

  /** Export energy results in the specified format. */
  def exportEnergyAs(format: OutputFormat): String =
    val byHost = energyRecords.groupBy(_.hostId)
    val summaries = byHost.toVector.sortBy(_._1).map { case (hostId, records) =>
      val totalE = records.map(_.energyWh.value).sum
      val avgP   = if records.nonEmpty then records.map(_.watts.value).sum / records.size else 0.0
      (hostId, totalE, avgP, records.size)
    }
    val columns = Vector(
      Column[(HostId, Double, Double, Int)]("Host", format = r => r._1.value.toString),
      Column[(HostId, Double, Double, Int)]("Total Energy (Wh)", align = Align.Right, format = r => f"${r._2}%.4f"),
      Column[(HostId, Double, Double, Int)]("Avg Power (W)", align = Align.Right, format = r => f"${r._3}%.2f"),
      Column[(HostId, Double, Double, Int)]("Samples", align = Align.Right, format = r => r._4.toString)
    )
    TableFormatter.format("Energy Consumption", summaries, columns, format)

  /** Export migration results in the specified format. */
  def exportMigrationsAs(format: OutputFormat): String =
    val columns = Vector(
      Column[MigrationRecord]("VM", format = r => r.vmId.value.toString),
      Column[MigrationRecord]("Source", format = r => r.sourceHostId.value.toString),
      Column[MigrationRecord]("Target", format = r => r.targetHostId.value.toString),
      Column[MigrationRecord]("Start Time", align = Align.Right, format = r => f"${r.startTime.value}%.2f"),
      Column[MigrationRecord]("Duration", align = Align.Right, format = r => f"${r.duration.value}%.4f"),
      Column[MigrationRecord]("Data (MB)", align = Align.Right, format = r => f"${r.dataTransferred.value}%.1f")
    )
    TableFormatter.format("VM Migrations", migrationRecords.sortBy(_.startTime), columns, format)

  /** Export cost results in the specified format. */
  def exportCostAs(format: OutputFormat): String =
    val columns = Vector(
      Column[VmCostRecord]("VM", format = r => r.vmId.value.toString),
      Column[VmCostRecord]("CPU", align = Align.Right, format = r => f"${r.cpuCost.value}%.4f"),
      Column[VmCostRecord]("RAM", align = Align.Right, format = r => f"${r.ramCost.value}%.4f"),
      Column[VmCostRecord]("BW", align = Align.Right, format = r => f"${r.bwCost.value}%.4f"),
      Column[VmCostRecord]("Storage", align = Align.Right, format = r => f"${r.storageCost.value}%.4f"),
      Column[VmCostRecord]("Total", align = Align.Right, format = r => f"${r.totalCost.value}%.4f")
    )
    TableFormatter.format("Cost Breakdown", costRecords.sortBy(_.vmId), columns, format)

  /** Export a summary table in the specified format (useful for LaTeX papers). */
  def exportSummaryAs(format: OutputFormat): String =
    val data = Vector(
      ("Workloads Completed", workloadResults.size.toString),
      ("Workloads Failed", failedWorkloads.size.toString),
      ("Avg Completion Time (s)", f"${avgCompletionTime.value}%.2f"),
      ("Total Energy (Wh)", f"${totalEnergyWh.value}%.4f"),
      ("Total Cost ($)", f"${totalCost.value}%.4f"),
      ("Migrations", migrationRecords.size.toString),
      ("Faults", faultRecords.size.toString),
      ("Events Processed", totalEventsProcessed.toString)
    )
    val columns = Vector(
      Column[(String, String)]("Metric", format = _._1),
      Column[(String, String)]("Value", align = Align.Right, format = _._2)
    )
    TableFormatter.format("Simulation Summary", data, columns, format)

  /** Export results as CSV string. */
  def toCSV: String =
    val header = "workload_id,vm_id,host_id,finish_time,mi_executed"
    val rows = workloadResults.sortBy(_.workloadId).map { r =>
      s"${r.workloadId.value},${r.vmId.value},${r.hostId.value},${r.finishTime.value},${r.executedMI.value}"
    }
    (header +: rows).mkString("\n")

  /** Full JSON export of all results. */
  def toJSON: String =
    val wl = workloadResults
      .sortBy(_.workloadId)
      .map { r =>
        s"""    {"workloadId":${r.workloadId.value},"vmId":${r.vmId.value},"hostId":${r.hostId.value},"finishTime":${r.finishTime.value},"executedMI":${r.executedMI.value}}"""
      }
      .mkString(",\n")

    val vp = vmPlacements
      .map { p =>
        s"""    {"vmId":${p.vmId.value},"hostId":${p.hostId.value},"datacenterId":${p.datacenterId.value},"time":${p.time.value}}"""
      }
      .mkString(",\n")

    val er = energyRecords
      .map { e =>
        s"""    {"hostId":${e.hostId.value},"watts":${e.watts.value},"fromTime":${e.fromTime.value},"toTime":${e.toTime.value},"energyWh":${e.energyWh.value}}"""
      }
      .mkString(",\n")

    val mr = migrationRecords
      .map { m =>
        s"""    {"vmId":${m.vmId.value},"sourceHostId":${m.sourceHostId.value},"targetHostId":${m.targetHostId.value},"startTime":${m.startTime.value},"duration":${m.duration.value},"dataTransferred":${m.dataTransferred.value}}"""
      }
      .mkString(",\n")

    val fw = failedWorkloads
      .map { f =>
        s"""    {"workloadId":${f.workloadId.value},"reason":"${f.reason}","time":${f.time.value}}"""
      }
      .mkString(",\n")

    val ir = invocationResults
      .sortBy(_.invocationId)
      .map { r =>
        s"""    {"invocationId":${r.invocationId.value},"functionId":${r.functionId.value},"startTime":${r.startTime.value},"finishTime":${r.finishTime.value},"billedGBSeconds":${r.billedGBSeconds.value},"coldStart":${r.coldStart}}"""
      }
      .mkString(",\n")

    val ti = throttledInvocations
      .map { t =>
        s"""    {"invocationId":${t.invocationId.value},"functionId":${t.functionId.value},"reason":"${t.reason}","time":${t.time.value}}"""
      }
      .mkString(",\n")

    val to = timedOutInvocations
      .map { t =>
        s"""    {"invocationId":${t.invocationId.value},"functionId":${t.functionId.value},"reason":"${t.reason}","time":${t.time.value}}"""
      }
      .mkString(",\n")

    val pr = podResults
      .sortBy(_.podId)
      .map { r =>
        val ft = r.finishTime.map(_.value.toString).getOrElse("null")
        s"""    {"podId":${r.podId.value},"hostId":${r.hostId.value},"startTime":${r.startTime.value},"finishTime":$ft,"phase":"${r.phase}"}"""
      }
      .mkString(",\n")

    val psr = podSchedulingRecords
      .map { r =>
        s"""    {"podId":${r.podId.value},"hostId":${r.hostId.value},"nodeName":"${r.nodeName}","time":${r.time.value}}"""
      }
      .mkString(",\n")

    val etr = edgeTaskResults
      .sortBy(_.taskId)
      .map { r =>
        s"""    {"taskId":${r.taskId.value},"sourceNodeName":"${r.sourceNodeName}","executionNodeName":"${r.executionNodeName}","offloaded":${r.offloaded},"networkLatency":${r.networkLatency.value},"startTime":${r.startTime.value},"finishTime":${r.finishTime.value},"offloadingReason":"${r.offloadingReason}"}"""
      }
      .mkString(",\n")

    val fet = failedEdgeTasks
      .map { f =>
        s"""    {"taskId":${f.taskId.value},"reason":"${f.reason}","time":${f.time.value}}"""
      }
      .mkString(",\n")

    val ftr = federatedTaskResults
      .sortBy(_.taskId)
      .map { r =>
        s"""    {"taskId":${r.taskId.value},"initialTier":"${r.initialTier}","finalTier":"${r.finalTier}","escalations":${r.escalations},"startTime":${r.startTime.value},"finishTime":${r.finishTime.value}}"""
      }
      .mkString(",\n")

    val fft = failedFederatedTasks
      .map { f =>
        s"""    {"taskId":${f.taskId.value},"reason":"${f.reason}","tiersAttempted":${f.tiersAttempted},"time":${f.time.value}}"""
      }
      .mkString(",\n")

    val cr = costRecords
      .map { c =>
        s"""    {"vmId":${c.vmId.value},"cpuCost":${c.cpuCost.value},"ramCost":${c.ramCost.value},"bwCost":${c.bwCost.value},"storageCost":${c.storageCost.value},"totalCost":${c.totalCost.value},"time":${c.time.value}}"""
      }
      .mkString(",\n")

    val faultR = faultRecords
      .map { f =>
        s"""    {"hostId":${f.hostId.value},"failedPEs":${f.failedPEs.value},"time":${f.time.value}}"""
      }
      .mkString(",\n")

    val vmDest = vmDestroyedRecords
      .map { d =>
        s"""    {"vmId":${d.vmId.value},"hostId":${d.hostId.value},"time":${d.time.value},"reason":"${d.reason}"}"""
      }
      .mkString(",\n")

    s"""{
  "simulationEndTime": ${simulationEndTime.value},
  "totalEventsProcessed": $totalEventsProcessed,
  "totalEnergyWh": ${totalEnergyWh.value},
  "avgCompletionTime": ${avgCompletionTime.value},
  "totalCost": ${totalCost.value},
  "workloadResults": [
$wl
  ],
  "failedWorkloads": [
$fw
  ],
  "vmPlacements": [
$vp
  ],
  "energyRecords": [
$er
  ],
  "migrationRecords": [
$mr
  ],
  "invocationResults": [
$ir
  ],
  "throttledInvocations": [
$ti
  ],
  "timedOutInvocations": [
$to
  ],
  "podResults": [
$pr
  ],
  "podSchedulingRecords": [
$psr
  ],
  "edgeTaskResults": [
$etr
  ],
  "failedEdgeTasks": [
$fet
  ],
  "federatedTaskResults": [
$ftr
  ],
  "failedFederatedTasks": [
$fft
  ],
  "costRecords": [
$cr
  ],
  "faultRecords": [
$faultR
  ],
  "vmDestroyedRecords": [
$vmDest
  ]
}"""

object SimulationResults:
  def fromMetrics(metrics: MetricsCollector, endTime: SimTime): SimulationResults =
    val snap = metrics.snapshot
    SimulationResults(
      simulationEndTime = endTime,
      workloadResults = snap.workloadResults,
      failedWorkloads = snap.failedWorkloads,
      vmPlacements = snap.vmPlacements,
      hostUtilizations = snap.hostUtilizations,
      energyRecords = snap.energyRecords,
      migrationRecords = snap.migrationRecords,
      costRecords = snap.costRecords,
      faultRecords = snap.faultRecords,
      vmDestroyedRecords = snap.vmDestroyedRecords,
      invocationResults = snap.invocationResults,
      throttledInvocations = snap.throttledInvocations,
      timedOutInvocations = snap.timedOutInvocations,
      podResults = snap.podResults,
      podSchedulingRecords = snap.podSchedulingRecords,
      unschedulablePods = snap.unschedulablePods,
      edgeTaskResults = snap.edgeTaskResults,
      failedEdgeTasks = snap.failedEdgeTasks,
      federatedTaskResults = snap.federatedTaskResults,
      failedFederatedTasks = snap.failedFederatedTasks,
      slaViolationRecords = snap.slaViolationRecords,
      consolidationRecords = snap.consolidationRecords,
      batchJobRecords = snap.batchJobRecords,
      inferenceResults = snap.inferenceResults,
      failedInferenceRequests = snap.failedInferenceRequests,
      gpuEnergyRecords = snap.gpuEnergyRecords,
      dvfsRecords = snap.dvfsRecords,
      totalEventsProcessed = snap.eventCount
    )

  val empty: SimulationResults = SimulationResults(
    simulationEndTime = SimTime.Zero,
    workloadResults = Vector.empty,
    failedWorkloads = Vector.empty,
    vmPlacements = Vector.empty,
    hostUtilizations = Vector.empty,
    energyRecords = Vector.empty,
    migrationRecords = Vector.empty,
    costRecords = Vector.empty,
    faultRecords = Vector.empty,
    vmDestroyedRecords = Vector.empty,
    invocationResults = Vector.empty,
    throttledInvocations = Vector.empty,
    timedOutInvocations = Vector.empty,
    podResults = Vector.empty,
    podSchedulingRecords = Vector.empty,
    unschedulablePods = Vector.empty,
    edgeTaskResults = Vector.empty,
    failedEdgeTasks = Vector.empty,
    federatedTaskResults = Vector.empty,
    failedFederatedTasks = Vector.empty,
    slaViolationRecords = Vector.empty,
    consolidationRecords = Vector.empty,
    batchJobRecords = Vector.empty,
    inferenceResults = Vector.empty,
    failedInferenceRequests = Vector.empty,
    gpuEnergyRecords = Vector.empty,
    dvfsRecords = Vector.empty,
    totalEventsProcessed = 0L
  )
