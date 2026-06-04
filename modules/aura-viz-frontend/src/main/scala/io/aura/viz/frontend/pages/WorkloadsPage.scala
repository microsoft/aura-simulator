// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz.frontend.pages

import com.raquo.laminar.api.L.*
import io.aura.viz.frontend.model.{FailedWorkload, SimulationData, WorkloadResult}
import io.aura.viz.frontend.components.{ChartComponent, DataTable, KpiCard}
import io.aura.viz.frontend.facades.ChartHelpers
import scala.scalajs.js

/** Workloads page with bar/line chart (MI vs finish time) and data tables. */
object WorkloadsPage:

  def render(data: SimulationData): HtmlElement =
    val results = data.workloadResults
    val failed  = data.failedWorkloads
    val avgMI   = if results.nonEmpty then results.map(_.executedMI).sum / results.size else 0.0

    div(
      div(cls := "page-header", h2("Workloads"), p("Workload execution results and performance metrics")),
      div(
        cls := "kpi-row",
        KpiCard.renderStatic("Completed", results.size.toString),
        KpiCard.renderStatic("Failed", failed.size.toString, isError = failed.nonEmpty),
        KpiCard.renderStatic("Avg MI", f"$avgMI%.0f"),
        KpiCard.renderStatic("Avg Time", f"${data.avgCompletionTime}%.2f s")
      ),

      // Chart
      div(
        cls := "chart-row",
        div(cls := "chart-card full-width", h3("MI Executed & Finish Time per Workload"), renderWorkloadChart(results))
      ),

      // Completed workloads table
      div(cls := "section", h3("Completed Workloads"), renderResultsTable(results)),

      // Failed workloads table
      if failed.nonEmpty then div(cls := "section", h3("Failed Workloads"), renderFailedTable(failed))
      else emptyNode
    )

  private def renderWorkloadChart(results: Vector[WorkloadResult]): HtmlElement =
    if results.isEmpty then div(cls := "empty-state", "No workload data")
    else
      val labels    = js.Array(results.map(r => s"WL-${r.workloadId}")*)
      val miData    = js.Array(results.map(_.executedMI.asInstanceOf[js.Any])*)
      val timeData  = js.Array(results.map(_.finishTime.asInstanceOf[js.Any])*)
      val ds1       = ChartHelpers.barDataset("MI Executed", miData, "#e94560")
      val ds2       = ChartHelpers.lineDataset("Finish Time (s)", timeData, "#4ecdc4", yAxisID = "y1")
      val chartData = ChartHelpers.chartData(labels, js.Array(ds1, ds2))
      val options   = ChartHelpers.dualAxisOptions("Workload", "MI", "Time (s)")
      ChartComponent.renderStatic("bar", chartData, options)

  private def renderResultsTable(results: Vector[WorkloadResult]): HtmlElement =
    DataTable.renderStatic(results)(
      DataTable.Column("Workload", r => r.workloadId.toString, Some(_.workloadId.toDouble)),
      DataTable.Column("VM", r => r.vmId.toString, Some(_.vmId.toDouble)),
      DataTable.Column("Host", r => r.hostId.toString, Some(_.hostId.toDouble)),
      DataTable.Column("Finish Time", r => f"${r.finishTime}%.2f", Some(_.finishTime)),
      DataTable.Column("MI Executed", r => f"${r.executedMI}%.0f", Some(_.executedMI))
    )

  private def renderFailedTable(failed: Vector[FailedWorkload]): HtmlElement =
    DataTable.renderStatic(failed)(
      DataTable.Column("Workload", r => r.workloadId.toString, Some(_.workloadId.toDouble)),
      DataTable.Column("Reason", _.reason),
      DataTable.Column("Time", r => f"${r.time}%.2f", Some(_.time))
    )
