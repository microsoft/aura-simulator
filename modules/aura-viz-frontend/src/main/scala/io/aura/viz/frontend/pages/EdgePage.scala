// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz.frontend.pages

import com.raquo.laminar.api.L.*
import io.aura.viz.frontend.model.*
import io.aura.viz.frontend.components.{ChartComponent, DataTable, KpiCard}
import io.aura.viz.frontend.facades.ChartHelpers
import scala.scalajs.js

/** Edge computing page with task offloading and latency charts. */
object EdgePage:

  def render(data: SimulationData): HtmlElement =
    val tasks      = data.edgeTaskResults
    val failed     = data.failedEdgeTasks
    val offloaded  = tasks.count(_.offloaded)
    val local      = tasks.size - offloaded
    val avgLatency = if tasks.nonEmpty then tasks.map(_.networkLatency).sum / tasks.size else 0.0

    div(
      div(cls := "page-header", h2("Edge Computing"), p("Edge task offloading and network latency")),
      div(
        cls := "kpi-row",
        KpiCard.renderStatic("Total Tasks", tasks.size.toString),
        KpiCard.renderStatic("Offloaded", offloaded.toString),
        KpiCard.renderStatic("Local", local.toString),
        KpiCard.renderStatic("Failed", failed.size.toString, isError = failed.nonEmpty),
        KpiCard.renderStatic("Avg Latency", f"$avgLatency%.4f s")
      ),
      div(
        cls := "chart-row",
        div(cls := "chart-card", h3("Network Latency vs Time"), renderLatencyChart(tasks)),
        div(cls := "chart-card", h3("Offloading Distribution"), renderOffloadingChart(tasks))
      ),
      div(cls := "section", h3("Edge Task Results"), renderTasksTable(tasks)),
      if failed.nonEmpty then div(cls := "section", h3("Failed Edge Tasks"), renderFailedTable(failed))
      else emptyNode
    )

  private def renderLatencyChart(tasks: Vector[EdgeTaskResult]): HtmlElement =
    if tasks.isEmpty then div(cls := "empty-state", "No edge task data")
    else
      val localPts = tasks
        .filterNot(_.offloaded)
        .map(r => js.Dynamic.literal(x = r.startTime, y = r.networkLatency).asInstanceOf[js.Any])
      val offloadedPts = tasks
        .filter(_.offloaded)
        .map(r => js.Dynamic.literal(x = r.startTime, y = r.networkLatency).asInstanceOf[js.Any])
      val datasets = js.Array(
        ChartHelpers.scatterDataset("Local", js.Array(localPts*), "#66bb6a"),
        ChartHelpers.scatterDataset("Offloaded", js.Array(offloadedPts*), "#ff7043")
      )
      val chartData = ChartHelpers.chartDataNoLabels(datasets)
      val options   = ChartHelpers.scatterOptions("Start Time (s)", "Network Latency (s)")
      ChartComponent.renderStatic("scatter", chartData, options)

  private def renderOffloadingChart(tasks: Vector[EdgeTaskResult]): HtmlElement =
    if tasks.isEmpty then div(cls := "empty-state", "No edge task data")
    else
      // Group by source node, stacked bar showing offloaded vs local
      val bySource      = tasks.groupBy(_.sourceNodeName).toSeq.sortBy(_._1)
      val labels        = js.Array(bySource.map(_._1)*)
      val localData     = js.Array(bySource.map((_, recs) => recs.count(!_.offloaded).asInstanceOf[js.Any])*)
      val offloadedData = js.Array(bySource.map((_, recs) => recs.count(_.offloaded).asInstanceOf[js.Any])*)
      val datasets = js.Array(
        ChartHelpers.barDataset("Local", localData, "#66bb6a"),
        ChartHelpers.barDataset("Offloaded", offloadedData, "#ff7043")
      )
      val chartData = ChartHelpers.chartData(labels, datasets)
      val options   = ChartHelpers.stackedBarOptions("Source Node", "Tasks")
      ChartComponent.renderStatic("bar", chartData, options)

  private def renderTasksTable(tasks: Vector[EdgeTaskResult]): HtmlElement =
    DataTable.renderStatic(tasks)(
      DataTable.Column("Task", r => r.taskId.toString, Some(_.taskId.toDouble)),
      DataTable.Column("Source", _.sourceNodeName),
      DataTable.Column("Execution", _.executionNodeName),
      DataTable.Column("Offloaded?", r => if r.offloaded then "Yes" else "No"),
      DataTable.Column("Latency", r => f"${r.networkLatency}%.4f", Some(_.networkLatency)),
      DataTable.Column("Start", r => f"${r.startTime}%.2f", Some(_.startTime)),
      DataTable.Column("Finish", r => f"${r.finishTime}%.2f", Some(_.finishTime))
    )

  private def renderFailedTable(failed: Vector[FailedEdgeTask]): HtmlElement =
    DataTable.renderStatic(failed)(
      DataTable.Column("Task", r => r.taskId.toString, Some(_.taskId.toDouble)),
      DataTable.Column("Reason", _.reason),
      DataTable.Column("Time", r => f"${r.time}%.2f", Some(_.time))
    )
