// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz.frontend.pages

import com.raquo.laminar.api.L.*
import io.aura.viz.frontend.model.*
import io.aura.viz.frontend.components.{ChartComponent, DataTable, KpiCard}
import io.aura.viz.frontend.facades.ChartHelpers
import scala.scalajs.js

/** Containers page with pod scheduling, phases, and per-node charts. */
object ContainersPage:

  def render(data: SimulationData): HtmlElement =
    val pods       = data.podResults
    val scheduling = data.podSchedulingRecords
    val running    = pods.count(_.phase == "Running")
    val completed  = pods.count(_.phase == "Succeeded")
    val nodeCount  = scheduling.map(_.nodeName).distinct.size

    div(
      div(cls := "page-header", h2("Containers"), p("Kubernetes pod scheduling and lifecycle")),
      div(
        cls := "kpi-row",
        KpiCard.renderStatic("Total Pods", pods.size.toString),
        KpiCard.renderStatic("Running", running.toString),
        KpiCard.renderStatic("Completed", completed.toString),
        KpiCard.renderStatic("Nodes", nodeCount.toString)
      ),
      div(
        cls := "chart-row",
        div(cls := "chart-card", h3("Pod Lifecycle (Gantt)"), renderGanttChart(pods)),
        div(cls := "chart-card", h3("Pods per Node"), renderPerNodeChart(scheduling))
      ),
      div(cls := "section", h3("Pod Results"), renderPodsTable(pods)),
      if scheduling.nonEmpty then div(cls := "section", h3("Scheduling Records"), renderSchedulingTable(scheduling))
      else emptyNode
    )

  private def renderGanttChart(pods: Vector[PodResult]): HtmlElement =
    if pods.isEmpty then div(cls := "empty-state", "No pod data")
    else
      // Horizontal bar chart showing pod start-to-finish as bars
      val labels    = js.Array(pods.map(p => s"Pod-${p.podId}")*)
      val startData = js.Array(pods.map(_.startTime.asInstanceOf[js.Any])*)
      val durationData = js.Array(pods.map { p =>
        val end = p.finishTime.getOrElse(pods.flatMap(_.finishTime).maxOption.getOrElse(p.startTime))
        (end - p.startTime).asInstanceOf[js.Any]
      }*)
      val datasets = js.Array(
        js.Dynamic.literal(
          label = "Start Offset",
          data = startData,
          backgroundColor = "transparent"
        ),
        ChartHelpers.barDataset("Duration", durationData, "#4ecdc4")
      )
      val chartData = ChartHelpers.chartData(labels, datasets)
      val options = js.Dynamic.literal(
        responsive = true,
        maintainAspectRatio = false,
        indexAxis = "y",
        plugins = js.Dynamic.literal(
          legend = js.Dynamic.literal(labels = js.Dynamic.literal(color = "#eee"))
        ),
        scales = js.Dynamic.literal(
          x = js.Dynamic.literal(
            stacked = true,
            ticks = js.Dynamic.literal(color = "#999"),
            grid = js.Dynamic.literal(color = "rgba(255,255,255,0.06)"),
            title = js.Dynamic.literal(display = true, text = "Time (s)", color = "#999")
          ),
          y = js.Dynamic.literal(
            stacked = true,
            ticks = js.Dynamic.literal(color = "#999"),
            grid = js.Dynamic.literal(color = "rgba(255,255,255,0.06)")
          )
        )
      )
      ChartComponent.renderStatic("bar", chartData, options)

  private def renderPerNodeChart(scheduling: Vector[PodSchedulingRecord]): HtmlElement =
    if scheduling.isEmpty then div(cls := "empty-state", "No scheduling data")
    else
      val byNode    = scheduling.groupBy(_.nodeName)
      val sorted    = byNode.toSeq.sortBy(_._1)
      val labels    = js.Array(sorted.map(_._1)*)
      val counts    = js.Array(sorted.map(_._2.size.asInstanceOf[js.Any])*)
      val ds        = ChartHelpers.barDataset("Pods Scheduled", counts, "#4fc3f7")
      val chartData = ChartHelpers.chartData(labels, js.Array(ds))
      val options   = ChartHelpers.commonOptions("Node", "Pod Count")
      ChartComponent.renderStatic("bar", chartData, options)

  private def renderPodsTable(pods: Vector[PodResult]): HtmlElement =
    DataTable.renderStatic(pods)(
      DataTable.Column("Pod", r => r.podId.toString, Some(_.podId.toDouble)),
      DataTable.Column("Host", r => r.hostId.toString, Some(_.hostId.toDouble)),
      DataTable.Column("Start", r => f"${r.startTime}%.2f", Some(_.startTime)),
      DataTable.Column("Finish", r => r.finishTime.map(t => f"$t%.2f").getOrElse("—")),
      DataTable.Column("Phase", _.phase)
    )

  private def renderSchedulingTable(scheduling: Vector[PodSchedulingRecord]): HtmlElement =
    DataTable.renderStatic(scheduling)(
      DataTable.Column("Pod", r => r.podId.toString, Some(_.podId.toDouble)),
      DataTable.Column("Host", r => r.hostId.toString, Some(_.hostId.toDouble)),
      DataTable.Column("Node", _.nodeName),
      DataTable.Column("Time", r => f"${r.time}%.2f", Some(_.time))
    )
