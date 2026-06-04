// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz.frontend.pages

import com.raquo.laminar.api.L.*
import io.aura.viz.frontend.model.{MigrationRecord, SimulationData}
import io.aura.viz.frontend.components.{ChartComponent, DataTable, KpiCard}
import io.aura.viz.frontend.facades.ChartHelpers
import scala.scalajs.js

/** Migrations page with scatter chart (duration vs time) and data table. */
object MigrationsPage:

  def render(data: SimulationData): HtmlElement =
    val records     = data.migrationRecords
    val avgDuration = if records.nonEmpty then records.map(_.duration).sum / records.size else 0.0
    val totalData   = records.map(_.dataTransferred).sum

    div(
      div(cls := "page-header", h2("Migrations"), p("VM migration events and performance")),
      div(
        cls := "kpi-row",
        KpiCard.renderStatic("Total Migrations", records.size.toString),
        KpiCard.renderStatic("Avg Duration", f"$avgDuration%.4f s"),
        KpiCard.renderStatic("Data Transferred", f"$totalData%.1f MB"),
        KpiCard.renderStatic("Unique VMs", records.map(_.vmId).distinct.size.toString)
      ),
      div(
        cls := "chart-row",
        div(cls := "chart-card full-width", h3("Migration Duration vs Start Time"), renderMigrationChart(records))
      ),
      div(cls := "section", h3("Migration Records"), renderTable(records))
    )

  private def renderMigrationChart(records: Vector[MigrationRecord]): HtmlElement =
    if records.isEmpty then div(cls := "empty-state", "No migration data")
    else
      val points = js.Array(records.map(r => js.Dynamic.literal(x = r.startTime, y = r.duration).asInstanceOf[js.Any])*)
      val ds     = ChartHelpers.scatterDataset("Migration Duration", points, "#e94560")
      val chartData = ChartHelpers.chartDataNoLabels(js.Array(ds))
      val options   = ChartHelpers.scatterOptions("Start Time (s)", "Duration (s)")
      ChartComponent.renderStatic("scatter", chartData, options)

  private def renderTable(records: Vector[MigrationRecord]): HtmlElement =
    DataTable.renderStatic(records)(
      DataTable.Column("VM", r => r.vmId.toString, Some(_.vmId.toDouble)),
      DataTable.Column("Source", r => r.sourceHostId.toString, Some(_.sourceHostId.toDouble)),
      DataTable.Column("Target", r => r.targetHostId.toString, Some(_.targetHostId.toDouble)),
      DataTable.Column("Start", r => f"${r.startTime}%.2f", Some(_.startTime)),
      DataTable.Column("Duration", r => f"${r.duration}%.4f", Some(_.duration)),
      DataTable.Column("Data (MB)", r => f"${r.dataTransferred}%.1f", Some(_.dataTransferred))
    )
