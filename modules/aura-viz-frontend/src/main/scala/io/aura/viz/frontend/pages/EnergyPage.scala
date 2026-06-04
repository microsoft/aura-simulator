// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz.frontend.pages

import com.raquo.laminar.api.L.*
import io.aura.viz.frontend.model.{EnergyRecord, SimulationData}
import io.aura.viz.frontend.components.{ChartComponent, DataTable, KpiCard}
import io.aura.viz.frontend.facades.ChartHelpers
import scala.scalajs.js

/** Energy page with multi-line chart by host and data table. */
object EnergyPage:

  def render(data: SimulationData): HtmlElement =
    val records   = data.energyRecords
    val totalWh   = data.totalEnergyWh
    val hostCount = records.map(_.hostId).distinct.size
    val avgWatts  = if records.nonEmpty then records.map(_.watts).sum / records.size else 0.0

    div(
      div(cls := "page-header", h2("Energy"), p("Energy consumption per host over time")),
      div(
        cls := "kpi-row",
        KpiCard.renderStatic("Total Energy", f"$totalWh%.2f Wh"),
        KpiCard.renderStatic("Hosts", hostCount.toString),
        KpiCard.renderStatic("Avg Watts", f"$avgWatts%.1f W"),
        KpiCard.renderStatic("Records", records.size.toString)
      ),
      div(
        cls := "chart-row",
        div(cls := "chart-card full-width", h3("Energy Over Time by Host"), renderEnergyChart(records))
      ),
      div(cls := "section", h3("Energy Records"), renderTable(records))
    )

  private def renderEnergyChart(records: Vector[EnergyRecord]): HtmlElement =
    if records.isEmpty then div(cls := "empty-state", "No energy data")
    else
      val byHost  = records.groupBy(_.hostId)
      val palette = ChartHelpers.palette
      val datasets = js.Array(byHost.toSeq.sortBy(_._1).zipWithIndex.map { case ((hostId, recs), i) =>
        val points = js
          .Array(recs.sortBy(_.toTime).map(r => js.Dynamic.literal(x = r.toTime, y = r.energyWh).asInstanceOf[js.Any])*)
        ChartHelpers.lineDataset(s"Host $hostId", points, palette(i % palette.size))
      }*)
      val chartData = ChartHelpers.chartDataNoLabels(datasets)
      val options   = ChartHelpers.scatterOptions("Time (s)", "Energy (Wh)")
      ChartComponent.renderStatic("line", chartData, options)

  private def renderTable(records: Vector[EnergyRecord]): HtmlElement =
    DataTable.renderStatic(records)(
      DataTable.Column("Host", r => r.hostId.toString, Some(_.hostId.toDouble)),
      DataTable.Column("Watts", r => f"${r.watts}%.1f", Some(_.watts)),
      DataTable.Column("From", r => f"${r.fromTime}%.1f", Some(_.fromTime)),
      DataTable.Column("To", r => f"${r.toTime}%.1f", Some(_.toTime)),
      DataTable.Column("Energy (Wh)", r => f"${r.energyWh}%.4f", Some(_.energyWh))
    )
