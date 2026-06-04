// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz.frontend.pages

import com.raquo.laminar.api.L.*
import io.aura.viz.frontend.model.{SimulationData, VmCostRecord}
import io.aura.viz.frontend.components.{ChartComponent, DataTable, KpiCard}
import io.aura.viz.frontend.facades.ChartHelpers
import scala.scalajs.js

/** Cost page with stacked bar chart (CPU/RAM/BW/Storage) and data table. */
object CostPage:

  def render(data: SimulationData): HtmlElement =
    val records  = data.costRecords
    val totalCpu = records.map(_.cpuCost).sum
    val totalRam = records.map(_.ramCost).sum
    val totalBw  = records.map(_.bwCost).sum
    val totalSto = records.map(_.storageCost).sum

    div(
      div(cls := "page-header", h2("Cost"), p("Cost breakdown by resource type per VM")),
      div(
        cls := "kpi-row",
        KpiCard.renderStatic("Total Cost", f"$$${data.totalCost}%.4f"),
        KpiCard.renderStatic("CPU Cost", f"$$${totalCpu}%.4f"),
        KpiCard.renderStatic("RAM Cost", f"$$${totalRam}%.4f"),
        KpiCard.renderStatic("BW + Storage", f"$$${totalBw + totalSto}%.4f")
      ),
      div(
        cls := "chart-row",
        div(cls := "chart-card full-width", h3("Cost Breakdown per VM"), renderCostChart(records))
      ),
      div(cls := "section", h3("Cost Records"), renderTable(records))
    )

  private def renderCostChart(records: Vector[VmCostRecord]): HtmlElement =
    if records.isEmpty then div(cls := "empty-state", "No cost data")
    else
      val labels  = js.Array(records.map(r => s"VM-${r.vmId}")*)
      val cpuData = js.Array(records.map(_.cpuCost.asInstanceOf[js.Any])*)
      val ramData = js.Array(records.map(_.ramCost.asInstanceOf[js.Any])*)
      val bwData  = js.Array(records.map(_.bwCost.asInstanceOf[js.Any])*)
      val stoData = js.Array(records.map(_.storageCost.asInstanceOf[js.Any])*)
      val datasets = js.Array(
        ChartHelpers.barDataset("CPU", cpuData, "#e94560"),
        ChartHelpers.barDataset("RAM", ramData, "#4ecdc4"),
        ChartHelpers.barDataset("BW", bwData, "#f9d423"),
        ChartHelpers.barDataset("Storage", stoData, "#a78bfa")
      )
      val chartData = ChartHelpers.chartData(labels, datasets)
      val options   = ChartHelpers.stackedBarOptions("VM", "Cost ($)")
      ChartComponent.renderStatic("bar", chartData, options)

  private def renderTable(records: Vector[VmCostRecord]): HtmlElement =
    DataTable.renderStatic(records)(
      DataTable.Column("VM", r => r.vmId.toString, Some(_.vmId.toDouble)),
      DataTable.Column("CPU", r => f"$$${r.cpuCost}%.4f", Some(_.cpuCost)),
      DataTable.Column("RAM", r => f"$$${r.ramCost}%.4f", Some(_.ramCost)),
      DataTable.Column("BW", r => f"$$${r.bwCost}%.4f", Some(_.bwCost)),
      DataTable.Column("Storage", r => f"$$${r.storageCost}%.4f", Some(_.storageCost)),
      DataTable.Column("Total", r => f"$$${r.totalCost}%.4f", Some(_.totalCost))
    )
