// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz.frontend.pages

import com.raquo.laminar.api.L.*
import io.aura.viz.frontend.model.{FaultRecord, SimulationData, VmDestroyedRecord}
import io.aura.viz.frontend.components.{ChartComponent, DataTable, KpiCard}
import io.aura.viz.frontend.facades.ChartHelpers
import scala.scalajs.js

/** Faults page with scatter timeline and data tables. */
object FaultsPage:

  def render(data: SimulationData): HtmlElement =
    val faults    = data.faultRecords
    val destroyed = data.vmDestroyedRecords
    val totalPEs  = faults.map(_.failedPEs).sum

    div(
      div(cls := "page-header", h2("Faults"), p("Fault injection events and VM destruction")),
      div(
        cls := "kpi-row",
        KpiCard.renderStatic("Total Faults", faults.size.toString, isError = faults.nonEmpty),
        KpiCard.renderStatic("Failed PEs", totalPEs.toString, isError = totalPEs > 0),
        KpiCard.renderStatic("VMs Destroyed", destroyed.size.toString, isError = destroyed.nonEmpty),
        KpiCard.renderStatic("Hosts Affected", faults.map(_.hostId).distinct.size.toString)
      ),
      div(cls := "chart-row", div(cls := "chart-card full-width", h3("Fault Timeline"), renderFaultChart(faults))),
      div(cls := "section", h3("Fault Records"), renderFaultTable(faults)),
      if destroyed.nonEmpty then div(cls := "section", h3("VMs Destroyed"), renderDestroyedTable(destroyed))
      else emptyNode
    )

  private def renderFaultChart(faults: Vector[FaultRecord]): HtmlElement =
    if faults.isEmpty then div(cls := "empty-state", "No fault data")
    else
      val points    = js.Array(faults.map(r => js.Dynamic.literal(x = r.time, y = r.failedPEs).asInstanceOf[js.Any])*)
      val ds        = ChartHelpers.scatterDataset("Faults (Failed PEs)", points, "#e94560")
      val chartData = ChartHelpers.chartDataNoLabels(js.Array(ds))
      val options   = ChartHelpers.scatterOptions("Time (s)", "Failed PEs")
      ChartComponent.renderStatic("scatter", chartData, options)

  private def renderFaultTable(faults: Vector[FaultRecord]): HtmlElement =
    DataTable.renderStatic(faults)(
      DataTable.Column("Host", r => r.hostId.toString, Some(_.hostId.toDouble)),
      DataTable.Column("Failed PEs", r => r.failedPEs.toString, Some(_.failedPEs.toDouble)),
      DataTable.Column("Time", r => f"${r.time}%.2f", Some(_.time))
    )

  private def renderDestroyedTable(destroyed: Vector[VmDestroyedRecord]): HtmlElement =
    DataTable.renderStatic(destroyed)(
      DataTable.Column("VM", r => r.vmId.toString, Some(_.vmId.toDouble)),
      DataTable.Column("Host", r => r.hostId.toString, Some(_.hostId.toDouble)),
      DataTable.Column("Reason", _.reason),
      DataTable.Column("Time", r => f"${r.time}%.2f", Some(_.time))
    )
