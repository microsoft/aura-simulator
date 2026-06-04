// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz.frontend.pages

import com.raquo.laminar.api.L.*
import io.aura.viz.frontend.model.{MetricsSnapshot, SimulationData}
import io.aura.viz.frontend.components.{ChartComponent, KpiCard}
import io.aura.viz.frontend.facades.ChartHelpers
import scala.scalajs.js

/** Overview page with 8 KPI cards and 4 time-series charts.
  *
  * Supports two modes:
  *   - Static: renders from final SimulationData (post-simulation)
  *   - Live: reactive KPIs and updating charts from polling snapshots
  */
object OverviewPage:

  /** Render with final simulation data + optional live snapshot overlay. */
  def render(
      data: SimulationData,
      snapshots$ : Signal[Vector[MetricsSnapshot]],
      liveActive$ : Signal[Boolean]
  ): HtmlElement =
    div(
      div(
        cls := "page-header",
        h2("Overview"),
        div(
          cls := "status-bar",
          child <-- liveActive$.map { live =>
            if live then
              span(
                styleAttr := "display:inline-flex;align-items:center;gap:0.4rem;font-size:0.85rem;color:#3fb950;",
                span(
                  styleAttr := "width:8px;height:8px;border-radius:50%;background:#3fb950;box-shadow:0 0 6px #3fb950;display:inline-block;"
                ),
                "Live"
              )
            else
              p(
                s"Simulation completed at t=${f"${data.simulationEndTime}%.1f"}s | ${data.totalEventsProcessed} events processed"
              )
          }
        )
      ),

      // KPI Cards — reactive from live snapshots when available, else static
      renderKpiRow(data, snapshots$),

      // Charts — static from results data
      div(
        cls := "chart-row",
        div(cls := "chart-card", h3("Workload Completion Timeline"), renderCompletionChart(data)),
        div(cls := "chart-card", h3("Energy Over Time"), renderEnergyChart(data))
      ),
      div(
        cls := "chart-row",
        div(cls := "chart-card", h3("Cost Accumulation"), renderCostChart(data)),
        div(cls := "chart-card", h3("Migration Timeline"), renderMigrationChart(data))
      ),

      // Live charts from snapshots (shown when live data is available)
      child <-- snapshots$.map { snaps =>
        if snaps.nonEmpty then renderLiveCharts(snaps)
        else emptyNode
      }
    )

  /** Render live-only mode (no results.json yet, only snapshots). */
  def renderLive(
      snapshots$ : Signal[Vector[MetricsSnapshot]],
      liveActive$ : Signal[Boolean]
  ): HtmlElement =
    div(
      div(
        cls := "page-header",
        h2("Overview"),
        div(
          cls := "status-bar",
          span(
            styleAttr := "display:inline-flex;align-items:center;gap:0.4rem;font-size:0.85rem;color:#3fb950;",
            span(
              styleAttr := "width:8px;height:8px;border-radius:50%;background:#3fb950;box-shadow:0 0 6px #3fb950;display:inline-block;"
            ),
            "Live — Waiting for simulation data..."
          )
        )
      ),

      // Reactive KPI cards from snapshots
      div(
        cls := "kpi-row",
        KpiCard.render("Completed", snapshots$.map(latestField(_, _.completedWorkloads.toString, "0"))),
        KpiCard.render("Failed", snapshots$.map(latestField(_, _.failedWorkloads.toString, "0")), isError = true),
        KpiCard.render("Active VMs", snapshots$.map(latestField(_, _.activeVms.toString, "0"))),
        KpiCard.render("Migrations", snapshots$.map(latestField(_, _.totalMigrations.toString, "0"))),
        KpiCard.render("Faults", snapshots$.map(latestField(_, _.totalFaults.toString, "0")), isError = true),
        KpiCard.render("Energy (Wh)", snapshots$.map(latestField(_, s => f"${s.totalEnergyWh}%.2f", "0.00"))),
        KpiCard.render("Avg Time (s)", snapshots$.map(latestField(_, s => f"${s.avgCompletionTime}%.2f", "0.00"))),
        KpiCard.render("Cost ($)", snapshots$.map(latestField(_, s => f"${s.totalCost}%.4f", "0.0000")))
      ),

      // Live charts from snapshots
      child <-- snapshots$.map { snaps =>
        if snaps.nonEmpty then renderLiveCharts(snaps)
        else div(cls := "loading-state", "Waiting for snapshots...")
      }
    )

  // ─── Live KPI row (overlays latest snapshot values) ────────────────

  private def renderKpiRow(
      data: SimulationData,
      snapshots$ : Signal[Vector[MetricsSnapshot]]
  ): HtmlElement =
    div(
      cls := "kpi-row",
      KpiCard.render(
        "Completed",
        snapshots$.map(latestOrDefault(_, _.completedWorkloads.toString, data.workloadResults.size.toString))
      ),
      KpiCard.render(
        "Failed",
        snapshots$.map(latestOrDefault(_, _.failedWorkloads.toString, data.failedWorkloads.size.toString)),
        isError = data.failedWorkloads.nonEmpty
      ),
      KpiCard.renderStatic("VMs Placed", data.vmPlacements.map(_.vmId).distinct.size.toString),
      KpiCard.render(
        "Migrations",
        snapshots$.map(latestOrDefault(_, _.totalMigrations.toString, data.migrationRecords.size.toString))
      ),
      KpiCard.render(
        "Faults",
        snapshots$.map(latestOrDefault(_, _.totalFaults.toString, data.faultRecords.size.toString)),
        isError = data.faultRecords.nonEmpty
      ),
      KpiCard.render(
        "Energy (Wh)",
        snapshots$.map(latestOrDefault(_, s => f"${s.totalEnergyWh}%.2f", f"${data.totalEnergyWh}%.2f"))
      ),
      KpiCard.render(
        "Avg Time (s)",
        snapshots$.map(latestOrDefault(_, s => f"${s.avgCompletionTime}%.2f", f"${data.avgCompletionTime}%.2f"))
      ),
      KpiCard.render(
        "Total Cost",
        snapshots$.map(latestOrDefault(_, s => f"$$${s.totalCost}%.4f", f"$$${data.totalCost}%.4f"))
      )
    )

  // ─── Live charts from snapshots ────────────────────────────────────

  private def renderLiveCharts(snaps: Vector[MetricsSnapshot]): HtmlElement =
    val labels = js.Array(snaps.map(s => f"${s.timestamp}%.1f")*)

    div(
      cls := "section",
      h3(styleAttr := "margin-bottom:1rem;", "Live Metrics"),
      div(
        cls := "chart-row",
        div(
          cls := "chart-card",
          h3("Throughput"),
          ChartComponent.renderStatic(
            "line",
            ChartHelpers.chartData(
              labels,
              js.Array(
                ChartHelpers.lineDataset(
                  "Completed",
                  js.Array(snaps.map(_.completedWorkloads.asInstanceOf[js.Any])*),
                  "#4fc3f7",
                  fill = true
                )
              )
            ),
            ChartHelpers.commonOptions("Time (s)", "Count")
          )
        ),
        div(
          cls := "chart-card",
          h3("Energy"),
          ChartComponent.renderStatic(
            "line",
            ChartHelpers.chartData(
              labels,
              js.Array(
                ChartHelpers.lineDataset(
                  "Energy (Wh)",
                  js.Array(snaps.map(_.totalEnergyWh.asInstanceOf[js.Any])*),
                  "#ff7043",
                  fill = true
                )
              )
            ),
            ChartHelpers.commonOptions("Time (s)", "Wh")
          )
        )
      ),
      div(
        cls := "chart-row",
        div(
          cls := "chart-card",
          h3("Utilization"),
          ChartComponent.renderStatic(
            "line",
            ChartHelpers.chartData(
              labels,
              js.Array(
                ChartHelpers.lineDataset(
                  "Utilization (%)",
                  js.Array(snaps.map(s => (s.currentUtilization * 100).asInstanceOf[js.Any])*),
                  "#66bb6a",
                  fill = true
                )
              )
            ),
            ChartHelpers.commonOptions("Time (s)", "%")
          )
        ),
        div(
          cls := "chart-card",
          h3("Cost"),
          ChartComponent.renderStatic(
            "line",
            ChartHelpers.chartData(
              labels,
              js.Array(
                ChartHelpers.lineDataset(
                  "Total Cost ($)",
                  js.Array(snaps.map(_.totalCost.asInstanceOf[js.Any])*),
                  "#ab47bc",
                  fill = true
                )
              )
            ),
            ChartHelpers.commonOptions("Time (s)", "$")
          )
        )
      )
    )

  // ─── Helpers ───────────────────────────────────────────────────────

  private def latestField(
      snaps: Vector[MetricsSnapshot],
      extract: MetricsSnapshot => String,
      default: String
  ): String =
    snaps.lastOption.map(extract).getOrElse(default)

  private def latestOrDefault(
      snaps: Vector[MetricsSnapshot],
      extract: MetricsSnapshot => String,
      default: String
  ): String =
    if snaps.nonEmpty then extract(snaps.last) else default

  // ─── Static charts from final results ─────────────────────────────

  private def renderCompletionChart(data: SimulationData): HtmlElement =
    if data.workloadResults.isEmpty then div(cls := "empty-state", "No workload data")
    else
      val sorted    = data.workloadResults.sortBy(_.finishTime)
      val labels    = js.Array(sorted.map(r => f"${r.finishTime}%.1f")*)
      val counts    = js.Array(sorted.zipWithIndex.map((_, i) => (i + 1).asInstanceOf[js.Any])*)
      val ds        = ChartHelpers.lineDataset("Completed Workloads", counts, "#4fc3f7", fill = true)
      val chartData = ChartHelpers.chartData(labels, js.Array(ds))
      val options   = ChartHelpers.commonOptions("Time (s)", "Count")
      ChartComponent.renderStatic("line", chartData, options)

  private def renderEnergyChart(data: SimulationData): HtmlElement =
    if data.energyRecords.isEmpty then div(cls := "empty-state", "No energy data")
    else
      val byHost  = data.energyRecords.groupBy(_.hostId)
      val palette = ChartHelpers.palette
      val datasets = js.Array(byHost.toSeq.sortBy(_._1).zipWithIndex.map { case ((hostId, records), i) =>
        val points = js.Array(
          records.sortBy(_.toTime).map(r => js.Dynamic.literal(x = r.toTime, y = r.energyWh).asInstanceOf[js.Any])*
        )
        ChartHelpers.lineDataset(s"Host $hostId", points, palette(i % palette.size))
      }*)
      val chartData = ChartHelpers.chartDataNoLabels(datasets)
      val options   = ChartHelpers.scatterOptions("Time (s)", "Energy (Wh)")
      ChartComponent.renderStatic("line", chartData, options)

  private def renderCostChart(data: SimulationData): HtmlElement =
    if data.costRecords.isEmpty then div(cls := "empty-state", "No cost data")
    else
      val sorted  = data.costRecords.sortBy(_.time)
      var cumCost = 0.0
      val points = sorted.map { r =>
        cumCost += r.totalCost
        js.Dynamic.literal(x = r.time, y = cumCost).asInstanceOf[js.Any]
      }
      val ds        = ChartHelpers.lineDataset("Cumulative Cost ($)", js.Array(points*), "#ab47bc", fill = true)
      val chartData = ChartHelpers.chartDataNoLabels(js.Array(ds))
      val options   = ChartHelpers.scatterOptions("Time (s)", "Cost ($)")
      ChartComponent.renderStatic("line", chartData, options)

  private def renderMigrationChart(data: SimulationData): HtmlElement =
    if data.migrationRecords.isEmpty then div(cls := "empty-state", "No migration data")
    else
      val points = js.Array(
        data.migrationRecords.map(r => js.Dynamic.literal(x = r.startTime, y = r.duration).asInstanceOf[js.Any])*
      )
      val ds        = ChartHelpers.scatterDataset("Migration Duration", points, "#e94560")
      val chartData = ChartHelpers.chartDataNoLabels(js.Array(ds))
      val options   = ChartHelpers.scatterOptions("Start Time (s)", "Duration (s)")
      ChartComponent.renderStatic("scatter", chartData, options)
