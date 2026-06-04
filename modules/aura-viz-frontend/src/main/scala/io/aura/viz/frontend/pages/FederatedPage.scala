// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz.frontend.pages

import com.raquo.laminar.api.L.*
import io.aura.viz.frontend.model.*
import io.aura.viz.frontend.components.{ChartComponent, DataTable, KpiCard}
import io.aura.viz.frontend.facades.ChartHelpers
import scala.scalajs.js

/** Federated computing page with tier distribution and escalation charts. */
object FederatedPage:

  def render(data: SimulationData): HtmlElement =
    val tasks     = data.federatedTaskResults
    val failed    = data.failedFederatedTasks
    val totalEsc  = tasks.map(_.escalations).sum
    val avgEsc    = if tasks.nonEmpty then totalEsc.toDouble / tasks.size else 0.0
    val escalated = tasks.count(_.escalations > 0)

    div(
      div(cls := "page-header", h2("Federated"), p("Federated task execution and tier escalation")),
      div(
        cls := "kpi-row",
        KpiCard.renderStatic("Total Tasks", tasks.size.toString),
        KpiCard.renderStatic("Failed", failed.size.toString, isError = failed.nonEmpty),
        KpiCard.renderStatic("Escalated", escalated.toString),
        KpiCard.renderStatic("Avg Escalations", f"$avgEsc%.2f"),
        KpiCard.renderStatic("Total Escalations", totalEsc.toString)
      ),
      div(
        cls := "chart-row",
        div(cls := "chart-card", h3("Final Tier Distribution"), renderTierDoughnut(tasks)),
        div(cls := "chart-card", h3("Escalations by Initial Tier"), renderEscalationChart(tasks))
      ),
      div(cls := "section", h3("Federated Task Results"), renderTasksTable(tasks)),
      if failed.nonEmpty then div(cls := "section", h3("Failed Federated Tasks"), renderFailedTable(failed))
      else emptyNode
    )

  private def renderTierDoughnut(tasks: Vector[FederatedTaskResult]): HtmlElement =
    if tasks.isEmpty then div(cls := "empty-state", "No federated task data")
    else
      val byTier  = tasks.groupBy(_.finalTier).toSeq.sortBy(_._1)
      val labels  = js.Array(byTier.map(_._1)*)
      val counts  = js.Array(byTier.map(_._2.size.asInstanceOf[js.Any])*)
      val palette = ChartHelpers.palette
      val colors  = js.Array(byTier.indices.map(i => palette(i % palette.size))*)
      val ds = js.Dynamic.literal(
        data = counts,
        backgroundColor = colors,
        borderWidth = 0
      )
      val chartData = js.Dynamic.literal(labels = labels, datasets = js.Array(ds))
      val options   = ChartHelpers.doughnutOptions
      ChartComponent.renderStatic("doughnut", chartData, options)

  private def renderEscalationChart(tasks: Vector[FederatedTaskResult]): HtmlElement =
    if tasks.isEmpty then div(cls := "empty-state", "No federated task data")
    else
      val byInitTier = tasks.groupBy(_.initialTier).toSeq.sortBy(_._1)
      val labels     = js.Array(byInitTier.map(_._1)*)
      val avgEscData = js.Array(byInitTier.map { (_, recs) =>
        val avg = if recs.nonEmpty then recs.map(_.escalations).sum.toDouble / recs.size else 0.0
        avg.asInstanceOf[js.Any]
      }*)
      val countData = js.Array(byInitTier.map(_._2.size.asInstanceOf[js.Any])*)
      val datasets = js.Array(
        ChartHelpers.barDataset("Avg Escalations", avgEscData, "#ff7043"),
        ChartHelpers.barDataset("Task Count", countData, "#4fc3f7")
      )
      val chartData = ChartHelpers.chartData(labels, datasets)
      val options   = ChartHelpers.commonOptions("Initial Tier", "Count")
      ChartComponent.renderStatic("bar", chartData, options)

  private def renderTasksTable(tasks: Vector[FederatedTaskResult]): HtmlElement =
    DataTable.renderStatic(tasks)(
      DataTable.Column("Task", r => r.taskId.toString, Some(_.taskId.toDouble)),
      DataTable.Column("Initial Tier", _.initialTier),
      DataTable.Column("Final Tier", _.finalTier),
      DataTable.Column("Escalations", r => r.escalations.toString, Some(_.escalations.toDouble)),
      DataTable.Column("Start", r => f"${r.startTime}%.2f", Some(_.startTime)),
      DataTable.Column("Finish", r => f"${r.finishTime}%.2f", Some(_.finishTime))
    )

  private def renderFailedTable(failed: Vector[FailedFederatedTask]): HtmlElement =
    DataTable.renderStatic(failed)(
      DataTable.Column("Task", r => r.taskId.toString, Some(_.taskId.toDouble)),
      DataTable.Column("Reason", _.reason),
      DataTable.Column("Tiers Attempted", r => r.tiersAttempted.toString, Some(_.tiersAttempted.toDouble)),
      DataTable.Column("Time", r => f"${r.time}%.2f", Some(_.time))
    )
