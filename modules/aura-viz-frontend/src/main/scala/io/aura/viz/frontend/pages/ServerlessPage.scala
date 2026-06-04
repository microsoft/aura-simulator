// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz.frontend.pages

import com.raquo.laminar.api.L.*
import io.aura.viz.frontend.model.*
import io.aura.viz.frontend.components.{ChartComponent, DataTable, KpiCard}
import io.aura.viz.frontend.facades.ChartHelpers
import scala.scalajs.js

/** Serverless page with invocations, cold starts, and concurrency charts. */
object ServerlessPage:

  def render(data: SimulationData): HtmlElement =
    val invocations = data.invocationResults
    val throttled   = data.throttledInvocations
    val timedOut    = data.timedOutInvocations
    val coldStarts  = invocations.count(_.coldStart)
    val totalBilled = invocations.map(_.billedGBSeconds).sum
    val avgDuration =
      if invocations.nonEmpty then invocations.map(r => r.finishTime - r.startTime).sum / invocations.size
      else 0.0

    div(
      div(cls := "page-header", h2("Serverless"), p("Function invocations, cold starts, and throttling")),
      div(
        cls := "kpi-row",
        KpiCard.renderStatic("Invocations", invocations.size.toString),
        KpiCard.renderStatic("Cold Starts", coldStarts.toString, isError = coldStarts > 0),
        KpiCard.renderStatic("Throttled", throttled.size.toString, isError = throttled.nonEmpty),
        KpiCard.renderStatic("Timed Out", timedOut.size.toString, isError = timedOut.nonEmpty),
        KpiCard.renderStatic("Avg Duration", f"$avgDuration%.4f s"),
        KpiCard.renderStatic("Billed GB-s", f"$totalBilled%.4f")
      ),
      div(
        cls := "chart-row",
        div(cls := "chart-card", h3("Invocation Timeline"), renderTimelineChart(invocations)),
        div(cls := "chart-card", h3("Per-Function Summary"), renderPerFunctionChart(invocations))
      ),
      div(cls := "section", h3("Invocation Results"), renderInvocationsTable(invocations)),
      if throttled.nonEmpty then div(cls := "section", h3("Throttled Invocations"), renderThrottledTable(throttled))
      else emptyNode,
      if timedOut.nonEmpty then div(cls := "section", h3("Timed Out Invocations"), renderTimedOutTable(timedOut))
      else emptyNode
    )

  private def renderTimelineChart(invocations: Vector[InvocationResult]): HtmlElement =
    if invocations.isEmpty then div(cls := "empty-state", "No invocation data")
    else
      val warm = invocations.filterNot(_.coldStart)
      val cold = invocations.filter(_.coldStart)
      val warmPts = js.Array(
        warm.map(r => js.Dynamic.literal(x = r.startTime, y = r.finishTime - r.startTime).asInstanceOf[js.Any])*
      )
      val coldPts = js.Array(
        cold.map(r => js.Dynamic.literal(x = r.startTime, y = r.finishTime - r.startTime).asInstanceOf[js.Any])*
      )
      val datasets = js.Array(
        ChartHelpers.scatterDataset("Warm Start", warmPts, "#4fc3f7"),
        ChartHelpers.scatterDataset("Cold Start", coldPts, "#e94560")
      )
      val chartData = ChartHelpers.chartDataNoLabels(datasets)
      val options   = ChartHelpers.scatterOptions("Start Time (s)", "Duration (s)")
      ChartComponent.renderStatic("scatter", chartData, options)

  private def renderPerFunctionChart(invocations: Vector[InvocationResult]): HtmlElement =
    if invocations.isEmpty then div(cls := "empty-state", "No invocation data")
    else
      val byFunction = invocations.groupBy(_.functionId)
      val sorted     = byFunction.toSeq.sortBy(_._1)
      val labels     = js.Array(sorted.map((fid, _) => s"F-$fid")*)
      val counts     = js.Array(sorted.map((_, recs) => recs.size.asInstanceOf[js.Any])*)
      val coldCounts = js.Array(sorted.map((_, recs) => recs.count(_.coldStart).asInstanceOf[js.Any])*)
      val datasets = js.Array(
        ChartHelpers.barDataset("Total", counts, "#4fc3f7"),
        ChartHelpers.barDataset("Cold Starts", coldCounts, "#e94560")
      )
      val chartData = ChartHelpers.chartData(labels, datasets)
      val options   = ChartHelpers.commonOptions("Function", "Count")
      ChartComponent.renderStatic("bar", chartData, options)

  private def renderInvocationsTable(invocations: Vector[InvocationResult]): HtmlElement =
    DataTable.renderStatic(invocations)(
      DataTable.Column("Invocation", r => r.invocationId.toString, Some(_.invocationId.toDouble)),
      DataTable.Column("Function", r => r.functionId.toString, Some(_.functionId.toDouble)),
      DataTable.Column("Start", r => f"${r.startTime}%.2f", Some(_.startTime)),
      DataTable.Column("Finish", r => f"${r.finishTime}%.2f", Some(_.finishTime)),
      DataTable.Column("GB-s", r => f"${r.billedGBSeconds}%.4f", Some(_.billedGBSeconds)),
      DataTable.Column("Cold?", r => if r.coldStart then "Yes" else "No")
    )

  private def renderThrottledTable(throttled: Vector[ThrottledInvocation]): HtmlElement =
    DataTable.renderStatic(throttled)(
      DataTable.Column("Invocation", r => r.invocationId.toString, Some(_.invocationId.toDouble)),
      DataTable.Column("Function", r => r.functionId.toString, Some(_.functionId.toDouble)),
      DataTable.Column("Reason", _.reason),
      DataTable.Column("Time", r => f"${r.time}%.2f", Some(_.time))
    )

  private def renderTimedOutTable(timedOut: Vector[TimedOutInvocation]): HtmlElement =
    DataTable.renderStatic(timedOut)(
      DataTable.Column("Invocation", r => r.invocationId.toString, Some(_.invocationId.toDouble)),
      DataTable.Column("Function", r => r.functionId.toString, Some(_.functionId.toDouble)),
      DataTable.Column("Reason", _.reason),
      DataTable.Column("Time", r => f"${r.time}%.2f", Some(_.time))
    )
