// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz.frontend.components

import com.raquo.laminar.api.L.*
import org.scalajs.dom.HTMLCanvasElement
import io.aura.viz.frontend.facades.{Chart, ChartHelpers}
import scala.scalajs.js

/** Chart.js ↔ Laminar lifecycle bridge.
  *
  * Creates a canvas element, initializes a Chart.js instance on mount, reactively updates chart data when the signal
  * changes, and destroys the chart on unmount.
  */
object ChartComponent:

  /** Render a chart that updates reactively from a data signal.
    *
    * @param config$
    *   Signal of (chartType, data, options) chart configuration
    * @param height
    *   CSS height for the chart container
    * @return
    *   Laminar HtmlElement containing the canvas
    */
  def render(
      config$ : Signal[(String, js.Dynamic, js.Dynamic)],
      height: String = "300px"
  ): HtmlElement =
    var chartInstance: Option[Chart] = None

    div(
      cls       := "chart-container",
      styleAttr := s"position:relative;height:$height;width:100%",
      canvasTag(
        onMountUnmountCallback(
          mount = ctx =>
            val canvas = ctx.thisNode.ref.asInstanceOf[HTMLCanvasElement]
            // Subscribe to config changes
            ctx.thisNode.amend(
              config$.changes --> Observer[(String, js.Dynamic, js.Dynamic)] { case (chartType, data, options) =>
                chartInstance.foreach(_.destroy())
                val cfg = ChartHelpers.chartConfig(chartType, data, options)
                chartInstance = Some(new Chart(canvas, cfg))
              },
              // Initial render from current value
              config$ --> Observer[(String, js.Dynamic, js.Dynamic)] { case (chartType, data, options) =>
                if chartInstance.isEmpty then
                  val cfg = ChartHelpers.chartConfig(chartType, data, options)
                  chartInstance = Some(new Chart(canvas, cfg))
              }
            )
          ,
          unmount = _ =>
            chartInstance.foreach(_.destroy())
            chartInstance = None
        )
      )
    )

  /** Render a static chart (non-reactive, created once on mount).
    *
    * @param chartType
    *   Chart.js chart type ("line", "bar", "scatter", "doughnut")
    * @param data
    *   Chart.js data object
    * @param options
    *   Chart.js options object
    * @param height
    *   CSS height for the chart container
    */
  def renderStatic(
      chartType: String,
      data: js.Dynamic,
      options: js.Dynamic,
      height: String = "300px"
  ): HtmlElement =
    var chartInstance: Option[Chart] = None

    div(
      cls       := "chart-container",
      styleAttr := s"position:relative;height:$height;width:100%",
      canvasTag(
        onMountUnmountCallback(
          mount = ctx =>
            val canvas = ctx.thisNode.ref.asInstanceOf[HTMLCanvasElement]
            val cfg    = ChartHelpers.chartConfig(chartType, data, options)
            chartInstance = Some(new Chart(canvas, cfg))
          ,
          unmount = _ =>
            chartInstance.foreach(_.destroy())
            chartInstance = None
        )
      )
    )

  /** Render a chart that updates its data reactively (same chart type/options).
    *
    * @param chartType
    *   Chart.js chart type
    * @param data$
    *   Signal of chart data to update
    * @param options
    *   Chart.js options (static)
    * @param height
    *   CSS height
    */
  def renderUpdating(
      chartType: String,
      data$ : Signal[js.Dynamic],
      options: js.Dynamic,
      height: String = "300px"
  ): HtmlElement =
    var chartInstance: Option[Chart] = None

    div(
      cls       := "chart-container",
      styleAttr := s"position:relative;height:$height;width:100%",
      canvasTag(
        onMountUnmountCallback(
          mount = ctx =>
            val canvas = ctx.thisNode.ref.asInstanceOf[HTMLCanvasElement]
            ctx.thisNode.amend(
              data$ --> Observer[js.Dynamic] { newData =>
                chartInstance match
                  case Some(chart) =>
                    chart.data = newData
                    chart.update("none")
                  case None =>
                    val cfg = ChartHelpers.chartConfig(chartType, newData, options)
                    chartInstance = Some(new Chart(canvas, cfg))
              }
            )
          ,
          unmount = _ =>
            chartInstance.foreach(_.destroy())
            chartInstance = None
        )
      )
    )
