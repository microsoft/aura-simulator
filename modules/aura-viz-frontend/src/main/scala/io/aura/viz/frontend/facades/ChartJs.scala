// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz.frontend.facades

import org.scalajs.dom.HTMLCanvasElement
import scala.scalajs.js
import scala.scalajs.js.annotation.*

/** Minimal typed Chart.js facade for Chart.js 4.x loaded via CDN. */
@js.native
@JSGlobal("Chart")
class Chart(@annotation.unused ctx: HTMLCanvasElement, @annotation.unused config: js.Dynamic) extends js.Object:
  def update(mode: js.UndefOr[String] = js.undefined): Unit = js.native
  def destroy(): Unit                                       = js.native
  var data: js.Dynamic                                      = js.native
  var options: js.Dynamic                                   = js.native

/** Helper builders for creating Chart.js configuration objects. */
object ChartHelpers:
  import js.Dynamic.literal

  private val gridColor   = "rgba(255,255,255,0.06)"
  private val tickColor   = "#999"
  private val legendColor = "#eee"

  /** Common chart options with dark theme. */
  def commonOptions(xLabel: String = "", yLabel: String = ""): js.Dynamic =
    literal(
      responsive = true,
      maintainAspectRatio = false,
      animation = literal(duration = 300),
      plugins = literal(
        legend = literal(labels = literal(color = legendColor))
      ),
      scales = literal(
        x = literal(
          ticks = literal(color = tickColor),
          grid = literal(color = gridColor),
          title = literal(display = xLabel.nonEmpty, text = xLabel, color = tickColor)
        ),
        y = literal(
          beginAtZero = true,
          ticks = literal(color = tickColor),
          grid = literal(color = gridColor),
          title = literal(display = yLabel.nonEmpty, text = yLabel, color = tickColor)
        )
      )
    )

  /** Scatter chart options with dark theme. */
  def scatterOptions(xLabel: String, yLabel: String): js.Dynamic =
    literal(
      responsive = true,
      maintainAspectRatio = false,
      animation = literal(duration = 300),
      plugins = literal(
        legend = literal(labels = literal(color = legendColor))
      ),
      scales = literal(
        x = literal(
          `type` = "linear",
          ticks = literal(color = tickColor),
          grid = literal(color = gridColor),
          title = literal(display = true, text = xLabel, color = tickColor)
        ),
        y = literal(
          beginAtZero = true,
          ticks = literal(color = tickColor),
          grid = literal(color = gridColor),
          title = literal(display = true, text = yLabel, color = tickColor)
        )
      )
    )

  /** Dual Y-axis options (left = y, right = y1). */
  def dualAxisOptions(xLabel: String, yLeftLabel: String, yRightLabel: String): js.Dynamic =
    literal(
      responsive = true,
      maintainAspectRatio = false,
      animation = literal(duration = 300),
      plugins = literal(
        legend = literal(labels = literal(color = legendColor))
      ),
      scales = literal(
        x = literal(
          ticks = literal(color = tickColor),
          grid = literal(color = gridColor),
          title = literal(display = xLabel.nonEmpty, text = xLabel, color = tickColor)
        ),
        y = literal(
          position = "left",
          beginAtZero = true,
          ticks = literal(color = tickColor),
          grid = literal(color = gridColor),
          title = literal(display = true, text = yLeftLabel, color = tickColor)
        ),
        y1 = literal(
          position = "right",
          beginAtZero = true,
          ticks = literal(color = tickColor),
          grid = literal(drawOnChartArea = false),
          title = literal(display = true, text = yRightLabel, color = tickColor)
        )
      )
    )

  /** Stacked bar chart options. */
  def stackedBarOptions(xLabel: String, yLabel: String): js.Dynamic =
    literal(
      responsive = true,
      maintainAspectRatio = false,
      animation = literal(duration = 300),
      plugins = literal(
        legend = literal(labels = literal(color = legendColor))
      ),
      scales = literal(
        x = literal(
          stacked = true,
          ticks = literal(color = tickColor),
          grid = literal(color = gridColor),
          title = literal(display = xLabel.nonEmpty, text = xLabel, color = tickColor)
        ),
        y = literal(
          stacked = true,
          beginAtZero = true,
          ticks = literal(color = tickColor),
          grid = literal(color = gridColor),
          title = literal(display = true, text = yLabel, color = tickColor)
        )
      )
    )

  /** Doughnut chart options. */
  def doughnutOptions: js.Dynamic =
    literal(
      responsive = true,
      maintainAspectRatio = false,
      animation = literal(duration = 300),
      plugins = literal(
        legend = literal(
          position = "right",
          labels = literal(color = legendColor)
        )
      )
    )

  /** Create a line dataset. */
  def lineDataset(
      label: String,
      data: js.Array[js.Any],
      color: String,
      fill: Boolean = false,
      yAxisID: String = "y"
  ): js.Dynamic =
    literal(
      label = label,
      data = data,
      borderColor = color,
      backgroundColor = if fill then s"${color}1A" else "transparent",
      fill = fill,
      tension = 0.3,
      yAxisID = yAxisID
    )

  /** Create a bar dataset. */
  def barDataset(label: String, data: js.Array[js.Any], color: String, yAxisID: String = "y"): js.Dynamic =
    literal(
      label = label,
      data = data,
      backgroundColor = s"${color}B3",
      yAxisID = yAxisID
    )

  /** Create a scatter dataset. */
  def scatterDataset(label: String, data: js.Array[js.Any], color: String): js.Dynamic =
    literal(
      label = label,
      data = data,
      backgroundColor = s"${color}CC",
      borderColor = color,
      pointRadius = 5
    )

  /** Chart.js configuration object. */
  def chartConfig(chartType: String, data: js.Dynamic, options: js.Dynamic): js.Dynamic =
    literal(
      `type` = chartType,
      data = data,
      options = options
    )

  /** Chart.js data object with labels and datasets. */
  def chartData(labels: js.Array[String], datasets: js.Array[js.Dynamic]): js.Dynamic =
    literal(labels = labels, datasets = datasets)

  /** Chart.js data object without labels (for scatter/linear). */
  def chartDataNoLabels(datasets: js.Array[js.Dynamic]): js.Dynamic =
    literal(datasets = datasets)

  /** Palette of chart colors. */
  val palette: Vector[String] = Vector(
    "#4fc3f7",
    "#e94560",
    "#66bb6a",
    "#f9d423",
    "#ab47bc",
    "#ff7043",
    "#4ecdc4",
    "#a78bfa",
    "#fb923c",
    "#38bdf8"
  )
