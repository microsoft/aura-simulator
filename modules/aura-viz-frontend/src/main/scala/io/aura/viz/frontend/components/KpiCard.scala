// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz.frontend.components

import com.raquo.laminar.api.L.*

/** Reusable KPI metric card component. */
object KpiCard:

  /** Render a KPI card with a label and reactive value.
    *
    * @param label
    *   Display label (e.g. "Completed", "Energy (Wh)")
    * @param value$
    *   Signal of the formatted value string
    * @param isError
    *   Whether to style as an error value (red)
    */
  def render(label: String, value$ : Signal[String], isError: Boolean = false): HtmlElement =
    div(
      cls := "kpi-card",
      span(cls := "kpi-label", label),
      span(
        cls := (if isError then "kpi-value error" else "kpi-value"),
        child.text <-- value$
      )
    )

  /** Render a KPI card with a static value. */
  def renderStatic(label: String, value: String, isError: Boolean = false): HtmlElement =
    div(
      cls := "kpi-card",
      span(cls := "kpi-label", label),
      span(
        cls := (if isError then "kpi-value error" else "kpi-value"),
        value
      )
    )
