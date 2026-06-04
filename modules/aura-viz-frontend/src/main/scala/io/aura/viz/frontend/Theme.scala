// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz.frontend

/** Dark theme color constants matching the CSS custom properties. */
object Theme:
  val bg: String        = "#0d1117"
  val surface: String   = "#161b22"
  val border: String    = "#30363d"
  val primary: String   = "#58a6ff"
  val accent: String    = "#f85149"
  val success: String   = "#3fb950"
  val warning: String   = "#f9d423"
  val text: String      = "#e6edf3"
  val textMuted: String = "#8b949e"

  /** Chart color palette. */
  val chartColors: Vector[String] = Vector(
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
