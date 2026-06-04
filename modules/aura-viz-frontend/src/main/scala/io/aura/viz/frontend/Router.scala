// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz.frontend

import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** Simple pushState SPA routing with Var[Page]. */
enum Page(val path: String, val title: String, val icon: String):
  case Overview   extends Page("/", "Overview", "dashboard")
  case Workloads  extends Page("/workloads", "Workloads", "task")
  case Energy     extends Page("/energy", "Energy", "bolt")
  case Migrations extends Page("/migrations", "Migrations", "swap")
  case Cost       extends Page("/cost", "Cost", "dollar")
  case Faults     extends Page("/faults", "Faults", "warning")
  case Serverless extends Page("/serverless", "Serverless", "cloud")
  case Containers extends Page("/containers", "Containers", "box")
  case Edge       extends Page("/edge", "Edge", "wifi")
  case Federated  extends Page("/federated", "Federated", "layers")

object Router:
  val currentPage: Var[Page] = Var(Page.Overview)

  /** Initialize routing: parse current URL and listen for popstate. */
  def init(): Unit =
    // Parse initial URL
    currentPage.set(pageFromPath(dom.window.location.pathname))

    // Listen for back/forward navigation
    dom.window.addEventListener(
      "popstate",
      (_: dom.Event) => currentPage.set(pageFromPath(dom.window.location.pathname))
    )

  /** Navigate to a page, updating browser history. */
  def navigateTo(page: Page): Unit =
    if currentPage.now() != page then
      dom.window.history.pushState(null, "", page.path)
      currentPage.set(page)

  /** Resolve a path to a Page. */
  private def pageFromPath(path: String): Page =
    Page.values.find(_.path == path).getOrElse(Page.Overview)
