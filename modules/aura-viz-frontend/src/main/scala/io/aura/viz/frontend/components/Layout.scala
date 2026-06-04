// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz.frontend.components

import com.raquo.laminar.api.L.*
import io.aura.viz.frontend.{Page, Router}
import io.aura.viz.frontend.model.SimulationData

/** App shell: header, nav sidebar, and content area. */
object Layout:

  /** Render the full application shell.
    *
    * @param data$
    *   Signal of loaded simulation data (None while loading)
    * @param content
    *   Content element signal based on current page
    */
  def render(data$ : Signal[Option[SimulationData]], content: Signal[HtmlElement]): HtmlElement =
    div(
      cls := "app-shell",
      renderHeader,
      div(
        cls := "app-body",
        renderSidebar(data$),
        mainTag(
          cls := "app-content",
          child <-- content
        )
      )
    )

  private def renderHeader: HtmlElement =
    headerTag(
      cls := "app-header",
      h1("Aura Dashboard"),
      span(cls := "header-subtitle", "Cloud Simulation Framework")
    )

  private def renderSidebar(data$ : Signal[Option[SimulationData]]): HtmlElement =
    navTag(
      cls := "app-sidebar",
      // Core pages (always shown)
      navSection(
        "Core",
        Vector(
          Page.Overview,
          Page.Workloads,
          Page.Energy,
          Page.Migrations,
          Page.Cost,
          Page.Faults
        )
      ),
      // Conditional pages based on data availability
      div(
        cls := "nav-section",
        child <-- data$.map {
          case Some(data) =>
            val conditionalPages = Vector(
              (Page.Serverless, data.hasServerlessData),
              (Page.Containers, data.hasContainerData),
              (Page.Edge, data.hasEdgeData),
              (Page.Federated, data.hasFederatedData)
            ).collect { case (page, true) => page }

            if conditionalPages.nonEmpty then
              div(
                div(cls := "nav-section-title", "Extensions"),
                conditionalPages.map(navItem)
              )
            else emptyNode
          case None => emptyNode
        }
      )
    )

  private def navSection(title: String, pages: Vector[Page]): HtmlElement =
    div(
      cls := "nav-section",
      div(cls := "nav-section-title", title),
      pages.map(navItem)
    )

  private def navItem(page: Page): HtmlElement =
    a(
      cls <-- Router.currentPage.signal.map(cp => if cp == page then "nav-item active" else "nav-item"),
      span(cls := "nav-icon", page.icon.take(2).toUpperCase),
      span(cls := "nav-label", page.title),
      onClick.preventDefault --> { _ => Router.navigateTo(page) },
      href := page.path
    )
