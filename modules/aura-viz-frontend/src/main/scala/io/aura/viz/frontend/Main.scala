// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz.frontend

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import io.aura.viz.frontend.model.{DataLoader, MetricsSnapshot, SimulationData}
import io.aura.viz.frontend.components.Layout
import io.aura.viz.frontend.pages.*
import scala.concurrent.ExecutionContext.Implicits.global

object Main:

  val dataVar: Var[Option[SimulationData]]       = Var(None)
  val errorVar: Var[Option[String]]              = Var(None)
  val snapshotsVar: Var[Vector[MetricsSnapshot]] = Var(Vector.empty)
  val liveActiveVar: Var[Boolean]                = Var(false)

  private val pollIntervalMs = 2000

  def main(args: Array[String]): Unit =
    // Initialize router
    Router.init()

    // Load simulation data
    DataLoader.loadResults().foreach { data =>
      dataVar.set(Some(data))
    }
    DataLoader.loadResults().failed.foreach { _ =>
      // results.json not available yet — start in live polling mode
      startLivePolling()
    }

    // Probe for snapshots.json to detect live mode
    DataLoader.loadSnapshots().foreach { snapshots =>
      if snapshots.nonEmpty then
        snapshotsVar.set(snapshots)
        startLivePolling()
    }

    // Render the app
    val appElement = renderApp()
    val container  = dom.document.getElementById("app")
    com.raquo.laminar.api.L.render(container, appElement)

  /** Start periodic polling of snapshots.json for live dashboard updates. */
  private def startLivePolling(): Unit =
    if !liveActiveVar.now() then
      liveActiveVar.set(true)
      dom.window.setInterval(
        () => pollSnapshots(),
        pollIntervalMs.toDouble
      )

  private def pollSnapshots(): Unit =
    DataLoader.loadSnapshots().foreach { snapshots =>
      if snapshots.nonEmpty then snapshotsVar.set(snapshots)
    }
    // Also try loading results.json (simulation may have completed)
    if dataVar.now().isEmpty then
      DataLoader.loadResults().foreach { data =>
        dataVar.set(Some(data))
        errorVar.set(None)
      }

  private def renderApp(): HtmlElement =
    val content$ = Router.currentPage.signal.combineWith(dataVar.signal, errorVar.signal).map {
      case (_, _, Some(err)) => renderError(err)
      case (_, None, _)      =>
        // No results yet — show live overview if we have snapshots
        OverviewPage.renderLive(snapshotsVar.signal, liveActiveVar.signal)
      case (page, Some(data), _) =>
        page match
          case Page.Overview   => OverviewPage.render(data, snapshotsVar.signal, liveActiveVar.signal)
          case Page.Workloads  => WorkloadsPage.render(data)
          case Page.Energy     => EnergyPage.render(data)
          case Page.Migrations => MigrationsPage.render(data)
          case Page.Cost       => CostPage.render(data)
          case Page.Faults     => FaultsPage.render(data)
          case Page.Serverless => ServerlessPage.render(data)
          case Page.Containers => ContainersPage.render(data)
          case Page.Edge       => EdgePage.render(data)
          case Page.Federated  => FederatedPage.render(data)
    }

    Layout.render(dataVar.signal, content$)

  private def renderError(msg: String): HtmlElement =
    div(cls := "error-state", msg)
