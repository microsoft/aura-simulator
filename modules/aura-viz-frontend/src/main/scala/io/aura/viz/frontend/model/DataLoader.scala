// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz.frontend.model

import org.scalajs.dom
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import upickle.default.*

/** Loads simulation data from JSON files via fetch API. */
object DataLoader:

  private def cacheBust: String = js.Date.now().toLong.toString

  /** Load full simulation results from data/results.json. */
  def loadResults(path: String = "data/results.json"): Future[SimulationData] =
    dom
      .fetch(path)
      .toFuture
      .flatMap(_.text().toFuture)
      .map(json => read[SimulationData](json))

  /** Load live snapshots from data/snapshots.json. */
  def loadSnapshots(path: String = "data/snapshots.json"): Future[Vector[MetricsSnapshot]] =
    dom
      .fetch(s"$path?$cacheBust")
      .toFuture
      .flatMap(_.text().toFuture)
      .map(json => read[Vector[MetricsSnapshot]](json))

  /** Load the latest snapshot from data/latest.json. */
  def loadLatest(path: String = "data/latest.json"): Future[MetricsSnapshot] =
    dom
      .fetch(s"$path?$cacheBust")
      .toFuture
      .flatMap(_.text().toFuture)
      .map(json => read[MetricsSnapshot](json))
