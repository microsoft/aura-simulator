// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.types.*
import io.aura.core.engine.{SimulationResults, WorkloadResult}
import java.nio.file.{Files, Path}

class LiveDashboardSpec extends AnyFlatSpec with Matchers:

  private def withTempDir(test: Path => Unit): Unit =
    val dir = Files.createTempDirectory("aura-live-test")
    try test(dir)
    finally deleteRecursive(dir)

  private def deleteRecursive(path: Path): Unit =
    if Files.isDirectory(path) then
      val stream = Files.list(path)
      try stream.forEach(p => deleteRecursive(p))
      finally stream.close()
    Files.deleteIfExists(path)

  // ─── create ─────────────────────────────────────────────────────────

  "LiveDashboard.create" should "create index.html" in withTempDir { dir =>
    val indexPath = LiveDashboard.create(dir)
    Files.exists(indexPath) shouldBe true
    val content = Files.readString(indexPath)
    content should include("Aura Live Dashboard")
    content should include("chart.js")
  }

  it should "create JS and CSS files" in withTempDir { dir =>
    LiveDashboard.create(dir)
    Files.exists(dir.resolve("js/app.js")) shouldBe true
    Files.exists(dir.resolve("css/dashboard.css")) shouldBe true
  }

  it should "create initial data files" in withTempDir { dir =>
    LiveDashboard.create(dir)
    Files.exists(dir.resolve("data/snapshots.json")) shouldBe true
    Files.exists(dir.resolve("data/latest.json")) shouldBe true
    Files.readString(dir.resolve("data/snapshots.json")) shouldBe "[]"
  }

  it should "use custom refresh interval" in withTempDir { dir =>
    LiveDashboard.create(dir, refreshIntervalMs = 5000)
    val html = Files.readString(dir.resolve("index.html"))
    html should include("Aura Live Dashboard")
  }

  // ─── updateData ─────────────────────────────────────────────────────

  "LiveDashboard.updateData" should "write snapshots JSON" in withTempDir { dir =>
    LiveDashboard.create(dir)
    val monitor = SimulationMonitor.empty
      .record(SimulationResults.empty.copy(simulationEndTime = SimTime(10.0)))
    LiveDashboard.updateData(dir, monitor)
    val content = Files.readString(dir.resolve("data/snapshots.json"))
    content should include("\"timestamp\": 10.0")
  }

  it should "write latest snapshot" in withTempDir { dir =>
    LiveDashboard.create(dir)
    val results = SimulationResults.empty.copy(
      simulationEndTime = SimTime(50.0),
      workloadResults = Vector(
        WorkloadResult(WorkloadId(0), VmId(0), HostId(0), SimTime(50.0), MI(1000.0))
      )
    )
    val monitor = SimulationMonitor.empty.record(results)
    LiveDashboard.updateData(dir, monitor)
    val latest = Files.readString(dir.resolve("data/latest.json"))
    latest should include("\"completedWorkloads\": 1")
  }

  // ─── updateResults ──────────────────────────────────────────────────

  "LiveDashboard.updateResults" should "write results JSON" in withTempDir { dir =>
    LiveDashboard.create(dir)
    val results = SimulationResults.empty.copy(simulationEndTime = SimTime(100.0))
    LiveDashboard.updateResults(dir, results)
    Files.exists(dir.resolve("data/results.json")) shouldBe true
    val content = Files.readString(dir.resolve("data/results.json"))
    content should include("simulationEndTime")
  }

  // ─── generateHtml ───────────────────────────────────────────────────

  "LiveDashboard.generateHtml" should "include app container and Chart.js CDN" in {
    val html = LiveDashboard.generateHtml()
    html should include("""<div id="app">""")
    html should include("chart.js@4.4.0")
    html should include("js/app.js")
    html should include("css/dashboard.css")
  }

  it should "include Aura title" in {
    val html = LiveDashboard.generateHtml()
    html should include("Aura Live Dashboard")
  }

  // ─── created files content ─────────────────────────────────────────

  "LiveDashboard files" should "write JS file (fallback or bundle)" in withTempDir { dir =>
    LiveDashboard.create(dir)
    val js = Files.readString(dir.resolve("js/app.js"))
    js.nonEmpty shouldBe true
  }

  it should "write CSS file (fallback or bundle)" in withTempDir { dir =>
    LiveDashboard.create(dir)
    val css = Files.readString(dir.resolve("css/dashboard.css"))
    css should include("--bg")
  }
