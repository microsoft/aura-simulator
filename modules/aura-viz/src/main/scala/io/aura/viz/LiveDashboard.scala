// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz

import io.aura.core.engine.SimulationResults
import java.nio.file.{Files, Path}

/** Generates a live auto-refreshing HTML dashboard.
  *
  * Unlike ReportGenerator which creates a static report, LiveDashboard generates an HTML page that auto-refreshes from
  * a JSON data file, with interactive Chart.js visualizations. The data file can be updated during simulation to
  * provide live monitoring.
  *
  * Usage:
  * {{{
  *   val dashboard = LiveDashboard.create(outputDir)
  *   // During simulation, periodically update:
  *   LiveDashboard.updateData(outputDir, monitor)
  *   // Open dashboard/index.html in browser for live view
  * }}}
  */
object LiveDashboard:

  /** Create the live dashboard HTML + JS + CSS files.
    *
    * @param outputDir
    *   Directory to write dashboard files to
    * @param refreshIntervalMs
    *   Auto-refresh interval in milliseconds
    * @return
    *   Path to the generated index.html
    */
  def create(outputDir: Path, refreshIntervalMs: Int = 2000): Path =
    Files.createDirectories(outputDir)
    Files.createDirectories(outputDir.resolve("data"))
    Files.createDirectories(outputDir.resolve("js"))
    Files.createDirectories(outputDir.resolve("css"))

    Files.writeString(outputDir.resolve("index.html"), generateHtml(refreshIntervalMs))
    writeAppJs(outputDir)
    writeAppCss(outputDir)

    // Write initial empty data
    Files.writeString(outputDir.resolve("data/snapshots.json"), "[]")
    Files.writeString(outputDir.resolve("data/latest.json"), MetricsSnapshot.empty.toJSON)

    outputDir.resolve("index.html")

  /** Update the dashboard data files from a SimulationMonitor.
    *
    * Call this periodically during simulation to keep the dashboard live.
    */
  def updateData(outputDir: Path, monitor: SimulationMonitor): Unit =
    val dataDir = outputDir.resolve("data")
    Files.createDirectories(dataDir)
    Files.writeString(dataDir.resolve("snapshots.json"), monitor.toJSON)
    monitor.latest.foreach { snap =>
      Files.writeString(dataDir.resolve("latest.json"), snap.toJSON)
    }

  /** Update from final simulation results. */
  def updateResults(outputDir: Path, results: SimulationResults): Unit =
    val dataDir = outputDir.resolve("data")
    Files.createDirectories(dataDir)
    Files.writeString(dataDir.resolve("results.json"), results.toJSON)

  /** Generate the main HTML page content. */
  def generateHtml(refreshIntervalMs: Int = 2000): String =
    s"""<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Aura Live Dashboard</title>
  <link rel="stylesheet" href="css/dashboard.css">
  <script src="https://cdn.jsdelivr.net/npm/chart.js@4.4.0/dist/chart.umd.min.js"></script>
</head>
<body>
  <div id="app"></div>
  <script src="js/app.js"></script>
</body>
</html>"""

  /** Write the Scala.js compiled app bundle to the output directory.
    *
    * Loads from classpath resource (embedded by sbt resource generator). Falls back to inline JS if the bundle is not
    * found.
    */
  private def writeAppJs(outputDir: Path): Unit =
    val bundlePath = outputDir.resolve("js/app.js")
    val resource   = Option(getClass.getResourceAsStream("/dashboard/js/app.js"))
    resource match
      case Some(is) =>
        try Files.write(bundlePath, is.readAllBytes())
        finally is.close()
      case None =>
        // Fallback: write a minimal script that loads data and renders basic dashboard
        Files.writeString(bundlePath, generateFallbackJs)

  /** Write the dashboard CSS to the output directory.
    *
    * Loads from classpath resource (embedded by sbt resource generator). Falls back to inline CSS if not found.
    */
  private def writeAppCss(outputDir: Path): Unit =
    val cssPath  = outputDir.resolve("css/dashboard.css")
    val resource = Option(getClass.getResourceAsStream("/css/dashboard.css"))
    resource match
      case Some(is) =>
        try Files.write(cssPath, is.readAllBytes())
        finally is.close()
      case None =>
        Files.writeString(cssPath, generateFallbackCss)

  /** Fallback JS when Scala.js bundle is not available. */
  private def generateFallbackJs: String =
    """// Aura Dashboard - Fallback (Scala.js bundle not found)
document.addEventListener('DOMContentLoaded', function() {
  var app = document.getElementById('app');
  app.innerHTML = '<div style="padding:2rem;text-align:center;color:#8b949e;">' +
    '<h1 style="color:#e6edf3;">Aura Dashboard</h1>' +
    '<p>Loading simulation data...</p>' +
    '<p id="status">Fetching results.json...</p></div>';

  fetch('data/results.json')
    .then(function(r) { return r.json(); })
    .then(function(data) {
      document.getElementById('status').textContent =
        'Loaded: ' + (data.workloadResults || []).length + ' workloads, ' +
        'Energy: ' + (data.totalEnergyWh || 0).toFixed(2) + ' Wh, ' +
        'Cost: $' + (data.totalCost || 0).toFixed(4);
    })
    .catch(function(err) {
      document.getElementById('status').textContent = 'Waiting for data... (' + err.message + ')';
    });

  // Poll for live snapshots
  setInterval(function() {
    fetch('data/snapshots.json?' + Date.now())
      .then(function(r) { return r.json(); })
      .then(function(snapshots) {
        if (snapshots.length > 0) {
          var latest = snapshots[snapshots.length - 1];
          document.getElementById('status').textContent =
            'Live: ' + latest.completedWorkloads + ' completed, ' +
            latest.totalEnergyWh.toFixed(2) + ' Wh, $' + latest.totalCost.toFixed(4);
        }
      }).catch(function() {});
  }, 2000);
});"""

  /** Fallback CSS when Scala.js bundle CSS is not available. */
  private def generateFallbackCss: String =
    """:root {
  --bg: #0d1117;
  --surface: #161b22;
  --border: #30363d;
  --primary: #58a6ff;
  --accent: #f85149;
  --success: #3fb950;
  --text: #e6edf3;
  --text-muted: #8b949e;
}
* { margin: 0; padding: 0; box-sizing: border-box; }
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; background: var(--bg); color: var(--text); min-height: 100vh; }
"""
