// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz

import io.aura.core.engine.SimulationResults
import java.nio.file.{Files, Path}

/** Generates a self-contained HTML report directory from simulation results.
  *
  * Pattern inspired by Gatling's report generator: zero runtime dependencies, bundled Chart.js, static HTML that opens
  * in any browser.
  */
object ReportGenerator:
  /** Generate a self-contained HTML report directory. */
  def generate(results: SimulationResults, outputDir: Path): Path =
    Files.createDirectories(outputDir)

    // Write results as JSON data
    val dataDir = outputDir.resolve("data")
    Files.createDirectories(dataDir)
    Files.writeString(dataDir.resolve("results.json"), results.toJSON)

    // Generate index.html with Scala.js app bundle
    val jsDir  = outputDir.resolve("js")
    val cssDir = outputDir.resolve("css")
    Files.createDirectories(jsDir)
    Files.createDirectories(cssDir)

    Files.writeString(outputDir.resolve("index.html"), generateIndexHtml)
    writeAppJs(jsDir)
    writeAppCss(cssDir)

    outputDir

  private def generateIndexHtml: String =
    """<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Aura Simulation Report</title>
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
  private def writeAppJs(jsDir: Path): Unit =
    val bundlePath = jsDir.resolve("app.js")
    val resource   = Option(getClass.getResourceAsStream("/dashboard/js/app.js"))
    resource match
      case Some(is) =>
        try Files.write(bundlePath, is.readAllBytes())
        finally is.close()
      case None =>
        // Fallback: write basic chart rendering JS
        Files.writeString(bundlePath, generateFallbackChartsJs)

  /** Write the dashboard CSS to the output directory. */
  private def writeAppCss(cssDir: Path): Unit =
    val cssPath  = cssDir.resolve("dashboard.css")
    val resource = Option(getClass.getResourceAsStream("/css/dashboard.css"))
    resource match
      case Some(is) =>
        try Files.write(cssPath, is.readAllBytes())
        finally is.close()
      case None =>
        Files.writeString(cssPath, generateFallbackStyleCss)

  /** Fallback JS when Scala.js bundle is not available. */
  private def generateFallbackChartsJs: String =
    """// Aura Report - Fallback (Scala.js bundle not found)
var chartDefaults = {
  responsive: true,
  plugins: { legend: { labels: { color: '#eee' } } },
  scales: {
    x: { ticks: { color: '#999' }, grid: { color: 'rgba(255,255,255,0.06)' } },
    y: { ticks: { color: '#999' }, grid: { color: 'rgba(255,255,255,0.06)' } }
  }
};

document.addEventListener('DOMContentLoaded', function() {
  var app = document.getElementById('app');
  app.innerHTML = '<div style="padding:2rem;text-align:center;color:#8b949e;">' +
    '<h1 style="color:#e6edf3;">Aura Simulation Report</h1>' +
    '<p id="status">Loading results...</p></div>';

  fetch('data/results.json')
    .then(function(r) { return r.json(); })
    .then(function(data) {
      var html = '<header style="padding:2rem;text-align:center;border-bottom:1px solid #30363d;">' +
        '<h1 style="color:#e6edf3;font-size:1.4rem;">Aura Simulation Report</h1>' +
        '<div style="display:flex;gap:1rem;justify-content:center;flex-wrap:wrap;margin-top:1rem;">';
      html += '<div style="background:#161b22;padding:0.8rem 1.2rem;border-radius:8px;border:1px solid #30363d;">' +
        '<div style="font-size:0.7rem;color:#8b949e;text-transform:uppercase;">Workloads</div>' +
        '<div style="font-size:1.3rem;font-weight:700;color:#58a6ff;">' + (data.workloadResults || []).length + '</div></div>';
      html += '<div style="background:#161b22;padding:0.8rem 1.2rem;border-radius:8px;border:1px solid #30363d;">' +
        '<div style="font-size:0.7rem;color:#8b949e;text-transform:uppercase;">Energy</div>' +
        '<div style="font-size:1.3rem;font-weight:700;color:#58a6ff;">' + (data.totalEnergyWh || 0).toFixed(2) + ' Wh</div></div>';
      html += '<div style="background:#161b22;padding:0.8rem 1.2rem;border-radius:8px;border:1px solid #30363d;">' +
        '<div style="font-size:0.7rem;color:#8b949e;text-transform:uppercase;">Cost</div>' +
        '<div style="font-size:1.3rem;font-weight:700;color:#58a6ff;">$' + (data.totalCost || 0).toFixed(4) + '</div></div>';
      html += '<div style="background:#161b22;padding:0.8rem 1.2rem;border-radius:8px;border:1px solid #30363d;">' +
        '<div style="font-size:0.7rem;color:#8b949e;text-transform:uppercase;">Events</div>' +
        '<div style="font-size:1.3rem;font-weight:700;color:#58a6ff;">' + (data.totalEventsProcessed || 0) + '</div></div>';
      html += '</div></header>';
      html += '<p style="text-align:center;padding:2rem;color:#8b949e;">Full dashboard requires Scala.js bundle. Run: sbt auraVizFrontend/fullLinkJS</p>';
      app.innerHTML = html;
    })
    .catch(function(err) {
      document.getElementById('status').textContent = 'Failed to load results: ' + err.message;
    });
});"""

  /** Fallback CSS when Scala.js bundle CSS is not available. */
  private def generateFallbackStyleCss: String =
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
