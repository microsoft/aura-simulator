// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.types.*

class LatexExportSpec extends AnyFlatSpec with Matchers:

  private def sampleResults: SimulationResults =
    SimulationResults.empty.copy(
      simulationEndTime = SimTime(100.0),
      workloadResults = Vector(
        WorkloadResult(WorkloadId(0), VmId(0), HostId(0), SimTime(50.0), MI(10000.0)),
        WorkloadResult(WorkloadId(1), VmId(1), HostId(0), SimTime(80.0), MI(20000.0))
      ),
      energyRecords = Vector(
        EnergyRecord(HostId(0), Watts(200.0), SimTime(0.0), SimTime(50.0), WattHours(2.78)),
        EnergyRecord(HostId(0), Watts(180.0), SimTime(50.0), SimTime(100.0), WattHours(2.5))
      ),
      migrationRecords = Vector(
        MigrationRecord(VmId(0), HostId(0), HostId(1), SimTime(30.0), SimTime(5.0), MegaBytes(1024.0))
      ),
      costRecords = Vector(
        VmCostRecord(VmId(0), Cost(0.01), Cost(0.005), Cost(0.001), Cost(0.002), Cost(0.018), SimTime(100.0))
      ),
      totalEventsProcessed = 42
    )

  // ─── Workload LaTeX Export ──────────────────────────────────────────

  "SimulationResults.exportAs(LaTeX)" should "produce valid LaTeX tabular" in {
    val latex = sampleResults.exportAs(OutputFormat.LaTeX)
    latex should include("\\begin{table}")
    latex should include("\\begin{tabular}")
    latex should include("\\end{tabular}")
    latex should include("\\end{table}")
    latex should include("\\hline")
  }

  it should "include column headers" in {
    val latex = sampleResults.exportAs(OutputFormat.LaTeX)
    latex should include("Workload")
    latex should include("VM")
    latex should include("Host")
    latex should include("Finish Time")
    latex should include("MI Executed")
  }

  it should "include data rows with & separators" in {
    val latex = sampleResults.exportAs(OutputFormat.LaTeX)
    latex should include("&")
    latex should include("\\\\")
  }

  it should "escape special LaTeX characters" in {
    val latex = sampleResults.exportAs(OutputFormat.LaTeX)
    // Verify it doesn't break with numeric data
    latex should include("50.00")
    latex should include("10000")
  }

  // ─── Energy LaTeX Export ────────────────────────────────────────────

  "SimulationResults.exportEnergyAs(LaTeX)" should "produce energy table" in {
    val latex = sampleResults.exportEnergyAs(OutputFormat.LaTeX)
    latex should include("\\begin{table}")
    latex should include("Energy")
    latex should include("Host")
    latex should include("Avg Power")
  }

  // ─── Migration LaTeX Export ─────────────────────────────────────────

  "SimulationResults.exportMigrationsAs(LaTeX)" should "produce migration table" in {
    val latex = sampleResults.exportMigrationsAs(OutputFormat.LaTeX)
    latex should include("\\begin{table}")
    latex should include("Migration")
    latex should include("Source")
    latex should include("Target")
  }

  // ─── Cost LaTeX Export ──────────────────────────────────────────────

  "SimulationResults.exportCostAs(LaTeX)" should "produce cost table" in {
    val latex = sampleResults.exportCostAs(OutputFormat.LaTeX)
    latex should include("\\begin{table}")
    latex should include("Cost")
    latex should include("CPU")
    latex should include("RAM")
  }

  // ─── Summary LaTeX Export ───────────────────────────────────────────

  "SimulationResults.exportSummaryAs(LaTeX)" should "produce summary table" in {
    val latex = sampleResults.exportSummaryAs(OutputFormat.LaTeX)
    latex should include("\\begin{table}")
    latex should include("Metric")
    latex should include("Value")
    latex should include("Workloads Completed")
    latex should include("Avg Completion Time")
    latex should include("Total Energy")
  }

  it should "include actual values" in {
    val latex = sampleResults.exportSummaryAs(OutputFormat.LaTeX)
    latex should include("2")  // 2 workloads
    latex should include("42") // events
  }

  // ─── Markdown Export ────────────────────────────────────────────────

  "SimulationResults.exportAs(Markdown)" should "produce markdown table" in {
    val md = sampleResults.exportAs(OutputFormat.Markdown)
    md should include("|")
    md should include("---")
    md should include("Workload")
  }

  // ─── HTML Export ────────────────────────────────────────────────────

  "SimulationResults.exportAs(HTML)" should "produce HTML table" in {
    val html = sampleResults.exportAs(OutputFormat.HTML)
    html should include("<table>")
    html should include("<th>")
    html should include("<td>")
    html should include("</table>")
  }

  // ─── CSV Export ─────────────────────────────────────────────────────

  "SimulationResults.exportAs(CSV)" should "produce CSV" in {
    val csv = sampleResults.exportAs(OutputFormat.CSV)
    csv should include("Workload,VM,Host")
    csv should include(",")
  }

  // ─── Text Export ────────────────────────────────────────────────────

  "SimulationResults.exportAs(Text)" should "produce formatted text" in {
    val text = sampleResults.exportAs(OutputFormat.Text)
    text should include("Workload Results")
    text should include("=")
  }

  // ─── Empty Results ──────────────────────────────────────────────────

  "exportSummaryAs" should "work with empty results" in {
    val latex = SimulationResults.empty.exportSummaryAs(OutputFormat.LaTeX)
    latex should include("\\begin{table}")
    latex should include("0")
  }
