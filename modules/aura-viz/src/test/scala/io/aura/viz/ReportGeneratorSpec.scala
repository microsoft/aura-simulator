// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.engine.SimulationResults
import io.aura.core.types.*
import java.nio.file.{Files, Path}

class ReportGeneratorSpec extends AnyFlatSpec with Matchers:

  "ReportGenerator.generate" should "create output directory" in {
    val outputDir = Files.createTempDirectory("aura-report-test")
    try
      val results = SimulationResults.empty.copy(
        simulationEndTime = SimTime(1000.0)
      )
      val result = ReportGenerator.generate(results, outputDir)
      result shouldBe outputDir
      Files.exists(outputDir) shouldBe true
    finally deleteRecursive(outputDir)
  }

  it should "create index.html" in {
    val outputDir = Files.createTempDirectory("aura-report-test")
    try
      val results = SimulationResults.empty
      ReportGenerator.generate(results, outputDir)

      val indexHtml = outputDir.resolve("index.html")
      Files.exists(indexHtml) shouldBe true

      val content = Files.readString(indexHtml)
      content should include("Aura Simulation Report")
      content should include("<!DOCTYPE html>")
      content should include("""<div id="app">""")
    finally deleteRecursive(outputDir)
  }

  it should "create results.json" in {
    val outputDir = Files.createTempDirectory("aura-report-test")
    try
      val results = SimulationResults.empty.copy(
        simulationEndTime = SimTime(500.0)
      )
      ReportGenerator.generate(results, outputDir)

      val resultsJson = outputDir.resolve("data").resolve("results.json")
      Files.exists(resultsJson) shouldBe true

      val content = Files.readString(resultsJson)
      content should include("simulationEndTime")
      content should include("500.0")
    finally deleteRecursive(outputDir)
  }

  it should "create js and css directories" in {
    val outputDir = Files.createTempDirectory("aura-report-test")
    try
      val results = SimulationResults.empty
      ReportGenerator.generate(results, outputDir)

      Files.exists(outputDir.resolve("js").resolve("app.js")) shouldBe true
      Files.exists(outputDir.resolve("css").resolve("dashboard.css")) shouldBe true
    finally deleteRecursive(outputDir)
  }

  it should "include Chart.js CDN in HTML" in {
    val outputDir = Files.createTempDirectory("aura-report-test")
    try
      val results = SimulationResults.empty
      ReportGenerator.generate(results, outputDir)

      val indexContent = Files.readString(outputDir.resolve("index.html"))
      indexContent should include("chart.js@4.4.0")
      indexContent should include("""<div id="app">""")
      indexContent should include("js/app.js")
      indexContent should include("css/dashboard.css")
    finally deleteRecursive(outputDir)
  }

  it should "write JS and CSS files" in {
    val outputDir = Files.createTempDirectory("aura-report-test")
    try
      val results = SimulationResults.empty
      ReportGenerator.generate(results, outputDir)

      val jsContent = Files.readString(outputDir.resolve("js").resolve("app.js"))
      jsContent.nonEmpty shouldBe true

      val cssContent = Files.readString(outputDir.resolve("css").resolve("dashboard.css"))
      cssContent should include("--bg")
    finally deleteRecursive(outputDir)
  }

  private def deleteRecursive(path: Path): Unit =
    if Files.isDirectory(path) then
      val stream = Files.list(path)
      try
        stream.forEach(p => deleteRecursive(p))
      finally
        stream.close()
    Files.deleteIfExists(path)
