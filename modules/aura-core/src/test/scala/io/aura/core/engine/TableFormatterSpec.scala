// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TableFormatterSpec extends AnyFlatSpec with Matchers:

  case class TestRow(name: String, value: Int, score: Double)

  val columns: Vector[Column[TestRow]] = Vector(
    Column("Name", format = _.name),
    Column("Value", align = Align.Right, format = _.value.toString),
    Column("Score", align = Align.Right, format = r => f"${r.score}%.2f")
  )

  val data: Vector[TestRow] = Vector(
    TestRow("Alice", 10, 95.5),
    TestRow("Bob", 20, 87.3),
    TestRow("Charlie", 15, 92.1)
  )

  "TableFormatter.format with Markdown" should "produce pipe-separated output" in {
    val result = TableFormatter.format("Test Table", data, columns, OutputFormat.Markdown)
    result should include("|")
    result should include("Name")
    result should include("Alice")
    result should include("---")
  }

  "TableFormatter.format with CSV" should "produce comma-separated output" in {
    val result = TableFormatter.format("Test Table", data, columns, OutputFormat.CSV)
    result should include(",")
    result should include("Name,Value,Score")
    result should include("Alice")
    result should not include "|"
  }

  it should "quote values containing commas" in {
    val dataWithComma = Vector(TestRow("Alice, Jr.", 10, 95.5))
    val result        = TableFormatter.format("", dataWithComma, columns, OutputFormat.CSV)
    result should include("\"Alice, Jr.\"")
  }

  "TableFormatter.format with HTML" should "produce valid HTML table" in {
    val result = TableFormatter.format("Test Table", data, columns, OutputFormat.HTML)
    result should include("<table>")
    result should include("</table>")
    result should include("<thead>")
    result should include("<tbody>")
    result should include("<th>Name</th>")
    result should include("<td>Alice</td>")
    result should include("<caption>Test Table</caption>")
  }

  it should "escape HTML entities" in {
    val dataWithHtml = Vector(TestRow("<script>", 1, 1.0))
    val result       = TableFormatter.format("", dataWithHtml, columns, OutputFormat.HTML)
    result should include("&lt;script&gt;")
    result should not include "<script>"
  }

  "TableFormatter.format with LaTeX" should "produce valid tabular environment" in {
    val result = TableFormatter.format("Test Table", data, columns, OutputFormat.LaTeX)
    result should include("\\begin{tabular}")
    result should include("\\end{tabular}")
    result should include("\\begin{table}")
    result should include("\\hline")
    result should include("\\caption{Test Table}")
    result should include("&")
    result should include("\\\\")
  }

  it should "escape LaTeX special characters" in {
    val dataWithSpecial = Vector(TestRow("test_name", 1, 1.0))
    val result          = TableFormatter.format("", dataWithSpecial, columns, OutputFormat.LaTeX)
    result should include("test\\_name")
  }

  "TableFormatter.format with Text" should "produce readable text table" in {
    val result = TableFormatter.format("Test Table", data, columns, OutputFormat.Text)
    result should include("Test Table")
    result should include("===")
    result should include("Name")
    result should include("Alice")
    result should include("---")
  }

  "TableFormatter" should "handle empty data" in {
    val result = TableFormatter.format("Empty", Vector.empty[TestRow], columns, OutputFormat.Markdown)
    result should include("Name")
    result should not include "Alice"
  }

  it should "handle empty title" in {
    val result = TableFormatter.format("", data, columns, OutputFormat.Markdown)
    result should include("Name")
  }
