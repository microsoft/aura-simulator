// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.engine

/** Column definition for table formatting. */
final case class Column[T](
    header: String,
    width: Int = 0,
    align: Align = Align.Left,
    format: T => String
)

enum Align:
  case Left, Right, Center

enum OutputFormat:
  case Markdown, CSV, HTML, LaTeX, Text

/** Generic table formatter producing multi-format output. */
object TableFormatter:
  def format[T](
      title: String,
      data: Vector[T],
      columns: Vector[Column[T]],
      outputFormat: OutputFormat
  ): String =
    outputFormat match
      case OutputFormat.Markdown => formatMarkdown(title, data, columns)
      case OutputFormat.CSV      => formatCSV(data, columns)
      case OutputFormat.HTML     => formatHTML(title, data, columns)
      case OutputFormat.LaTeX    => formatLaTeX(title, data, columns)
      case OutputFormat.Text     => formatText(title, data, columns)

  private def computeWidths[T](data: Vector[T], columns: Vector[Column[T]]): Vector[Int] =
    columns.map { col =>
      val headerW = col.header.length
      val dataW   = if data.isEmpty then 0 else data.map(d => col.format(d).length).max
      math.max(if col.width > 0 then col.width else 0, math.max(headerW, dataW))
    }

  private def padCell(text: String, width: Int, align: Align): String =
    align match
      case Align.Left  => text.padTo(width, ' ')
      case Align.Right => text.reverse.padTo(width, ' ').reverse
      case Align.Center =>
        val pad   = width - text.length
        val left  = pad / 2
        val right = pad - left
        " " * left + text + " " * right

  private def formatMarkdown[T](title: String, data: Vector[T], columns: Vector[Column[T]]): String =
    val widths = computeWidths(data, columns)
    val sb     = new StringBuilder

    if title.nonEmpty then sb.append(s"## $title\n\n")

    // Header
    sb.append("| ")
    columns.zip(widths).foreach { case (col, w) =>
      sb.append(padCell(col.header, w, Align.Left))
      sb.append(" | ")
    }
    sb.append("\n")

    // Separator
    sb.append("| ")
    widths.foreach { w =>
      sb.append("-" * w)
      sb.append(" | ")
    }
    sb.append("\n")

    // Data rows
    data.foreach { row =>
      sb.append("| ")
      columns.zip(widths).foreach { case (col, w) =>
        sb.append(padCell(col.format(row), w, col.align))
        sb.append(" | ")
      }
      sb.append("\n")
    }

    sb.result()

  private def formatCSV[T](data: Vector[T], columns: Vector[Column[T]]): String =
    val sb = new StringBuilder

    // Header
    sb.append(columns.map(_.header).mkString(","))
    sb.append("\n")

    // Data rows
    data.foreach { row =>
      sb.append(
        columns
          .map { col =>
            val v = col.format(row)
            if v.contains(",") || v.contains("\"") then s"\"${v.replace("\"", "\"\"")}\""
            else v
          }
          .mkString(",")
      )
      sb.append("\n")
    }

    sb.result()

  private def formatHTML[T](title: String, data: Vector[T], columns: Vector[Column[T]]): String =
    val sb = new StringBuilder
    sb.append("<table>\n")

    if title.nonEmpty then sb.append(s"  <caption>$title</caption>\n")

    // Header
    sb.append("  <thead>\n    <tr>")
    columns.foreach { col =>
      sb.append(s"<th>${escapeHtml(col.header)}</th>")
    }
    sb.append("</tr>\n  </thead>\n")

    // Body
    sb.append("  <tbody>\n")
    data.foreach { row =>
      sb.append("    <tr>")
      columns.foreach { col =>
        sb.append(s"<td>${escapeHtml(col.format(row))}</td>")
      }
      sb.append("</tr>\n")
    }
    sb.append("  </tbody>\n")
    sb.append("</table>")

    sb.result()

  private def formatLaTeX[T](title: String, data: Vector[T], columns: Vector[Column[T]]): String =
    val sb = new StringBuilder
    val colSpec = columns
      .map { col =>
        col.align match
          case Align.Left   => "l"
          case Align.Right  => "r"
          case Align.Center => "c"
      }
      .mkString("|", "|", "|")

    sb.append("\\begin{table}[htbp]\n")
    sb.append("\\centering\n")
    if title.nonEmpty then sb.append(s"\\caption{${escapeLaTeX(title)}}\n")
    sb.append(s"\\begin{tabular}{$colSpec}\n")
    sb.append("\\hline\n")

    // Header
    sb.append(columns.map(c => escapeLaTeX(c.header)).mkString(" & "))
    sb.append(" \\\\\n\\hline\n")

    // Data rows
    data.foreach { row =>
      sb.append(columns.map(col => escapeLaTeX(col.format(row))).mkString(" & "))
      sb.append(" \\\\\n")
    }

    sb.append("\\hline\n")
    sb.append("\\end{tabular}\n")
    sb.append("\\end{table}")

    sb.result()

  private def formatText[T](title: String, data: Vector[T], columns: Vector[Column[T]]): String =
    val widths = computeWidths(data, columns)
    val sb     = new StringBuilder

    if title.nonEmpty then
      sb.append(title)
      sb.append("\n")
      sb.append("=" * title.length)
      sb.append("\n")

    // Header
    columns.zip(widths).foreach { case (col, w) =>
      sb.append(padCell(col.header, w, Align.Left))
      sb.append("  ")
    }
    sb.append("\n")

    // Separator
    widths.foreach { w =>
      sb.append("-" * w)
      sb.append("  ")
    }
    sb.append("\n")

    // Data rows
    data.foreach { row =>
      columns.zip(widths).foreach { case (col, w) =>
        sb.append(padCell(col.format(row), w, col.align))
        sb.append("  ")
      }
      sb.append("\n")
    }

    sb.result()

  private def escapeHtml(s: String): String =
    s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

  private def escapeLaTeX(s: String): String =
    s.replace("_", "\\_")
      .replace("%", "\\%")
      .replace("&", "\\&")
      .replace("#", "\\#")
      .replace("$", "\\$")
