// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz.frontend.components

import com.raquo.laminar.api.L.*

/** Generic sortable data table component. */
object DataTable:

  /** Column definition for the data table. */
  final case class Column[A](
      header: String,
      render: A => String,
      sortKey: Option[A => Double] = None
  )

  /** Sort direction. */
  enum SortDir:
    case Asc, Desc

  /** Render a sortable data table.
    *
    * @param data$
    *   Signal of the data rows
    * @param columns
    *   Column definitions (curried for type inference)
    * @tparam A
    *   Row data type
    */
  def render[A](data$ : Signal[Vector[A]])(columns: Column[A]*): HtmlElement =
    val cols    = columns.toVector
    val sortCol = Var(Option.empty[Int])
    val sortDir = Var(SortDir.Asc)

    val sorted$ = data$.combineWith(sortCol.signal, sortDir.signal).map {
      case (rows, Some(colIdx), dir) =>
        cols(colIdx).sortKey match
          case Some(key) =>
            val sorted = rows.sortBy(key)
            if dir == SortDir.Desc then sorted.reverse else sorted
          case None => rows
      case (rows, _, _) => rows
    }

    div(
      cls := "table-wrapper",
      table(
        thead(
          tr(
            cols.zipWithIndex.map { case (col, idx) =>
              th(
                cls := (if col.sortKey.isDefined then "sortable" else ""),
                col.header,
                col.sortKey.map { _ =>
                  onClick --> { _ =>
                    if sortCol.now().contains(idx) then
                      sortDir.update(d => if d == SortDir.Asc then SortDir.Desc else SortDir.Asc)
                    else
                      sortCol.set(Some(idx))
                      sortDir.set(SortDir.Asc)
                  }
                }
              )
            }
          )
        ),
        tbody(
          children <-- sorted$.map { rows =>
            rows.map { row =>
              tr(
                cols.map(col => td(col.render(row)))
              )
            }
          }
        )
      )
    )

  /** Render a static (non-reactive) data table.
    *
    * Uses curried parameters so A is inferred from data.
    */
  def renderStatic[A](data: Vector[A])(columns: Column[A]*): HtmlElement =
    render(Val(data))(columns*)
