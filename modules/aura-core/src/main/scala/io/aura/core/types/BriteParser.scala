// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

/** Parser for BRITE network topology format.
  *
  * BRITE format has two sections:
  *   - "Nodes: (N)" — node ID, x, y coordinates (and possibly more fields)
  *   - "Edges: (M)" — edge ID, from, to, delay, bandwidth (and possibly more fields)
  *
  * Produces a NetworkTopology from the parsed links.
  */
object BriteParser:

  /** Parse BRITE format string into a NetworkTopology. */
  def parse(content: String): Either[String, NetworkTopology] =
    val lines = content.linesIterator.toVector

    // Find section markers
    val nodesIdx = lines.indexWhere(_.startsWith("Nodes:"))
    val edgesIdx = lines.indexWhere(_.startsWith("Edges:"))

    if nodesIdx < 0 then Left("Missing 'Nodes:' section")
    else if edgesIdx < 0 then Left("Missing 'Edges:' section")
    else
      // Parse nodes: lines between Nodes header and Edges header
      val nodeLines = lines.slice(nodesIdx + 1, edgesIdx).filter(_.trim.nonEmpty)
      val nodeIds = nodeLines.flatMap { line =>
        val parts = line.trim.split("\\s+")
        if parts.length >= 1 then parts(0).toLongOption.map(DatacenterId(_))
        else None
      }

      if nodeIds.isEmpty then Left("No valid nodes found")
      else
        // Parse edges: lines after Edges header until end
        val edgeLines = lines.drop(edgesIdx + 1).filter(_.trim.nonEmpty)
        val edgesOrErrors = edgeLines.map { line =>
          val parts = line.trim.split("\\s+")
          if parts.length >= 5 then
            val result = for
              from  <- parts(1).toLongOption
              to    <- parts(2).toLongOption
              delay <- parts(3).toDoubleOption
              bw    <- parts(4).toDoubleOption
            yield NetworkLink(
              from = DatacenterId(from),
              to = DatacenterId(to),
              latency = SimTime(delay),
              bandwidth = Mbps(bw)
            )
            result.toRight(s"Invalid edge line: $line")
          else Left(s"Edge line has too few fields: $line")
        }

        val errors = edgesOrErrors.collect { case Left(e) => e }
        if errors.nonEmpty then Left(errors.mkString("; "))
        else
          val links = edgesOrErrors.collect { case Right(link) => link }
          Right(NetworkTopology.fromLinks(links))

  /** Parse a BRITE file by reading its content and delegating to parse. */
  def parseFile(path: String): Either[String, NetworkTopology] =
    try
      val content = scala.io.Source.fromFile(path).mkString
      parse(content)
    catch case e: Exception => Left(s"Failed to read file: ${e.getMessage}")
