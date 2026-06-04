// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.inference.traces

import io.aura.core.types.*
import scala.io.Source

/** Reader for the Azure LLM Inference Dataset 2024.
  *
  * Parses conversation and code generation traces with prompt/completion token counts and inter-arrival times.
  *
  * Expected CSV format: timestamp_ms,prompt_tokens,completion_tokens,model
  */
object AzureLlmTraceReader:

  /** A single trace entry representing an inference request. */
  final case class TraceEntry(
      timestampMs: Long,
      promptTokens: Int,
      completionTokens: Int,
      model: String
  ):
    def arrivalTime: SimTime = SimTime(timestampMs / 1000.0)

  /** Read trace entries from a CSV file path. */
  def readFromFile(path: String): Vector[TraceEntry] =
    val source = Source.fromFile(path)
    try
      source
        .getLines()
        .drop(1) // Skip header
        .flatMap(parseLine)
        .toVector
        .sortBy(_.timestampMs)
    finally source.close()

  /** Read trace entries from a CSV string (for testing). */
  def readFromString(csv: String): Vector[TraceEntry] =
    csv.linesIterator
      .drop(1)
      .flatMap(parseLine)
      .toVector
      .sortBy(_.timestampMs)

  private def parseLine(line: String): Option[TraceEntry] =
    val parts = line.split(",").map(_.trim)
    if parts.length >= 3 then
      try
        Some(
          TraceEntry(
            timestampMs = parts(0).toLong,
            promptTokens = parts(1).toInt,
            completionTokens = parts(2).toInt,
            model = if parts.length > 3 then parts(3) else "unknown"
          )
        )
      catch case _: NumberFormatException => None
    else None

  /** Generate synthetic Poisson trace for testing.
    *
    * @param requestCount
    *   number of requests to generate
    * @param ratePerSecond
    *   average arrival rate
    * @param avgPromptTokens
    *   mean prompt token count
    * @param avgOutputTokens
    *   mean output token count
    * @param seed
    *   random seed for reproducibility
    */
  def generateSyntheticTrace(
      requestCount: Int,
      ratePerSecond: Double,
      avgPromptTokens: Int = 512,
      avgOutputTokens: Int = 128,
      seed: Long = 42L
  ): Vector[TraceEntry] =
    val rng = new java.util.Random(seed)
    (0 until requestCount)
      .foldLeft((Vector.empty[TraceEntry], 0L)) { case ((entries, timeMs), _) =>
        val interArrival = (-math.log(1.0 - rng.nextDouble()) / ratePerSecond * 1000.0).toLong.max(1)
        val nextTimeMs   = timeMs + interArrival
        val prompt       = (avgPromptTokens * (0.5 + rng.nextDouble())).toInt.max(1)
        val output       = (avgOutputTokens * (0.5 + rng.nextDouble())).toInt.max(1)
        (entries :+ TraceEntry(nextTimeMs, prompt, output, "synthetic"), nextTimeMs)
      }
      ._1
