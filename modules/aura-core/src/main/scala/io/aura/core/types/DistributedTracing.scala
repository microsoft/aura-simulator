// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

/** Distributed tracing correlation: span trees, critical path analysis, bottleneck detection, and service dependency
  * mapping — modeled as immutable data with pure-function analysis.
  */

opaque type TraceId = String
object TraceId:
  def apply(value: String): TraceId = value
  def generate(seed: Long): TraceId =
    val rng = new scala.util.Random(seed)
    f"${rng.nextLong()}%016x${rng.nextLong()}%016x"
  extension (id: TraceId) def value: String = id
  given Ordering[TraceId] with
    def compare(x: TraceId, y: TraceId): Int = x.compareTo(y)

opaque type SpanId = String
object SpanId:
  def apply(value: String): SpanId = value
  def generate(seed: Long): SpanId =
    f"${new scala.util.Random(seed).nextLong()}%016x"
  val root: SpanId                         = "0000000000000000"
  extension (id: SpanId) def value: String = id
  given Ordering[SpanId] with
    def compare(x: SpanId, y: SpanId): Int = x.compareTo(y)

/** Status of a span. */
enum SpanStatus:
  case Ok, Error, Timeout, Cancelled

/** A single span in a distributed trace. */
final case class Span(
    traceId: TraceId,
    spanId: SpanId,
    parentSpanId: Option[SpanId],
    operationName: String,
    serviceName: String,
    startTime: SimTime,
    endTime: SimTime,
    status: SpanStatus = SpanStatus.Ok,
    tags: Map[String, String] = Map.empty
):
  def duration: SimTime = SimTime(math.max(0.0, endTime.value - startTime.value))
  def isRoot: Boolean   = parentSpanId.isEmpty
  def isError: Boolean  = status == SpanStatus.Error || status == SpanStatus.Timeout

/** Propagation context carried between services. */
final case class SpanContext(
    traceId: TraceId,
    spanId: SpanId,
    baggage: Map[String, String] = Map.empty
):
  def createChild(childSpanId: SpanId): SpanContext =
    copy(spanId = childSpanId)

/** A complete trace: a tree of spans. */
final case class Trace(
    traceId: TraceId,
    spans: Vector[Span]
):
  def rootSpan: Option[Span] = spans.find(_.isRoot)
  def spanCount: Int         = spans.size
  def serviceCount: Int      = spans.map(_.serviceName).distinct.size
  def hasErrors: Boolean     = spans.exists(_.isError)

  def childrenOf(spanId: SpanId): Vector[Span] =
    spans.filter(_.parentSpanId.contains(spanId))

  def depthOf(spanId: SpanId): Int =
    spans.find(_.spanId == spanId) match
      case None => 0
      case Some(span) =>
        span.parentSpanId match
          case None      => 0
          case Some(pid) => 1 + depthOf(pid)

  def maxDepth: Int =
    if spans.isEmpty then 0
    else spans.map(s => depthOf(s.spanId)).max

/** Pure-function trace analysis engine. */
object TraceCorrelator:

  /** Build a Trace from a flat list of spans (must share same traceId). */
  def buildTrace(spans: Vector[Span]): Option[Trace] =
    if spans.isEmpty then None
    else
      val traceId = spans.head.traceId
      Some(Trace(traceId, spans.sortBy(_.startTime.value)))

  /** Find the critical path: the chain of spans that determines end-to-end latency. Uses longest path through the span
    * tree by duration.
    */
  def criticalPath(trace: Trace): Vector[Span] =
    trace.rootSpan match
      case None       => Vector.empty
      case Some(root) => longestPath(root, trace)

  private def longestPath(span: Span, trace: Trace): Vector[Span] =
    val children = trace.childrenOf(span.spanId)
    if children.isEmpty then Vector(span)
    else
      val paths   = children.map(child => longestPath(child, trace))
      val longest = paths.maxBy(_.map(_.duration.value).sum)
      span +: longest

  /** Detect the bottleneck: the slowest span on the critical path. */
  def detectBottleneck(trace: Trace): Option[Span] =
    val cp = criticalPath(trace)
    if cp.isEmpty then None
    else Some(cp.maxBy(_.duration.value))

  /** End-to-end trace latency (root span duration). */
  def traceLatency(trace: Trace): SimTime =
    trace.rootSpan.map(_.duration).getOrElse(SimTime.Zero)

  /** Fan-out degree: number of direct children of a span. */
  def fanOutDegree(trace: Trace, spanId: SpanId): Int =
    trace.childrenOf(spanId).size

  /** Build a service dependency map from multiple traces. */
  def serviceMap(traces: Vector[Trace]): Map[String, Set[String]] =
    val edges = for
      trace    <- traces
      span     <- trace.spans
      parentId <- span.parentSpanId
      parent   <- trace.spans.find(_.spanId == parentId)
      if parent.serviceName != span.serviceName
    yield parent.serviceName -> span.serviceName

    edges.groupBy(_._1).map((svc, pairs) => svc -> pairs.map(_._2).toSet)

  /** Compute error rate across traces. */
  def errorRate(traces: Vector[Trace]): Double =
    if traces.isEmpty then 0.0
    else traces.count(_.hasErrors).toDouble / traces.size

  /** Find all spans for a specific service across traces. */
  def spansForService(traces: Vector[Trace], serviceName: String): Vector[Span] =
    traces.flatMap(_.spans.filter(_.serviceName == serviceName))

  /** Compute per-service latency statistics. */
  def serviceLatencyStats(traces: Vector[Trace]): Map[String, (Double, Double, Double)] =
    val allSpans = traces.flatMap(_.spans)
    val grouped  = allSpans.groupBy(_.serviceName)
    grouped.map { (svc, spans) =>
      val durations = spans.map(_.duration.value).sorted
      val avg       = durations.sum / durations.size
      val p50       = durations(durations.size / 2)
      val p99idx    = math.min(durations.size - 1, (durations.size * 0.99).toInt)
      val p99       = durations(p99idx)
      svc -> (avg, p50, p99)
    }
