// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DistributedTracingSpec extends AnyFlatSpec with Matchers:

  private val tid = TraceId("abc123")

  private def span(
      id: String,
      parent: Option[String],
      svc: String,
      op: String,
      start: Double,
      end: Double,
      status: SpanStatus = SpanStatus.Ok
  ) =
    Span(tid, SpanId(id), parent.map(SpanId(_)), op, svc, SimTime(start), SimTime(end), status)

  // A sample trace tree:
  //   gateway (0-100)
  //     ├─ auth (5-25)
  //     └─ api (10-90)
  //          ├─ db-read (15-45)
  //          └─ cache (20-30)
  private val spans = Vector(
    span("s1", None, "gateway", "handle-request", 0, 100),
    span("s2", Some("s1"), "auth", "verify-token", 5, 25),
    span("s3", Some("s1"), "api", "process", 10, 90),
    span("s4", Some("s3"), "db", "read-users", 15, 45),
    span("s5", Some("s3"), "cache", "check-cache", 20, 30)
  )
  private val trace = Trace(tid, spans)

  // ── TraceId / SpanId ────────────────────────────────────────────────────

  "TraceId.generate" should "produce hex string" in {
    val id = TraceId.generate(42L)
    id.value should have length 32
  }

  "SpanId.generate" should "produce hex string" in {
    val id = SpanId.generate(42L)
    id.value should have length 16
  }

  // ── Span ────────────────────────────────────────────────────────────────

  "Span" should "compute duration" in {
    spans.head.duration.value shouldBe 100.0
  }

  it should "identify root span" in {
    spans.head.isRoot shouldBe true
    spans(1).isRoot shouldBe false
  }

  it should "detect error status" in {
    val errSpan = span("e1", None, "svc", "op", 0, 10, SpanStatus.Error)
    errSpan.isError shouldBe true
    spans.head.isError shouldBe false
  }

  // ── Trace ───────────────────────────────────────────────────────────────

  "Trace" should "find root span" in {
    trace.rootSpan.map(_.spanId) shouldBe Some(SpanId("s1"))
  }

  it should "count services" in {
    trace.serviceCount shouldBe 5 // gateway, auth, api, db, cache
  }

  it should "find children of a span" in {
    trace.childrenOf(SpanId("s1")).map(_.spanId) should contain allOf (SpanId("s2"), SpanId("s3"))
    trace.childrenOf(SpanId("s3")).map(_.spanId) should contain allOf (SpanId("s4"), SpanId("s5"))
  }

  it should "compute max depth" in {
    trace.maxDepth shouldBe 2 // s1 -> s3 -> s4
  }

  // ── TraceCorrelator.buildTrace ──────────────────────────────────────────

  "TraceCorrelator.buildTrace" should "sort spans by start time" in {
    val shuffled = spans.reverse
    val built    = TraceCorrelator.buildTrace(shuffled)
    built shouldBe defined
    built.get.spans.head.startTime.value shouldBe 0.0
  }

  it should "return None for empty spans" in {
    TraceCorrelator.buildTrace(Vector.empty) shouldBe None
  }

  // ── Critical Path ───────────────────────────────────────────────────────

  "TraceCorrelator.criticalPath" should "find longest path" in {
    val cp = TraceCorrelator.criticalPath(trace)
    cp should not be empty
    // Critical path: gateway -> api -> db-read (longest durations)
    cp.head.serviceName shouldBe "gateway"
    cp.map(_.serviceName) should contain("api")
  }

  // ── Bottleneck Detection ────────────────────────────────────────────────

  "TraceCorrelator.detectBottleneck" should "find slowest span on critical path" in {
    val bottleneck = TraceCorrelator.detectBottleneck(trace)
    bottleneck shouldBe defined
    // Gateway (100s) is the longest span
    bottleneck.get.duration.value shouldBe 100.0
  }

  // ── Trace Latency ───────────────────────────────────────────────────────

  "TraceCorrelator.traceLatency" should "return root span duration" in {
    TraceCorrelator.traceLatency(trace).value shouldBe 100.0
  }

  // ── Fan-out ─────────────────────────────────────────────────────────────

  "TraceCorrelator.fanOutDegree" should "count direct children" in {
    TraceCorrelator.fanOutDegree(trace, SpanId("s1")) shouldBe 2
    TraceCorrelator.fanOutDegree(trace, SpanId("s3")) shouldBe 2
    TraceCorrelator.fanOutDegree(trace, SpanId("s4")) shouldBe 0
  }

  // ── Service Map ─────────────────────────────────────────────────────────

  "TraceCorrelator.serviceMap" should "build dependency graph" in {
    val map = TraceCorrelator.serviceMap(Vector(trace))
    map("gateway") should contain("auth")
    map("gateway") should contain("api")
    map("api") should contain("db")
    map("api") should contain("cache")
  }

  // ── Error Rate ──────────────────────────────────────────────────────────

  "TraceCorrelator.errorRate" should "compute fraction of errored traces" in {
    val errTrace = Trace(
      TraceId("err"),
      Vector(
        span("e1", None, "svc", "op", 0, 10, SpanStatus.Error)
      )
    )
    val rate = TraceCorrelator.errorRate(Vector(trace, errTrace))
    rate shouldBe 0.5 +- 0.01
  }

  it should "return 0 for empty traces" in {
    TraceCorrelator.errorRate(Vector.empty) shouldBe 0.0
  }

  // ── Service Latency Stats ──────────────────────────────────────────────

  "TraceCorrelator.serviceLatencyStats" should "compute per-service stats" in {
    val stats = TraceCorrelator.serviceLatencyStats(Vector(trace))
    stats should contain key "gateway"
    stats should contain key "db"
    val (avg, _, _) = stats("gateway")
    avg shouldBe 100.0 // Only one gateway span
  }
