// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ServiceMeshSpec extends AnyFlatSpec with Matchers:

  private def instance(id: String, conns: Int = 0, weight: Int = 100, healthy: Boolean = true) =
    ServiceInstance(id, ServiceId("svc-a"), HostId(1), weight, healthy, conns)

  private val instances = Vector(
    instance("i-1", conns = 5),
    instance("i-2", conns = 2),
    instance("i-3", conns = 8)
  )

  // ── Load Balancing ──────────────────────────────────────────────────────

  "ServiceMesh.routeRequest RoundRobin" should "cycle through instances" in {
    val r0 = ServiceMesh.routeRequest(instances, LoadBalancingStrategy.RoundRobin, counter = 0)
    val r1 = ServiceMesh.routeRequest(instances, LoadBalancingStrategy.RoundRobin, counter = 1)
    val r2 = ServiceMesh.routeRequest(instances, LoadBalancingStrategy.RoundRobin, counter = 2)
    r0.map(_.instanceId) shouldBe Some("i-1")
    r1.map(_.instanceId) shouldBe Some("i-2")
    r2.map(_.instanceId) shouldBe Some("i-3")
  }

  "ServiceMesh.routeRequest LeastConnections" should "pick instance with fewest connections" in {
    val result = ServiceMesh.routeRequest(instances, LoadBalancingStrategy.LeastConnections)
    result.map(_.instanceId) shouldBe Some("i-2")
  }

  "ServiceMesh.routeRequest WeightedRandom" should "respect weights" in {
    val weighted = Vector(
      instance("heavy", weight = 900),
      instance("light", weight = 1)
    )
    // With high weight difference, heavy should be picked most of the time
    val results = (0 until 100).map { i =>
      ServiceMesh.routeRequest(weighted, LoadBalancingStrategy.WeightedRandom(seed = i.toLong), s"req-$i")
    }.flatten
    val heavyCount = results.count(_.instanceId == "heavy")
    heavyCount should be > 80
  }

  "ServiceMesh.routeRequest ConsistentHash" should "route same key to same instance" in {
    val r1 = ServiceMesh.routeRequest(instances, LoadBalancingStrategy.ConsistentHash, "user-123")
    val r2 = ServiceMesh.routeRequest(instances, LoadBalancingStrategy.ConsistentHash, "user-123")
    r1 shouldBe r2
  }

  it should "route different keys to potentially different instances" in {
    val results = (0 until 20)
      .map { i =>
        ServiceMesh
          .routeRequest(instances, LoadBalancingStrategy.ConsistentHash, s"key-$i")
          .map(_.instanceId)
      }
      .flatten
      .toSet
    results.size should be > 1
  }

  "ServiceMesh.routeRequest" should "skip unhealthy instances" in {
    val mixed = Vector(
      instance("healthy-1"),
      instance("unhealthy", healthy = false),
      instance("healthy-2")
    )
    val result = ServiceMesh.routeRequest(mixed, LoadBalancingStrategy.RoundRobin, counter = 1)
    result.map(_.instanceId) shouldBe Some("healthy-2")
  }

  it should "return None when all instances are unhealthy" in {
    val allSick = instances.map(_.copy(healthy = false))
    ServiceMesh.routeRequest(allSick, LoadBalancingStrategy.RoundRobin) shouldBe None
  }

  // ── Circuit Breaker ─────────────────────────────────────────────────────

  "CircuitBreaker" should "start in Closed state" in {
    CircuitBreaker().state shouldBe CircuitBreakerState.Closed
  }

  it should "open after reaching failure threshold" in {
    var cb = CircuitBreaker(failureThreshold = 3)
    cb = cb.onFailure.onFailure.onFailure
    cb.state shouldBe CircuitBreakerState.Open
    cb.failureCount shouldBe 3
  }

  it should "stay closed below failure threshold" in {
    var cb = CircuitBreaker(failureThreshold = 5)
    cb = cb.onFailure.onFailure
    cb.state shouldBe CircuitBreakerState.Closed
    cb.failureCount shouldBe 2
  }

  it should "reset failure count on success in Closed state" in {
    val cb = CircuitBreaker(failureThreshold = 5).onFailure.onFailure.onSuccess
    cb.failureCount shouldBe 0
  }

  it should "transition from Open to HalfOpen via tryReset" in {
    val open = CircuitBreaker(failureThreshold = 1).onFailure
    open.state shouldBe CircuitBreakerState.Open
    val halfOpen = open.tryReset
    halfOpen.state shouldBe CircuitBreakerState.HalfOpen
  }

  it should "close after enough successes in HalfOpen" in {
    val halfOpen = CircuitBreaker(failureThreshold = 1, successThreshold = 2).onFailure.tryReset
    halfOpen.state shouldBe CircuitBreakerState.HalfOpen
    val closed = halfOpen.onSuccess.onSuccess
    closed.state shouldBe CircuitBreakerState.Closed
  }

  it should "reopen on failure in HalfOpen" in {
    val halfOpen = CircuitBreaker(failureThreshold = 1).onFailure.tryReset
    val reopened = halfOpen.onFailure
    reopened.state shouldBe CircuitBreakerState.Open
  }

  it should "not allow requests when Open" in {
    val open = CircuitBreaker(failureThreshold = 1).onFailure
    open.allowsRequest shouldBe false
  }

  // ── Retry Policy ────────────────────────────────────────────────────────

  "RetryPolicy" should "compute exponential backoff" in {
    val policy = RetryPolicy(initialBackoff = SimTime(1.0), backoffMultiplier = 2.0)
    policy.retryDelay(1).value shouldBe 1.0
    policy.retryDelay(2).value shouldBe 2.0
    policy.retryDelay(3).value shouldBe 4.0
  }

  it should "cap at maxBackoff" in {
    val policy = RetryPolicy(initialBackoff = SimTime(1.0), backoffMultiplier = 10.0, maxBackoff = SimTime(5.0))
    policy.retryDelay(3).value shouldBe 5.0
  }

  it should "return zero delay for first attempt" in {
    RetryPolicy.standard.retryDelay(0) shouldBe SimTime.Zero
  }

  // ── Rate Limiter ────────────────────────────────────────────────────────

  "RateLimiter" should "allow requests within rate" in {
    val limiter      = RateLimiter(100.0) // 100 rps
    val (allowed, _) = limiter.tryAcquire(SimTime(0.001))
    allowed shouldBe true
  }

  it should "reject when tokens exhausted" in {
    val limiter   = RateLimiter(1.0, currentTokens = 1.0, maxTokens = 1.0) // 1 rps
    val (ok1, l1) = limiter.tryAcquire(SimTime(0.0))
    ok1 shouldBe true
    val (ok2, _) = l1.tryAcquire(SimTime(0.0)) // No time passed, no refill
    ok2 shouldBe false
  }

  // ── Latency & Overhead ─────────────────────────────────────────────────

  "ServiceMesh.requestLatency" should "include sidecar overhead" in {
    val config  = ServiceMeshConfig(sidecarOverhead = SimTime(0.001), mtlsEnabled = false)
    val latency = ServiceMesh.requestLatency(config, SimTime(0.01))
    latency.value shouldBe 0.012 +- 0.0001 // 10ms network + 2x1ms sidecar
  }

  it should "include mTLS overhead" in {
    val config  = ServiceMeshConfig(sidecarOverhead = SimTime(0.001), mtlsEnabled = true)
    val latency = ServiceMesh.requestLatency(config, SimTime(0.01))
    latency.value shouldBe 0.0125 +- 0.0001 // 10ms + 2ms sidecar + 0.5ms mTLS
  }

  "ServiceMesh.meshOverhead" should "compute overhead without network" in {
    val config = ServiceMeshConfig(sidecarOverhead = SimTime(0.002), mtlsEnabled = false)
    ServiceMesh.meshOverhead(config).value shouldBe 0.004 +- 0.0001
  }

  "ServiceMesh.wouldTimeout" should "detect timeout" in {
    val config = ServiceMeshConfig(timeout = SimTime(5.0))
    ServiceMesh.wouldTimeout(config, SimTime(6.0)) shouldBe true
    ServiceMesh.wouldTimeout(config, SimTime(4.0)) shouldBe false
  }
