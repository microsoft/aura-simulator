// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

/** Service mesh simulation: sidecar proxies, load balancing, circuit breakers, retries, and request routing — modeled
  * as pure functions and immutable state.
  */

opaque type ServiceId = String
object ServiceId:
  def apply(value: String): ServiceId         = value
  extension (id: ServiceId) def value: String = id
  given Ordering[ServiceId] with
    def compare(x: ServiceId, y: ServiceId): Int = x.compareTo(y)

/** A running instance of a service. */
final case class ServiceInstance(
    instanceId: String,
    serviceId: ServiceId,
    hostId: HostId,
    weight: Int = 100,
    healthy: Boolean = true,
    activeConnections: Int = 0
)

/** Load balancing strategies for request routing. */
enum LoadBalancingStrategy:
  case RoundRobin
  case LeastConnections
  case WeightedRandom(seed: Long = 42L)
  case ConsistentHash

/** Circuit breaker states. */
enum CircuitBreakerState:
  case Closed, Open, HalfOpen

/** Circuit breaker with failure tracking and state transitions. */
final case class CircuitBreaker(
    state: CircuitBreakerState = CircuitBreakerState.Closed,
    failureCount: Int = 0,
    successCount: Int = 0,
    failureThreshold: Int = 5,
    successThreshold: Int = 3,
    halfOpenMaxRequests: Int = 1
):
  def onSuccess: CircuitBreaker = state match
    case CircuitBreakerState.Closed =>
      copy(failureCount = 0, successCount = successCount + 1)
    case CircuitBreakerState.HalfOpen =>
      val newSuccessCount = successCount + 1
      if newSuccessCount >= successThreshold then
        copy(state = CircuitBreakerState.Closed, failureCount = 0, successCount = 0)
      else copy(successCount = newSuccessCount)
    case CircuitBreakerState.Open => this // Shouldn't receive requests when open

  def onFailure: CircuitBreaker = state match
    case CircuitBreakerState.Closed =>
      val newFailureCount = failureCount + 1
      if newFailureCount >= failureThreshold then copy(state = CircuitBreakerState.Open, failureCount = newFailureCount)
      else copy(failureCount = newFailureCount)
    case CircuitBreakerState.HalfOpen =>
      copy(state = CircuitBreakerState.Open, failureCount = failureCount + 1, successCount = 0)
    case CircuitBreakerState.Open => this

  def allowsRequest: Boolean = state match
    case CircuitBreakerState.Closed   => true
    case CircuitBreakerState.Open     => false
    case CircuitBreakerState.HalfOpen => true

  def tryReset: CircuitBreaker =
    if state == CircuitBreakerState.Open then copy(state = CircuitBreakerState.HalfOpen, successCount = 0)
    else this

/** Retry policy configuration. */
final case class RetryPolicy(
    maxRetries: Int = 3,
    initialBackoff: SimTime = SimTime(0.1),
    backoffMultiplier: Double = 2.0,
    maxBackoff: SimTime = SimTime(10.0)
):
  /** Compute delay before the nth retry (0-indexed). */
  def retryDelay(attempt: Int): SimTime =
    if attempt <= 0 then SimTime.Zero
    else
      val delay = initialBackoff.value * math.pow(backoffMultiplier, (attempt - 1).toDouble)
      SimTime(math.min(delay, maxBackoff.value))

  /** Total delay for all retries up to the given attempt count. */
  def totalDelay(attempts: Int): SimTime =
    val total = (1 to math.min(attempts, maxRetries)).map(i => retryDelay(i).value).sum
    SimTime(total)

object RetryPolicy:
  val noRetry: RetryPolicy    = RetryPolicy(maxRetries = 0)
  val standard: RetryPolicy   = RetryPolicy()
  val aggressive: RetryPolicy = RetryPolicy(maxRetries = 5, initialBackoff = SimTime(0.05))

/** Rate limiter state. */
final case class RateLimiter(
    requestsPerSecond: Double,
    currentTokens: Double,
    maxTokens: Double,
    lastRefillTime: SimTime = SimTime.Zero
):
  def tryAcquire(now: SimTime): (Boolean, RateLimiter) =
    val elapsed   = now.value - lastRefillTime.value
    val newTokens = math.min(maxTokens, currentTokens + elapsed * requestsPerSecond)
    if newTokens >= 1.0 then (true, copy(currentTokens = newTokens - 1.0, lastRefillTime = now))
    else (false, copy(currentTokens = newTokens, lastRefillTime = now))

  def availableTokens(now: SimTime): Double =
    val elapsed = now.value - lastRefillTime.value
    math.min(maxTokens, currentTokens + elapsed * requestsPerSecond)

object RateLimiter:
  def apply(rps: Double): RateLimiter = RateLimiter(rps, rps, rps)

/** Per-service sidecar proxy configuration. */
final case class ServiceMeshConfig(
    sidecarOverhead: SimTime = SimTime(0.001), // 1ms proxy overhead per hop
    retryPolicy: RetryPolicy = RetryPolicy.standard,
    circuitBreaker: CircuitBreaker = CircuitBreaker(),
    rateLimiter: Option[RateLimiter] = None,
    timeout: SimTime = SimTime(30.0),
    mtlsEnabled: Boolean = true
)

object ServiceMeshConfig:
  val default: ServiceMeshConfig = ServiceMeshConfig()
  val lowLatency: ServiceMeshConfig = ServiceMeshConfig(
    sidecarOverhead = SimTime(0.0005),
    retryPolicy = RetryPolicy.noRetry,
    timeout = SimTime(5.0)
  )
  val resilient: ServiceMeshConfig = ServiceMeshConfig(
    sidecarOverhead = SimTime(0.002),
    retryPolicy = RetryPolicy.aggressive,
    circuitBreaker = CircuitBreaker(failureThreshold = 3, successThreshold = 5)
  )

/** Pure-function service mesh operations. */
object ServiceMesh:

  /** Route a request to an instance using the given strategy. */
  def routeRequest(
      instances: Vector[ServiceInstance],
      strategy: LoadBalancingStrategy,
      requestKey: String = "",
      counter: Int = 0
  ): Option[ServiceInstance] =
    val healthy = instances.filter(_.healthy)
    if healthy.isEmpty then None
    else
      strategy match
        case LoadBalancingStrategy.RoundRobin =>
          Some(healthy(counter % healthy.size))

        case LoadBalancingStrategy.LeastConnections =>
          Some(healthy.minBy(_.activeConnections))

        case LoadBalancingStrategy.WeightedRandom(seed) =>
          val rng         = new scala.util.Random(seed + requestKey.hashCode.toLong)
          val totalWeight = healthy.map(_.weight).sum
          if totalWeight <= 0 then Some(healthy.head)
          else
            val target = rng.nextInt(totalWeight)
            val (_, result) = healthy.foldLeft((target, Option.empty[ServiceInstance])) {
              case ((rem, found @ Some(_)), _) => (rem, found)
              case ((rem, None), inst) =>
                val newRem = rem - inst.weight
                if newRem < 0 then (newRem, Some(inst))
                else (newRem, None)
            }
            result.orElse(Some(healthy.last))

        case LoadBalancingStrategy.ConsistentHash =>
          val hash = math.abs(requestKey.hashCode)
          Some(healthy(hash % healthy.size))

  /** Compute total request latency including sidecar overhead. */
  def requestLatency(
      config: ServiceMeshConfig,
      networkDelay: SimTime,
      retryAttempt: Int = 0
  ): SimTime =
    // Sidecar overhead on both sender and receiver side
    val sidecarTotal = config.sidecarOverhead.value * 2
    val retryDelay   = if retryAttempt > 0 then config.retryPolicy.retryDelay(retryAttempt).value else 0.0
    val mtlsOverhead = if config.mtlsEnabled then 0.0005 else 0.0 // 0.5ms for TLS handshake
    SimTime(networkDelay.value + sidecarTotal + retryDelay + mtlsOverhead)

  /** Compute total mesh overhead (without network delay). */
  def meshOverhead(config: ServiceMeshConfig): SimTime =
    val sidecarTotal = config.sidecarOverhead.value * 2
    val mtlsOverhead = if config.mtlsEnabled then 0.0005 else 0.0
    SimTime(sidecarTotal + mtlsOverhead)

  /** Check if a request would time out given total elapsed time. */
  def wouldTimeout(config: ServiceMeshConfig, elapsedTime: SimTime): Boolean =
    elapsedTime.value > config.timeout.value

  /** Compute end-to-end latency for a request with retries on failure. */
  def totalLatencyWithRetries(
      config: ServiceMeshConfig,
      networkDelay: SimTime,
      failedAttempts: Int
  ): SimTime =
    val retriesToUse = math.min(failedAttempts, config.retryPolicy.maxRetries)
    // Each failed attempt takes network + overhead
    val baseLatency = requestLatency(config, networkDelay).value
    val failedLatency = (0 until retriesToUse).map { i =>
      baseLatency + config.retryPolicy.retryDelay(i + 1).value
    }.sum
    // Final successful attempt
    SimTime(failedLatency + baseLatency)
