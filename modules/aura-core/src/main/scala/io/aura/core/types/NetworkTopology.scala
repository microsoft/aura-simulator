// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

/** Immutable network topology with Floyd-Warshall shortest-path delays. */

final case class NetworkLink(
    from: DatacenterId,
    to: DatacenterId,
    latency: SimTime,
    bandwidth: Mbps
)

final case class NetworkTopology(
    links: Vector[NetworkLink],
    delays: Map[(DatacenterId, DatacenterId), SimTime],
    bandwidths: Map[(DatacenterId, DatacenterId), Mbps]
):
  def delay(from: DatacenterId, to: DatacenterId): SimTime =
    if from == to then SimTime.Zero
    else delays.getOrElse((from, to), SimTime.Zero)

  def bandwidth(from: DatacenterId, to: DatacenterId): Mbps =
    if from == to then Mbps(Double.MaxValue)
    else bandwidths.getOrElse((from, to), Mbps.Zero)

object NetworkTopology:

  val empty: NetworkTopology = NetworkTopology(Vector.empty, Map.empty, Map.empty)

  /** Build topology from links using Floyd-Warshall for shortest-path delays. O(n^3) but n = number of DCs, which is
    * typically tiny.
    */
  def fromLinks(links: Vector[NetworkLink]): NetworkTopology =
    if links.isEmpty then empty
    else
      // Collect all DC IDs
      val dcIds   = (links.map(_.from) ++ links.map(_.to)).distinct.sorted
      val n       = dcIds.size
      val idToIdx = dcIds.zipWithIndex.toMap
      val idxToId = dcIds.toArray

      // Initialize distance and bandwidth matrices
      val dist = Array.fill(n, n)(Double.MaxValue / 2.0)
      val bw   = Array.fill(n, n)(0.0)

      // Self-loops have zero delay
      for i <- 0 until n do dist(i)(i) = 0.0

      // Fill direct links (bidirectional)
      for link <- links do
        val i = idToIdx(link.from)
        val j = idToIdx(link.to)
        if link.latency.value < dist(i)(j) then
          dist(i)(j) = link.latency.value
          dist(j)(i) = link.latency.value
        if link.bandwidth.value > bw(i)(j) then
          bw(i)(j) = link.bandwidth.value
          bw(j)(i) = link.bandwidth.value

      // Floyd-Warshall
      for k <- 0 until n; i <- 0 until n; j <- 0 until n do
        if dist(i)(k) + dist(k)(j) < dist(i)(j) then dist(i)(j) = dist(i)(k) + dist(k)(j)

      // Build result maps
      val delayMap = (for
        i <- 0 until n
        j <- 0 until n
        if i != j && dist(i)(j) < Double.MaxValue / 2.0
      yield (idxToId(i), idxToId(j)) -> SimTime(dist(i)(j))).toMap

      val bwMap = (for
        i <- 0 until n
        j <- 0 until n
        if i != j && bw(i)(j) > 0.0
      yield (idxToId(i), idxToId(j)) -> Mbps(bw(i)(j))).toMap

      NetworkTopology(links, delayMap, bwMap)
