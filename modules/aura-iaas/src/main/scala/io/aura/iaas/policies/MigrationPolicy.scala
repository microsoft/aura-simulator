// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import io.aura.core.types.*
import io.aura.iaas.state.HostState

/** Overload detection and VM selection policies for live migration.
  *
  * OverloadDetector: decides if a host is overloaded. VmSelectionPolicy: picks which VM to migrate from an overloaded
  * host.
  */

object MigrationOverloadDetector:

  /** Static threshold: host is overloaded if utilization > threshold. */
  def staticThreshold(threshold: Double): (HostState, Vector[(SimTime, Utilization)]) => Boolean =
    (host, _) => host.cpuUtilization.value > threshold

  /** MAD (Median Absolute Deviation) dynamic threshold.
    *
    * threshold = 1 - safetyParameter * 1.4826 * MAD where MAD = median(|xi - median(X)|)
    *
    * Host is overloaded if utilization > computed threshold.
    */
  def mad(safetyParameter: Double = 2.5): (HostState, Vector[(SimTime, Utilization)]) => Boolean =
    (host, history) =>
      if history.size < 2 then false
      else
        val values     = history.map(_._2.value).sorted
        val median     = values(values.size / 2)
        val deviations = values.map(v => math.abs(v - median)).sorted
        val madValue   = deviations(deviations.size / 2)
        val threshold  = 1.0 - safetyParameter * 1.4826 * madValue
        host.cpuUtilization.value > math.max(threshold, 0.0)

  /** IQR (Interquartile Range) dynamic threshold.
    *
    * threshold = 1 - safetyParameter * (Q3 - Q1)
    *
    * Host is overloaded if utilization > computed threshold.
    */
  def iqr(safetyParameter: Double = 1.5): (HostState, Vector[(SimTime, Utilization)]) => Boolean =
    (host, history) =>
      if history.size < 4 then false
      else
        val values    = history.map(_._2.value).sorted
        val n         = values.size
        val q1        = values(n / 4)
        val q3        = values(3 * n / 4)
        val iqrValue  = q3 - q1
        val threshold = 1.0 - safetyParameter * iqrValue
        host.cpuUtilization.value > math.max(threshold, 0.0)

object MigrationVmSelector:

  /** Select VM with minimum migration time (smallest RAM). */
  val minimumMigrationTime: HostState => Option[VmId] =
    (host: HostState) =>
      if host.vms.isEmpty then None
      else
        val runningVms = host.vms.values.filter(_.status == io.aura.iaas.state.VmStatus.Running)
        if runningVms.isEmpty then None
        else Some(runningVms.minBy(_.spec.ram.value).id)

  /** Select VM with maximum CPU utilization. */
  val maximumUtilization: HostState => Option[VmId] =
    (host: HostState) =>
      if host.vms.isEmpty then None
      else
        val runningVms = host.vms.values.filter(_.status == io.aura.iaas.state.VmStatus.Running)
        if runningVms.isEmpty then None
        else Some(runningVms.maxBy(_.cpuUtilization.value).id)

  /** Deterministic selection: picks the middle element by VM ID. */
  val random: HostState => Option[VmId] =
    (host: HostState) =>
      val runningVms = host.vms.values.filter(_.status == io.aura.iaas.state.VmStatus.Running).toVector
      if runningVms.isEmpty then None
      else
        val sorted = runningVms.sortBy(_.id)
        Some(sorted(sorted.size / 2).id)
