// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import io.aura.core.types.*
import io.aura.iaas.state.HostState

/** VM allocation policies as pure functions.
  *
  * Implemented as simple functions: (hosts, vmSpec) => Option[HostId]. Composable, testable, no inheritance needed.
  */
type VmAllocationPolicy = (IndexedSeq[HostState], ResourceSpec) => Option[HostId]

object VmAllocationPolicy:

  /** First Fit: select the first host that can fit the VM. */
  val firstFit: VmAllocationPolicy = (hosts, vmSpec) => hosts.find(_.canPlaceVm(vmSpec)).map(_.id)

  /** Best Fit: select the host with the least available resources that can still fit the VM. Minimizes wasted
    * resources.
    */
  val bestFit: VmAllocationPolicy = (hosts, vmSpec) =>
    hosts
      .filter(_.canPlaceVm(vmSpec))
      .sortBy(h => h.available.mips.value)
      .headOption
      .map(_.id)

  /** Worst Fit: select the host with the most available resources. Spreads load across hosts.
    */
  val worstFit: VmAllocationPolicy = (hosts, vmSpec) =>
    hosts
      .filter(_.canPlaceVm(vmSpec))
      .sortBy(h => -h.available.mips.value)
      .headOption
      .map(_.id)

  /** Round Robin: select hosts in round-robin order. Uses a counter tracked externally.
    */
  def roundRobin(counter: Int): (VmAllocationPolicy, Int) =
    val policy: VmAllocationPolicy = (hosts, vmSpec) =>
      val suitable = hosts.filter(_.canPlaceVm(vmSpec))
      if suitable.isEmpty then None
      else Some(suitable(counter % suitable.size).id)
    (policy, counter + 1)

  /** Random: select a random suitable host. The Random instance is created once and reused across invocations,
    * producing genuine variation while remaining deterministic for a given seed.
    */
  def random(seed: Long): VmAllocationPolicy =
    val rng = new scala.util.Random(seed)
    (hosts, vmSpec) =>
      val suitable = hosts.filter(_.canPlaceVm(vmSpec))
      if suitable.isEmpty then None
      else Some(suitable(rng.nextInt(suitable.size)).id)

  /** Power-aware: prefer hosts that are already active (avoid waking idle hosts). Among active hosts, pick the one with
    * highest utilization (bin-packing). This reduces total active hosts and thus total power consumption.
    */
  val powerAware: VmAllocationPolicy = (hosts, vmSpec) =>
    val suitable = hosts.filter(_.canPlaceVm(vmSpec))
    if suitable.isEmpty then None
    else
      val activeWithVms = suitable.filter(h => h.active && h.vms.nonEmpty)
      val target =
        if activeWithVms.nonEmpty then activeWithVms.maxBy(_.cpuUtilization.value)
        else suitable.head
      Some(target.id)

  /** Heterogeneous-aware: uses HeterogeneousHost metadata for placement. Delegates to HeterogeneousScheduler.bestFit
    * when hardware topology is available. Falls back to bestFit for hosts without heterogeneous metadata.
    */
  def heterogeneousAware(hostMap: Map[HostId, HeterogeneousHost]): VmAllocationPolicy =
    (hosts, vmSpec) =>
      val hetHosts = hosts.flatMap(h => hostMap.get(h.id)).toVector
      if hetHosts.nonEmpty then
        val req = HeterogeneousWorkloadReq(vmSpec.mips, vmSpec.ram)
        HeterogeneousScheduler
          .bestFit(hetHosts, req)
          .orElse(
            bestFit(hosts, vmSpec)
          )
      else bestFit(hosts, vmSpec)
