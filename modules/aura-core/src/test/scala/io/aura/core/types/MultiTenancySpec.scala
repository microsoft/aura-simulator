// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MultiTenancySpec extends AnyFlatSpec with Matchers:

  private val smallVm = ResourceSpec(PEs(2), MIPS(1000.0), MegaBytes(4096.0), Mbps(1000.0), MegaBytes(50000.0))

  "ResourceQuota.small" should "limit VMs to 5" in {
    ResourceQuota.small.maxVms shouldBe 5
  }

  "TenantUsage.withinQuota" should "allow allocation within limits" in {
    val usage = TenantUsage(TenantId(1))
    usage.withinQuota(ResourceQuota.small, smallVm) shouldBe true
  }

  it should "reject when VM count exceeded" in {
    var usage = TenantUsage(TenantId(1))
    for _ <- 0 until 5 do usage = usage.allocateVm(smallVm)
    usage.withinQuota(ResourceQuota.small, smallVm) shouldBe false
  }

  it should "reject when PE limit exceeded" in {
    val quota = ResourceQuota(maxVms = 100, maxPEs = PEs(4))
    val usage = TenantUsage(TenantId(1)).allocateVm(smallVm) // 2 PEs used
    usage.withinQuota(quota, smallVm) shouldBe true                      // 4 PEs total
    usage.allocateVm(smallVm).withinQuota(quota, smallVm) shouldBe false // 6 > 4
  }

  "TenantManager" should "register tenants" in {
    val mgr = TenantManager.empty.registerTenant(TenantId(1), ResourceQuota.small)
    mgr.allTenants should have size 1
  }

  it should "allow allocation within quota" in {
    val mgr = TenantManager.empty.registerTenant(TenantId(1), ResourceQuota.small)
    mgr.canAllocate(TenantId(1), smallVm) shouldBe true
  }

  it should "reject allocation exceeding quota" in {
    var mgr = TenantManager.empty.registerTenant(TenantId(1), ResourceQuota.small)
    for _ <- 0 until 5 do mgr = mgr.allocate(TenantId(1), smallVm)
    mgr.canAllocate(TenantId(1), smallVm) shouldBe false
  }

  it should "allow different tenants independent quotas" in {
    var mgr = TenantManager.empty
      .registerTenant(TenantId(1), ResourceQuota.small)
      .registerTenant(TenantId(2), ResourceQuota.medium)
    for _ <- 0 until 5 do mgr = mgr.allocate(TenantId(1), smallVm)
    mgr.canAllocate(TenantId(1), smallVm) shouldBe false
    mgr.canAllocate(TenantId(2), smallVm) shouldBe true
  }

  it should "release resources correctly" in {
    val mgr = TenantManager.empty
      .registerTenant(TenantId(1), ResourceQuota.small)
      .allocate(TenantId(1), smallVm)
      .release(TenantId(1), smallVm)
    mgr.usage(TenantId(1)).vmCount shouldBe 0
  }

  "TenantManager.noisyNeighbors" should "detect tenants using most of quota" in {
    val mgr = TenantManager.empty
      .registerTenant(TenantId(1), ResourceQuota(maxPEs = PEs(10)))
      .allocate(TenantId(1), ResourceSpec(PEs(9), MIPS(1000.0), MegaBytes(1024.0), Mbps(100.0), MegaBytes(1000.0)))
    mgr.noisyNeighbors(threshold = 0.8) should contain(TenantId(1))
  }

  it should "not flag tenants below threshold" in {
    val mgr = TenantManager.empty
      .registerTenant(TenantId(1), ResourceQuota(maxPEs = PEs(10)))
      .allocate(TenantId(1), smallVm) // 2/10 = 20%
    mgr.noisyNeighbors(threshold = 0.8) shouldBe empty
  }

  "TenantManager.tenantUtilization" should "compute PE utilization" in {
    val mgr = TenantManager.empty
      .registerTenant(TenantId(1), ResourceQuota(maxPEs = PEs(10)))
      .allocate(TenantId(1), smallVm) // 2/10 = 0.2
    mgr.tenantUtilization(TenantId(1)) shouldBe 0.2 +- 0.001
  }

  "TenantManager.fairShareWeight" should "weight by priority" in {
    val mgr = TenantManager.empty
      .registerTenant(TenantId(1), ResourceQuota(priority = 3))
      .registerTenant(TenantId(2), ResourceQuota(priority = 1))
    mgr.fairShareWeight(TenantId(1)) should be > mgr.fairShareWeight(TenantId(2))
  }
