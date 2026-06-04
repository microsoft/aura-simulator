// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.iaas.policies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import io.aura.core.types.*
import io.aura.core.events.WorkloadSpec
import io.aura.iaas.state.HostState

class PropertyBasedIaasSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  // ─── VmAllocation Properties ──────────────────────────────────────────

  private val resourceGen = for
    pes  <- Gen.chooseNum(1, 16)
    mips <- Gen.chooseNum(1000.0, 50000.0)
    ram  <- Gen.chooseNum(1024.0, 65536.0)
  yield ResourceSpec(PEs(pes), MIPS(mips), MegaBytes(ram), Mbps(10000.0), MegaBytes(1000000.0))

  "VmAllocationPolicy.firstFit" should "never place on host with insufficient resources" in {
    forAll(resourceGen, resourceGen) { (hostSpec, vmSpec) =>
      val host = HostState.create(HostId(0), DatacenterId(0), hostSpec)
      VmAllocationPolicy.firstFit(IndexedSeq(host), vmSpec) match
        case Some(_) =>
          hostSpec.pes >= vmSpec.pes shouldBe true
          hostSpec.ram >= vmSpec.ram shouldBe true
        case None => succeed
    }
  }

  "VmAllocationPolicy.bestFit" should "agree with firstFit on feasibility" in {
    forAll(resourceGen, resourceGen) { (hostSpec, vmSpec) =>
      val host  = HostState.create(HostId(0), DatacenterId(0), hostSpec)
      val hosts = IndexedSeq(host)
      val ff    = VmAllocationPolicy.firstFit(hosts, vmSpec)
      val bf    = VmAllocationPolicy.bestFit(hosts, vmSpec)
      // With a single host, both should agree
      ff.isDefined shouldBe bf.isDefined
    }
  }

  // ─── DAG Properties ──────────────────────────────────────────────────

  private def linearDag(n: Int): Vector[WorkloadSpec] =
    (0 until n).map { i =>
      val preds = if i == 0 then Set.empty[WorkloadId] else Set(WorkloadId((i - 1).toLong))
      WorkloadSpec.simple(WorkloadId(i.toLong), MI(1000.0), PEs(1)).copy(predecessors = preds)
    }.toVector

  "DagScheduler.topologicalSort" should "preserve all elements" in {
    forAll(Gen.chooseNum(1, 50)) { n =>
      val dag    = linearDag(n)
      val sorted = DagScheduler.topologicalSort(dag)
      sorted should have size n
    }
  }

  it should "respect dependency ordering" in {
    forAll(Gen.chooseNum(2, 30)) { n =>
      val dag     = linearDag(n)
      val sorted  = DagScheduler.topologicalSort(dag)
      val indexOf = sorted.zipWithIndex.map((wl, i) => wl.id.value -> i).toMap
      // Each task should come after its predecessor
      for i <- 1 until n do indexOf((i - 1).toLong) should be < indexOf(i.toLong)
    }
  }

  "DagScheduler.validate" should "accept all valid linear DAGs" in {
    forAll(Gen.chooseNum(1, 30)) { n =>
      DagScheduler.validate(linearDag(n)) shouldBe DagScheduler.DagValid
    }
  }

  "DagScheduler.criticalPathLength" should "equal sum for linear chain" in {
    forAll(Gen.chooseNum(1, 20)) { n =>
      val dag      = linearDag(n)
      val critPath = DagScheduler.criticalPathLength(dag)
      critPath.value shouldBe (n * 1000.0) +- 0.001
    }
  }

  "DagScheduler.rootTasks" should "return only tasks with no predecessors" in {
    forAll(Gen.chooseNum(1, 30)) { n =>
      val dag   = linearDag(n)
      val roots = DagScheduler.rootTasks(dag)
      roots should have size 1
      roots.head.id.value shouldBe 0L
    }
  }

  "DagScheduler.leafTasks" should "return only tasks with no successors" in {
    forAll(Gen.chooseNum(1, 30)) { n =>
      val dag    = linearDag(n)
      val leaves = DagScheduler.leafTasks(dag)
      leaves should have size 1
      leaves.head.id.value shouldBe (n - 1).toLong
    }
  }

  // ─── Migration Model Properties ───────────────────────────────────

  "MigrationModel.preCopy" should "produce positive duration for any RAM size" in {
    forAll(Gen.chooseNum(1.0, 1e6)) { ramMB =>
      val plan = MigrationModel.preCopy(MigrationParams(MegaBytes(ramMB), Mbps(10000.0)))
      plan.totalTime.value should be > 0.0
      plan.totalDataTransferred.value should be > 0.0
    }
  }

  "MigrationModel.postCopy" should "produce shorter downtime than preCopy" in {
    forAll(Gen.chooseNum(1024.0, 65536.0)) { ramMB =>
      val params       = MigrationParams(MegaBytes(ramMB), Mbps(10000.0))
      val preCopyPlan  = MigrationModel.preCopy(params)
      val postCopyPlan = MigrationModel.postCopy()(params)
      postCopyPlan.downtime.value should be <= preCopyPlan.downtime.value
    }
  }
