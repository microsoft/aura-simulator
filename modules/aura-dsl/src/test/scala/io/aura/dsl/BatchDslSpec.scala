// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.dsl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*
import io.aura.dsl.SimulationDsl.*

class BatchDslSpec extends AnyFlatSpec with Matchers:

  "Batch DSL" should "create a valid batch scheduling configuration" in {
    val config = simulation("batch-test", endTime = SimTime(1000.0)) {
      datacenter("dc-1") {
        hosts(count = 2, pes = PEs(4), mips = MIPS(10000.0), ram = MegaBytes(8192.0))
      }
      broker("iaas-broker") {
        vm()
        workload()
      }
      batchBroker("hpc-cluster") {
        computeNodes(4, cpu = MIPS(2000.0), ram = MegaBytes(4096.0))
        batchAlgorithm(BatchScheduler.fcfs)
        batchTickInterval(SimTime(5.0))
        batchJobs(
          count = 3,
          namePrefix = "sim-job",
          priority = JobPriority.Normal,
          requiredNodes = 1,
          cpuPerNode = MIPS(500.0),
          ramPerNode = MegaBytes(1024.0),
          estimatedRuntime = SimTime(50.0)
        )
      }
    }

    config.batchBrokers should have size 1
    config.batchBrokers.head.name shouldBe "hpc-cluster"
    config.batchBrokers.head.jobs should have size 3
    config.batchBrokers.head.nodeCount shouldBe 4
    config.batchBrokers.head.tickInterval.value shouldBe 5.0
  }

  it should "assign globally unique job IDs" in {
    val config = simulation("batch-ids", endTime = SimTime(1000.0)) {
      datacenter("dc-1")(host())
      broker("b") { vm(); workload() }
      batchBroker("cluster-1") {
        computeNodes(2)
        batchJobs(count = 3, namePrefix = "a")
      }
      batchBroker("cluster-2") {
        computeNodes(2)
        batchJobs(count = 2, namePrefix = "b")
      }
    }

    val allJobIds = config.batchBrokers.flatMap(_.jobs.map(_.id.value))
    allJobIds should have size 5
    allJobIds.distinct should have size 5
  }

  it should "include batch brokers in brokerCount" in {
    val config = simulation("batch-count", endTime = SimTime(1000.0)) {
      datacenter("dc-1")(host())
      broker("b") { vm(); workload() }
      batchBroker("hpc") {
        computeNodes(2)
        batchJobs(count = 1)
      }
    }

    config.brokerCount shouldBe 2 // 1 IaaS broker + 1 batch broker
  }

  it should "support different scheduling algorithms" in {
    val algos = Vector(
      ("fcfs", BatchScheduler.fcfs),
      ("sjf", BatchScheduler.sjf),
      ("priority", BatchScheduler.priorityBased),
      ("gang", BatchScheduler.gangSchedule),
      ("backfill", BatchScheduler.backfill)
    )

    algos.foreach { case (name, algo) =>
      val config = simulation(s"batch-$name", endTime = SimTime(1000.0)) {
        datacenter("dc-1")(host())
        broker("b") { vm(); workload() }
        batchBroker(s"hpc-$name") {
          computeNodes(4)
          batchAlgorithm(algo)
          batchJobs(count = 2)
        }
      }
      config.batchBrokers.head.jobs should have size 2
    }
  }

  it should "support individual job configuration" in {
    val config = simulation("batch-individual", endTime = SimTime(1000.0)) {
      datacenter("dc-1")(host())
      broker("b") { vm(); workload() }
      batchBroker("hpc") {
        computeNodes(4)
        batchJob("critical-job") {
          summon[BatchJobBuilder].priority(JobPriority.Critical)
          summon[BatchJobBuilder].requiredNodes(2)
          summon[BatchJobBuilder].gang(true)
          summon[BatchJobBuilder].estimatedRuntime(SimTime(200.0))
        }
        batchJob("small-job") {
          summon[BatchJobBuilder].priority(JobPriority.Low)
          summon[BatchJobBuilder].requiredNodes(1)
          summon[BatchJobBuilder].estimatedRuntime(SimTime(10.0))
        }
      }
    }

    val jobs = config.batchBrokers.head.jobs
    jobs should have size 2
    jobs(0).priority shouldBe JobPriority.Critical
    jobs(0).isGang shouldBe true
    jobs(0).requiredNodes shouldBe 2
    jobs(1).priority shouldBe JobPriority.Low
    jobs(1).requiredNodes shouldBe 1
  }

  it should "support mixed paradigms with batch scheduling" in {
    val config = simulation("mixed-batch", endTime = SimTime(1000.0)) {
      datacenter("dc-1") {
        hosts(count = 4, pes = PEs(4), mips = MIPS(10000.0), ram = MegaBytes(16384.0))
      }
      broker("iaas") {
        vm()
        workload()
      }
      batchBroker("hpc") {
        computeNodes(8)
        batchAlgorithm(BatchScheduler.backfill)
        batchJobs(count = 10, estimatedRuntime = SimTime(50.0))
      }
    }

    config.brokers should have size 1
    config.batchBrokers should have size 1
    config.brokerCount shouldBe 2
    config.batchBrokers.head.jobs should have size 10
  }
