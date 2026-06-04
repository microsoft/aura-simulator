// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.traces

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.aura.core.types.*

class AzureFunctionsTraceSpec extends AnyFlatSpec with Matchers:

  import AzureFunctionsTrace.*

  // ─── Invocation profile parsing ──────────────────────────────────────

  private val sampleProfile = FunctionInvocationProfile(
    hashOwner = "owner1",
    hashApp = "app1",
    hashFunction = "func1",
    trigger = TriggerType.Http,
    minuteCounts = Vector.fill(1440)(0).updated(0, 5).updated(1, 3).updated(60, 10)
  )

  "FunctionInvocationProfile.totalInvocations" should "sum all minute counts" in {
    sampleProfile.totalInvocations shouldBe 18
  }

  "FunctionInvocationProfile.activeMinutes" should "count non-zero minutes" in {
    sampleProfile.activeMinutes shouldBe 3
  }

  "FunctionInvocationProfile.functionKey" should "combine owner/app/function" in {
    sampleProfile.functionKey shouldBe "owner1/app1/func1"
  }

  // ─── TriggerType parsing ─────────────────────────────────────────────

  "TriggerType.fromString" should "parse known trigger types" in {
    TriggerType.fromString("http") shouldBe TriggerType.Http
    TriggerType.fromString("timer") shouldBe TriggerType.Timer
    TriggerType.fromString("queue") shouldBe TriggerType.Queue
    TriggerType.fromString("event") shouldBe TriggerType.Event
    TriggerType.fromString("orchestration") shouldBe TriggerType.Orchestration
    TriggerType.fromString("storage") shouldBe TriggerType.Storage
  }

  it should "default to Other for unknown triggers" in {
    TriggerType.fromString("unknown") shouldBe TriggerType.Other
    TriggerType.fromString("") shouldBe TriggerType.Other
  }

  // ─── CSV parsing via temp files ──────────────────────────────────────

  "readInvocationProfiles" should "parse a valid CSV" in {
    val header = "HashOwner,HashApp,HashFunction,Trigger" + (1 to 1440).mkString(",", ",", "")
    val counts = Vector.fill(1440)(0).updated(0, 7).updated(59, 3)
    val row    = s"ownerA,appA,funcA,http,${counts.mkString(",")}"
    val csv    = s"$header\n$row"

    val file = java.io.File.createTempFile("azure_inv_", ".csv")
    file.deleteOnExit()
    val pw = new java.io.PrintWriter(file)
    pw.write(csv)
    pw.close()

    val result = readInvocationProfiles(file.getAbsolutePath)
    result.isRight shouldBe true
    val profiles = result.toOption.get
    profiles.size shouldBe 1
    profiles.head.hashOwner shouldBe "ownerA"
    profiles.head.trigger shouldBe TriggerType.Http
    profiles.head.totalInvocations shouldBe 10
    profiles.head.activeMinutes shouldBe 2
  }

  it should "return error for non-existent file" in {
    val result = readInvocationProfiles("/nonexistent/path.csv")
    result.isLeft shouldBe true
  }

  it should "handle empty count fields as zero" in {
    val header = "HashOwner,HashApp,HashFunction,Trigger" + (1 to 1440).mkString(",", ",", "")
    val counts = Vector.fill(1440)("")
    val row    = s"ownerB,appB,funcB,timer,${counts.mkString(",")}"
    val csv    = s"$header\n$row"

    val file = java.io.File.createTempFile("azure_inv_empty_", ".csv")
    file.deleteOnExit()
    val pw = new java.io.PrintWriter(file)
    pw.write(csv)
    pw.close()

    val result = readInvocationProfiles(file.getAbsolutePath)
    result.isRight shouldBe true
    val profiles = result.toOption.get
    profiles.head.totalInvocations shouldBe 0
  }

  // ─── Duration profile parsing ────────────────────────────────────────

  "readDurationProfiles" should "parse a valid duration CSV" in {
    val header = "HashOwner,HashApp,HashFunction,Average,Count,Minimum,Maximum,p1,p25,p50,p75,p99,p100"
    val row    = "ownerA,appA,funcA,150.5,1000,10.0,5000.0,12.0,50.0,100.0,200.0,4500.0,5000.0"
    val csv    = s"$header\n$row"

    val file = java.io.File.createTempFile("azure_dur_", ".csv")
    file.deleteOnExit()
    val pw = new java.io.PrintWriter(file)
    pw.write(csv)
    pw.close()

    val result = readDurationProfiles(file.getAbsolutePath)
    result.isRight shouldBe true
    val profiles = result.toOption.get
    profiles.size shouldBe 1
    profiles.head.average shouldBe 150.5
    profiles.head.count shouldBe 1000
    profiles.head.minimum shouldBe 10.0
    profiles.head.maximum shouldBe 5000.0
  }

  // ─── toArrivalTimes ──────────────────────────────────────────────────

  "toArrivalTimes" should "produce correct total count" in {
    val arrivals = toArrivalTimes(sampleProfile)
    arrivals.size shouldBe sampleProfile.totalInvocations
  }

  it should "produce sorted arrival times" in {
    val arrivals = toArrivalTimes(sampleProfile)
    arrivals.sliding(2).foreach { window =>
      if window.size == 2 then window(0).value should be <= window(1).value
    }
  }

  it should "place arrivals within correct minute windows" in {
    val profile = FunctionInvocationProfile(
      hashOwner = "o",
      hashApp = "a",
      hashFunction = "f",
      trigger = TriggerType.Http,
      minuteCounts = Vector.fill(1440)(0).updated(5, 10)
    )
    val arrivals = toArrivalTimes(profile)
    arrivals.size shouldBe 10
    arrivals.foreach { t =>
      t.value should be >= 300.0 // minute 5 = 300s
      t.value should be < 360.0  // before minute 6 = 360s
    }
  }

  it should "produce empty vector for all-zero profile" in {
    val profile = FunctionInvocationProfile(
      hashOwner = "o",
      hashApp = "a",
      hashFunction = "f",
      trigger = TriggerType.Timer,
      minuteCounts = Vector.fill(1440)(0)
    )
    toArrivalTimes(profile) shouldBe empty
  }

  it should "be deterministic with same seed" in {
    val a1 = toArrivalTimes(sampleProfile, seed = 123L)
    val a2 = toArrivalTimes(sampleProfile, seed = 123L)
    a1 shouldBe a2
  }
