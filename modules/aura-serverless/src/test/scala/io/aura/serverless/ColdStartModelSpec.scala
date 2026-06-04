// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.serverless

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.core.types.*

class ColdStartModelSpec extends AnyFlatSpec with Matchers:

  "ColdStartModel.fixed" should "return constant delay regardless of runtime" in {
    val model = ColdStartModel.fixed(SimTime(1.0))
    model(MegaBytes(256.0), Runtime.Python).value shouldBe 1.0
    model(MegaBytes(512.0), Runtime.Java).value shouldBe 1.0
    model(MegaBytes(1024.0), Runtime.Go).value shouldBe 1.0
  }

  "ColdStartModel.byRuntime" should "return Python delay of 250ms" in {
    ColdStartModel.byRuntime(MegaBytes(256.0), Runtime.Python).value shouldBe 0.250
  }

  it should "return NodeJs delay of 170ms" in {
    ColdStartModel.byRuntime(MegaBytes(256.0), Runtime.NodeJs).value shouldBe 0.170
  }

  it should "return Java delay of 3.5s" in {
    ColdStartModel.byRuntime(MegaBytes(256.0), Runtime.Java).value shouldBe 3.500
  }

  it should "return Go delay of 100ms" in {
    ColdStartModel.byRuntime(MegaBytes(256.0), Runtime.Go).value shouldBe 0.100
  }

  it should "return Rust delay of 80ms" in {
    ColdStartModel.byRuntime(MegaBytes(256.0), Runtime.Rust).value shouldBe 0.080
  }

  it should "return DotNet delay of 1.2s" in {
    ColdStartModel.byRuntime(MegaBytes(256.0), Runtime.DotNet).value shouldBe 1.200
  }

  "ColdStartModel.loadDependent" should "scale delay with memory size" in {
    val model    = ColdStartModel.loadDependent(1.0)
    val delay256 = model(MegaBytes(256.0), Runtime.Python)
    val delay512 = model(MegaBytes(512.0), Runtime.Python)

    // 512 MB should have a larger delay than 256 MB
    delay512.value should be > delay256.value
  }

  it should "use base runtime delay with factor" in {
    val model = ColdStartModel.loadDependent(1.0)
    val delay = model(MegaBytes(256.0), Runtime.Python)
    // At 256 MB with factor 1.0: baseDelay * (1 + 256/256 * 1.0) = 0.25 * 2.0 = 0.5
    delay.value shouldBe (0.250 * 2.0) +- 0.001
  }
