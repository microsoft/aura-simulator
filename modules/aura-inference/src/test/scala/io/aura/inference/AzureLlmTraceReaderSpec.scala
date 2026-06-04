// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.inference

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.aura.inference.traces.*

class AzureLlmTraceReaderSpec extends AnyFlatSpec with Matchers:

  "readFromString" should "parse valid CSV" in {
    val csv =
      """timestamp_ms,prompt_tokens,completion_tokens,model
        |1000,512,128,llama-7b
        |2000,256,64,llama-7b
        |3000,1024,256,llama-70b""".stripMargin
    val entries = AzureLlmTraceReader.readFromString(csv)
    entries.size shouldBe 3
    entries(0).timestampMs shouldBe 1000
    entries(0).promptTokens shouldBe 512
    entries(0).completionTokens shouldBe 128
    entries(0).model shouldBe "llama-7b"
  }

  it should "sort by timestamp" in {
    val csv =
      """timestamp_ms,prompt_tokens,completion_tokens,model
        |3000,100,50,m
        |1000,200,100,m
        |2000,150,75,m""".stripMargin
    val entries = AzureLlmTraceReader.readFromString(csv)
    entries.map(_.timestampMs) shouldBe Vector(1000, 2000, 3000)
  }

  it should "skip malformed lines" in {
    val csv =
      """timestamp_ms,prompt_tokens,completion_tokens,model
        |1000,512,128,llama
        |bad,line
        |3000,256,64,llama""".stripMargin
    val entries = AzureLlmTraceReader.readFromString(csv)
    entries.size shouldBe 2
  }

  it should "handle missing model column" in {
    val csv =
      """timestamp_ms,prompt_tokens,completion_tokens
        |1000,512,128""".stripMargin
    val entries = AzureLlmTraceReader.readFromString(csv)
    entries.size shouldBe 1
    entries(0).model shouldBe "unknown"
  }

  it should "convert timestamp to SimTime" in {
    val csv =
      """timestamp_ms,prompt_tokens,completion_tokens,model
        |5000,100,50,m""".stripMargin
    val entries = AzureLlmTraceReader.readFromString(csv)
    entries(0).arrivalTime.value shouldBe 5.0
  }

  "generateSyntheticTrace" should "produce correct count" in {
    val trace = AzureLlmTraceReader.generateSyntheticTrace(100, 10.0)
    trace.size shouldBe 100
  }

  it should "produce sorted timestamps" in {
    val trace = AzureLlmTraceReader.generateSyntheticTrace(50, 5.0)
    trace.map(_.timestampMs) shouldBe trace.map(_.timestampMs).sorted
  }

  it should "produce positive token counts" in {
    val trace = AzureLlmTraceReader.generateSyntheticTrace(100, 10.0)
    trace.foreach { e =>
      e.promptTokens should be > 0
      e.completionTokens should be > 0
    }
  }

  it should "be deterministic with same seed" in {
    val t1 = AzureLlmTraceReader.generateSyntheticTrace(20, 5.0, seed = 123)
    val t2 = AzureLlmTraceReader.generateSyntheticTrace(20, 5.0, seed = 123)
    t1 shouldBe t2
  }

  it should "produce different traces with different seeds" in {
    val t1 = AzureLlmTraceReader.generateSyntheticTrace(20, 5.0, seed = 1)
    val t2 = AzureLlmTraceReader.generateSyntheticTrace(20, 5.0, seed = 2)
    t1 should not be t2
  }

  it should "respect average token parameters" in {
    val trace = AzureLlmTraceReader.generateSyntheticTrace(
      1000,
      10.0,
      avgPromptTokens = 1000,
      avgOutputTokens = 200
    )
    val avgPrompt = trace.map(_.promptTokens.toDouble).sum / trace.size
    avgPrompt shouldBe 1000.0 +- 200.0
  }
