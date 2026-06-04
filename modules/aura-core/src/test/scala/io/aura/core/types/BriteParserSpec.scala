// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BriteParserSpec extends AnyFlatSpec with Matchers:

  val validBrite: String =
    """Model (1 - Loss of Autonomy):
      |
      |Nodes: (3)
      |0	1.0	2.0	3	3	0	RT_NODE
      |1	5.0	6.0	3	3	0	RT_NODE
      |2	9.0	1.0	3	3	0	RT_NODE
      |
      |Edges: (3)
      |0	0	1	0.050	10000.0	1.0	1.0	E_RT	U
      |1	1	2	0.100	5000.0	1.0	1.0	E_RT	U
      |2	0	2	0.200	8000.0	1.0	1.0	E_RT	U
      |""".stripMargin

  "BriteParser" should "parse valid BRITE format into NetworkTopology" in {
    val result = BriteParser.parse(validBrite)
    result.isRight shouldBe true

    val topo = result.toOption.get
    topo.links should have size 3

    // Check direct link 0→1
    topo.delay(DatacenterId(0), DatacenterId(1)).value shouldBe 0.050 +- 0.001
    topo.bandwidth(DatacenterId(0), DatacenterId(1)).value shouldBe 10000.0
  }

  it should "reject malformed input without Nodes section" in {
    val bad = "Edges: (1)\n0\t0\t1\t0.050\t10000.0\n"
    BriteParser.parse(bad).isLeft shouldBe true
  }

  it should "compute correct shortest paths via Floyd-Warshall" in {
    val result = BriteParser.parse(validBrite)
    val topo   = result.toOption.get

    // Direct 0→1 = 0.050, direct 0→2 = 0.200
    // Via 1: 0→1→2 = 0.050 + 0.100 = 0.150, which is shorter than direct 0.200
    topo.delay(DatacenterId(0), DatacenterId(2)).value shouldBe 0.150 +- 0.001
  }

  it should "handle single-node topology" in {
    val singleNode =
      """Nodes: (1)
        |0	1.0	2.0
        |
        |Edges: (0)
        |""".stripMargin

    val result = BriteParser.parse(singleNode)
    result.isRight shouldBe true
    result.toOption.get.links shouldBe empty
  }
