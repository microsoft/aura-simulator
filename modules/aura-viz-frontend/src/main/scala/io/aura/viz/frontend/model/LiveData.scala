// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.viz.frontend.model

import upickle.default.*

/** Metrics snapshot model for live dashboard, mirrors MetricsSnapshot.toJSON. */
final case class MetricsSnapshot(
    timestamp: Double,
    completedWorkloads: Int,
    failedWorkloads: Int,
    activeVms: Int,
    totalMigrations: Int,
    totalFaults: Int,
    totalEnergyWh: Double,
    avgCompletionTime: Double,
    throughput: Double,
    currentUtilization: Double,
    totalCost: Double,
    eventCount: Long
) derives ReadWriter
