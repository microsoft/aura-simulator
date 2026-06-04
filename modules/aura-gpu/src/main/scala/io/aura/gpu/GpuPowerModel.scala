// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.gpu

import io.aura.core.types.*

/** Multi-component GPU power model.
  *
  * Models power as: P_total = P_compute + P_hbm + P_nvlink + P_pcie + P_idle
  *
  * Each component scales with its respective utilization metric:
  *   - Compute: scales with SM utilization × (freq/freq_max)^2 (DVFS quadratic)
  *   - HBM: scales with memory bandwidth utilization
  *   - NVLink: scales with NVLink utilization
  *   - PCIe: small constant when active
  *   - Idle: base power (leakage + fans + regulators)
  */
object MultiComponentGpuPower:

  /** Calculate multi-component GPU power given device spec and activity profile. */
  def calculate(spec: GpuDeviceSpec, activity: GpuActivityProfile): GpuPowerComponents =
    val dynamicBudget = spec.tdpWatts.value - spec.idleWatts.value

    // Compute power: scales with SM util × (freq_ratio)^2
    val freqRatio =
      if spec.frequencyMaxMHz > 0 then activity.currentFreqMHz.toDouble / spec.frequencyMaxMHz.toDouble
      else 1.0
    val computeWatts = Watts(dynamicBudget * 0.6 * activity.smUtilization.value * freqRatio * freqRatio)

    // HBM power: scales with bandwidth utilization
    val hbmUtil =
      if spec.hbmBandwidthGBps > 0 then (activity.hbmTotalBWGBps / spec.hbmBandwidthGBps).min(1.0)
      else 0.0
    val hbmWatts = Watts(dynamicBudget * 0.25 * hbmUtil)

    // NVLink power: scales with link utilization
    val nvlinkWatts = Watts(dynamicBudget * 0.1 * activity.nvlinkUtilization.value)

    // PCIe power: small constant overhead
    val pcieWatts = Watts(dynamicBudget * 0.05 * (if activity.smUtilization.value > 0 then 1.0 else 0.0))

    GpuPowerComponents(
      computeWatts = computeWatts,
      hbmWatts = hbmWatts,
      nvlinkWatts = nvlinkWatts,
      pcieWatts = pcieWatts,
      idleWatts = spec.idleWatts
    )

  /** Calculate total power from device spec and activity. */
  def totalPower(spec: GpuDeviceSpec, activity: GpuActivityProfile): Watts =
    calculate(spec, activity).total

  /** Estimate energy consumed over a time interval (watt-hours). */
  def energy(spec: GpuDeviceSpec, activity: GpuActivityProfile, durationSeconds: Double): WattHours =
    val power = totalPower(spec, activity)
    WattHours(power.value * durationSeconds / 3600.0)
