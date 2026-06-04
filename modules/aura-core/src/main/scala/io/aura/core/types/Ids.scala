// Copyright (c) Microsoft Corporation
// SPDX-License-Identifier: MIT

package io.aura.core.types

/** Type-safe entity identifiers using opaque types.
  *
  * Prevents accidentally passing a VmId where a HostId is expected, etc. Each ID type wraps a Long and supports
  * equality/ordering.
  */

opaque type DatacenterId = Long

object DatacenterId:
  def apply(value: Long): DatacenterId = value

  given Ordering[DatacenterId] = Ordering.Long

  extension (id: DatacenterId)
    def value: Long  = id
    def toLong: Long = id

opaque type HostId = Long

object HostId:
  def apply(value: Long): HostId = value

  given Ordering[HostId] = Ordering.Long

  extension (id: HostId)
    def value: Long  = id
    def toLong: Long = id

opaque type VmId = Long

object VmId:
  def apply(value: Long): VmId = value

  given Ordering[VmId] = Ordering.Long

  extension (id: VmId)
    def value: Long  = id
    def toLong: Long = id

opaque type WorkloadId = Long

object WorkloadId:
  def apply(value: Long): WorkloadId = value

  given Ordering[WorkloadId] = Ordering.Long

  extension (id: WorkloadId)
    def value: Long  = id
    def toLong: Long = id

opaque type BrokerId = Long

object BrokerId:
  def apply(value: Long): BrokerId = value

  given Ordering[BrokerId] = Ordering.Long

  extension (id: BrokerId)
    def value: Long  = id
    def toLong: Long = id

opaque type FunctionId = Long

object FunctionId:
  def apply(value: Long): FunctionId = value

  given Ordering[FunctionId] = Ordering.Long

  extension (id: FunctionId)
    def value: Long  = id
    def toLong: Long = id

opaque type InvocationId = Long

object InvocationId:
  def apply(value: Long): InvocationId = value

  given Ordering[InvocationId] = Ordering.Long

  extension (id: InvocationId)
    def value: Long  = id
    def toLong: Long = id

opaque type ContainerId = Long

object ContainerId:
  def apply(value: Long): ContainerId = value

  given Ordering[ContainerId] = Ordering.Long

  extension (id: ContainerId)
    def value: Long  = id
    def toLong: Long = id

opaque type PodId = Long

object PodId:
  def apply(value: Long): PodId = value

  given Ordering[PodId] = Ordering.Long

  extension (id: PodId)
    def value: Long  = id
    def toLong: Long = id

opaque type DeploymentId = Long

object DeploymentId:
  def apply(value: Long): DeploymentId = value

  given Ordering[DeploymentId] = Ordering.Long

  extension (id: DeploymentId)
    def value: Long  = id
    def toLong: Long = id

opaque type EdgeTaskId = Long

object EdgeTaskId:
  def apply(value: Long): EdgeTaskId = value

  given Ordering[EdgeTaskId] = Ordering.Long

  extension (id: EdgeTaskId)
    def value: Long  = id
    def toLong: Long = id

opaque type FederatedTaskId = Long

object FederatedTaskId:
  def apply(value: Long): FederatedTaskId = value

  given Ordering[FederatedTaskId] = Ordering.Long

  extension (id: FederatedTaskId)
    def value: Long  = id
    def toLong: Long = id

// ─── GPU & Inference IDs ──────────────────────────────────────────────────────

opaque type GpuNodeId = Long

object GpuNodeId:
  def apply(value: Long): GpuNodeId = value

  given Ordering[GpuNodeId] = Ordering.Long

  extension (id: GpuNodeId)
    def value: Long  = id
    def toLong: Long = id

opaque type GpuDeviceId = Long

object GpuDeviceId:
  def apply(value: Long): GpuDeviceId = value

  given Ordering[GpuDeviceId] = Ordering.Long

  extension (id: GpuDeviceId)
    def value: Long  = id
    def toLong: Long = id

opaque type GpuClusterId = Long

object GpuClusterId:
  def apply(value: Long): GpuClusterId = value

  given Ordering[GpuClusterId] = Ordering.Long

  extension (id: GpuClusterId)
    def value: Long  = id
    def toLong: Long = id

opaque type InferenceRequestId = Long

object InferenceRequestId:
  def apply(value: Long): InferenceRequestId = value

  given Ordering[InferenceRequestId] = Ordering.Long

  extension (id: InferenceRequestId)
    def value: Long  = id
    def toLong: Long = id

opaque type ModelId = Long

object ModelId:
  def apply(value: Long): ModelId = value

  given Ordering[ModelId] = Ordering.Long

  extension (id: ModelId)
    def value: Long  = id
    def toLong: Long = id

/** Monotonic serial number for event ordering within the same SimTime. */
opaque type SerialNumber = Long

object SerialNumber:
  def apply(value: Long): SerialNumber = value
  val Zero: SerialNumber               = 0L

  given Ordering[SerialNumber] = Ordering.Long

  extension (sn: SerialNumber)
    def value: Long        = sn
    def next: SerialNumber = sn + 1L
