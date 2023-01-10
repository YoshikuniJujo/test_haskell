{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Buffer.Middle (

	-- * Type
	
	B,

	-- * Create and Destroy

	create, destroy, CreateInfo(..),

	-- ** CreateFlags

	CreateFlags, CreateFlagBits,
	pattern CreateSparseBindingBit, pattern CreateSparseResidencyBit,
	pattern CreateSparseAliasedBit, pattern CreateProtectedBit,
	pattern CreateDeviceAddressCaptureReplayBit,
	pattern CreateDeviceAddressCaptureReplayBitExt,
	pattern CreateDeviceAddressCaptureReplayBitKhr,
	pattern CreateFlagBitsMaxEnum,

	-- ** UsageFlags

	UsageFlags, UsageFlagBits,
	pattern UsageTransferSrcBit, pattern UsageTransferDstBit,
	pattern UsageUniformTexelBufferBit, pattern UsageStorageTexelBufferBit,
	pattern UsageUniformBufferBit, pattern UsageStorageBufferBit,
	pattern UsageIndexBufferBit, pattern UsageVertexBufferBit,
	pattern UsageIndirectBufferBit,
	pattern UsageTransformFeedbackBufferBitExt,
	pattern UsageTransformFeedbackCounterBufferBitExt,
	pattern UsageConditionalRenderingBitExt,
	pattern UsageAccelerationStructureBuildInputReadOnlyBitKhr,
	pattern UsageShaderBindingTableBitKhr, pattern UsageRayTracingBitNv,
	pattern UsageShaderDeviceAddressBitExt,
	pattern UsageShaderDeviceAddressBitKhr, pattern UsageFlagBitsMaxEnum,

	-- * Bind and Get Memory

	bindMemory, getMemoryRequirements,

	-- * ImageCopy and MemoryBarrier

	ImageCopy(..), MemoryBarrier(..) ) where

import Gpu.Vulkan.Buffer.Middle.Internal
import Gpu.Vulkan.Buffer.Enum
