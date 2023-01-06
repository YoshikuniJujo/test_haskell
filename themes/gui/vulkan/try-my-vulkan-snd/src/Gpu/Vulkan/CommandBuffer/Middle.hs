{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandBuffer.Middle (

	-- * Type

	C,

	-- * Allocate

	allocate, freeCs, AllocateInfo(..),
	Level,
	pattern LevelPrimary, pattern LevelSecondary, pattern LevelMaxEnum,

	-- * Begin and End

	begin, end,

	BeginInfo(..),
	UsageFlags, UsageFlagBits,
	pattern UsageOneTimeSubmitBit, pattern UsageRenderPassContinueBit,
	pattern UsageSimultaneousUseBit, pattern UsageFlagBitsMaxEnum,

	InheritanceInfo(..),

	-- * Reset

	reset,
	ResetFlags, ResetFlagBits,
	pattern ResetReleaseResourcesBit, pattern ResetFlagBitsMaxEnum

	) where

import Gpu.Vulkan.CommandBuffer.Middle.Internal
import Gpu.Vulkan.CommandBuffer.Enum
