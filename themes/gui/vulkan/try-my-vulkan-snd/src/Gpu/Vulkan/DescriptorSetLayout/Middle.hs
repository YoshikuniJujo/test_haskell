{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSetLayout.Middle (

	-- * Type

	L,

	-- * Create and Destroy

	create, destroy,
	CreateInfo(..),

	CreateFlags,
	pattern CreateUpdateAfterBindPoolBit,
	pattern CreatePushDescriptorBitKhr, pattern CreateHostOnlyPoolBitValve,
	pattern CreateUpdateAfterBindPoolBitExt, pattern CreateFlagBitsMaxEnum,

	Binding(..) ) where

import Gpu.Vulkan.DescriptorSetLayout.Middle.Internal
import Gpu.Vulkan.DescriptorSetLayout.Enum
