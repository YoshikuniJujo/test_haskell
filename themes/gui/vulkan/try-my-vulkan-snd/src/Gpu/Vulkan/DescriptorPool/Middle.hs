{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorPool.Middle (

	-- * Type

	D,

	-- * Create and Destroy

	create, destroy, CreateInfo(..),

	-- ** CreateFlags

	CreateFlags,
	pattern CreateFreeDescriptorSetBit, pattern CreateUpdateAfterBindBit,
	pattern CreateHostOnlyBitValve, pattern CreateUpdateAfterBindBitExt,
	pattern CreateFlagBitsMaxEnum,

	-- ** Size

	Size(..) ) where

import Gpu.Vulkan.DescriptorPool.Middle.Internal
import Gpu.Vulkan.DescriptorPool.Enum
