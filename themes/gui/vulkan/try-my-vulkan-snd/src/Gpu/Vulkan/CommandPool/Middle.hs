{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandPool.Middle (

	-- * Type

	C,

	-- * Create and Destroy

	create, destroy, CreateInfo(..),

	CreateFlags, CreateFlagBits,
	pattern CreateTransientBit, pattern CreateResetCommandBufferBit,
	pattern CreateProtectedBit, pattern CreateFlagBitsMaxEnum ) where

import Gpu.Vulkan.CommandPool.Middle.Internal
import Gpu.Vulkan.CommandPool.Enum
