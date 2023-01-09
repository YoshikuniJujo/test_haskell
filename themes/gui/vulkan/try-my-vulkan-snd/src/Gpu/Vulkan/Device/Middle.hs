{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Middle (

	-- * Type

	D,

	-- * Create and Destroy

	create, destroy,
	CreateInfo(..), CreateFlags, CreateFlagBits,
	QueueCreateInfo(..),
	QueueCreateFlags, QueueCreateFlagBits,
	pattern QueueCreateProtectedBit, pattern QueueCreateFlagBitsMaxEnum,

	-- * Others

	getQueue, waitIdle, Size
	) where

import Gpu.Vulkan.Device.Middle.Internal
import Gpu.Vulkan.Device.Enum
