{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Middle (

	-- * Type

	D,

	-- * Create and Destroy

	create, destroy,
	CreateInfo(..), CreateFlags, CreateFlagBits,
	QueueCreateInfo(..),

	-- * Others

	getQueue, waitIdle, Size
	) where

import Gpu.Vulkan.Device.Middle.Internal
