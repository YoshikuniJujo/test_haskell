{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device (

	-- * CREATE

	create, D, CreateInfo(..),
	CreateFlags, QueueCreateInfo(..),

	-- * GET QUEUE AND WAIT IDLE

	getQueue, waitIdle,

	-- * SIZE

	Size

	) where

import Gpu.Vulkan.Device.Internal
