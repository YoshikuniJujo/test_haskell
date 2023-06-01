{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Middle (

	-- * CREATE AND DESTROY

	create, destroy, D,
	CreateInfo(..), CreateFlags, CreateFlagBits,
	QueueCreateInfo(..),

	-- * GET QUEUE AND WAIT IDLE

	getQueue, waitIdle,

	-- * SIZE

	Size

	) where

import Gpu.Vulkan.Device.Middle.Internal
