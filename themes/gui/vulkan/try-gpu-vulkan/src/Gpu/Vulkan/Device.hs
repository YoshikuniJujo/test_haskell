{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device (

	-- * CREATE

	create, D, CreateInfo(..),
	CreateFlags, QueueCreateInfo(..),

	-- ** Group

	group, Group, create', destroy, lookup,

	-- * GET QUEUE AND WAIT IDLE

	getQueue, waitIdle,

	-- * SIZE

	Size

	) where

import Prelude hiding (lookup)
import Gpu.Vulkan.Device.Internal
