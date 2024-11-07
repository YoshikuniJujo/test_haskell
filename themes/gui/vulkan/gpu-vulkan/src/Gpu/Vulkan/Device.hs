{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device (

	-- * CREATE

	create, D, CreateInfo(..),
	CreateFlags, QueueCreateInfo(..),

	-- ** Group

	group, Group, create', unsafeDestroy, lookup,

	-- * GET QUEUE AND WAIT IDLE

	getQueue, waitIdle,

	-- * SIZE

	Size,

	-- * ENUM

	module Gpu.Vulkan.Device.Enum

	) where

import Prelude hiding (lookup)
import Gpu.Vulkan.Device.Internal
import Gpu.Vulkan.Device.Enum
