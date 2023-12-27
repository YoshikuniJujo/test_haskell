{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Fence (

	-- * CREATE

	create, F, CreateInfo(..),

	-- ** Group

	group, Group, create', unsafeDestroy, lookup,

	-- * WAIT FOR FENCES AND RESET FENCES

	waitForFs, resetFs,

	-- * ENUM

	module Gpu.Vulkan.Fence.Enum

	) where

import Prelude hiding (lookup)
import Gpu.Vulkan.Fence.Internal
import Gpu.Vulkan.Fence.Enum
