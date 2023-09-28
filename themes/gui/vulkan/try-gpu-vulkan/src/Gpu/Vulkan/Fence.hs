{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Fence (

	-- * CREATE

	create, F, CreateInfo(..),

	-- ** Group

	group, Group, create', destroy, lookup,

	-- * WAIT FOR FENCES AND RESET FENCES

	waitForFs, resetFs

	) where

import Prelude hiding (lookup)
import Gpu.Vulkan.Fence.Internal
