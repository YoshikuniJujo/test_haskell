{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Fence (

	-- * CREATE

	create, F, CreateInfo(..),

	-- * WAIT FOR FENCES AND RESET FENCES

	waitForFs, resetFs

	) where

import Gpu.Vulkan.Fence.Internal
