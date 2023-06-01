{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Fence.Middle (

	-- * CREATE AND DESTROY

	create, destroy, F, CreateInfo(..),

	-- * RESET AND WAIT

	resetFs, waitForFs

	) where

import Gpu.Vulkan.Fence.Middle.Internal
