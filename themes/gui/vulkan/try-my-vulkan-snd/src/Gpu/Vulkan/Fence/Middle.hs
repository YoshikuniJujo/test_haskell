{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Fence.Middle (

	-- * Type

	F,

	-- * Create and Destroy

	create, destroy, CreateInfo(..),

	-- * Wait and Reset

	waitForFs, resetFs ) where

import Gpu.Vulkan.Fence.Middle.Internal
