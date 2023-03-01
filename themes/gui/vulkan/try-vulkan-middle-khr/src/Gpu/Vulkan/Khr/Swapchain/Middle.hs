{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Swapchain.Middle (

	-- * Name

	extensionName,

	-- * Type

	S,

	-- * Creaet and Destroy

	create, recreate, destroy, CreateInfo(..),

	-- * Get Images

	getImages ) where

import Gpu.Vulkan.Khr.Swapchain.Middle.Internal
