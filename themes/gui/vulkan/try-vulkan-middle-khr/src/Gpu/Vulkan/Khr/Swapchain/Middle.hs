{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Swapchain.Middle (

	-- * EXTENSION NAME

	extensionName,

	-- * CREAET AND DESTROY

	create, recreate, destroy, S, CreateInfo(..),

	-- * GET IMAGES

	getImages

	) where

import Gpu.Vulkan.Khr.Swapchain.Middle.Internal
