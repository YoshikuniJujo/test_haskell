{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Swapchain.Middle (
	extensionName,
	S, CreateInfo(..), create, recreate, destroy,

	getImages
	) where

import Gpu.Vulkan.Khr.Swapchain.Middle.Internal
