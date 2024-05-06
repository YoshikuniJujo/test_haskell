{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Swapchain.Middle (

	-- * EXTENSION NAME

	extensionName,

	-- * CREAET AND DESTROY

	create, recreate, destroy, S, CreateInfo(..),

	-- * GET IMAGES

	getImages,

	-- * ACQUIRE NEXT IMAGE

	acquireNextImage, acquireNextImageResult,	-- VK_KHR_swapchain

	-- * QUEUE PRESENT

	queuePresent, PresentInfo(..)			-- VK_KHR_swapchain

	) where

import Gpu.Vulkan.Khr.Swapchain.Middle.Internal
