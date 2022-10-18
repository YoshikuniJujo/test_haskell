{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Middle (
	D, CreateInfo(..), QueueCreateInfo(..), CreateFlags, CreateFlagBits, create, destroy,

	getQueue, waitIdle, Size
	) where

import Gpu.Vulkan.Device.Middle.Internal
