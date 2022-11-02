{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Surface.PhysicalDevice (
	getSupport, getFormats, getCapabilities, getPresentModes,

	getSupport', getFormats', getCapabilities', getPresentModes' ) where

import Gpu.Vulkan.Khr.Surface.PhysicalDevice.Middle
