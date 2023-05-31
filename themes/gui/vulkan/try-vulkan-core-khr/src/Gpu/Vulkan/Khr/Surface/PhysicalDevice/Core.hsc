{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Surface.PhysicalDevice.Core (

	-- * FUNCTIONS

	getSupport, getCapabilities, getFormats, getPresentModes

	) where

import Foreign.Ptr
import Data.Word
import Data.Int

import qualified Gpu.Vulkan.PhysicalDevice.Core as PhysicalDevice
import qualified Gpu.Vulkan.Khr.Surface.Core as Surface

#include <vulkan/vulkan.h>

foreign import ccall "vkGetPhysicalDeviceSurfaceSupportKHR" getSupport ::
	PhysicalDevice.P -> #{type uint32_t} -> Surface.S ->
	Ptr #{type VkBool32} -> IO #{type VkResult}

foreign import ccall "vkGetPhysicalDeviceSurfaceCapabilitiesKHR"
	getCapabilities ::
	PhysicalDevice.P -> Surface.S -> Ptr Surface.Capabilities ->
	IO #{type VkResult}

foreign import ccall "vkGetPhysicalDeviceSurfaceFormatsKHR" getFormats ::
	PhysicalDevice.P -> Surface.S ->
	Ptr #{type uint32_t} -> Ptr Surface.Format -> IO #{type VkResult}

foreign import ccall "vkGetPhysicalDeviceSurfacePresentModesKHR"
	getPresentModes ::
	PhysicalDevice.P -> Surface.S ->
	Ptr #{type uint32_t} -> Ptr #{type VkPresentModeKHR} -> IO #{type VkResult}
