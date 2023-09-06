{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr.Surface.PhysicalDevice where

import Foreign.Ptr
import Data.Word
import Data.Int

import Vulkan.Khr.Surface

import qualified Vulkan.PhysicalDevice as PhysicalDevice
import qualified Vulkan.Khr.Present as Present

#include <vulkan/vulkan.h>

foreign import ccall "vkGetPhysicalDeviceSurfaceCapabilitiesKHR"
	getCapabilities ::
	PhysicalDevice.PhysicalDevice -> Surface -> Ptr Capabilities ->
	IO #{type VkResult}

foreign import ccall "vkGetPhysicalDeviceSurfaceFormatsKHR" getFormats ::
	PhysicalDevice.PhysicalDevice -> Surface ->
	Ptr #{type uint32_t} -> Ptr Format -> IO #{type VkResult}

foreign import ccall "vkGetPhysicalDeviceSurfacePresentModesKHR"
	getPresentModes ::
	PhysicalDevice.PhysicalDevice -> Surface ->
	Ptr #{type uint32_t} -> Ptr Present.Mode -> IO #{type VkResult}
