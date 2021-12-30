{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Device where

import Foreign.Ptr
import Foreign.Marshal.Array
import Data.Word

import Vulkan.Base
import Vulkan.Exception
import Vulkan.AllocationCallbacks
import Vulkan.PhysicalDevice

import qualified Vulkan.Device.Internal as I

#include <vulkan/vulkan.h>

data DeviceQueueCreateInfo a = DeviceQueueCreateInfo {
	deviceQueueCreateInfoNext :: Maybe a,
	deviceQueueCreateInfoFlags :: I.DeviceQueueCreateFlags,
	deviceQueueCreateInfoQueueFamilyIndex :: #{type uint32_t},
	deviceQueueCreateInfoQueuePriorities :: [#{type float}] }
	deriving Show

deviceQueueCreateInfoToC :: Pointable a =>
	DeviceQueueCreateInfo a -> (I.DeviceQueueCreateInfo -> IO b) -> IO b
deviceQueueCreateInfoToC DeviceQueueCreateInfo {
	deviceQueueCreateInfoNext = mnxt,
	deviceQueueCreateInfoFlags = flgs,
	deviceQueueCreateInfoQueueFamilyIndex = fi,
	deviceQueueCreateInfoQueuePriorities = prs } f =
	withPointerMaybe mnxt \(castPtr -> pnxt) -> withArrayLen prs \cnt pprs -> f
		$ I.DeviceQueueCreateInfo () pnxt flgs fi (fromIntegral cnt) pprs

{-
foreign import ccall "VkCreateDevice" c_VkCreateDevice ::
	PhysicalDevice -> Ptr I.DeviceCreateInfo -> Ptr I.AllocationCallbacks ->
	Ptr VkDevice -> IO Result
	-}
