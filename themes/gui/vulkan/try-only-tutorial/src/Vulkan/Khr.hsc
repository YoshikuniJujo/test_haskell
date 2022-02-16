{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr where

import Foreign.Ptr
import Data.Word
import Data.Int

import Vulkan.Device (Device)
import Vulkan.Semaphore (Semaphore)
import Vulkan.Fence (Fence)
import Vulkan.Khr.Swapchain (Swapchain)

#include <vulkan/vulkan.h>

compositeAlphaOpaqueBit :: #{type VkCompositeAlphaFlagBitsKHR}
compositeAlphaOpaqueBit = #{const VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR}

foreign import ccall "vkAcquireNextImageKHR" acquireNextImage ::
	Device -> Swapchain -> #{type uint64_t} -> Semaphore -> Fence ->
	Ptr #{type uint32_t} -> IO #{type VkResult}
