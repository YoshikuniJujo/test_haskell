{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Swapchain.Core where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word
import Data.Int

import Gpu.Vulkan.Core
import Gpu.Vulkan.Base

import qualified Gpu.Vulkan.AllocationCallbacks.Core as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Core as Device
import qualified Gpu.Vulkan.Khr.Surface.Core as Surface
import qualified Gpu.Vulkan.Image.Core as Image

#include <vulkan/vulkan.h>

data STag
type S = Ptr STag
type PtrS = Ptr S

strType :: #{type VkStructureType}
strType = #{const VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR}

struct "CreateInfo" #{size VkSwapchainCreateInfoKHR}
		#{alignment VkSwapchainCreateInfoKHR} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkSwapchainCreateInfoKHR, sType} p strType |]),
	("pNext", ''PtrVoid, [| #{peek VkSwapchainCreateInfoKHR, pNext} |],
		[| #{poke VkSwapchainCreateInfoKHR, pNext} |]),
	("flags", ''#{type VkSwapchainCreateFlagsKHR},
		[| #{peek VkSwapchainCreateInfoKHR, flags} |],
		[| #{poke VkSwapchainCreateInfoKHR, flags} |]),
	("surface", ''Surface.S,
		[| #{peek VkSwapchainCreateInfoKHR, surface} |],
		[| #{poke VkSwapchainCreateInfoKHR, surface} |]),
	("minImageCount", ''#{type uint32_t},
		[| #{peek VkSwapchainCreateInfoKHR, minImageCount} |],
		[| #{poke VkSwapchainCreateInfoKHR, minImageCount} |]),
	("imageFormat", ''#{type VkFormat},
		[| #{peek VkSwapchainCreateInfoKHR, imageFormat} |],
		[| #{poke VkSwapchainCreateInfoKHR, imageFormat} |]),
	("imageColorSpace", ''#{type VkColorSpaceKHR},
		[| #{peek VkSwapchainCreateInfoKHR, imageColorSpace} |],
		[| #{poke VkSwapchainCreateInfoKHR, imageColorSpace} |]),
	("imageExtent", ''Extent2d,
		[| #{peek VkSwapchainCreateInfoKHR, imageExtent} |],
		[| #{poke VkSwapchainCreateInfoKHR, imageExtent} |]),
	("imageArrayLayers", ''#{type uint32_t},
		[| #{peek VkSwapchainCreateInfoKHR, imageArrayLayers} |],
		[| #{poke VkSwapchainCreateInfoKHR, imageArrayLayers} |]),
	("imageUsage", ''#{type VkImageUsageFlags},
		[| #{peek VkSwapchainCreateInfoKHR, imageUsage} |],
		[| #{poke VkSwapchainCreateInfoKHR, imageUsage} |]),
	("imageSharingMode", ''#{type VkSharingMode},
		[| #{peek VkSwapchainCreateInfoKHR, imageSharingMode} |],
		[| #{poke VkSwapchainCreateInfoKHR, imageSharingMode} |]),
	("queueFamilyIndexCount", ''#{type uint32_t},
		[| #{peek VkSwapchainCreateInfoKHR, queueFamilyIndexCount} |],
		[| #{poke VkSwapchainCreateInfoKHR, queueFamilyIndexCount} |]),
	("pQueueFamilyIndices", ''PtrUint32T,
		[| #{peek VkSwapchainCreateInfoKHR, pQueueFamilyIndices} |],
		[| #{poke VkSwapchainCreateInfoKHR, pQueueFamilyIndices} |]),
	("preTransform", ''#{type VkSurfaceTransformFlagBitsKHR},
		[| #{peek VkSwapchainCreateInfoKHR, preTransform} |],
		[| #{poke VkSwapchainCreateInfoKHR, preTransform} |]),
	("compositeAlpha", ''#{type VkCompositeAlphaFlagBitsKHR},
		[| #{peek VkSwapchainCreateInfoKHR, compositeAlpha} |],
		[| #{poke VkSwapchainCreateInfoKHR, compositeAlpha} |]),
	("presentMode", ''#{type VkPresentModeKHR},
		[| #{peek VkSwapchainCreateInfoKHR, presentMode} |],
		[| #{poke VkSwapchainCreateInfoKHR, presentMode} |]),
	("clipped", ''#{type VkBool32},
		[| #{peek VkSwapchainCreateInfoKHR, clipped} |],
		[| #{poke VkSwapchainCreateInfoKHR, clipped} |]),
	("oldSwapchain", ''S,
		[| #{peek VkSwapchainCreateInfoKHR, oldSwapchain} |],
		[| #{poke VkSwapchainCreateInfoKHR, oldSwapchain} |]) ]
	[''Show, ''Storable]

foreign import ccall "vkCreateSwapchainKHR" create ::
	Device.D -> Ptr CreateInfo -> Ptr AllocationCallbacks.A -> Ptr S ->
	IO #{type VkResult}

foreign import ccall "vkDestroySwapchainKHR" destroy ::
	Device.D -> S -> Ptr AllocationCallbacks.A -> IO ()

foreign import ccall "vkGetSwapchainImagesKHR" getImages ::
	Device.D -> S -> Ptr #{type uint32_t} -> Ptr Image.I ->
	IO #{type VkResult}
