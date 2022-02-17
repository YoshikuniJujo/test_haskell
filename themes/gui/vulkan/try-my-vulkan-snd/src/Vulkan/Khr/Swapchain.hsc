{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr.Swapchain where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word
import Data.Int

import Vulkan
import Vulkan.Base
import Vulkan.Device (Device)

import qualified Vulkan.Khr.Surface as Surface
import qualified Vulkan.Image as Image

#include <vulkan/vulkan.h>

data SwapchainTag
type Swapchain = Ptr SwapchainTag
type PtrSwapchain = Ptr Swapchain

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
	("surface", ''Surface.Surface,
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
	("oldSwapchain", ''Swapchain,
		[| #{peek VkSwapchainCreateInfoKHR, oldSwapchain} |],
		[| #{poke VkSwapchainCreateInfoKHR, oldSwapchain} |]) ]
	[''Show, ''Storable]

foreign import ccall "vkCreateSwapchainKHR" create ::
	Device -> Ptr CreateInfo -> Ptr () -> Ptr Swapchain ->
	IO #{type VkResult}

foreign import ccall "vkDestroySwapchainKHR" destroy ::
	Device -> Swapchain -> Ptr () -> IO ()

foreign import ccall "vkGetSwapchainImagesKHR" getImages ::
	Device -> Swapchain -> Ptr #{type uint32_t} -> Ptr Image.Image ->
	IO #{type VkResult}
