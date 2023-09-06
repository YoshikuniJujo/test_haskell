{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr.Swapchain.Internal where

import Prelude
import qualified Prelude as P

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Enum
import Foreign.C.Struct
import Data.Bits
import Data.Word

import Vulkan.Base
import Vulkan.Format
import Vulkan.Image
import Vulkan.Khr.Surface

#include <vulkan/vulkan.h>

enum "SwapchainCreateFlagBits" ''#{type VkSwapchainCreateFlagBitsKHR}
		[''Show, ''Eq, ''Storable, ''Bits] [
	("SwapchainCreateFlagBitsZero", 0),
	("SwapchainCreateSplitInstanceBindRegionsBit", #{const
		VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR}),
	("SwapchainCreateProtectedBit",
		#{const VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR}),
	("SwapchainCreateMutableFormatBit",
		#{const VK_SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR}),
	("SwapchainCreateFlagBitsMaxEnum",
		#{const VK_SWAPCHAIN_CREATE_FLAG_BITS_MAX_ENUM_KHR})
	]

type SwapchainCreateFlags = SwapchainCreateFlagBits

data SwapchainTag
newtype Swapchain = Swapchain (Ptr SwapchainTag) deriving (Show, Storable)
type PtrSwapchain = Ptr Swapchain

pattern SwapchainNull :: Swapchain
pattern SwapchainNull <- Swapchain ((== (wordPtrToPtr $ WordPtr #{const VK_NULL_HANDLE})) -> P.True) where
	SwapchainNull = Swapchain . wordPtrToPtr $ WordPtr #{const VK_NULL_HANDLE}

struct "SwapchainCreateInfo" #{size VkSwapchainCreateInfoKHR}
		#{alignment VkSwapchainCreateInfoKHR} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkSwapchainCreateInfoKHR, sType} p
			(#{const VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR} ::
				#{type VkStructureType}) |]),
	("pNext", ''PtrVoid, [| #{peek VkSwapchainCreateInfoKHR, pNext} |],
		[| #{poke VkSwapchainCreateInfoKHR, pNext} |]),
	("flags", ''SwapchainCreateFlags,
		[| #{peek VkSwapchainCreateInfoKHR, flags} |],
		[| #{poke VkSwapchainCreateInfoKHR, flags} |]),
	("surface", ''Surface, [| #{peek VkSwapchainCreateInfoKHR, surface} |],
		[| #{poke VkSwapchainCreateInfoKHR, surface} |]),
	("minImageCount", ''#{type uint32_t},
		[| #{peek VkSwapchainCreateInfoKHR, minImageCount} |],
		[| #{poke VkSwapchainCreateInfoKHR, minImageCount} |]),
	("imageFormat", ''Format,
		[| #{peek VkSwapchainCreateInfoKHR, imageFormat} |],
		[| #{poke VkSwapchainCreateInfoKHR, imageFormat} |]),
	("imageColorSpace", ''ColorSpace,
		[| #{peek VkSwapchainCreateInfoKHR, imageColorSpace} |],
		[| #{poke VkSwapchainCreateInfoKHR, imageColorSpace} |]),
	("imageExtent", ''Extent2d,
		[| #{peek VkSwapchainCreateInfoKHR, imageExtent} |],
		[| #{poke VkSwapchainCreateInfoKHR, imageExtent} |]),
	("imageArrayLayers", ''#{type uint32_t},
		[| #{peek VkSwapchainCreateInfoKHR, imageArrayLayers} |],
		[| #{poke VkSwapchainCreateInfoKHR, imageArrayLayers} |]),
	("imageUsage", ''ImageUsageFlags,
		[| #{peek VkSwapchainCreateInfoKHR, imageUsage} |],
		[| #{poke VkSwapchainCreateInfoKHR, imageUsage} |]),
	("imageSharingMode", ''SharingMode,
		[| #{peek VkSwapchainCreateInfoKHR, imageSharingMode} |],
		[| #{poke VkSwapchainCreateInfoKHR, imageSharingMode} |]),
	("queueFamilyIndexCount", ''#{type uint32_t},
		[| #{peek VkSwapchainCreateInfoKHR, queueFamilyIndexCount} |],
		[| #{poke VkSwapchainCreateInfoKHR, queueFamilyIndexCount} |]),
	("pQueueFamilyIndices", ''PtrUint32T,
		[| #{peek VkSwapchainCreateInfoKHR, pQueueFamilyIndices} |],
		[| #{poke VkSwapchainCreateInfoKHR, pQueueFamilyIndices} |]),
	("preTransform", ''SurfaceTransformFlagBits,
		[| #{peek VkSwapchainCreateInfoKHR, preTransform} |],
		[| #{poke VkSwapchainCreateInfoKHR, preTransform} |]),
	("compositeAlpha", ''CompositeAlphaFlagBits,
		[| #{peek VkSwapchainCreateInfoKHR, compositeAlpha} |],
		[| #{poke VkSwapchainCreateInfoKHR, compositeAlpha} |]),
	("presentMode", ''PresentMode,
		[| #{peek VkSwapchainCreateInfoKHR, presentMode} |],
		[| #{poke VkSwapchainCreateInfoKHR, presentMode} |]),
	("clipped", ''Bool32,
		[| #{peek VkSwapchainCreateInfoKHR, clipped} |],
		[| #{poke VkSwapchainCreateInfoKHR, clipped} |]),
	("oldSwapchain", ''Swapchain,
		[| #{peek VkSwapchainCreateInfoKHR, oldSwapchain} |],
		[| #{poke VkSwapchainCreateInfoKHR, oldSwapchain} |])
	]
	[''Show, ''Storable]
