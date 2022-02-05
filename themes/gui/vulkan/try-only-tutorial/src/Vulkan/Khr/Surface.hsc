{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr.Surface where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan (Extent2d)
import Vulkan.Instance (Instance)

#include <vulkan/vulkan.h>

data SurfaceTag
type Surface = Ptr SurfaceTag

foreign import ccall "vkDestroySurfaceKHR" destroy ::
	Instance -> Surface -> Ptr () -> IO ()

struct "Capabilities" #{size VkSurfaceCapabilitiesKHR}
		#{alignment VkSurfaceCapabilitiesKHR} [
	("minImageCount", ''#{type uint32_t},
		[| #{peek VkSurfaceCapabilitiesKHR, minImageCount} |],
		[| #{poke VkSurfaceCapabilitiesKHR, minImageCount} |]),
	("maxImageCount", ''#{type uint32_t},
		[| #{peek VkSurfaceCapabilitiesKHR, maxImageCount} |],
		[| #{poke VkSurfaceCapabilitiesKHR, maxImageCount} |]),
	("currentExtent", ''Extent2d,
		[| #{peek VkSurfaceCapabilitiesKHR, currentExtent} |],
		[| #{poke VkSurfaceCapabilitiesKHR, currentExtent} |]),
	("minImageExtent", ''Extent2d,
		[| #{peek VkSurfaceCapabilitiesKHR, minImageExtent} |],
		[| #{poke VkSurfaceCapabilitiesKHR, minImageExtent} |]),
	("maxImageExtent", ''Extent2d,
		[| #{peek VkSurfaceCapabilitiesKHR, maxImageExtent} |],
		[| #{poke VkSurfaceCapabilitiesKHR, maxImageExtent} |]),
	("maxImageArrayLayers", ''#{type uint32_t},
		[| #{peek VkSurfaceCapabilitiesKHR, maxImageArrayLayers} |],
		[| #{poke VkSurfaceCapabilitiesKHR, maxImageArrayLayers} |]),
	("supportedTransforms", ''#{type VkSurfaceTransformFlagsKHR},
		[| #{peek VkSurfaceCapabilitiesKHR, supportedTransforms} |],
		[| #{poke VkSurfaceCapabilitiesKHR, supportedTransforms} |]),
	("currentTransform", ''#{type VkSurfaceTransformFlagBitsKHR},
		[| #{peek VkSurfaceCapabilitiesKHR, currentTransform} |],
		[| #{poke VkSurfaceCapabilitiesKHR, currentTransform} |]),
	("supportedCompositeAlpha", ''#{type VkCompositeAlphaFlagsKHR},
		[| #{peek VkSurfaceCapabilitiesKHR, supportedCompositeAlpha} |],
		[| #{poke VkSurfaceCapabilitiesKHR,
			supportedCompositeAlpha} |]),
	("supportedUsageFlags", ''#{type VkImageUsageFlags},
		[| #{peek VkSurfaceCapabilitiesKHR, supportedUsageFlags} |],
		[| #{poke VkSurfaceCapabilitiesKHR, supportedUsageFlags} |]) ]
	[''Show, ''Storable]

struct "Format" #{size VkSurfaceFormatKHR} #{alignment VkSurfaceFormatKHR} [
	("format", ''#{type VkFormat}, [| #{peek VkSurfaceFormatKHR, format} |],
		[| #{poke VkSurfaceFormatKHR, format} |]),
	("colorSpace", ''#{type VkColorSpaceKHR},
		[| #{peek VkSurfaceFormatKHR, colorSpace} |],
		[| #{poke VkSurfaceFormatKHR, colorSpace} |]) ]
	[''Show, ''Storable]
