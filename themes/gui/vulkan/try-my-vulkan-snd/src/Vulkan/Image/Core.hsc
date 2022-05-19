{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Image.Core where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word
import Data.Int

import Vulkan.Base
import Vulkan.Core

import qualified Vulkan.AllocationCallbacks.Core as AllocationCallbacks
import qualified Vulkan.Device.Core as Device
import qualified Vulkan.Memory.Core as Memory

#include <vulkan/vulkan.h>

struct "SubresourceRange" #{size VkImageSubresourceRange}
		#{alignment VkImageSubresourceRange} [
	("aspectMask", ''#{type VkImageAspectFlags},
		[| #{peek VkImageSubresourceRange, aspectMask} |],
		[| #{poke VkImageSubresourceRange, aspectMask} |]),
	("baseMipLevel", ''#{type uint32_t},
		[| #{peek VkImageSubresourceRange, baseMipLevel} |],
		[| #{poke VkImageSubresourceRange, baseMipLevel} |]),
	("levelCount", ''#{type uint32_t},
		[| #{peek VkImageSubresourceRange, levelCount} |],
		[| #{poke VkImageSubresourceRange, levelCount} |]),
	("baseArrayLayer", ''#{type uint32_t},
		[| #{peek VkImageSubresourceRange, baseArrayLayer} |],
		[| #{poke VkImageSubresourceRange, baseArrayLayer} |]),
	("layerCount", ''#{type uint32_t},
		[| #{peek VkImageSubresourceRange, layerCount} |],
		[| #{poke VkImageSubresourceRange, layerCount} |]) ]
	[''Show, ''Storable]

data ITag
type I = Ptr ITag

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO}

struct "CreateInfo" #{size VkImageCreateInfo} #{alignment VkImageCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkImageCreateInfo, sType} p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkImageCreateInfo, pNext} |],
		[| #{poke VkImageCreateInfo, pNext} |]),
	("flags", ''#{type VkImageCreateFlags},
		[| #{peek VkImageCreateInfo, flags} |],
		[| #{poke VkImageCreateInfo, flags} |]),
	("imageType", ''#{type VkImageType},
		[| #{peek VkImageCreateInfo, imageType} |],
		[| #{poke VkImageCreateInfo, imageType} |]),
	("format", ''#{type VkFormat},
		[| #{peek VkImageCreateInfo, format} |],
		[| #{poke VkImageCreateInfo, format} |]),
	("extent", ''Extent3d,
		[| #{peek VkImageCreateInfo, extent} |],
		[| #{poke VkImageCreateInfo, extent} |]),
	("mipLevels", ''#{type uint32_t},
		[| #{peek VkImageCreateInfo, mipLevels} |],
		[| #{poke VkImageCreateInfo, mipLevels} |]),
	("arrayLayers", ''#{type uint32_t},
		[| #{peek VkImageCreateInfo, arrayLayers} |],
		[| #{poke VkImageCreateInfo, arrayLayers} |]),
	("samples", ''#{type VkSampleCountFlagBits},
		[| #{peek VkImageCreateInfo, samples} |],
		[| #{poke VkImageCreateInfo, samples} |]),
	("tiling", ''#{type VkImageTiling},
		[| #{peek VkImageCreateInfo, tiling} |],
		[| #{poke VkImageCreateInfo, tiling} |]),
	("usage", ''#{type VkImageUsageFlags},
		[| #{peek VkImageCreateInfo, usage} |],
		[| #{poke VkImageCreateInfo, usage} |]),
	("sharingMode", ''#{type VkSharingMode},
		[| #{peek VkImageCreateInfo, sharingMode} |],
		[| #{poke VkImageCreateInfo, sharingMode} |]),
	("queueFamilyIndexCount", ''#{type uint32_t},
		[| #{peek VkImageCreateInfo, queueFamilyIndexCount} |],
		[| #{poke VkImageCreateInfo, queueFamilyIndexCount} |]),
	("pQueueFamilyIndices", ''PtrUint32T,
		[| #{peek VkImageCreateInfo, pQueueFamilyIndices} |],
		[| #{poke VkImageCreateInfo, pQueueFamilyIndices} |]),
	("initialLayout", ''#{type VkImageLayout},
		[| #{peek VkImageCreateInfo, initialLayout} |],
		[| #{poke VkImageCreateInfo, initialLayout} |]) ]
	[''Show, ''Storable]

foreign import ccall "vkCreateImage" create ::
	Device.D -> Ptr CreateInfo -> Ptr AllocationCallbacks.A -> Ptr I ->
	IO #{type VkResult}

foreign import ccall "vkGetImageMemoryRequirements" getMemoryRequirements ::
	Device.D -> I -> Ptr Memory.Requirements -> IO ()
