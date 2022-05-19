{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Image.Core where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base

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
		[| #{poke VkImageCreateInfo, flags} |])
	]
	[''Show, ''Storable]
