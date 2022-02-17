{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.ImageView where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word
import Data.Int

import Vulkan.Base
import Vulkan.Device (Device)

import qualified Vulkan.Image as Image
import qualified Vulkan.Component as Component

#include <vulkan/vulkan.h>

data ImageViewTag
type ImageView = Ptr ImageViewTag
type PtrImageView = Ptr ImageView

strType :: #{type VkStructureType}
strType = #{const VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO}

struct "CreateInfo" #{size VkImageViewCreateInfo}
		#{alignment VkImageViewCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkImageViewCreateInfo, sType} p strType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkImageViewCreateInfo, pNext} |],
		[| #{poke VkImageViewCreateInfo, pNext} |]),
	("flags", ''#{type VkImageViewCreateFlags},
		[| #{peek VkImageViewCreateInfo, flags} |],
		[| #{poke VkImageViewCreateInfo, flags} |]),
	("image", ''Image.Image,
		[| #{peek VkImageViewCreateInfo, image} |],
		[| #{poke VkImageViewCreateInfo, image} |]),
	("viewType", ''#{type VkImageViewType},
		[| #{peek VkImageViewCreateInfo, viewType} |],
		[| #{poke VkImageViewCreateInfo, viewType} |]),
	("format", ''#{type VkFormat},
		[| #{peek VkImageViewCreateInfo, format} |],
		[| #{poke VkImageViewCreateInfo, format} |]),
	("components", ''Component.Mapping,
		[| #{peek VkImageViewCreateInfo, components} |],
		[| #{poke VkImageViewCreateInfo, components} |]),
	("subresourceRange", ''Image.SubresourceRange,
		[| #{peek VkImageViewCreateInfo, subresourceRange} |],
		[| #{poke VkImageViewCreateInfo, subresourceRange} |]) ]
	[''Show, ''Storable]

type2d :: #{type VkImageViewType}
type2d = #{const VK_IMAGE_VIEW_TYPE_2D}

foreign import ccall "vkCreateImageView" create ::
	Device -> Ptr CreateInfo -> Ptr () -> Ptr ImageView ->
	IO #{type VkResult}

foreign import ccall "vkDestroyImageView" destroy ::
	Device -> ImageView -> Ptr () -> IO ()
