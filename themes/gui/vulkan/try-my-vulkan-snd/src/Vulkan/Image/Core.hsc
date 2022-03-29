{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Image.Core where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

#include <vulkan/vulkan.h>

data ImageTag
type Image = Ptr ImageTag

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

layoutColorAttachmentOptimal :: #{type VkImageLayout}
layoutColorAttachmentOptimal = #{const VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL}
