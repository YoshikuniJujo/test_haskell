-- This file is automatically generated by the tools/makeEnumVkImageAspectFlagBits.hs
--	% stack runghc --cwd tools/ makeEnumVkImageAspectFlagBits.hs

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.ImageAspectFlagBits where

import Foreign.Storable
import Foreign.C.Enum
import Data.Word

#include <vulkan/vulkan.h>

enum "ImageAspectFlagBits" ''#{type VkImageAspectFlagBits} [''Show, ''Eq, ''Storable] [
	("ImageAspectColorBit", #{const VK_IMAGE_ASPECT_COLOR_BIT}),
	("ImageAspectDepthBit", #{const VK_IMAGE_ASPECT_DEPTH_BIT}),
	("ImageAspectStencilBit", #{const VK_IMAGE_ASPECT_STENCIL_BIT}),
	("ImageAspectMetadataBit", #{const VK_IMAGE_ASPECT_METADATA_BIT}),
	("ImageAspectPlane0Bit", #{const VK_IMAGE_ASPECT_PLANE_0_BIT}),
	("ImageAspectPlane1Bit", #{const VK_IMAGE_ASPECT_PLANE_1_BIT}),
	("ImageAspectPlane2Bit", #{const VK_IMAGE_ASPECT_PLANE_2_BIT}),
	("ImageAspectMemoryPlane0BitExt",
		#{const VK_IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT}),
	("ImageAspectMemoryPlane1BitExt",
		#{const VK_IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT}),
	("ImageAspectMemoryPlane2BitExt",
		#{const VK_IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT}),
	("ImageAspectMemoryPlane3BitExt",
		#{const VK_IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT}),
	("ImageAspectPlane0BitKhr", #{const VK_IMAGE_ASPECT_PLANE_0_BIT_KHR}),
	("ImageAspectPlane1BitKhr", #{const VK_IMAGE_ASPECT_PLANE_1_BIT_KHR}),
	("ImageAspectPlane2BitKhr", #{const VK_IMAGE_ASPECT_PLANE_2_BIT_KHR}),
	("ImageAspectFlagBitsMaxEnum",
		#{const VK_IMAGE_ASPECT_FLAG_BITS_MAX_ENUM}) ]

type ImageAspectFlags = ImageAspectFlagBits