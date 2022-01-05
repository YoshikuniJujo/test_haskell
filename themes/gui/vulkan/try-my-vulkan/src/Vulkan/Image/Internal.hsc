{-# LANGUAGE TemplateHaskell, TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Image.Internal where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Enum
import Foreign.C.Struct
import Data.Bits
import Data.Word

import Vulkan.Base
import Vulkan.Format
import Vulkan.ImageAspectFlagBits

#define VK_ENABLE_BETA_EXTENSIONS
#include <vulkan/vulkan.h>

enum "ImageUsageFlagBits" ''#{type VkImageUsageFlags}
		[''Show, ''Eq, ''Bits, ''Storable] [
	("ImageUsageTransferSrcBit", #{const VK_IMAGE_USAGE_TRANSFER_SRC_BIT}),
	("ImageUsageTransferDstBit", #{const VK_IMAGE_USAGE_TRANSFER_DST_BIT}),
	("ImageUsageSampledBit", #{const VK_IMAGE_USAGE_SAMPLED_BIT}),
	("ImageUsageStorageBit", #{const VK_IMAGE_USAGE_STORAGE_BIT}),
	("ImageUsageColorAttachmentBit",
		#{const VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT}),
	("ImageUsageDepthStencilAttachmentBit",
		#{const VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT}),
	("ImageUsageTransientAttachmentBit",
		#{const VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT}),
	("ImageUsageInputAttachmentBit",
		#{const VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT}),
	("ImageUsageVideoDecodeDstBit",
		#{const VK_IMAGE_USAGE_VIDEO_DECODE_DST_BIT_KHR}),
	("ImageUsageVideoDecodeSrcBit",
		#{const VK_IMAGE_USAGE_VIDEO_DECODE_SRC_BIT_KHR}),
	("ImageUsageVideoDecodeDpbBit",
		#{const VK_IMAGE_USAGE_VIDEO_DECODE_DPB_BIT_KHR}),
	("ImageUsageDensityMapBit",
		#{const VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT}),
	("ImageUsageFragmentShadingRateAttachmentBit",
		#{const VK_IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR}),
	("ImageUsageVideoEncodeDstBit",
		#{const VK_IMAGE_USAGE_VIDEO_ENCODE_DST_BIT_KHR}),
	("ImageUsageVideoEncodeSrcBit",
		#{const VK_IMAGE_USAGE_VIDEO_ENCODE_SRC_BIT_KHR}),
	("ImageUsageVideoEncodeDpbBit",
		#{const VK_IMAGE_USAGE_VIDEO_ENCODE_DPB_BIT_KHR}),
	("ImageUsageInvocationMaskBitHuawei",
		#{const VK_IMAGE_USAGE_INVOCATION_MASK_BIT_HUAWEI}),
	("ImageUsageShadingRateImageBitNv",
		#{const VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV}) ]

type ImageUsageFlags = ImageUsageFlagBits

newtype Image = Image (Ptr Image) deriving (Show, Storable)

enum "ImageViewCreateFlagBits" ''#{type VkImageViewCreateFlagBits}
		[''Show, ''Eq, ''Storable, ''Bits] [
	("ImageViewCreateFlagsZero", 0),
	("ImageViewCreateFragmentDensityMapDynamicBitExt", #{const
		VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT}),
	("ImageViewCreateFragmentDensityMapDeferredBitExt", #{const
		VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DEFERRED_BIT_EXT}) ]

type ImageViewCreateFlags = ImageViewCreateFlagBits

enum "ImageViewType" ''#{type VkImageViewType} [''Show, ''Storable] [
	("ImageViewType1d", #{const VK_IMAGE_VIEW_TYPE_1D}),
	("ImageViewType2d", #{const VK_IMAGE_VIEW_TYPE_2D}),
	("ImageViewType3d", #{const VK_IMAGE_VIEW_TYPE_3D}),
	("ImageViewTypeCube", #{const VK_IMAGE_VIEW_TYPE_CUBE}),
	("ImageViewType1dArray", #{const VK_IMAGE_VIEW_TYPE_1D_ARRAY}),
	("ImageViewType2dArray", #{const VK_IMAGE_VIEW_TYPE_2D_ARRAY}),
	("ImageViewTypeCubeArray", #{const VK_IMAGE_VIEW_TYPE_CUBE_ARRAY}) ]

enum "ComponentSwizzle" ''#{type VkComponentSwizzle} [''Show, ''Storable] [
	("ComponentSwizzleIdentity", #{const VK_COMPONENT_SWIZZLE_IDENTITY}),
	("ComponentSwizzleZero", #{const VK_COMPONENT_SWIZZLE_ZERO}),
	("ComponentSwizzleOne", #{const VK_COMPONENT_SWIZZLE_ONE}),
	("ComponentSwizzleR", #{const VK_COMPONENT_SWIZZLE_R}),
	("ComponentSwizzleG", #{const VK_COMPONENT_SWIZZLE_G}),
	("ComponentSwizzleB", #{const VK_COMPONENT_SWIZZLE_B}),
	("ComponentSwizzleA", #{const VK_COMPONENT_SWIZZLE_A}) ]

struct "ComponentMapping" #{size VkComponentMapping}
		#{alignment VkComponentMapping} [
	("r", ''ComponentSwizzle, [| #{peek VkComponentMapping, r} |],
		[| #{poke VkComponentMapping, r} |]),
	("g", ''ComponentSwizzle, [| #{peek VkComponentMapping, g} |],
		[| #{poke VkComponentMapping, g} |]),
	("b", ''ComponentSwizzle, [| #{peek VkComponentMapping, b} |],
		[| #{poke VkComponentMapping, b} |]),
	("a", ''ComponentSwizzle, [| #{peek VkComponentMapping, a} |],
		[| #{poke VkComponentMapping, a} |]) ]
	[''Show, ''Storable]

struct "ImageSubresourceRange" #{size VkImageSubresourceRange}
		#{alignment VkImageSubresourceRange} [
	("aspectMask", ''ImageAspectFlags,
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

struct "ImageViewCreateInfo" #{size VkImageViewCreateInfo}
		#{alignment VkImageViewCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkImageViewCreateInfo, sType} p
			(#{const VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO} ::
				#{type VkStructureType}) |]),
	("pNext", ''PtrVoid, [| #{peek VkImageViewCreateInfo, pNext} |],
		[| #{poke VkImageViewCreateInfo, pNext} |]),
	("flags", ''ImageViewCreateFlags,
		[| #{peek VkImageViewCreateInfo, flags} |],
		[| #{poke VkImageViewCreateInfo, flags} |]),
	("image", ''Image,
		[| #{peek VkImageViewCreateInfo, image} |],
		[| #{poke VkImageViewCreateInfo, image} |]),
	("viewType", ''ImageViewType,
		[| #{peek VkImageViewCreateInfo, viewType} |],
		[| #{poke VkImageViewCreateInfo, viewType} |]),
	("format", ''Format,
		[| #{peek VkImageViewCreateInfo, format} |],
		[| #{poke VkImageViewCreateInfo, format} |]),
	("components", ''ComponentMapping,
		[| #{peek VkImageViewCreateInfo, components} |],
		[| #{poke VkImageViewCreateInfo, components} |]),
	("subresourceRange", ''ImageSubresourceRange,
		[| #{peek VkImageViewCreateInfo, subresourceRange} |],
		[| #{poke VkImageViewCreateInfo, subresourceRange} |]) ]
	[''Show]
