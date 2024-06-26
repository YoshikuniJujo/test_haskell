-- This file is automatically generated by the tools/makeEnumVkImageLayout
--	% stack runghc --cwd tools/ makeEnumVkImageLayout

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.ImageLayout where

import Foreign.Storable
import Foreign.C.Enum
import Data.Word

#include <vulkan/vulkan.h>

enum "ImageLayout" ''#{type VkImageLayout}
		[''Show, ''Eq, ''Storable] [
	("ImageLayoutUndefined", #{const VK_IMAGE_LAYOUT_UNDEFINED}),
	("ImageLayoutGeneral", #{const VK_IMAGE_LAYOUT_GENERAL}),
	("ImageLayoutColorAttachmentOptimal",
		#{const VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL}),
	("ImageLayoutDepthStencilAttachmentOptimal",
		#{const VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL}),
	("ImageLayoutDepthStencilReadOnlyOptimal",
		#{const VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL}),
	("ImageLayoutShaderReadOnlyOptimal",
		#{const VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL}),
	("ImageLayoutTransferSrcOptimal",
		#{const VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL}),
	("ImageLayoutTransferDstOptimal",
		#{const VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL}),
	("ImageLayoutPreinitialized", #{const VK_IMAGE_LAYOUT_PREINITIALIZED}),
	("ImageLayoutDepthReadOnlyStencilAttachmentOptimal",
		#{const VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL}),
	("ImageLayoutDepthAttachmentStencilReadOnlyOptimal",
		#{const VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL}),
	("ImageLayoutDepthAttachmentOptimal",
		#{const VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL}),
	("ImageLayoutDepthReadOnlyOptimal",
		#{const VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL}),
	("ImageLayoutStencilAttachmentOptimal",
		#{const VK_IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL}),
	("ImageLayoutStencilReadOnlyOptimal",
		#{const VK_IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL}),
	("ImageLayoutPresentSrcKhr", #{const VK_IMAGE_LAYOUT_PRESENT_SRC_KHR}),
	("ImageLayoutSharedPresentKhr",
		#{const VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR}),
	("ImageLayoutFragmentDensityMapOptimalExt",
		#{const VK_IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT}),
	("ImageLayoutFragmentShadingRateAttachmentOptimalKhr",
		#{const VK_IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR}),
	("ImageLayoutReadOnlyOptimalKhr",
		#{const VK_IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR}),
	("ImageLayoutAttachmentOptimalKhr",
		#{const VK_IMAGE_LAYOUT_ATTACHMENT_OPTIMAL_KHR}),
	("ImageLayoutDepthReadOnlyStencilAttachmentOptimalKhr",
		#{const VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL_KHR}),
	("ImageLayoutDepthAttachmentStencilReadOnlyOptimalKhr",
		#{const VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL_KHR}),
	("ImageLayoutShadingRateOptimalNv",
		#{const VK_IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV}),
	("ImageLayoutDepthAttachmentOptimalKhr",
		#{const VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL_KHR}),
	("ImageLayoutDepthReadOnlyOptimalKhr",
		#{const VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL_KHR}),
	("ImageLayoutStencilAttachmentOptimalKhr",
		#{const VK_IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL_KHR}),
	("ImageLayoutStencilReadOnlyOptimalKhr",
		#{const VK_IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL_KHR}),
	("ImageLayoutMaxEnum", #{const VK_IMAGE_LAYOUT_MAX_ENUM}) ]
