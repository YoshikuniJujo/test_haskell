{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Image.Internal where

import Foreign.Storable
import Foreign.C.Enum
import Data.Bits
import Data.Word

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
