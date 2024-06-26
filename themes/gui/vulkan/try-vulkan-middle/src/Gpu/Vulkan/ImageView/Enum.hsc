-- This file is automatically generated by the tools/makeEnum.hs
--	% stack runghc --cwd tools/ makeEnum

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-export-lists -fno-warn-tabs #-}

module Gpu.Vulkan.ImageView.Enum where

import Foreign.Storable
import Foreign.C.Enum
import Data.Bits
import Data.Word

#include <vulkan/vulkan.h>

enum "CreateFlagBits" ''#{type VkImageViewCreateFlagBits}
		[''Show, ''Eq, ''Storable, ''Bits] [
	("CreateFlagsZero", 0),
	("CreateFragmentDensityMapDynamicBitExt",
		#{const VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT}),
	("CreateDescriptorBufferCaptureReplayBitExt",
		#{const VK_IMAGE_VIEW_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT}),
	("CreateFragmentDensityMapDeferredBitExt",
		#{const VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DEFERRED_BIT_EXT}),
	("CreateFlagBitsMaxEnum",
		#{const VK_IMAGE_VIEW_CREATE_FLAG_BITS_MAX_ENUM}) ]

enum "Type" ''#{type VkImageViewType}
		[''Show, ''Eq, ''Storable] [
	("Type1d", #{const VK_IMAGE_VIEW_TYPE_1D}),
	("Type2d", #{const VK_IMAGE_VIEW_TYPE_2D}),
	("Type3d", #{const VK_IMAGE_VIEW_TYPE_3D}),
	("TypeCube", #{const VK_IMAGE_VIEW_TYPE_CUBE}),
	("Type1dArray", #{const VK_IMAGE_VIEW_TYPE_1D_ARRAY}),
	("Type2dArray", #{const VK_IMAGE_VIEW_TYPE_2D_ARRAY}),
	("TypeCubeArray", #{const VK_IMAGE_VIEW_TYPE_CUBE_ARRAY}),
	("TypeMaxEnum", #{const VK_IMAGE_VIEW_TYPE_MAX_ENUM}) ]

type CreateFlags = CreateFlagBits
