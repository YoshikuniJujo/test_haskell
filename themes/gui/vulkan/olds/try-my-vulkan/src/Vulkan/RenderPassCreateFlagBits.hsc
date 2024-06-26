-- This file is automatically generated by the tools/makeEnumVkRenderPassCreateFlagBits
--	% stack runghc --cwd tools/ makeEnumVkRenderPassCreateFlagBits

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.RenderPassCreateFlagBits where

import Foreign.Storable
import Foreign.C.Enum
import Data.Word
import Data.Bits

#include <vulkan/vulkan.h>

enum "RenderPassCreateFlagBits" ''#{type VkRenderPassCreateFlagBits}
		[''Show, ''Eq, ''Storable, ''Bits] [
	("RenderPassCreateFlagsZero", 0),
	("RenderPassCreateTransformBitQcom",
		#{const VK_RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM}),
	("RenderPassCreateFlagBitsMaxEnum",
		#{const VK_RENDER_PASS_CREATE_FLAG_BITS_MAX_ENUM}) ]

type RenderPassCreateFlags = RenderPassCreateFlagBits
