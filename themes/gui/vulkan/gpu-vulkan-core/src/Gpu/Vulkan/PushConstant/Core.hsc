{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PushConstant.Core (

	-- * RANGE

	Range, PtrRange, pattern Range, rangeStageFlags, rangeOffset, rangeSize

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

#include <vulkan/vulkan.h>

struct "Range" #{size VkPushConstantRange} #{alignment VkPushConstantRange} [
	("stageFlags", ''#{type VkShaderStageFlags},
		[| #{peek VkPushConstantRange, stageFlags} |],
		[| #{poke VkPushConstantRange, stageFlags} |]),
	("offset", ''#{type uint32_t},
		[| #{peek VkPushConstantRange, offset} |],
		[| #{poke VkPushConstantRange, offset} |]),
	("size", ''#{type uint32_t},
		[| #{peek VkPushConstantRange, size} |],
		[| #{poke VkPushConstantRange, size} |]) ]
	[''Show, ''Storable]

type PtrRange = Ptr Range
