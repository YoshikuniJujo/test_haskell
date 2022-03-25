{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.MultisampleState where

import Foreign.Storable
import Foreign.C.Enum
import Data.Bits
import Data.Word

import Vulkan.Sample.Enum as Sample

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineMultisampleStateCreateFlags}
	[''Show, ''Eq, ''Storable, ''Bits] [("CreateFlagsZero", 0)]

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoRasterizationSamples :: Sample.CountFlagBits,
	createInfoSampleShadingEnable :: Bool,
	createInfoMinSampleShading :: Float
--	createInfo
	}
	deriving Show
