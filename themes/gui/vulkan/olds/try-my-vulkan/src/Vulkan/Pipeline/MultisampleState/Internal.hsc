{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.MultisampleState.Internal where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Enum
import Foreign.C.Struct
import Data.Word

import Vulkan.Base
import Vulkan.SampleCountFlagBits

import qualified Vulkan.StructureType as ST

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineMultisampleStateCreateFlags}
	[''Show, ''Storable] [("CreateFlagsZero", 0)]

struct "CreateInfo" #{size VkPipelineMultisampleStateCreateInfo}
		#{alignment VkPipelineMultisampleStateCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPipelineMultisampleStateCreateInfo, sType} p
			ST.pipelineMultisampleStateCreateInfo |]),
	("pNext", ''PtrVoid,
		[| #{peek VkPipelineMultisampleStateCreateInfo, pNext} |],
		[| #{poke VkPipelineMultisampleStateCreateInfo, pNext} |]),
	("flags", ''CreateFlags,
		[| #{peek VkPipelineMultisampleStateCreateInfo, flags} |],
		[| #{poke VkPipelineMultisampleStateCreateInfo, flags} |]),
	("rasterizationSamples", ''SampleCountFlags,
		[| #{peek VkPipelineMultisampleStateCreateInfo,
			rasterizationSamples} |],
		[| #{poke VkPipelineMultisampleStateCreateInfo,
			rasterizationSamples} |]),
	("sampleShadingEnable", ''Bool32,
		[| #{peek VkPipelineMultisampleStateCreateInfo,
			sampleShadingEnable} |],
		[| #{poke VkPipelineMultisampleStateCreateInfo,
			sampleShadingEnable} |]),
	("minSampleShading", ''#{type float},
		[| #{peek VkPipelineMultisampleStateCreateInfo,
			minSampleShading} |],
		[| #{poke VkPipelineMultisampleStateCreateInfo,
			minSampleShading} |]),
	("pSampleMask", ''PtrSampleMask,
		[| #{peek VkPipelineMultisampleStateCreateInfo, pSampleMask} |],
		[| #{poke VkPipelineMultisampleStateCreateInfo,
			pSampleMask} |]),
	("alphaToCoverageEnable", ''Bool32,
		[| #{peek VkPipelineMultisampleStateCreateInfo,
			alphaToCoverageEnable} |],
		[| #{poke VkPipelineMultisampleStateCreateInfo,
			alphaToCoverageEnable} |]),
	("alphaToOneEnable", ''Bool32,
		[| #{peek VkPipelineMultisampleStateCreateInfo,
			alphaToOneEnable} |],
		[| #{poke VkPipelineMultisampleStateCreateInfo,
			alphaToOneEnable} |]) ]
	[''Show, ''Storable]

type PtrCreateInfo = Ptr CreateInfo
