{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.MultisampleState.Core (

	-- * CREATE INFO

	CreateInfo, PtrCreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags,
	createInfoRasterizationSamples, createInfoSampleShadingEnable,
	createInfoMinSampleShading, createInfoPSampleMask,
	createInfoAlphaToCoverageEnable, createInfoAlphaToOneEnable

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word

#include <vulkan/vulkan.h>

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO}

type PtrSampleMask = Ptr #{type VkSampleMask}

struct "CreateInfo" #{size VkPipelineMultisampleStateCreateInfo}
		#{alignment VkPipelineMultisampleStateCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPipelineMultisampleStateCreateInfo, sType}
			p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkPipelineMultisampleStateCreateInfo, pNext} |],
		[| #{poke VkPipelineMultisampleStateCreateInfo, pNext} |]),
	("flags", ''#{type VkPipelineMultisampleStateCreateFlags},
		[| #{peek VkPipelineMultisampleStateCreateInfo, flags} |],
		[| #{poke VkPipelineMultisampleStateCreateInfo, flags} |]),
	("rasterizationSamples", ''#{type VkSampleCountFlagBits},
		[| #{peek VkPipelineMultisampleStateCreateInfo,
			rasterizationSamples} |],
		[| #{poke VkPipelineMultisampleStateCreateInfo,
			rasterizationSamples} |]),
	("sampleShadingEnable", ''#{type VkBool32},
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
	("alphaToCoverageEnable", ''#{type VkBool32},
		[| #{peek VkPipelineMultisampleStateCreateInfo,
			alphaToCoverageEnable} |],
		[| #{poke VkPipelineMultisampleStateCreateInfo,
			alphaToCoverageEnable} |]),
	("alphaToOneEnable", ''#{type VkBool32},
		[| #{peek VkPipelineMultisampleStateCreateInfo,
			alphaToOneEnable} |],
		[| #{poke VkPipelineMultisampleStateCreateInfo,
			alphaToOneEnable} |]) ]
	[''Show, ''Storable]

type PtrCreateInfo = Ptr CreateInfo
