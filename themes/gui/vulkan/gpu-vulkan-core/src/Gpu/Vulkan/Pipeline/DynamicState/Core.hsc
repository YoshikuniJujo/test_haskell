{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.DynamicState.Core (

	-- * CREATE INFO

	CreateInfo, PtrCreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags,
	createInfoDynamicStateCount, createInfoPDynamicStates

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word

#include <vulkan/vulkan.h>

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO}

type PtrVkDynamicState = Ptr #{type VkDynamicState}

struct "CreateInfo" #{size VkPipelineDynamicStateCreateInfo}
		#{alignment VkPipelineDynamicStateCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPipelineDynamicStateCreateInfo, sType}
			p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkPipelineDynamicStateCreateInfo, pNext} |],
		[| #{poke VkPipelineDynamicStateCreateInfo, pNext} |]),
	("flags", ''#{type VkPipelineDynamicStateCreateFlags},
		[| #{peek VkPipelineDynamicStateCreateInfo, flags} |],
		[| #{poke VkPipelineDynamicStateCreateInfo, flags} |]),
	("dynamicStateCount", ''#{type uint32_t},
		[| #{peek VkPipelineDynamicStateCreateInfo,
			dynamicStateCount} |],
		[| #{poke VkPipelineDynamicStateCreateInfo,
			dynamicStateCount} |]),
	("pDynamicStates", ''PtrVkDynamicState,
		[| #{peek VkPipelineDynamicStateCreateInfo, pDynamicStates} |],
		[| #{poke VkPipelineDynamicStateCreateInfo, pDynamicStates} |])
	]
	[''Show, ''Storable]

type PtrCreateInfo = Ptr CreateInfo
