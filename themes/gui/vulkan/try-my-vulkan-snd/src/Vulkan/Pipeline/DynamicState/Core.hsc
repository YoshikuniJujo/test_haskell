{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.DynamicState.Core where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Enum
import Vulkan.Base

#include <vulkan/vulkan.h>

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO}

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
	("pDynamicStates", ''PtrDynamicState,
		[| #{peek VkPipelineDynamicStateCreateInfo, pDynamicStates} |],
		[| #{poke VkPipelineDynamicStateCreateInfo, pDynamicStates} |])
	]
	[''Show, ''Storable]

type PtrCreateInfo = Ptr CreateInfo
