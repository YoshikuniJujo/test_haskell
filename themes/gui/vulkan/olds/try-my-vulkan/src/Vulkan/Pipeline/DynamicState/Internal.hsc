{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.DynamicState.Internal where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Enum
import Foreign.C.Struct
import Data.Word

import Vulkan.Base
import Vulkan.DynamicState

import qualified Vulkan.StructureType as ST

#include <vulkan/vulkan.h>

enum "CreateFlagBits" ''#{type VkPipelineDynamicStateCreateFlags}
	[''Show, ''Storable] [("CreateFlagBitsZero", 0)]

type CreateFlags = CreateFlagBits

struct "CreateInfo" #{size VkPipelineDynamicStateCreateInfo}
		#{alignment VkPipelineDynamicStateCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPipelineDynamicStateCreateInfo, sType} p
			ST.pipelineDynamicStateCreateInfo |]),
	("pNext", ''PtrVoid,
		[| #{peek VkPipelineDynamicStateCreateInfo, pNext} |],
		[| #{poke VkPipelineDynamicStateCreateInfo, pNext} |]),
	("flags", ''CreateFlags,
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
