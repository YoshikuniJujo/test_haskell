{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Layout.Internal where

import Foreign.Storable
import Foreign.C.Enum
import Foreign.C.Struct
import Data.Word

import Vulkan.Base

import qualified Vulkan.StructureType as ST

#include <vulkan/vulkan.h>

enum "CreateFlagBits" ''#{type VkPipelineLayoutCreateFlags}
	[''Show, ''Storable] [("CreateFlagBitsZero", 0)]

type CreateFlags = CreateFlagBits

struct "CreateInfo" #{size VkPipelineLayoutCreateInfo}
		#{alignment VkPipelineLayoutCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPipelineLayoutCreateInfo, sType} p
			ST.pipelineLayoutCreateInfo |]),
	("pNext", ''PtrVoid, [| #{peek VkPipelineLayoutCreateInfo, pNext} |],
		[| #{poke VkPipelineLayoutCreateInfo, pNext} |]),
	("flags", ''CreateFlags,
		[| #{peek VkPipelineLayoutCreateInfo, flags} |],
		[| #{poke VkPipelineLayoutCreateInfo, flags} |]),
	("setLayoutCount", ''#{type uint32_t},
		[| #{peek VkPipelineLayoutCreateInfo, setLayoutCount} |],
		[| #{poke VkPipelineLayoutCreateInfo, setLayoutCount} |])
	]
	[''Show, ''Storable]
