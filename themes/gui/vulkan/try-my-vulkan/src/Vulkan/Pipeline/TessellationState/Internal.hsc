{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.TessellationState.Internal where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Enum
import Foreign.C.Struct
import Data.Word

import Vulkan.Base

import qualified Vulkan.StructureType as ST

#include <vulkan/vulkan.h>

enum "CreateFlagBits" ''#{type VkPipelineTessellationStateCreateFlags}
	[''Show, ''Storable] [("CreateFlagsZero", 0)]

type CreateFlags = CreateFlagBits

struct "CreateInfo" #{size VkPipelineTessellationStateCreateInfo}
		#{alignment VkPipelineTessellationStateCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPipelineTessellationStateCreateInfo, sType}
			p ST.pipelineTessellationStateCreateInfo |]),
	("pNext", ''PtrVoid,
		[| #{peek VkPipelineTessellationStateCreateInfo, pNext} |],
		[| #{poke VkPipelineTessellationStateCreateInfo, pNext} |]),
	("flags", ''CreateFlags,
		[| #{peek VkPipelineTessellationStateCreateInfo, flags} |],
		[| #{poke VkPipelineTessellationStateCreateInfo, flags} |]),
	("patchControlPoints", ''#{type uint32_t},
		[| #{peek VkPipelineTessellationStateCreateInfo,
			patchControlPoints} |],
		[| #{poke VkPipelineTessellationStateCreateInfo,
			patchControlPoints} |]) ]
	[''Show, ''Storable]

type PtrCreateInfo = Ptr CreateInfo
