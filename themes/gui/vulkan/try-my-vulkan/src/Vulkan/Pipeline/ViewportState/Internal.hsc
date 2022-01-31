{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.ViewportState.Internal where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Enum
import Foreign.C.Struct
import Data.Word

import Vulkan.Base
import Vulkan.Viewport

import qualified Vulkan.StructureType as ST

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineViewportStateCreateFlags}
		[''Show, ''Storable] [ ("CreateFlagsZero", 0) ]

struct "CreateInfo" #{size VkPipelineViewportStateCreateInfo}
		#{alignment VkPipelineViewportStateCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPipelineViewportStateCreateInfo, sType} p
			ST.pipelineViewportStateCreateInfo |]),
	("pNext", ''PtrVoid,
		[| #{peek VkPipelineViewportStateCreateInfo, pNext} |],
		[| #{poke VkPipelineViewportStateCreateInfo, pNext} |]),
	("flags", ''CreateFlags,
		[| #{peek VkPipelineViewportStateCreateInfo, flags} |],
		[| #{poke VkPipelineViewportStateCreateInfo, flags} |]),
	("viewportCount", ''#{type uint32_t},
		[| #{peek VkPipelineViewportStateCreateInfo, viewportCount} |],
		[| #{poke VkPipelineViewportStateCreateInfo, viewportCount} |]),
	("pViewports", ''PtrViewport,
		[| #{peek VkPipelineViewportStateCreateInfo, pViewports} |],
		[| #{poke VkPipelineViewportStateCreateInfo, pViewports} |]),
	("scissorCount", ''#{type uint32_t},
		[| #{peek VkPipelineViewportStateCreateInfo, scissorCount} |],
		[| #{poke VkPipelineViewportStateCreateInfo, scissorCount} |]),
	("pScissors", ''PtrRect2d,
		[| #{peek VkPipelineViewportStateCreateInfo, pScissors} |],
		[| #{poke VkPipelineViewportStateCreateInfo, pScissors} |]) ]
	[''Show, ''Storable]

type PtrCreateInfo = Ptr CreateInfo
