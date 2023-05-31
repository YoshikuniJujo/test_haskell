{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.ViewportState.Core (

	-- * CREATE INFO

	CreateInfo, PtrCreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags,
	createInfoViewportCount, createInfoPViewports,
	createInfoScissorCount, createInfoPScissors

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word

import Gpu.Vulkan.Core

#include <vulkan/vulkan.h>

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO}

struct "CreateInfo" #{size VkPipelineViewportStateCreateInfo}
		#{alignment VkPipelineViewportStateCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPipelineViewportStateCreateInfo, sType}
			p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkPipelineViewportStateCreateInfo, pNext} |],
		[| #{poke VkPipelineViewportStateCreateInfo, pNext} |]),
	("flags", ''#{type VkPipelineViewportStateCreateFlags},
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
