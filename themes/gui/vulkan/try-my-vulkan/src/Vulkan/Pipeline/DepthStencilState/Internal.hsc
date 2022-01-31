{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.DepthStencilState.Internal where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Enum
import Foreign.C.Struct
import Data.Word

import Vulkan.Base
import Vulkan.CompareOp
import Vulkan.StencilOp

import qualified Vulkan.StructureType as ST

#include <vulkan/vulkan.h>

enum "CreateFlagBits" ''#{type VkPipelineDepthStencilStateCreateFlags}
		[''Show, ''Storable] [("CreateFlagBitsZero", 0)]

type CreateFlags = CreateFlagBits

struct "StencilOpState" #{size VkStencilOpState} #{alignment VkStencilOpState} [
	("failOp", ''StencilOp, [| #{peek VkStencilOpState, failOp} |],
		[| #{poke VkStencilOpState, failOp} |]),
	("passOp", ''StencilOp, [| #{peek VkStencilOpState, passOp} |],
		[| #{poke VkStencilOpState, passOp } |]),
	("depthFailOp", ''StencilOp,
		[| #{peek VkStencilOpState, depthFailOp} |],
		[| #{poke VkStencilOpState, depthFailOp} |]),
	("compareOp", ''CompareOp,
		[| #{peek VkStencilOpState, compareOp} |],
		[| #{poke VkStencilOpState, compareOp} |]),
	("compareMask", ''#{type uint32_t},
		[| #{peek VkStencilOpState, compareMask} |],
		[| #{poke VkStencilOpState, compareMask} |]),
	("writeMask", ''#{type uint32_t},
		[| #{peek VkStencilOpState, writeMask} |],
		[| #{poke VkStencilOpState, writeMask} |]),
	("reference", ''#{type uint32_t},
		[| #{peek VkStencilOpState, reference} |],
		[| #{poke VkStencilOpState, reference} |]) ]
	[''Show, ''Storable]

struct "CreateInfo" #{size VkPipelineDepthStencilStateCreateInfo}
		#{alignment VkPipelineDepthStencilStateCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPipelineDepthStencilStateCreateInfo, sType}
			p ST.pipelineDepthStencilStateCreateInfo |]),
	("pNext", ''PtrVoid,
		[| #{peek VkPipelineDepthStencilStateCreateInfo, pNext} |],
		[| #{poke VkPipelineDepthStencilStateCreateInfo, pNext} |]),
	("flags", ''CreateFlags,
		[| #{peek VkPipelineDepthStencilStateCreateInfo, flags} |],
		[| #{poke VkPipelineDepthStencilStateCreateInfo, flags} |]),
	("depthTestEnable", ''Bool32,
		[| #{peek VkPipelineDepthStencilStateCreateInfo,
			depthTestEnable} |],
		[| #{poke VkPipelineDepthStencilStateCreateInfo,
			depthTestEnable} |]),
	("depthWriteEnable", ''Bool32,
		[| #{peek VkPipelineDepthStencilStateCreateInfo,
			depthWriteEnable} |],
		[| #{poke VkPipelineDepthStencilStateCreateInfo,
			depthWriteEnable} |]),
	("depthCompareOp", ''CompareOp,
		[| #{peek VkPipelineDepthStencilStateCreateInfo,
			depthCompareOp} |],
		[| #{poke VkPipelineDepthStencilStateCreateInfo,
			depthCompareOp} |]),
	("depthBoundsTestEnable", ''Bool32,
		[| #{peek VkPipelineDepthStencilStateCreateInfo,
			depthBoundsTestEnable} |],
		[| #{poke VkPipelineDepthStencilStateCreateInfo,
			depthBoundsTestEnable} |]),
	("stencilTestEnable", ''Bool32,
		[| #{peek VkPipelineDepthStencilStateCreateInfo,
			stencilTestEnable} |],
		[| #{poke VkPipelineDepthStencilStateCreateInfo,
			stencilTestEnable} |]),
	("front", ''StencilOpState,
		[| #{peek VkPipelineDepthStencilStateCreateInfo, front} |],
		[| #{poke VkPipelineDepthStencilStateCreateInfo, front} |]),
	("back", ''StencilOpState,
		[| #{peek VkPipelineDepthStencilStateCreateInfo, back} |],
		[| #{poke VkPipelineDepthStencilStateCreateInfo, back} |]),
	("minDepthBounds", ''#{type float},
		[| #{peek VkPipelineDepthStencilStateCreateInfo,
			minDepthBounds} |],
		[| #{poke VkPipelineDepthStencilStateCreateInfo,
			minDepthBounds} |]),
	("maxDepthBounds", ''#{type float},
		[| #{peek VkPipelineDepthStencilStateCreateInfo,
			maxDepthBounds} |],
		[| #{poke VkPipelineDepthStencilStateCreateInfo,
			maxDepthBounds} |])
	]
	[''Show, ''Storable]

type PtrCreateInfo = Ptr CreateInfo
