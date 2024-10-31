{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.DepthStencilState.Core (

	-- * CREATE INFO

	CreateInfo, PtrCreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags,
	createInfoDepthTestEnable, createInfoDepthWriteEnable,
	createInfoDepthCompareOp, createInfoDepthBoundsTestEnable,
	createInfoStencilTestEnable, createInfoFront, createInfoBack,
	createInfoMinDepthBounds, createInfoMaxDepthBounds

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word

import Gpu.Vulkan.Core

#include <vulkan/vulkan.h>

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO}

struct "CreateInfo" #{size VkPipelineDepthStencilStateCreateInfo}
		#{alignment VkPipelineDepthStencilStateCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPipelineDepthStencilStateCreateInfo, sType}
			p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkPipelineDepthStencilStateCreateInfo, pNext} |],
		[| #{poke VkPipelineDepthStencilStateCreateInfo, pNext} |]),
	("flags", ''#{type VkPipelineDepthStencilStateCreateFlags},
		[| #{peek VkPipelineDepthStencilStateCreateInfo, flags} |],
		[| #{poke VkPipelineDepthStencilStateCreateInfo, flags} |]),
	("depthTestEnable", ''#{type VkBool32},
		[| #{peek VkPipelineDepthStencilStateCreateInfo,
			depthTestEnable} |],
		[| #{poke VkPipelineDepthStencilStateCreateInfo,
			depthTestEnable} |]),
	("depthWriteEnable", ''#{type VkBool32},
		[| #{peek VkPipelineDepthStencilStateCreateInfo,
			depthWriteEnable} |],
		[| #{poke VkPipelineDepthStencilStateCreateInfo,
			depthWriteEnable} |]),
	("depthCompareOp", ''#{type VkCompareOp},
		[| #{peek VkPipelineDepthStencilStateCreateInfo,
			depthCompareOp} |],
		[| #{poke VkPipelineDepthStencilStateCreateInfo,
			depthCompareOp} |]),
	("depthBoundsTestEnable", ''#{type VkBool32},
		[| #{peek VkPipelineDepthStencilStateCreateInfo,
			depthBoundsTestEnable} |],
		[| #{poke VkPipelineDepthStencilStateCreateInfo,
			depthBoundsTestEnable} |]),
	("stencilTestEnable", ''#{type VkBool32},
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
			maxDepthBounds} |]) ]
	[''Show, ''Storable]

type PtrCreateInfo = Ptr CreateInfo
