{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.DepthStencilState.Core where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base

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
			stencilTestEnable} |])
	-- TODO
	]
	[''Show, ''Storable]

type PtrCreateInfo = Ptr CreateInfo
