{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline where

import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base

import qualified Vulkan.Pipeline.ShaderStage as ShaderStage
import qualified Vulkan.Pipeline.VertexInputState as VertexInputState

#include <vulkan/vulkan.h>

bindPointGraphics :: #{type VkPipelineBindPoint}
bindPointGraphics = #{const VK_PIPELINE_BIND_POINT_GRAPHICS}

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO}

struct "CreateInfo" #{size VkGraphicsPipelineCreateInfo}
		#{alignment VkGraphicsPipelineCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkGraphicsPipelineCreateInfo, sType}
			p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkGraphicsPipelineCreateInfo, pNext} |],
		[| #{poke VkGraphicsPipelineCreateInfo, pNext} |]),
	("flags", ''#{type VkPipelineCreateFlags},
		[| #{peek VkGraphicsPipelineCreateInfo, flags} |],
		[| #{poke VkGraphicsPipelineCreateInfo, flags} |]),
	("stageCount", ''#{type uint32_t},
		[| #{peek VkGraphicsPipelineCreateInfo, stageCount} |],
		[| #{poke VkGraphicsPipelineCreateInfo, stageCount} |]),
	("pStages", ''ShaderStage.PtrCreateInfo,
		[| #{peek VkGraphicsPipelineCreateInfo, pStages} |],
		[| #{poke VkGraphicsPipelineCreateInfo, pStages} |]),
	("pVertexInputState", ''VertexInputState.PtrCreateInfo,
		[| #{peek VkGraphicsPipelineCreateInfo, pVertexInputState} |],
		[| #{poke VkGraphicsPipelineCreateInfo, pVertexInputState} |])
	]
	[''Show, ''Storable]
