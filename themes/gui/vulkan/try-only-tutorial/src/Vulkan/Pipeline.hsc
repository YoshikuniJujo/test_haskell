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
import qualified Vulkan.Pipeline.InputAssemblyState as InputAssemblyState
import qualified Vulkan.Pipeline.TessellationState as TessellationState
import qualified Vulkan.Pipeline.ViewportState as ViewportState
import qualified Vulkan.Pipeline.RasterizationState as RasterizationState
import qualified Vulkan.Pipeline.MultisampleState as MultisampleState
import qualified Vulkan.Pipeline.DepthStencilState as DepthStencilState

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
		[| #{poke VkGraphicsPipelineCreateInfo, pVertexInputState} |]),
	("pInputAssemblyState", ''InputAssemblyState.PtrCreateInfo,
		[| #{peek VkGraphicsPipelineCreateInfo, pInputAssemblyState} |],
		[| #{poke VkGraphicsPipelineCreateInfo,
			pInputAssemblyState} |]),
	("pTessellationState", ''TessellationState.PtrCreateInfo,
		[| #{peek VkGraphicsPipelineCreateInfo, pTessellationState} |],
		[| #{poke VkGraphicsPipelineCreateInfo, pTessellationState} |]),
	("pViewportState", ''ViewportState.PtrCreateInfo,
		[| #{peek VkGraphicsPipelineCreateInfo, pViewportState} |],
		[| #{poke VkGraphicsPipelineCreateInfo, pViewportState} |]),
	("pRasterizationState", ''RasterizationState.PtrCreateInfo,
		[| #{peek VkGraphicsPipelineCreateInfo, pRasterizationState} |],
		[| #{poke VkGraphicsPipelineCreateInfo,
			pRasterizationState} |]),
	("pMultisampleState", ''MultisampleState.PtrCreateInfo,
		[| #{peek VkGraphicsPipelineCreateInfo, pMultisampleState} |],
		[| #{poke VkGraphicsPipelineCreateInfo, pMultisampleState} |]),
	("pDepthStencilState", ''DepthStencilState.PtrCreateInfo,
		[| #{peek VkGraphicsPipelineCreateInfo, pDepthStencilState} |],
		[| #{poke VkGraphicsPipelineCreateInfo, pDepthStencilState} |])
	]
	[''Show, ''Storable]
