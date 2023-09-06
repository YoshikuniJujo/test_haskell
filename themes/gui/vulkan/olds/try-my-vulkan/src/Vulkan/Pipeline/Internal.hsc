{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Internal where

import Foreign.Storable
import Foreign.C.Struct
import Data.Word
import Data.Int

import Vulkan.Base
import Vulkan.PipelineCreateFlagBits

import qualified Vulkan.StructureType as ST
import qualified Vulkan.Pipeline.ShaderStage.Internal as ShaderStage.I
import qualified Vulkan.Pipeline.VertexInputState.Internal as VertexInputState.I
import qualified Vulkan.Pipeline.InputAssemblyState.Internal as
	InputAssemblyState.I
import qualified Vulkan.Pipeline.TessellationState.Internal as
	TessellationState.I
import qualified Vulkan.Pipeline.ViewportState.Internal as ViewportState.I
import qualified Vulkan.Pipeline.RasterizationState.Internal as
	RasterizationState.I
import qualified Vulkan.Pipeline.MultisampleState.Internal as MultisampleState.I
import qualified Vulkan.Pipeline.DepthStencilState.Internal as
	DepthStencilState.I
import qualified Vulkan.Pipeline.ColorBlendState.Internal as ColorBlendState.I
import qualified Vulkan.Pipeline.DynamicState.Internal as DynamicState.I
import qualified Vulkan.Pipeline.Layout as Layout

#include <vulkan/vulkan.h>

struct "CreateInfo" #{size VkGraphicsPipelineCreateInfo}
		#{alignment VkGraphicsPipelineCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkGraphicsPipelineCreateInfo, sType} p
			ST.pipelineCreateInfo |]),
	("pNext", ''PtrVoid,
		[| #{peek VkGraphicsPipelineCreateInfo, pNext} |],
		[| #{poke VkGraphicsPipelineCreateInfo, pNext} |]),
	("flags", ''PipelineCreateFlags,
		[| #{peek VkGraphicsPipelineCreateInfo, flags} |],
		[| #{poke VkGraphicsPipelineCreateInfo, flags} |]),
	("stageCount", ''#{type uint32_t},
		[| #{peek VkGraphicsPipelineCreateInfo, stageCount} |],
		[| #{poke VkGraphicsPipelineCreateInfo, stageCount} |]),
	("pStages", ''ShaderStage.I.PtrCreateInfo,
		[| #{peek VkGraphicsPipelineCreateInfo, pStages} |],
		[| #{poke VkGraphicsPipelineCreateInfo, pStages} |]),
	("pVertexInputState", ''VertexInputState.I.PtrCreateInfo,
		[| #{peek VkGraphicsPipelineCreateInfo, pVertexInputState} |],
		[| #{poke VkGraphicsPipelineCreateInfo, pVertexInputState} |]),
	("pInputAssemblyState", ''InputAssemblyState.I.PtrCreateInfo,
		[| #{peek VkGraphicsPipelineCreateInfo, pInputAssemblyState} |],
		[| #{poke VkGraphicsPipelineCreateInfo,
			pInputAssemblyState} |]),
	("pTessellationState", ''TessellationState.I.PtrCreateInfo,
		[| #{peek VkGraphicsPipelineCreateInfo, pTessellationState} |],
		[| #{poke VkGraphicsPipelineCreateInfo, pTessellationState} |]),
	("pViewportState", ''ViewportState.I.PtrCreateInfo,
		[| #{peek VkGraphicsPipelineCreateInfo, pViewportState} |],
		[| #{poke VkGraphicsPipelineCreateInfo, pViewportState} |]),
	("pRasterizationState", ''RasterizationState.I.PtrCreateInfo,
		[| #{peek VkGraphicsPipelineCreateInfo, pRasterizationState} |],
		[| #{poke VkGraphicsPipelineCreateInfo,
			pRasterizationState} |]),
	("pMultisampleState", ''MultisampleState.I.PtrCreateInfo,
		[| #{peek VkGraphicsPipelineCreateInfo, pMultisampleState} |],
		[| #{poke VkGraphicsPipelineCreateInfo, pMultisampleState} |]),
	("pDepthStencilState", ''DepthStencilState.I.PtrCreateInfo,
		[| #{peek VkGraphicsPipelineCreateInfo, pDepthStencilState} |],
		[| #{poke VkGraphicsPipelineCreateInfo, pDepthStencilState} |]),
	("pColorBlendState", ''ColorBlendState.I.PtrCreateInfo,
		[| #{peek VkGraphicsPipelineCreateInfo, pColorBlendState} |],
		[| #{poke VkGraphicsPipelineCreateInfo, pColorBlendState} |]),
	("pDynamicState", ''DynamicState.I.PtrCreateInfo,
		[| #{peek VkGraphicsPipelineCreateInfo, pDynamicState} |],
		[| #{poke VkGraphicsPipelineCreateInfo, pDynamicState} |]),
	("layout", ''Layout.PipelineLayout,
		[| #{peek VkGraphicsPipelineCreateInfo, layout} |],
		[| #{poke VkGraphicsPipelineCreateInfo, layout} |]),
	("renderPass", ''RenderPass,
		[| #{peek VkGraphicsPipelineCreateInfo, renderPass} |],
		[| #{poke VkGraphicsPipelineCreateInfo, renderPass} |]),
	("subpass", ''#{type uint32_t},
		[| #{peek VkGraphicsPipelineCreateInfo, subpass} |],
		[| #{poke VkGraphicsPipelineCreateInfo, subpass} |]),
	("basePipelineHandle", ''PipelineC,
		[| #{peek VkGraphicsPipelineCreateInfo, basePipelineHandle} |],
		[| #{poke VkGraphicsPipelineCreateInfo, basePipelineHandle} |]),
	("basePipelineIndex", ''#{type int32_t},
		[| #{peek VkGraphicsPipelineCreateInfo, basePipelineIndex} |],
		[| #{poke VkGraphicsPipelineCreateInfo, basePipelineIndex} |]) ]
	[''Show, ''Storable]
