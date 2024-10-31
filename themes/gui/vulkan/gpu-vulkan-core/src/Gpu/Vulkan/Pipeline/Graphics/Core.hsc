{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Graphics.Core (

	-- * CREATE

	create, CreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags,
	createInfoStageCount, createInfoPStages,
	createInfoPVertexInputState, createInfoPInputAssemblyState,
	createInfoPTessellationState, createInfoPViewportState,
	createInfoPRasterizationState, createInfoPMultisampleState,
	createInfoPDepthStencilState, createInfoPColorBlendState,
	createInfoPDynamicState,
	createInfoLayout, createInfoRenderPass, createInfoSubpass,
	createInfoBasePipelineHandle, createInfoBasePipelineIndex

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word
import Data.Int

import qualified Gpu.Vulkan.Device.Core as Device
import qualified Gpu.Vulkan.Pipeline.Core as Pipeline
import qualified Gpu.Vulkan.Pipeline.ShaderStage.Core as ShaderStage
import qualified Gpu.Vulkan.Pipeline.VertexInputState.Core as VertexInputState
import qualified Gpu.Vulkan.Pipeline.InputAssemblyState.Core as InputAssemblyState
import qualified Gpu.Vulkan.Pipeline.TessellationState.Core as TessellationState
import qualified Gpu.Vulkan.Pipeline.ViewportState.Core as ViewportState
import qualified Gpu.Vulkan.Pipeline.RasterizationState.Core as RasterizationState
import qualified Gpu.Vulkan.Pipeline.MultisampleState.Core as MultisampleState
import qualified Gpu.Vulkan.Pipeline.DepthStencilState.Core as DepthStencilState
import qualified Gpu.Vulkan.Pipeline.ColorBlendState.Core as ColorBlendState
import qualified Gpu.Vulkan.Pipeline.DynamicState.Core as DynamicState
import qualified Gpu.Vulkan.PipelineLayout.Core as Layout
import qualified Gpu.Vulkan.RenderPass.Core as RenderPass

import qualified Gpu.Vulkan.PipelineCache.Core as Cache
import qualified Gpu.Vulkan.AllocationCallbacks.Core as AllocationCallbacks

#include <vulkan/vulkan.h>

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
		[| #{poke VkGraphicsPipelineCreateInfo, pDepthStencilState} |]),
	("pColorBlendState", ''ColorBlendState.PtrCreateInfo,
		[| #{peek VkGraphicsPipelineCreateInfo, pColorBlendState} |],
		[| #{poke VkGraphicsPipelineCreateInfo, pColorBlendState} |]),
	("pDynamicState", ''DynamicState.PtrCreateInfo,
		[| #{peek VkGraphicsPipelineCreateInfo, pDynamicState} |],
		[| #{poke VkGraphicsPipelineCreateInfo, pDynamicState} |]),
	("layout", ''Layout.P,
		[| #{peek VkGraphicsPipelineCreateInfo, layout} |],
		[| #{poke VkGraphicsPipelineCreateInfo, layout} |]),
	("renderPass", ''RenderPass.R,
		[| #{peek VkGraphicsPipelineCreateInfo, renderPass} |],
		[| #{poke VkGraphicsPipelineCreateInfo, renderPass} |]),
	("subpass", ''#{type uint32_t},
		[| #{peek VkGraphicsPipelineCreateInfo, subpass} |],
		[| #{poke VkGraphicsPipelineCreateInfo, subpass} |]),
	("basePipelineHandle", ''Pipeline.P,
		[| #{peek VkGraphicsPipelineCreateInfo, basePipelineHandle} |],
		[| #{poke VkGraphicsPipelineCreateInfo, basePipelineHandle} |]),
	("basePipelineIndex", ''#{type int32_t},
		[| #{peek VkGraphicsPipelineCreateInfo, basePipelineIndex} |],
		[| #{poke VkGraphicsPipelineCreateInfo, basePipelineIndex} |]) ]
	[''Show, ''Storable]

foreign import ccall "vkCreateGraphicsPipelines" create ::
	Device.D -> Cache.P -> #{type uint32_t} -> Ptr CreateInfo ->
	Ptr AllocationCallbacks.A -> Ptr Pipeline.P -> IO #{type VkResult}
