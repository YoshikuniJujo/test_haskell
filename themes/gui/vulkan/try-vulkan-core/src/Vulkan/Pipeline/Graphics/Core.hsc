{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Graphics.Core where

import Foreign.Ptr
import Foreign.Ptr.Synonyms
import Foreign.Storable
import Foreign.C.Struct
import Data.Word
import Data.Int

import qualified Vulkan.Device.Core as Device
import qualified Vulkan.Pipeline.Core as Pipeline
import qualified Vulkan.Pipeline.ShaderStage.Core as ShaderStage
import qualified Vulkan.Pipeline.VertexInputState.Core as VertexInputState
import qualified Vulkan.Pipeline.InputAssemblyState.Core as InputAssemblyState
import qualified Vulkan.Pipeline.TessellationState.Core as TessellationState
import qualified Vulkan.Pipeline.ViewportState.Core as ViewportState
import qualified Vulkan.Pipeline.RasterizationState.Core as RasterizationState
import qualified Vulkan.Pipeline.MultisampleState.Core as MultisampleState
import qualified Vulkan.Pipeline.DepthStencilState.Core as DepthStencilState
import qualified Vulkan.Pipeline.ColorBlendState.Core as ColorBlendState
import qualified Vulkan.Pipeline.DynamicState.Core as DynamicState
import qualified Vulkan.Pipeline.Layout.Core as Layout
import qualified Vulkan.RenderPass.Core as RenderPass

import qualified Vulkan.Pipeline.Cache.Core as Cache
import qualified Vulkan.AllocationCallbacks.Core as AllocationCallbacks

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
		[| #{poke VkGraphicsPipelineCreateInfo, pDepthStencilState} |]),
	("pColorBlendState", ''ColorBlendState.PtrCreateInfo,
		[| #{peek VkGraphicsPipelineCreateInfo, pColorBlendState} |],
		[| #{poke VkGraphicsPipelineCreateInfo, pColorBlendState} |]),
	("pDynamicState", ''DynamicState.PtrCreateInfo,
		[| #{peek VkGraphicsPipelineCreateInfo, pDynamicState} |],
		[| #{poke VkGraphicsPipelineCreateInfo, pDynamicState} |]),
	("layout", ''Layout.L,
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
	Device.D -> Cache.C -> #{type uint32_t} -> Ptr CreateInfo ->
	Ptr AllocationCallbacks.A -> Ptr Pipeline.P -> IO #{type VkResult}

stageColorAttachmentOutputBit :: #{type VkPipelineStageFlagBits}
stageColorAttachmentOutputBit =
	#{const VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT}
