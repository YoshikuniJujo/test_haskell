{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word
import Data.Int

import Vulkan.Base
import Vulkan.Device (Device)

import qualified Vulkan.Pipeline.ShaderStage as ShaderStage
import qualified Vulkan.Pipeline.VertexInputState as VertexInputState
import qualified Vulkan.Pipeline.InputAssemblyState as InputAssemblyState
import qualified Vulkan.Pipeline.TessellationState as TessellationState
import qualified Vulkan.Pipeline.ViewportState as ViewportState
import qualified Vulkan.Pipeline.RasterizationState as RasterizationState
import qualified Vulkan.Pipeline.MultisampleState as MultisampleState
import qualified Vulkan.Pipeline.DepthStencilState as DepthStencilState
import qualified Vulkan.Pipeline.ColorBlendState as ColorBlendState
import qualified Vulkan.Pipeline.DynamicState as DynamicState
import qualified Vulkan.Pipeline.Layout as Layout
import qualified Vulkan.RenderPass as RenderPass

#include <vulkan/vulkan.h>

bindPointGraphics :: #{type VkPipelineBindPoint}
bindPointGraphics = #{const VK_PIPELINE_BIND_POINT_GRAPHICS}

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO}

data PipelineTag
type Pipeline = Ptr PipelineTag

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
	("layout", ''Layout.Layout,
		[| #{peek VkGraphicsPipelineCreateInfo, layout} |],
		[| #{poke VkGraphicsPipelineCreateInfo, layout} |]),
	("renderPass", ''RenderPass.RenderPass,
		[| #{peek VkGraphicsPipelineCreateInfo, renderPass} |],
		[| #{poke VkGraphicsPipelineCreateInfo, renderPass} |]),
	("subpass", ''#{type uint32_t},
		[| #{peek VkGraphicsPipelineCreateInfo, subpass} |],
		[| #{poke VkGraphicsPipelineCreateInfo, subpass} |]),
	("basePipelineHandle", ''Pipeline,
		[| #{peek VkGraphicsPipelineCreateInfo, basePipelineHandle} |],
		[| #{poke VkGraphicsPipelineCreateInfo, basePipelineHandle} |]),
	("basePipelineIndex", ''#{type int32_t},
		[| #{peek VkGraphicsPipelineCreateInfo, basePipelineIndex} |],
		[| #{poke VkGraphicsPipelineCreateInfo, basePipelineIndex} |]) ]
	[''Show, ''Storable]

foreign import ccall "vkCreateGraphicsPipelines" create ::
	Device -> Ptr () -> #{type uint32_t} -> Ptr CreateInfo -> Ptr () ->
	Ptr Pipeline -> IO #{type VkResult}

foreign import ccall "vkDestroyPipeline" destroy ::
	Device -> Pipeline -> Ptr () -> IO ()

stageColorAttachmentOutputBit :: #{type VkPipelineStageFlagBits}
stageColorAttachmentOutputBit =
	#{const VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT}
