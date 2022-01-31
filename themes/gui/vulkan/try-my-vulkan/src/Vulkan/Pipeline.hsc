{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline where

import Data.Word
import Data.Int

import Vulkan.Base
import Vulkan.PipelineCreateFlagBits

import Vulkan.Pipeline.Internal as I
import Vulkan.Pipeline.ShaderStage as ShaderStage
import Vulkan.Pipeline.VertexInputState as VertexInputState
import Vulkan.Pipeline.InputAssemblyState as InputAssemblyState
import Vulkan.Pipeline.TessellationState as TessellationState
import Vulkan.Pipeline.ViewportState as ViewportState
import Vulkan.Pipeline.RasterizationState as RasterizationState
import Vulkan.Pipeline.MultisampleState as MultisampleState
import Vulkan.Pipeline.DepthStencilState as DepthStencilState
import Vulkan.Pipeline.ColorBlendState as ColorBlendState
import Vulkan.Pipeline.DynamicState as DynamicState
import Vulkan.Pipeline.Layout as Layout
import Vulkan.RenderPass as RenderPass

#include <vulkan/vulkan.h>

data CreateInfo n n1 n2 vs ts n3 n4 n5 n6 n7 n8 n9 n10 = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: PipelineCreateFlags,
	createInfoStages :: [ShaderStage.CreateInfo n1],
	createInfoVertexInputState ::
		Maybe (VertexInputState.CreateInfo n2 vs ts),
	createInfoInputAssemblyState ::
		Maybe (InputAssemblyState.CreateInfo n3),
	createInfoTessellationState :: Maybe (TessellationState.CreateInfo n4),
	createInfoViewportState :: Maybe (ViewportState.CreateInfo n5),
	createInfoRasterizationState :: RasterizationState.CreateInfo n6,
	createInfoMultisampleState :: Maybe (MultisampleState.CreateInfo n7),
	createInfoDepthStencilState :: Maybe (DepthStencilState.CreateInfo n8),
	createInfoColorBlendState :: Maybe (ColorBlendState.CreateInfo n9),
	createInfoDynamicState :: Maybe (DynamicState.CreateInfo n10),
	createInfoLayout :: Layout.PipelineLayout,
	createInfoRenderPass :: RenderPass.RenderPass,
	createInfoSubpass :: Word32,
	createInfoBasePipelineHandle :: Pipeline,
	createInfoBasePipelineIndex :: Int32 }
	deriving Show

word32ToUint32T :: Word32 -> #{type uint32_t}
word32ToUint32T = fromIntegral

int32ToInt32T :: Int32 -> #{type int32_t}
int32ToInt32T = fromIntegral
