{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline where

import Vulkan.PipelineCreateFlagBits

import Vulkan.Pipeline.Internal as I
import Vulkan.Pipeline.ShaderStage as ShaderStage
import Vulkan.Pipeline.VertexInputState as VertexInputState
import Vulkan.Pipeline.InputAssemblyState as InputAssemblyState
import Vulkan.Pipeline.TessellationState as TessellationState
import Vulkan.Pipeline.ViewportState as ViewportState
import Vulkan.Pipeline.RasterizationState as RasterizationState
import Vulkan.Pipeline.MultisampleState as MultisampleState

data CreateInfo n n1 n2 vs ts n3 n4 n5 n6 n7 = CreateInfo {
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
	createInfoMultisampleState :: MultisampleState.CreateInfo n7
	}
	deriving Show
