{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline where

import Vulkan.PipelineCreateFlagBits

import Vulkan.Pipeline.Internal as I
import Vulkan.Pipeline.ShaderStage as ShaderStage
import Vulkan.Pipeline.VertexInputState as VertexInputState

data CreateInfo n n1 n2 vs ts = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: PipelineCreateFlags,
	createInfoStages :: [ShaderStage.CreateInfo n1],
	createInfoVertexInputState ::
		Maybe (VertexInputState.CreateInfo n2 vs ts)
	}
	deriving Show
