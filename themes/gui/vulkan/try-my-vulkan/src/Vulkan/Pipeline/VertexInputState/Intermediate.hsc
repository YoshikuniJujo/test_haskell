{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.VertexInputState.Intermediate where

import qualified Vulkan.Pipeline.VertexInputState.Internal as I

data PipelineVertexInputStateCreateInfo n = PipelineVertexInputStateCreateInfo {
	pipelineVertexInputStateCreateInfoNext :: Maybe n,
	pipelineVertexInputStateCreateInfoFlags ::
		I.PipelineVertexInputStateCreateFlags,
	pipelineVertexInputStateCreateInfoVertexBindingDescriptions ::
		[I.VertexInputBindingDescription],
	pipelineVertexInputStateCreateInfoVertexAttributeDescriptions ::
		[I.VertexInputAttributeDescription] }
	deriving Show
