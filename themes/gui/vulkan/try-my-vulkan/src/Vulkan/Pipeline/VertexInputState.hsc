{-# LANGUAGE KindSignatures, DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.VertexInputState where

#include <vulkan/vulkan.h>

import qualified Vulkan.Pipeline.VertexInputState.Intermediate as Im
import qualified Vulkan.Pipeline.VertexInputState.Internal as In

data PipelineVertexInputStateCreateInfo n vs (ts :: [*]) =
	PipelineVertexInputStateCreateInfo {
		pipelineVertexInputStateCreateInfoNext :: Maybe n,
		pipelineVertexInputStateCreateInfoFlags ::
			In.PipelineVertexInputStateCreateFlags }
	deriving Show

pipelineVertexInputStateCreateInfoToBindingDescription ::
	PipelineVertexInputStateCreateInfo n vs ts ->
	[In.VertexInputBindingDescription]
pipelineVertexInputStateCreateInfoToBindingDescription = undefined

class PipelineVertexInputStateCreateInfoAttributeDescription (ts :: [*]) where
	pipelineVertexInputStateCreateInfoToAttributeDescription ::
		PipelineVertexInputStateCreateInfo n vs ts ->
		[In.VertexInputAttributeDescription]
