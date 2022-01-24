{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.VertexInputState where

#include <vulkan/vulkan.h>

import GHC.Generics
import Foreign.Storable.SizeAlignment

import Vulkan.Pipeline.VertexInputState.BindingStrideList
import Vulkan.Pipeline.VertexInputState.Flatten

import qualified Vulkan.Pipeline.VertexInputState.Intermediate as Im
import qualified Vulkan.Pipeline.VertexInputState.Internal as In

data PipelineVertexInputStateCreateInfo n vs (ts :: [*]) =
	PipelineVertexInputStateCreateInfo {
		pipelineVertexInputStateCreateInfoNext :: Maybe n,
		pipelineVertexInputStateCreateInfoFlags ::
			In.PipelineVertexInputStateCreateFlags }
	deriving Show

samplePipelineVertexInputStateCreateInfo0 ::
	PipelineVertexInputStateCreateInfo () () '[]
samplePipelineVertexInputStateCreateInfo0 = undefined

type SamplePipelineVertexInputStateCreateInfoType = (
		(AddType [(Int, Double)] 'VertexInputRateVertex),
		(AddType [(Bool, Float)] 'VertexInputRateVertex) )

samplePipelineVertexInputStateCreateInfo ::
	PipelineVertexInputStateCreateInfo () (
		(AddType [(Int, Double)] 'VertexInputRateVertex),
		(AddType [(Bool, Float)] 'VertexInputRateVertex)) '[]
samplePipelineVertexInputStateCreateInfo = undefined

pipelineVertexInputStateCreateInfoToBindingDescription ::
	BindingStrideListList (Flatten (Rep vs))
		VertexInputRate In.VertexInputRate =>
	PipelineVertexInputStateCreateInfo n vs ts ->
	[In.VertexInputBindingDescription]
pipelineVertexInputStateCreateInfoToBindingDescription =
	bindingDescriptionFromRaw
		. pipelineVertexInputStateCreateInfoToBindingDescriptionRaw

bindingDescriptionFromRaw ::
	[(SizeAlignment, In.VertexInputRate)] ->
	[In.VertexInputBindingDescription]
bindingDescriptionFromRaw sars = (<$> zip [0 ..] sars)
	\(b, ((fromIntegral -> sz, _algn), r)) ->
		In.VertexInputBindingDescription b sz r

pipelineVertexInputStateCreateInfoToBindingDescriptionRaw :: forall n vs ts .
	BindingStrideListList (Flatten (Rep vs))
		VertexInputRate In.VertexInputRate =>
	PipelineVertexInputStateCreateInfo n vs ts ->
	[(SizeAlignment, In.VertexInputRate)]
pipelineVertexInputStateCreateInfoToBindingDescriptionRaw _ =
	bindingStrideList @vs @VertexInputRate @In.VertexInputRate

{-
class PipelineVertexInputStateCreateInfoAttributeDescription (ts :: [*]) where
	pipelineVertexInputStateCreateInfoToAttributeDescription ::
		PipelineVertexInputStateCreateInfo n vs ts ->
		[In.VertexInputAttributeDescription]
		-}

data VertexInputRate = VertexInputRateVertex | VertexInputRateInstance
	deriving Show

instance TypeVal 'VertexInputRateVertex In.VertexInputRate where
	typeVal = In.VertexInputRateVertex

instance TypeVal 'VertexInputRateInstance In.VertexInputRate where
	typeVal = In.VertexInputRateInstance
