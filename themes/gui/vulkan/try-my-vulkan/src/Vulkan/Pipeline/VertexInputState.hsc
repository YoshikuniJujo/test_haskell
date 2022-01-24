{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.VertexInputState where

#include <vulkan/vulkan.h>

import GHC.Generics
import Foreign.Storable.SizeAlignment

import Vulkan.Format
import Vulkan.Pipeline.VertexInputState.BindingStrideList
import Vulkan.Pipeline.VertexInputState.GetBindingOffset
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
		(AddType [(Bool, Float)] 'VertexInputRateVertex)) '[Double, Float, Bool, Int]
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

class PipelineVertexInputStateCreateInfoAttributeDescription vs (ts :: [*]) where
	pipelineVertexInputStateCreateInfoToAttributeDescription ::
		PipelineVertexInputStateCreateInfo n vs ts ->
		[In.VertexInputAttributeDescription]

instance PipelineVertexInputStateCreateInfoAttributeDescription vs '[] where
	pipelineVertexInputStateCreateInfoToAttributeDescription _ = []

instance (
	BindingOffset vs t, Formattable t,
	PipelineVertexInputStateCreateInfoAttributeDescription vs ts) =>
	PipelineVertexInputStateCreateInfoAttributeDescription vs (t ': ts) where
	pipelineVertexInputStateCreateInfoToAttributeDescription :: forall n .
		PipelineVertexInputStateCreateInfo n vs (t ': ts) ->
		[In.VertexInputAttributeDescription]
	pipelineVertexInputStateCreateInfoToAttributeDescription _ = In.VertexInputAttributeDescription {
		In.vertexInputAttributeDescriptionLocation = 0,
		In.vertexInputAttributeDescriptionBinding = bd,
		In.vertexInputAttributeDescriptionFormat = formatOf @t,
		In.vertexInputAttributeDescriptionOffset = os } : (succLocation <$> ads)
		where
		Just (fromIntegral -> bd, fromIntegral -> os) = bindingOffset @vs @t
		ads = pipelineVertexInputStateCreateInfoToAttributeDescription @vs @ts undefined

succLocation :: In.VertexInputAttributeDescription -> In.VertexInputAttributeDescription
succLocation ad@In.VertexInputAttributeDescription { In.vertexInputAttributeDescriptionLocation = l } =
	ad { In.vertexInputAttributeDescriptionLocation = l + 1 }

data VertexInputRate = VertexInputRateVertex | VertexInputRateInstance
	deriving Show

instance TypeVal 'VertexInputRateVertex In.VertexInputRate where
	typeVal = In.VertexInputRateVertex

instance TypeVal 'VertexInputRateInstance In.VertexInputRate where
	typeVal = In.VertexInputRateInstance

class Formattable a where formatOf :: Format

instance Formattable Double where formatOf = FormatUndefined
instance Formattable Int where formatOf = FormatUndefined
instance Formattable Bool where formatOf = FormatUndefined
instance Formattable Float where formatOf = FormatUndefined
