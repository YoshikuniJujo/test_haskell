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

import Foreign.Storable.SizeAlignment

import Vulkan.Base
import Vulkan.Format
import Vulkan.Pipeline.VertexInputState.BindingStrideList
import Vulkan.Pipeline.VertexInputState.GetBindingOffset

import qualified Vulkan.Pipeline.VertexInputState.Intermediate as Im
import qualified Vulkan.Pipeline.VertexInputState.Internal as In

createInfoToC :: (
	Pointable n,
	BindingStrideList vs VertexInputRate In.VertexInputRate,
	PipelineVertexInputStateCreateInfoAttributeDescription vs ts ) =>
	CreateInfo n vs ts -> (In.CreateInfo -> IO a) -> IO a
createInfoToC = Im.pipelineVertexInputStateCreateInfoToC
	. pipelineVertexInputStateCreateInfoToIntermediate

pipelineVertexInputStateCreateInfoToIntermediate :: (
	BindingStrideList vs VertexInputRate In.VertexInputRate,
	PipelineVertexInputStateCreateInfoAttributeDescription vs ts ) =>
	CreateInfo n vs ts -> Im.PipelineVertexInputStateCreateInfo n
pipelineVertexInputStateCreateInfoToIntermediate
	ci@CreateInfo {
		createInfoNext = mnxt,
		createInfoFlags = flgs } =
	Im.PipelineVertexInputStateCreateInfo {
		Im.pipelineVertexInputStateCreateInfoNext = mnxt,
		Im.pipelineVertexInputStateCreateInfoFlags = flgs,
		Im.pipelineVertexInputStateCreateInfoVertexBindingDescriptions =
			pipelineVertexInputStateCreateInfoToBindingDescription
				ci,
		Im.pipelineVertexInputStateCreateInfoVertexAttributeDescriptions
			=
			pipelineVertexInputStateCreateInfoToAttributeDescription
				ci }

data CreateInfo n vs (ts :: [*]) = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: In.PipelineVertexInputStateCreateFlags }
	deriving Show

samplePipelineVertexInputStateCreateInfo0 :: CreateInfo () () '[]
samplePipelineVertexInputStateCreateInfo0 = CreateInfo {
	createInfoNext = Nothing,
	createInfoFlags = In.PipelineVertexInputStateCreateFlagsZero }

type SamplePipelineVertexInputStateCreateInfoType = (
		(AddType [(Int, Double)] 'VertexInputRateVertex),
		(AddType [(Bool, Float)] 'VertexInputRateVertex) )

samplePipelineVertexInputStateCreateInfo ::
	CreateInfo () (
		(AddType [(Int, Double)] 'VertexInputRateVertex),
		(AddType [(Bool, Float)] 'VertexInputRateVertex)) '[Double, Float, Bool, Int]
samplePipelineVertexInputStateCreateInfo = CreateInfo {
	createInfoNext = Nothing,
	createInfoFlags = In.PipelineVertexInputStateCreateFlagsZero }

pipelineVertexInputStateCreateInfoToBindingDescription ::
	BindingStrideList vs
		VertexInputRate In.VertexInputRate =>
	CreateInfo n vs ts -> [In.VertexInputBindingDescription]
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
	BindingStrideList vs
		VertexInputRate In.VertexInputRate =>
	CreateInfo n vs ts -> [(SizeAlignment, In.VertexInputRate)]
pipelineVertexInputStateCreateInfoToBindingDescriptionRaw _ =
	bindingStrideList @vs @VertexInputRate @In.VertexInputRate

class PipelineVertexInputStateCreateInfoAttributeDescription vs (ts :: [*]) where
	pipelineVertexInputStateCreateInfoToAttributeDescription ::
		CreateInfo n vs ts -> [In.VertexInputAttributeDescription]

instance PipelineVertexInputStateCreateInfoAttributeDescription vs '[] where
	pipelineVertexInputStateCreateInfoToAttributeDescription _ = []

instance (
	BindingOffset vs t, Formattable t,
	PipelineVertexInputStateCreateInfoAttributeDescription vs ts) =>
	PipelineVertexInputStateCreateInfoAttributeDescription vs (t ': ts) where
	pipelineVertexInputStateCreateInfoToAttributeDescription :: forall n .
		CreateInfo n vs (t ': ts) -> [In.VertexInputAttributeDescription]
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
