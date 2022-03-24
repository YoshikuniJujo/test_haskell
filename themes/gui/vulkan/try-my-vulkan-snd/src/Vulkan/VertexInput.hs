{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.VertexInput where

import Foreign.Storable.SizeAlignment
import Data.Word

import Vulkan.Enum

import qualified Vulkan.VertexInput.Enum as E
import qualified Vulkan.VertexInput.Core as C

data Rate = RateVertex | RateInstance deriving Show

rateToEnum :: Rate -> E.Rate
rateToEnum RateVertex = E.RateVertex
rateToEnum RateInstance = E.RateInstance

data BindingDescription = BindingDescription {
	bindingDescriptionBinding :: Word32,
	bindingDescriptionStride :: Word32,
	bindingDescriptionInputRate :: Rate }
	deriving Show

bindingDescriptionFromRaw :: [(SizeAlignment, Rate)] -> [BindingDescription]
bindingDescriptionFromRaw sars = (<$> zip [0 ..] sars)
	\(b, ((fromIntegral -> sz, _algn), r)) -> BindingDescription b sz r

bindingDescriptionToCore :: BindingDescription -> C.BindingDescription
bindingDescriptionToCore BindingDescription {
	bindingDescriptionBinding = bd,
	bindingDescriptionStride = st,
	bindingDescriptionInputRate = rateToEnum -> E.Rate ir
	} = C.BindingDescription {
		C.bindingDescriptionBinding = bd,
		C.bindingDescriptionStride = st,
		C.bindingDescriptionInputRate = ir }

data AttributeDescription = AttributeDescription {
	attributeDescriptionLocation :: Word32,
	attributeDescriptionBinding :: Word32,
	attributeDescriptionFormat :: Format,
	attributeDescriptionOffset :: Word32 }
	deriving Show

attributeDescriptionToCore :: AttributeDescription -> C.AttributeDescription
attributeDescriptionToCore AttributeDescription {
	attributeDescriptionLocation = loc,
	attributeDescriptionBinding = bnd,
	attributeDescriptionFormat = Format fmt,
	attributeDescriptionOffset = oft } = C.AttributeDescription {
		C.attributeDescriptionLocation = loc,
		C.attributeDescriptionBinding = bnd,
		C.attributeDescriptionFormat = fmt,
		C.attributeDescriptionOffset = oft }

succAttributeDescriptionLocation :: AttributeDescription -> AttributeDescription
succAttributeDescriptionLocation
	ad@AttributeDescription { attributeDescriptionLocation = loc } =
	ad { attributeDescriptionLocation = loc + 1 }
