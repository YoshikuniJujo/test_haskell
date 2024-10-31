{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.VertexInput.Middle.Internal where

import Data.Word

import Gpu.Vulkan.Enum

import qualified Gpu.Vulkan.VertexInput.Enum as E
import qualified Gpu.Vulkan.VertexInput.Core as C

data BindingDescription = BindingDescription {
	bindingDescriptionBinding :: Word32,
	bindingDescriptionStride :: Word32,
	bindingDescriptionInputRate :: E.Rate }
	deriving Show

bindingDescriptionToCore :: BindingDescription -> C.BindingDescription
bindingDescriptionToCore BindingDescription {
	bindingDescriptionBinding = bd,
	bindingDescriptionStride = st,
	bindingDescriptionInputRate = E.Rate ir
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
