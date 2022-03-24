{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.VertexInput where

import Data.Word

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

bindingDescriptionToCore :: BindingDescription -> C.BindingDescription
bindingDescriptionToCore BindingDescription {
	bindingDescriptionBinding = bd,
	bindingDescriptionStride = st,
	bindingDescriptionInputRate = rateToEnum -> E.Rate ir
	} = C.BindingDescription {
		C.bindingDescriptionBinding = bd,
		C.bindingDescriptionStride = st,
		C.bindingDescriptionInputRate = ir }
