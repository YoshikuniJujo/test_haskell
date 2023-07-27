{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGe MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.VertexInput (
	Rate(..),
	M.AttributeDescription(..),
	BindingDescription(..),

	bindingDescriptionFromRaw,
	bindingDescriptionToMiddle,

	TypeVal(..)

	) where

import Foreign.Storable.SizeAlignment
import Data.Word

import Gpu.Vulkan.VertexInput.Middle qualified as M
import Gpu.Vulkan.VertexInput.Enum qualified as M

data Rate = RateVertex | RateInstance deriving Show

instance TypeVal 'RateVertex Rate where typeVal = RateVertex
instance TypeVal 'RateInstance Rate where typeVal = RateInstance

data BindingDescription = BindingDescription {
	bindingDescriptionBinding :: Word32,
	bindingDescriptionStride :: Word32,
	bindingDescriptionInputRate :: Rate }
	deriving Show

bindingDescriptionFromRaw :: [(SizeAlignment, Rate)] -> [BindingDescription]
bindingDescriptionFromRaw sars = (<$> zip [0 ..] sars)
	\(b, ((fromIntegral -> sz, _algn), r)) -> BindingDescription b sz r

bindingDescriptionToMiddle :: BindingDescription -> M.BindingDescription
bindingDescriptionToMiddle BindingDescription {
	bindingDescriptionBinding = bdg,
	bindingDescriptionStride = strd,
	bindingDescriptionInputRate = rt } = M.BindingDescription {
	M.bindingDescriptionBinding = bdg,
	M.bindingDescriptionStride = strd,
	M.bindingDescriptionInputRate = rateToEnum rt }

rateToEnum :: Rate -> M.Rate
rateToEnum RateVertex = M.RateVertex
rateToEnum RateInstance = M.RateInstance

class TypeVal (t :: k) v where typeVal :: v
