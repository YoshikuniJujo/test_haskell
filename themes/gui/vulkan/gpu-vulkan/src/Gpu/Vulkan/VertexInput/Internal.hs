{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGe MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.VertexInput.Internal (
	Rate(..),
	M.AttributeDescription(..),
	BindingDescription(..),

	bindingDescriptionFromRaw,
	bindingDescriptionToMiddle,

	) where

import Data.Word
import Data.TypeLevel.TypeVal qualified as TypeVal

import Gpu.Vulkan.VertexInput.Middle qualified as M
import Gpu.Vulkan.VertexInput.Enum qualified as M

data Rate = RateVertex | RateInstance deriving Show

instance TypeVal.T 'RateVertex Rate where t = RateVertex
instance TypeVal.T 'RateInstance Rate where t = RateInstance

data BindingDescription = BindingDescription {
	bindingDescriptionBinding :: Word32,
	bindingDescriptionStride :: Word32,
	bindingDescriptionInputRate :: Rate }
	deriving Show

type SizeAlignment = (Int, Int)

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
	where rateToEnum = \case
		RateVertex -> M.RateVertex; RateInstance -> M.RateInstance
