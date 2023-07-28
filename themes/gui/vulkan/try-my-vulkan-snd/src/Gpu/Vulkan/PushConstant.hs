{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PushConstant (

	-- * PUSHCONSTANT LAYOUT

	Layout(..), Range(..),

	-- * OEHERS

	ShaderStageFlagBitsToMiddle(..),
	pushConstantLayoutToRanges,
	RangesToMiddle

	) where

import Foreign.Storable.HeteroList
import Data.Kind
import Data.Bits

import Gpu.Vulkan.Enum
import Gpu.Vulkan.TypeEnum qualified as T
import Gpu.Vulkan.PushConstant.Middle qualified as M

class ShaderStageFlagBitsToMiddle (sss :: [T.ShaderStageFlagBits]) where
	shaderStageFlagBitsToMiddle :: ShaderStageFlagBits

instance ShaderStageFlagBitsToMiddle '[] where
	shaderStageFlagBitsToMiddle = zeroBits

instance (T.ShaderStageFlagBitsToValue ss, ShaderStageFlagBitsToMiddle sss) =>
	ShaderStageFlagBitsToMiddle (ss ': sss) where
	shaderStageFlagBitsToMiddle =
		T.shaderStageFlagBitsToValue @ss .|.
		shaderStageFlagBitsToMiddle @sss

data Range = Range [T.ShaderStageFlagBits] [Type]

class RangeToMiddle (whole :: [Type]) (range :: Range) where
	rangeToMiddle :: M.Range

instance (ShaderStageFlagBitsToMiddle sss, InfixOffsetSize part whole) =>
	RangeToMiddle whole ('Range sss part) where
	rangeToMiddle = M.Range {
		M.rangeStageFlags = shaderStageFlagBitsToMiddle @sss,
		M.rangeOffset = fromIntegral offt,
		M.rangeSize = fromIntegral sz }
		where
		(offt, sz) = infixOffsetSize @part @whole

class RangesToMiddle (whole :: [Type]) (ranges :: [Range]) where
	rangesToMiddle :: [M.Range]

instance RangesToMiddle whole '[] where rangesToMiddle = []

instance (RangeToMiddle whole range, RangesToMiddle whole ranges) =>
	RangesToMiddle whole (range ': ranges) where
	rangesToMiddle =
		rangeToMiddle @whole @range : rangesToMiddle @whole @ranges

data Layout = Layout [Type] [Range]

pushConstantLayoutToRanges ::
	forall (pcl :: Layout) whole ranges .
	(pcl ~ ('Layout whole ranges), RangesToMiddle whole ranges) =>
	[M.Range]
pushConstantLayoutToRanges = rangesToMiddle @whole @ranges
