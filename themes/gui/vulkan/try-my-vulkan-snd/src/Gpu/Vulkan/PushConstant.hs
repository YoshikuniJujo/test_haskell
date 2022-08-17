{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PushConstant where

import Foreign.Storable
import Data.Kind
import Data.Bits
import Data.Word

import Gpu.Vulkan.Enum

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.PushConstant.Middle as M

class IsPrefixOf (part :: [Type]) (whole :: [Type])

instance IsPrefixOf '[] whole

instance IsPrefixOf part whole => IsPrefixOf (t ': part) (t ': whole)

class Size (ts :: [Type]) where sizeSize :: Int -> Int

instance Size '[] where sizeSize sz = sz

instance (Storable t, Size ts) => Size (t ': ts) where
	sizeSize sz = ((sz - 1) `div` al + 1) * al + sizeOf @t undefined
		where al = alignment @t undefined

class OffsetSize (whole :: [Type]) (part :: [Type]) where
	offset :: Word32 -> Word32
	size :: Word32

instance (IsPrefixOf (t ': part) whole, Size (t ': part), Storable t) =>
	OffsetSize whole (t ': part) where
	offset oft = ((oft - 1) `div` al + 1) * al
		where al = fromIntegral $ alignment @t undefined
	size = fromIntegral $ sizeSize @(t ': part) 0

instance (Storable t, OffsetSize whole part) =>
	OffsetSize (t ': whole) part where
	offset oft = ((oft - 1) `div` al + 1) * al +
		fromIntegral (sizeOf @t undefined)
		where al = fromIntegral $ alignment @t undefined
	size = size @whole @part

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

instance (ShaderStageFlagBitsToMiddle sss, OffsetSize whole part) =>
	RangeToMiddle whole ('Range sss part) where
	rangeToMiddle = M.Range {
		M.rangeStageFlags = shaderStageFlagBitsToMiddle @sss,
		M.rangeOffset = offset @whole @part 0,
		M.rangeSize = size @whole @part }

class RangesToMiddle (whole :: [Type]) (ranges :: [Range]) where
	rangesToMiddle :: [M.Range]

instance RangesToMiddle whole '[] where rangesToMiddle = []

instance (RangeToMiddle whole range, RangesToMiddle whole ranges) =>
	RangesToMiddle whole (range ': ranges) where
	rangesToMiddle =
		rangeToMiddle @whole @range : rangesToMiddle @whole @ranges

data PushConstantLayout = PushConstantLayout [Type] [Range]

pushConstantLayoutToRanges ::
	forall (pcl :: PushConstantLayout) whole ranges .
	(pcl ~ ('PushConstantLayout whole ranges), RangesToMiddle whole ranges) =>
	[M.Range]
pushConstantLayoutToRanges = rangesToMiddle @whole @ranges
