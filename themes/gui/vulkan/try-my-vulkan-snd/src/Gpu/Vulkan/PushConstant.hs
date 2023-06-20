{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PushConstant where

import Foreign.Storable
import Data.Kind
import Data.Bits
import Data.Word
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))

import Gpu.Vulkan.Enum
import Gpu.Vulkan.TypeEnum qualified as T
import Gpu.Vulkan.PushConstant.Middle qualified as M

infixOffsetSize :: forall (part :: [Type]) (whole :: [Type]) .
	InfixOffsetSize part whole => (Offset, Size)
infixOffsetSize =
	infixOffsetSizeFromSizeAlignmentList @part @whole 0 (sizeAlignmentList @whole)

data SizeAlignmentOfType (tp :: Type) = SizeAlignmentOfType Size Alignment
	deriving Show

type Size = Word32; type Alignment = Word32

class SizeAlignmentList ts where
	sizeAlignmentList :: HeteroParList.PL SizeAlignmentOfType ts

instance SizeAlignmentList '[] where
	sizeAlignmentList = HeteroParList.Nil

instance (Storable t, SizeAlignmentList ts) => SizeAlignmentList (t ': ts) where
	sizeAlignmentList =
		SizeAlignmentOfType
			(fromIntegral $ sizeOf @t undefined)
			(fromIntegral $ alignment @t undefined) :**
		sizeAlignmentList @ts

class SizeAlignmentList whole => InfixOffsetSize (part :: [Type]) whole where
	infixOffsetSizeFromSizeAlignmentList :: Size ->
		HeteroParList.PL SizeAlignmentOfType whole -> (Offset, Size)

type Offset = Word32

instance (
	Storable t, SizeAlignmentList whole,
	(t ': ts) `PrefixSize` (t ': whole) ) =>
	InfixOffsetSize (t ': ts) (t ': whole) where
	infixOffsetSizeFromSizeAlignmentList sz0
		saa@(SizeAlignmentOfType _ algn :** _) = (
		align algn sz0,
		prefixSizeFromSizeAlignmentList @(t ': ts) 0 saa )

instance {-# OVERLAPPABLE #-} (Storable t, InfixOffsetSize ts whole) =>
	InfixOffsetSize ts (t ': whole) where
	infixOffsetSizeFromSizeAlignmentList sz0
		(SizeAlignmentOfType sz algn :** sas) =
		infixOffsetSizeFromSizeAlignmentList @ts
			(align algn sz0 + sz) sas

class PrefixSize (part :: [Type]) whole where
	prefixSizeFromSizeAlignmentList :: Size ->
		HeteroParList.PL SizeAlignmentOfType whole -> Size

instance PrefixSize '[] whole where
	prefixSizeFromSizeAlignmentList sz _ = sz

instance PrefixSize  ts whole => PrefixSize (t ': ts) (t ': whole) where
	prefixSizeFromSizeAlignmentList sz0
		(SizeAlignmentOfType sz algn :** sas) =
		prefixSizeFromSizeAlignmentList @ts (align algn sz0 + sz) sas

align :: Word32 -> Word32 -> Word32
align algn offt = ((offt - 1) `div` algn + 1) * algn

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
		M.rangeOffset = offt, M.rangeSize = sz }
		where
		(offt, sz) = infixOffsetSize @part @whole

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
