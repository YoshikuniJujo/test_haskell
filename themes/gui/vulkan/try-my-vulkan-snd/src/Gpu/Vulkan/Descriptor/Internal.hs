{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Descriptor.Internal (

	-- * BUFFER INFO

	BufferInfo(..), BufferInfoArgs, bufferInfoToMiddleNew,
	bufferInfoListToLengthNew, Map3_4,

	-- * IMAGE INFO

	ImageInfo(..), imageInfoToMiddle,

	) where

import GHC.TypeLits
import Data.Kind
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import Gpu.Vulkan.Object qualified as VObj
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))

import qualified Gpu.Vulkan.Buffer as Buffer
import qualified Gpu.Vulkan.Buffer.Type as Buffer
import qualified Gpu.Vulkan.Descriptor.Middle as M

import qualified Gpu.Vulkan.Sampler as Sampler
import qualified Gpu.Vulkan.Image.Enum as Image
import qualified Gpu.Vulkan.ImageView as ImageView

data BufferInfo sm sb nm obj = forall objs .
	(Show (Buffer.Binded sm sb nm objs), VObj.Offset obj objs) =>
	BufferInfo (Buffer.Binded sm sb nm objs)

deriving instance Show (BufferInfo sm sb nm obj)

type BufferInfoArgs = (Type, Type, Symbol, VObj.Object)

bufferInfoToLengthNew :: BufferInfo sb sm nm obj -> VObj.ObjectLength obj
bufferInfoToLengthNew (BufferInfo (Buffer.Binded lns _)) = HeteroParList.typeIndex lns

bufferInfoListToLengthNew :: Map3_4 sbsmobjsobjs =>
	HeteroParList.PL (U4 BufferInfo) sbsmobjsobjs ->
	HeteroParList.PL VObj.ObjectLength (TMapIndex.M3_4 sbsmobjsobjs)
bufferInfoListToLengthNew = map3_4 $ bufferInfoToLengthNew . unU4

class Map3_4 (ss :: [(k0, k1, k2, k3)]) where
	map3_4 :: (forall (a :: k0) (b :: k1) (c :: k2) (d :: k3) . t '(a, b, c, d) -> t' d) ->
		HeteroParList.PL t (ss :: [(k0, k1, k2, k3)]) -> HeteroParList.PL t' (TMapIndex.M3_4 ss)

instance Map3_4 '[] where map3_4 _ HeteroParList.Nil = HeteroParList.Nil

instance Map3_4 ts => Map3_4 ('(a, b, c, d) ': ts) where
	map3_4 f (x :** xs) = f x :** map3_4 f xs

bufferInfoToMiddleNew :: forall sb sm nm obj .
	BufferInfo sm sb nm obj -> M.BufferInfo
bufferInfoToMiddleNew (BufferInfo (Buffer.Binded lns b)) = M.BufferInfo {
	M.bufferInfoBuffer = b,
	M.bufferInfoOffset = fromIntegral $ VObj.offset @obj 0 lns,
	M.bufferInfoRange = fromIntegral $ VObj.range @obj lns }

data ImageInfo ss fmt nm si = ImageInfo {
	imageInfoSampler :: Sampler.S ss,
	imageInfoImageView :: ImageView.I fmt nm si,
	imageInfoImageLayout :: Image.Layout }
	deriving Show

imageInfoToMiddle ::
	ImageInfo ss fmt nm si -> M.ImageInfo
imageInfoToMiddle ImageInfo {
	imageInfoSampler = s,
	imageInfoImageView = ImageView.I iv,
	imageInfoImageLayout = lyt } = M.ImageInfo {
	M.imageInfoSampler = Sampler.sToMiddle s,
	M.imageInfoImageView = iv,
	M.imageInfoImageLayout = lyt }
