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

	BufferInfo(..), bufferInfoToMiddle,

	-- * IMAGE INFO

	ImageInfo(..), imageInfoToMiddle,

	) where

import Gpu.Vulkan.Object qualified as VObj

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

bufferInfoToMiddle :: forall sb sm nm obj .
	BufferInfo sm sb nm obj -> M.BufferInfo
bufferInfoToMiddle (BufferInfo (Buffer.Binded lns b)) = M.BufferInfo {
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
