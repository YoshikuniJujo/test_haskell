{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Descriptor where

import GHC.TypeLits
import Data.Kind
import Data.Kind.Object
import Data.HeteroList

import qualified Gpu.Vulkan.Buffer as Buffer
import qualified Gpu.Vulkan.Buffer.Middle as Buffer.M
import qualified Gpu.Vulkan.Descriptor.Middle as M
import qualified Gpu.Vulkan.Descriptor.Core as C

import qualified Gpu.Vulkan.Sampler as Sampler
import qualified Gpu.Vulkan.Image.Enum as Image
import qualified Gpu.Vulkan.ImageView as ImageView

data BufferInfo (sbsmobjsobj :: BufferInfoArg) where
	BufferInfoAtom ::
		{ bufferInfoAtomBuffer :: Buffer.Binded sm sb nm objs } ->
		BufferInfo '(sb, sm, nm, objs, 'Atom v)
	BufferInfoList ::
		{ bufferInfoListBuffer :: Buffer.Binded sm sb nm objs } ->
		BufferInfo '(sb, sm, nm, objs, 'List v)

type BufferInfoArg = (Type, Type, Symbol, [Object], Object)

deriving instance Show (HeteroVarList ObjectLength objs) =>
	Show (BufferInfo '(sb, sm, nm, objs, obj))

bufferInfoToCore :: Offset obj objs =>
	BufferInfo '(sb, sm, nm, objs, obj) -> C.BufferInfo
bufferInfoToCore = M.bufferInfoToCore . bufferInfoToMiddle

bufferInfoToMiddle :: forall sb sm nm objs obj . Offset obj objs =>
	BufferInfo '(sb, sm, nm, objs, obj) -> M.BufferInfo
bufferInfoToMiddle BufferInfoAtom {
	bufferInfoAtomBuffer = Buffer.Binded lns b } = M.BufferInfo {
	M.bufferInfoBuffer = Buffer.M.B b,
	M.bufferInfoOffset = fromIntegral $ offset @obj 0 lns,
	M.bufferInfoRange = fromIntegral $ range @obj lns }
bufferInfoToMiddle BufferInfoList {
	bufferInfoListBuffer = Buffer.Binded lns b } = M.BufferInfo {
	M.bufferInfoBuffer = Buffer.M.B b,
	M.bufferInfoOffset = fromIntegral $ offset @obj 0 lns,
	M.bufferInfoRange = fromIntegral $ range @obj lns }

data ImageInfo ss fmt nm si = ImageInfo {
	imageInfoSampler :: Sampler.S ss,
	imageInfoImageView :: ImageView.INew fmt nm si,
	imageInfoImageLayout :: Image.Layout }
	deriving Show

imageInfoToMiddle ::
	ImageInfo ss fmt nm si -> M.ImageInfo
imageInfoToMiddle ImageInfo {
	imageInfoSampler = s,
	imageInfoImageView = ImageView.INew iv,
	imageInfoImageLayout = lyt } = M.ImageInfo {
	M.imageInfoSampler = Sampler.sToMiddle s,
	M.imageInfoImageView = iv,
	M.imageInfoImageLayout = lyt }
