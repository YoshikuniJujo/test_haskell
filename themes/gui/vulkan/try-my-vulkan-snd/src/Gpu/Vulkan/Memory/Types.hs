{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Memory.Types (

	-- * IMAGE BUFFER

	ImageBuffer(..), ImageBufferBinded(..), ImageBufferArg(..),
	Alignments(..),

	-- * OTHERS

	adjustOffsetSize

	) where

import Prelude hiding (map, read)
import GHC.TypeLits
import Data.Kind
import Gpu.Vulkan.Object qualified as VObj
import Data.TypeLevel.Tuple.Uncurry
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))
import Data.IORef

import qualified Gpu.Vulkan.Image.Type as Image
import qualified Gpu.Vulkan.Buffer.Type as Buffer
import qualified Gpu.Vulkan.Memory.Middle as M

import qualified Gpu.Vulkan.TypeEnum as T

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Device.Middle as Device.M

import qualified Gpu.Vulkan.Image.Type as Image
import qualified Gpu.Vulkan.Image.Middle as Image.M
import qualified Gpu.Vulkan.Buffer.Type as Buffer
import qualified Gpu.Vulkan.Buffer.Middle as Buffer.M
import qualified Gpu.Vulkan.Memory.Middle as Memory.M

data ImageBuffer sib (ib :: ImageBufferArg) where
	Image :: Image.I si nm fmt -> ImageBuffer si ('ImageArg nm fmt)
	Buffer :: Buffer.B sb nm objs -> ImageBuffer sb ('BufferArg nm objs)

deriving instance Show (Image.I sib nm fmt) =>
	Show (ImageBuffer sib ('ImageArg nm fmt))

deriving instance Show (HeteroParList.PL VObj.ObjectLength objs) =>
	Show (ImageBuffer sib ('BufferArg nm objs))

data ImageBufferBinded sm sib (ib :: ImageBufferArg) where
	ImageBinded :: Image.Binded sm si nm fmt ->
		ImageBufferBinded sm si ('ImageArg nm fmt)
	BufferBinded :: Buffer.Binded sm sb nm objs ->
		ImageBufferBinded sm sb ('BufferArg nm objs)

data ImageBufferArg = ImageArg Symbol T.Format | BufferArg Symbol [VObj.Object]

class Alignments (ibs :: [(Type, ImageBufferArg)]) where
	alignments :: [Maybe Int]

instance Alignments '[] where alignments = []

instance Alignments ibs =>
	Alignments ('(_s, 'ImageArg _nm _fmt) ': ibs) where
	alignments = Nothing : alignments @ibs

instance (VObj.SizeAlignment obj, Alignments ibs) =>
	Alignments ('(_s, 'BufferArg _nm (obj ': _objs)) ': ibs) where
	alignments = Just (VObj.objectAlignment @obj) : alignments @ibs

adjustOffsetSize :: Device.D sd -> ImageBuffer sib ibarg -> Device.M.Size ->
	IO (Device.M.Size, Device.M.Size)
adjustOffsetSize dvc ib ost = do
	reqs <- getMemoryRequirements dvc ib
	let	algn = Memory.M.requirementsAlignment reqs
		sz = Memory.M.requirementsSize reqs
	pure (((ost - 1) `div` algn + 1) * algn, sz)

getMemoryRequirements ::
	Device.D sd -> ImageBuffer sib fos -> IO Memory.M.Requirements
getMemoryRequirements (Device.D dvc) (Buffer (Buffer.B _ b)) =
	Buffer.M.getMemoryRequirements dvc b
getMemoryRequirements (Device.D dvc) (Image (Image.I i)) =
	Image.M.getMemoryRequirements dvc i
