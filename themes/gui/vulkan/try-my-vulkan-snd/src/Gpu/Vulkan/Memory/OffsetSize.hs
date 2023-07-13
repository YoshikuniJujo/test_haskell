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

module Gpu.Vulkan.Memory.OffsetSize (

	-- * OFFSET

	offset, Offset',

	-- * OFFSET SIZE

	offsetSize, offsetSizeLength,
	OffsetSize,

	) where

import Prelude hiding (map, read)
import GHC.TypeLits
import Data.Kind
import Gpu.Vulkan.Object qualified as VObj
import Data.TypeLevel.Tuple.Uncurry
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Device.Middle as Device.M
import qualified Gpu.Vulkan.Image.Type as Image
import qualified Gpu.Vulkan.Image.Middle as Image.M
import qualified Gpu.Vulkan.Buffer.Type as Buffer
import qualified Gpu.Vulkan.Buffer.Middle as Buffer.M
import qualified Gpu.Vulkan.Memory.Middle as Memory.M

import Gpu.Vulkan.Memory.Types

offset :: forall sib ib sibfoss sd sm . Offset' sib ib sibfoss =>
	Device.D sd -> M sm sibfoss -> Device.M.Size -> IO Device.M.Size
offset dvc m ost = do
	(ibs, _) <- readM'' m
	offset' @sib @ib @sibfoss dvc ibs ost

class Offset'
	sib (ib :: ImageBufferArg) (sibfoss :: [(Type, ImageBufferArg)]) where
	offset' :: Device.D sd -> HeteroParList.PL (U2 ImageBuffer) sibfoss ->
		Device.M.Size -> IO Device.M.Size

instance Offset' sib ib ('(sib, ib) ': sibfoss) where
	offset' dvc (ib :** _ibs) ost = do
		reqs <- getMemoryRequirements' dvc ib
		let	algn = Memory.M.requirementsAlignment reqs
		pure $ ((ost - 1) `div` algn + 1) * algn

instance {-# OVERLAPPABLE #-} Offset' sib ib sibfoss =>
	Offset' sib ib ('(sib', ib') ': sibfoss) where
	offset' dvc (ib :** ibs) ost = do
		reqs <- getMemoryRequirements' dvc ib
		let	sz = Memory.M.requirementsSize reqs
			algn = Memory.M.requirementsAlignment reqs
		offset' @sib @ib dvc ibs
			$ ((ost - 1) `div` algn + 1) * algn + sz

getMemoryRequirements' ::
	Device.D sd -> U2 ImageBuffer sibfos -> IO Memory.M.Requirements
getMemoryRequirements' dvc (U2 bi) = getMemoryRequirements dvc bi

getMemoryRequirements ::
	Device.D sd -> ImageBuffer sib fos -> IO Memory.M.Requirements
getMemoryRequirements (Device.D dvc) (Buffer (Buffer.B _ b)) =
	Buffer.M.getMemoryRequirements dvc b
getMemoryRequirements (Device.D dvc) (Image (Image.I i)) =
	Image.M.getMemoryRequirements dvc i

offsetSize :: forall nm obj sibfoss sd sm . OffsetSize nm obj sibfoss =>
	Device.D sd -> M sm sibfoss -> Device.M.Size ->
	IO (Device.M.Size, Device.M.Size)
offsetSize dvc m ost = do
	(ibs, _m) <- readM'' m
	offsetSize' @nm @obj @sibfoss dvc ibs ost

offsetSizeLength :: forall nm obj sibfoss sm . OffsetSize nm obj sibfoss =>
	M sm sibfoss -> IO (VObj.ObjectLength obj)
offsetSizeLength m = do
	(lns, _m) <- readM'' m
	offsetSizeLength' @nm @obj @sibfoss lns

class OffsetSize (nm :: Symbol) (obj :: VObj.Object) sibfoss where
	offsetSize' :: Device.D sd -> HeteroParList.PL (U2 ImageBuffer) sibfoss ->
		Device.M.Size -> IO (Device.M.Size, Device.M.Size)
	offsetSizeLength' :: HeteroParList.PL (U2 ImageBuffer) sibfoss -> IO (VObj.ObjectLength obj)

instance (
	VObj.Offset obj objs, VObj.ObjectLengthOf obj objs
	) =>
	OffsetSize nm obj ('(sib, 'BufferArg nm objs) ': sibfoss) where
	offsetSize' dvc (ib@(U2 (Buffer (Buffer.B lns _))) :** _ibs) ost = do
		reqs <- getMemoryRequirements' dvc ib
		let	algn = Memory.M.requirementsAlignment reqs
		pure $ offsetSizeObject @obj
			(((ost - 1) `div` algn + 1) * algn) lns
	offsetSizeLength' (U2 (Buffer (Buffer.B lns _)) :** _) =
		pure $ VObj.objectLengthOf @obj lns

instance {-# OVERLAPPABLE #-}
	OffsetSize nm obj sibfoss =>
	OffsetSize nm obj ('(sib, ib) ': sibfoss) where
	offsetSize' dvc (ib :** ibs) ost = do
		reqs <- getMemoryRequirements' dvc ib
		let	algn = Memory.M.requirementsAlignment reqs
			sz = Memory.M.requirementsSize reqs
		offsetSize' @nm @obj dvc ibs
			$ ((ost - 1) `div` algn + 1) * algn + sz
	offsetSizeLength' (_ :** lns) = offsetSizeLength' @nm @obj lns

offsetSizeObject :: forall (obj :: VObj.Object) objs .
	VObj.Offset obj objs =>
	Device.M.Size -> HeteroParList.PL VObj.ObjectLength objs ->
		(Device.M.Size, Device.M.Size)
offsetSizeObject n = VObj.offsetFromSizeAlignmentList' @obj (fromIntegral n)
	. VObj.sizeAlignmentList
