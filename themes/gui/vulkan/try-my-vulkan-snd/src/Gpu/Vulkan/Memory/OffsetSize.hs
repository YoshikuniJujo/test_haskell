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

	-- * OTHERS

	offset,

	OffsetSize(..), OffsetSizeObject(..),
	offsetSize,
	Alignments(..),

	Offset',

	offsetSizeLength

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

getMemoryRequirements ::
	Device.D sd -> ImageBuffer sib fos -> IO Memory.M.Requirements
getMemoryRequirements (Device.D dvc) (Buffer (Buffer.B _ b)) =
	Buffer.M.getMemoryRequirements dvc b
getMemoryRequirements (Device.D dvc) (Image (Image.I i)) =
	Image.M.getMemoryRequirements dvc i

getMemoryRequirements' ::
	Device.D sd -> U2 ImageBuffer sibfos -> IO Memory.M.Requirements
getMemoryRequirements' dvc (U2 bi) = getMemoryRequirements dvc bi

class RebindAll sibfoss sibfoss' where
	rebindAll :: Device.D sd ->
		HeteroParList.PL (U2 (ImageBufferBinded sm)) sibfoss ->
		M sm sibfoss' -> IO ()

instance RebindAll '[] sibfoss' where rebindAll _ _ _ = pure ()

instance (
	Offset' si ('ImageArg nm fmt) sibfoss', RebindAll fibfoss sibfoss' ) =>
	RebindAll ('(si, 'ImageArg nm fmt) ': fibfoss) sibfoss' where
		rebindAll dvc (U2 (ImageBinded img) :** ibs) m = do
			rebindImage dvc img m
			rebindAll dvc ibs m

instance (
	Offset' sb ('BufferArg nm objs) sibfoss', RebindAll sibfoss sibfoss' ) =>
	RebindAll ('(sb, 'BufferArg nm objs) ': sibfoss) sibfoss' where
	rebindAll dvc (U2 (BufferBinded bf) :** ibs) m = do
		rebindBuffer dvc bf m
		rebindAll dvc ibs m

class BindAll sibfoss sibfoss' where
	bindAll :: Device.D sd -> HeteroParList.PL (U2 ImageBuffer) sibfoss ->
		M sm sibfoss' ->
		IO (HeteroParList.PL (U2 (ImageBufferBinded sm)) sibfoss)

instance BindAll '[] sibfoss' where bindAll _ _ _ = pure HeteroParList.Nil

instance (Offset' si ('ImageArg nm fmt) sibfoss', BindAll fibfoss sibfoss') =>
	BindAll ('(si, ('ImageArg nm fmt)) ': fibfoss) sibfoss' where
	bindAll dvc (U2 (Image img) :** ibs) m = (:**)
		<$> (U2 . ImageBinded <$> bindImage dvc img m)
		<*> bindAll dvc ibs m

instance (Offset' sb ('BufferArg nm objs) sibfoss', BindAll fibfoss sibfoss') =>
	BindAll ('(sb, ('BufferArg nm objs)) ': fibfoss) sibfoss' where
	bindAll dvc (U2 (Buffer bf) :** ibs) m = (:**)
		<$> (U2 . BufferBinded <$> bindBuffer dvc bf m)
		<*> bindAll dvc ibs m

bindImage :: forall sd si nm fmt sm sibfoss .
	Offset' si ('ImageArg nm fmt) sibfoss =>
	Device.D sd -> Image.I si nm fmt -> M sm sibfoss ->
	IO (Image.Binded sm si nm fmt)
bindImage dvc@(Device.D mdvc) (Image.I i) m = do
	(_, mm) <- readM'' m
	ost <- offset @si @('ImageArg nm fmt) dvc m 0
	Image.M.bindMemory mdvc i mm ost
	pure (Image.Binded i)

rebindImage :: forall sd si sm nm fmt sibfoss .
	Offset' si ('ImageArg nm fmt) sibfoss =>
	Device.D sd -> Image.Binded sm si nm fmt -> M sm sibfoss -> IO ()
rebindImage dvc@(Device.D mdvc) (Image.Binded i) m = do
	(_, mm) <- readM'' m
	ost <- offset @si @('ImageArg nm fmt) dvc m 0
	Image.M.bindMemory mdvc i mm ost

bindBuffer :: forall sd sb nm objs sm sibfoss . Offset' sb ('BufferArg nm objs) sibfoss =>
	Device.D sd -> Buffer.B sb nm objs -> M sm sibfoss ->
	IO (Buffer.Binded sm sb nm objs)
bindBuffer dvc@(Device.D mdvc) (Buffer.B lns b) m = do
	(_, mm) <- readM'' m
	ost <- offset @sb @('BufferArg nm objs) dvc m 0
	Buffer.M.bindMemory mdvc b mm ost
	pure (Buffer.Binded lns b)

rebindBuffer :: forall sd sb sm nm objs sibfoss .
	Offset' sb ('BufferArg nm objs) sibfoss =>
	Device.D sd -> Buffer.Binded sm sb nm objs -> M sm sibfoss -> IO ()
rebindBuffer dvc@(Device.D mdvc) (Buffer.Binded _lns b) m = do
	(_, mm) <- readM'' m
	ost <- offset @sb @('BufferArg nm objs) dvc m 0
	Buffer.M.bindMemory mdvc b mm ost

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

instance OffsetSizeObject obj objs =>
	OffsetSize nm obj ('(sib, 'BufferArg nm objs) ': sibfoss) where
	offsetSize' dvc (ib@(U2 (Buffer (Buffer.B lns _))) :** _ibs) ost = do
		reqs <- getMemoryRequirements' dvc ib
		let	algn = Memory.M.requirementsAlignment reqs
		pure $ offsetSizeObject @obj
			(((ost - 1) `div` algn + 1) * algn) lns
	offsetSizeLength' (U2 (Buffer (Buffer.B lns _)) :** _) =
		pure $ offsetSizeObjectLength @obj lns

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

class OffsetSizeObject (obj :: VObj.Object) (objs :: [VObj.Object]) where
	offsetSizeObject :: Device.M.Size -> HeteroParList.PL VObj.ObjectLength objs ->
		(Device.M.Size, Device.M.Size)
	offsetSizeObjectLength :: HeteroParList.PL VObj.ObjectLength objs ->
		VObj.ObjectLength obj

instance VObj.SizeAlignment obj => OffsetSizeObject obj (obj ': objs) where
	offsetSizeObject n (ln :** _) = (ost, fromIntegral $ VObj.objectSize' ln)
		where
		ost = ((n - 1) `div` algn + 1) * algn
		algn = fromIntegral (VObj.objectAlignment @obj)
	offsetSizeObjectLength (ln :** _) = ln

instance {-# OVERLAPPABLE #-} (
	VObj.SizeAlignment obj, VObj.SizeAlignment obj',
	OffsetSizeObject obj objs ) =>
	OffsetSizeObject obj (obj' ': objs) where
	offsetSizeObject n (ln :** lns) =
		offsetSizeObject @obj (ost + fromIntegral (VObj.objectSize' ln)) lns
		where
		ost = ((n - 1) `div` algn + 1) * algn
		algn = fromIntegral (VObj.objectAlignment @obj)
	offsetSizeObjectLength (_ :** lns) = offsetSizeObjectLength @obj lns

class Alignments (ibs :: [(Type, ImageBufferArg)]) where
	alignments :: [Maybe Int]

instance Alignments '[] where alignments = []

instance Alignments ibs =>
	Alignments ('(_s, 'ImageArg _nm _fmt) ': ibs) where
	alignments = Nothing : alignments @ibs

instance (VObj.SizeAlignment obj, Alignments ibs) =>
	Alignments ('(_s, 'BufferArg _nm (obj ': _objs)) ': ibs) where
	alignments = Just (VObj.objectAlignment @obj) : alignments @ibs
