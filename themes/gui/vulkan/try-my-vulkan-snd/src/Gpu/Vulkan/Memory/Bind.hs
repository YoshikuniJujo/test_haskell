{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Memory.Bind (

	-- * BIND AND REBIND

	BindAll(..), RebindAll(..), Offset

	) where

import Data.Kind
import Data.TypeLevel.Tuple.Uncurry
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))

import Gpu.Vulkan.Device.Type qualified as Device
import Gpu.Vulkan.Device.Middle qualified as Device.M
import Gpu.Vulkan.Memory.ImageBuffer

import Gpu.Vulkan.Buffer.Type qualified as Buffer
import Gpu.Vulkan.Buffer.Middle qualified as Buffer.M

import Gpu.Vulkan.Image.Type qualified as Image
import Gpu.Vulkan.Image.Middle qualified as Image.M

import Gpu.Vulkan.Memory.Type

offset :: forall ib ibargs sd sm . Offset ib ibargs =>
	Device.D sd -> M sm ibargs -> Device.M.Size -> IO Device.M.Size
offset dvc m ost = do
	(ibs, _) <- readM m
	offset' @ib @ibargs dvc ibs ost

class Offset (ib :: ImageBufferArg) (ibargs :: [(Type, ImageBufferArg)]) where
	offset' :: Device.D sd -> HeteroParList.PL (U2 ImageBuffer) ibargs ->
		Device.M.Size -> IO Device.M.Size

instance Offset ib ('(sib, ib) ': ibargs) where
	offset' dvc (U2 ib :** _ibs) ost = fst <$> adjustOffsetSize dvc ib ost

instance {-# OVERLAPPABLE #-} Offset ib ibargs =>
	Offset ib ('(sib', ib') ': ibargs) where
	offset' dvc (U2 ib :** ibs) ost = do
		(ost', sz) <- adjustOffsetSize dvc ib ost
		offset' @ib dvc ibs $ ost' + sz

class BindAll sibfoss sibfoss' where
	bindAll :: Device.D sd -> HeteroParList.PL (U2 ImageBuffer) sibfoss ->
		M sm sibfoss' ->
		IO (HeteroParList.PL (U2 (ImageBufferBinded sm)) sibfoss)

instance BindAll '[] sibfoss' where bindAll _ _ _ = pure HeteroParList.Nil

instance (Offset ('ImageArg nm fmt) sibfoss', BindAll fibfoss sibfoss') =>
	BindAll ('(si, ('ImageArg nm fmt)) ': fibfoss) sibfoss' where
	bindAll dvc (U2 (Image img) :** ibs) m = (:**)
		<$> (U2 . ImageBinded <$> bindImage dvc img m)
		<*> bindAll dvc ibs m

instance (Offset ('BufferArg nm objs) sibfoss', BindAll fibfoss sibfoss') =>
	BindAll ('(sb, ('BufferArg nm objs)) ': fibfoss) sibfoss' where
	bindAll dvc (U2 (Buffer bf) :** ibs) m = (:**)
		<$> (U2 . BufferBinded <$> bindBuffer dvc bf m)
		<*> bindAll dvc ibs m

bindImage :: forall sd si nm fmt sm sibfoss .
	Offset ('ImageArg nm fmt) sibfoss =>
	Device.D sd -> Image.I si nm fmt -> M sm sibfoss ->
	IO (Image.Binded sm si nm fmt)
bindImage dvc@(Device.D mdvc) (Image.I i) m = do
	(_, mm) <- readM m
	ost <- offset @('ImageArg nm fmt) dvc m 0
	Image.M.bindMemory mdvc i mm ost
	pure (Image.Binded i)

bindBuffer :: forall sd sb nm objs sm sibfoss . Offset ('BufferArg nm objs) sibfoss =>
	Device.D sd -> Buffer.B sb nm objs -> M sm sibfoss ->
	IO (Buffer.Binded sm sb nm objs)
bindBuffer dvc@(Device.D mdvc) (Buffer.B lns b) m = do
	(_, mm) <- readM m
	ost <- offset @('BufferArg nm objs) dvc m 0
	Buffer.M.bindMemory mdvc b mm ost
	pure (Buffer.Binded lns b)

class RebindAll sibfoss sibfoss' where
	rebindAll :: Device.D sd ->
		HeteroParList.PL (U2 (ImageBufferBinded sm)) sibfoss ->
		M sm sibfoss' -> IO ()

instance RebindAll '[] sibfoss' where rebindAll _ _ _ = pure ()

instance (
	Offset ('ImageArg nm fmt) sibfoss', RebindAll fibfoss sibfoss' ) =>
	RebindAll ('(si, 'ImageArg nm fmt) ': fibfoss) sibfoss' where
		rebindAll dvc (U2 (ImageBinded img) :** ibs) m = do
			rebindImage dvc img m
			rebindAll dvc ibs m

instance (
	Offset ('BufferArg nm objs) sibfoss', RebindAll sibfoss sibfoss' ) =>
	RebindAll ('(sb, 'BufferArg nm objs) ': sibfoss) sibfoss' where
	rebindAll dvc (U2 (BufferBinded bf) :** ibs) m = do
		rebindBuffer dvc bf m
		rebindAll dvc ibs m

rebindImage :: forall sd si sm nm fmt sibfoss .
	Offset ('ImageArg nm fmt) sibfoss =>
	Device.D sd -> Image.Binded sm si nm fmt -> M sm sibfoss -> IO ()
rebindImage dvc@(Device.D mdvc) (Image.Binded i) m = do
	(_, mm) <- readM m
	ost <- offset @('ImageArg nm fmt) dvc m 0
	Image.M.bindMemory mdvc i mm ost

rebindBuffer :: forall sd sb sm nm objs sibfoss .
	Offset ('BufferArg nm objs) sibfoss =>
	Device.D sd -> Buffer.Binded sm sb nm objs -> M sm sibfoss -> IO ()
rebindBuffer dvc@(Device.D mdvc) (Buffer.Binded _lns b) m = do
	(_, mm) <- readM m
	ost <- offset @('BufferArg nm objs) dvc m 0
	Buffer.M.bindMemory mdvc b mm ost
