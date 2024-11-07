{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Memory.Bind (

	-- * BIND AND REBIND

	Bindable, Rebindable, BindAll(..), RebindAll(..)

	) where

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

class (BindAll ibargs ibargs, Alignments ibargs) => Bindable ibargs
instance (BindAll ibargs ibargs , Alignments ibargs) => Bindable ibargs

class BindAll ibargs mibargs where
	bindAll :: Device.D sd -> HeteroParList.PL (U2 ImageBuffer) ibargs ->
		M sm mibargs -> Device.M.Size ->
		IO (HeteroParList.PL (U2 (ImageBufferBinded sm)) ibargs)

instance BindAll '[] mibargs where bindAll _ _ _ _ = pure HeteroParList.Nil

instance BindAll ibargs mibargs =>
	BindAll ('(si, ('ImageArg nm fmt)) ': ibargs) mibargs where
	bindAll dv@(Device.D mdv) (U2 ii@(Image (Image.I i)) :** ibs) m ost = do
		(_, mm) <- readM m
		(ost', sz) <- adjustOffsetSize dv ii ost
		Image.M.bindMemory mdv i mm ost'
		(U2 (ImageBinded $ Image.Binded i) :**)
			<$> bindAll dv ibs m (ost' + sz)

instance BindAll ibargs mibargs =>
	BindAll ('(sb, ('BufferArg nm objs)) ': ibargs) mibargs where
	bindAll dv@(Device.D mdv)
		(U2 bb@(Buffer (Buffer.B lns b)) :** ibs) m ost  = do
		(_, mm) <- readM m
		(ost', sz) <- adjustOffsetSize dv bb ost
		Buffer.M.bindMemory mdv b mm ost'
		(U2 (BufferBinded $ Buffer.Binded lns b) :**)
			<$> bindAll dv ibs m (ost' + sz)

class (RebindAll ibargs ibargs, Alignments ibargs) => Rebindable ibargs
instance (RebindAll ibargs ibargs, Alignments ibargs) => Rebindable ibargs

class RebindAll ibargs mibargs where
	rebindAll :: Device.D sd ->
		HeteroParList.PL (U2 (ImageBufferBinded sm)) ibargs ->
		M sm mibargs -> Device.M.Size -> IO ()

instance RebindAll '[] mibargs where rebindAll _ _ _ _ = pure ()

instance RebindAll ibargs mibargs =>
	RebindAll ('(si, 'ImageArg nm fmt) ': ibargs) mibargs where
	rebindAll dv@(Device.D mdv)
		(U2 ii@(ImageBinded (Image.Binded i)) :** ibs) m ost = do
		(_, mm) <- readM m
		(ost', sz) <- adjustOffsetSizeBinded dv ii ost
		Image.M.bindMemory mdv i mm ost'
		rebindAll dv ibs m $ ost' + sz

instance RebindAll ibargs mibargs =>
	RebindAll ('(sb, 'BufferArg nm objs) ': ibargs) mibargs where
	rebindAll dv@(Device.D mdv)
		(U2 bb@(BufferBinded (Buffer.Binded _lns b)) :** ibs) m ost = do
		(_, mm) <- readM m
		(ost', sz) <- adjustOffsetSizeBinded dv bb ost
		Buffer.M.bindMemory mdv b mm ost'
		rebindAll dv ibs m $ ost' + sz
