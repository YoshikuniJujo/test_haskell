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

	BindAll(..), RebindAll(..)

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

class BindAll ibargs mibargs where
	bindAll :: Device.D sd -> HeteroParList.PL (U2 ImageBuffer) ibargs ->
		M sm mibargs -> Device.M.Size ->
		IO (HeteroParList.PL (U2 (ImageBufferBinded sm)) ibargs)

instance BindAll '[] mibargs where bindAll _ _ _ _ = pure HeteroParList.Nil

instance BindAll ibargs mibargs =>
	BindAll ('(si, ('ImageArg nm fmt)) ': ibargs) mibargs where
	bindAll dvc@(Device.D mdvc) (U2 ii@(Image (Image.I i)) :** ibs) m ost0 = do
		(_, mm) <- readM m
		(ost, sz) <- adjustOffsetSize dvc ii ost0
		Image.M.bindMemory mdvc i mm ost
		(:**)	<$> (pure . U2 . ImageBinded $ Image.Binded i)
			<*> bindAll dvc ibs m (ost + sz)

instance BindAll ibargs mibargs =>
	BindAll ('(sb, ('BufferArg nm objs)) ': ibargs) mibargs where
	bindAll dvc@(Device.D mdvc) (U2 bb@(Buffer (Buffer.B lns b)) :** ibs) m ost0  = do
		(_, mm) <- readM m
		(ost, sz) <- adjustOffsetSize dvc bb ost0
		Buffer.M.bindMemory mdvc b mm ost
		(:**)	<$> (pure $ U2 . BufferBinded $ Buffer.Binded lns b)
			<*> bindAll dvc ibs m (ost + sz)

class RebindAll ibargs mibargs where
	rebindAll :: Device.D sd ->
		HeteroParList.PL (U2 (ImageBufferBinded sm)) ibargs ->
		M sm mibargs -> Device.M.Size -> IO ()

instance RebindAll '[] mibargs where rebindAll _ _ _ _ = pure ()

instance RebindAll ibargs mibargs =>
	RebindAll ('(si, 'ImageArg nm fmt) ': ibargs) mibargs where
	rebindAll dvc@(Device.D mdvc) (U2 (ImageBinded (Image.Binded i)) :** ibs) m ost0 = do
		(_, mm) <- readM m
		(ost', sz) <- adjustOffsetSize dvc (Image $ Image.I i) ost0
		Image.M.bindMemory mdvc i mm ost'
		rebindAll dvc ibs m $ ost' + sz

instance RebindAll ibargs mibargs =>
	RebindAll ('(sb, 'BufferArg nm objs) ': ibargs) mibargs where
	rebindAll dvc@(Device.D mdvc) (U2 (BufferBinded (Buffer.Binded lns b)) :** ibs) m ost0 = do
		(_, mm) <- readM m
		(ost', sz) <- adjustOffsetSize dvc (Buffer $ Buffer.B lns b) ost0
		Buffer.M.bindMemory mdvc b mm ost'
		rebindAll dvc ibs m $ ost' + sz
