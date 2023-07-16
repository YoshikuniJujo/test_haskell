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

	-- * OFFSET SIZE

	offsetSize, OffsetSize,

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

import Gpu.Vulkan.Memory.Type
import Gpu.Vulkan.Memory.Types

offsetSize :: forall nm obj ibargs sd sm . OffsetSize nm obj ibargs =>
	Device.D sd -> M sm ibargs -> Device.M.Size ->
	IO (Device.M.Size, Device.M.Size)
offsetSize dvc m ost = do
	(ibs, _m) <- readM m
	offsetSize' @nm @obj @ibargs dvc ibs ost

class ObjectLength nm obj ibargs => OffsetSize (nm :: Symbol) (obj :: VObj.Object) ibargs where
	offsetSize' :: Device.D sd -> HeteroParList.PL (U2 ImageBuffer) ibargs ->
		Device.M.Size -> IO (Device.M.Size, Device.M.Size)

instance (VObj.Offset obj objs, VObj.ObjectLengthOf obj objs) =>
	OffsetSize nm obj ('(sib, 'BufferArg nm objs) ': ibargs) where
	offsetSize' dvc ((U2 ib@(Buffer (Buffer.B lns _))) :** _ibs) ost = do
		(ost', _sz) <- adjustOffsetSize dvc ib ost
		pure $ VObj.offsetFromSizeAlignmentList' @obj (fromIntegral ost')
			$ VObj.sizeAlignmentList lns

instance {-# OVERLAPPABLE #-}
	OffsetSize nm obj ibargs =>
	OffsetSize nm obj ('(sib, ib) ': ibargs) where
	offsetSize' dvc (U2 ib :** ibs) ost = do
		(ost', sz) <- adjustOffsetSize dvc ib ost
		offsetSize' @nm @obj dvc ibs $ ost' + sz
