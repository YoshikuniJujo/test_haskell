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
import Gpu.Vulkan.Object qualified as VObj
import Data.TypeLevel.Tuple.Uncurry
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Device.Middle as Device.M
import qualified Gpu.Vulkan.Buffer.Type as Buffer

import Gpu.Vulkan.Memory.Type
import Gpu.Vulkan.Memory.ImageBuffer

offsetSize :: forall nm obj ibargs i sd sm . OffsetSize nm obj ibargs i =>
	Device.D sd -> M sm ibargs -> Device.M.Size ->
	IO (Device.M.Size, Device.M.Size)
offsetSize dvc m ost =
	readM m >>= \(ibs, _mm) -> offsetSize' @nm @obj @ibargs @i dvc ibs ost

class ObjectLength nm obj ibargs =>
	OffsetSize (nm :: Symbol) (obj :: VObj.O) ibargs (i :: Nat) where
	offsetSize' ::
		Device.D sd -> HeteroParList.PL (U2 ImageBuffer) ibargs ->
		Device.M.Size -> IO (Device.M.Size, Device.M.Size)

instance (VObj.OffsetRange obj objs i, VObj.LengthOf obj objs) =>
	OffsetSize nm obj ('(sib, 'BufferArg nm objs) ': ibargs) i where
	offsetSize' dvc ((U2 ib@(Buffer (Buffer.B lns _))) :** _ibs) ost =
		(<$> adjustOffsetSize dvc ib ost) \(ost', _sz) ->
		VObj.offsetSize @obj @_ @i ost' lns

instance {-# OVERLAPPABLE #-}
	OffsetSize nm obj ibargs i =>
	OffsetSize nm obj ('(sib, ib) ': ibargs) i where
	offsetSize' dvc (U2 ib :** ibs) ost =
		adjustOffsetSize dvc ib ost >>=
		offsetSize' @nm @obj @_ @i dvc ibs . uncurry (+)
