{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.BufferView.Internal (

	-- * CREATE

	create, B(..), CreateInfo(..), FormatOf

	) where

import GHC.TypeLits
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.HeteroParList qualified as HeteroParList
import Gpu.Vulkan.Object qualified as VObj

import Gpu.Vulkan.AllocationCallbacks qualified as AllocationCallbacks
import Gpu.Vulkan.AllocationCallbacks.Type qualified as AllocationCallbacks
import Gpu.Vulkan.Device qualified as Device
import Gpu.Vulkan.Device.Type qualified as Device
import Gpu.Vulkan.Device.Middle qualified as Device.M
import Gpu.Vulkan.TypeEnum qualified as TEnum
import Gpu.Vulkan.Buffer.Type qualified as Buffer
import Gpu.Vulkan.BufferView.Middle qualified as M

newtype B s (nm :: Symbol) t = B M.B deriving Show

create :: (
	WithPoked (TMaybe.M mn),
	Storable t, TEnum.FormatToValue (FormatOf t),
	VObj.OffsetOfList t nm objs,
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd -> CreateInfo mn t nm '(sm, sb, bnm, objs) ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	(forall s . B s nm t -> IO a) -> IO a
create (Device.D dvc) ci
	(AllocationCallbacks.toMiddle -> macc) f = bracket
	(M.create dvc (createInfoToMiddle ci) macc)
	(\b -> M.destroy dvc b macc) (f . B)

data CreateInfo mn t (nm :: Symbol) snmobjs = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: M.CreateFlags,
	createInfoBuffer :: U4 Buffer.Binded snmobjs }

createInfoToMiddle :: forall n t nm sm sb bnm objs . (
	TEnum.FormatToValue (FormatOf t),
	VObj.OffsetOfList t nm objs, Storable t ) =>
	CreateInfo n t nm '(sm, sb, bnm, objs) -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoBuffer = U4 (Buffer.Binded lns b) } = M.CreateInfo {
	M.createInfoNext = mnxt,
	M.createInfoFlags = flgs,
	M.createInfoBuffer = b,
	M.createInfoFormat = TEnum.formatToValue @(FormatOf t),
	M.createInfoOffset = ost, M.createInfoRange = rng }
	where
	(ost, rng) = offsetRange @t @nm lns

type family FormatOf t :: TEnum.Format

offsetRange :: forall t nm objs .
	VObj.OffsetOfList t nm objs =>
	HeteroParList.PL VObj.Length objs -> (Device.M.Size, Device.M.Size)
offsetRange = VObj.offsetOfList @t @nm
