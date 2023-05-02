{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.BufferView where

import GHC.TypeLits
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))
import Gpu.Vulkan.Object qualified as VObj

import Gpu.Vulkan.AllocationCallbacks qualified as AllocationCallbacks
import Gpu.Vulkan.AllocationCallbacks.Type qualified as AllocationCallbacks
import Gpu.Vulkan.Device qualified as Device
import Gpu.Vulkan.Device.Type qualified as Device
import Gpu.Vulkan.Device.Middle qualified as Device.M
import Gpu.Vulkan.TypeEnum qualified as TEnum
import Gpu.Vulkan.Buffer qualified as Buffer
import Gpu.Vulkan.BufferView.Middle qualified as M

newtype B s (nm :: Symbol) t = B M.B deriving Show

create :: (
	WithPoked (TMaybe.M mn), AllocationCallbacks.ToMiddle' mscc,
	TEnum.FormatToValue (FormatOf t), OffsetRange t nm objs ) =>
	Device.D sd -> CreateInfo mn t nm '(sm, sb, bnm, objs) ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	(forall s . B s nm t -> IO a) -> IO a
create (Device.D dvc) ci
	(AllocationCallbacks.toMiddle' -> macc) f = bracket
	(M.create dvc (createInfoToMiddle ci) macc)
	(\b -> M.destroy dvc b macc) (f . B)

data CreateInfo mn t (nm :: Symbol) snmobjs = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: M.CreateFlags,
	createInfoBuffer :: U4 Buffer.Binded snmobjs }

createInfoToMiddle :: forall n t nm sm sb bnm objs . (
	TEnum.FormatToValue (FormatOf t),
	OffsetRange t nm objs ) =>
	CreateInfo n t nm '(sm, sb, bnm, objs) -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoBuffer = U4 (Buffer.Binded lns b) } = M.CreateInfo {
	M.createInfoNext = mnxt,
	M.createInfoFlags = flgs,
	M.createInfoBuffer = b,
	M.createInfoFormat = TEnum.formatToValue @(FormatOf t),
	M.createInfoOffset = offset @t @nm 0 lns,
	M.createInfoRange = range @t @nm lns }

type family FormatOf t :: TEnum.Format

class OffsetRange t (nm :: Symbol) (objs :: [VObj.Object]) where
	offset :: Device.M.Size ->
		HeteroParList.PL VObj.ObjectLength objs -> Device.M.Size
	range :: HeteroParList.PL VObj.ObjectLength objs -> Device.M.Size

instance (KnownNat algn, Storable t) =>
	OffsetRange t nm (VObj.List algn t nm ': _objs) where
	offset sz _ = sz
	range (ln :** _) = fromIntegral $ VObj.objectSize' ln

instance {-# OVERLAPPABLE #-} (VObj.SizeAlignment obj, OffsetRange t nm objs) =>
	OffsetRange t nm (obj ': objs) where
	offset sz (ln :** lns) = offset @t @nm (sz + objectSize ln) lns
	range (_ :** lns) = range @t @nm lns

objectSize :: forall obj . VObj.SizeAlignment obj =>
	VObj.ObjectLength obj -> Device.M.Size
objectSize ln = ((fromIntegral (VObj.objectSize' ln) - 1) `div` algn + 1) * algn
	where algn = fromIntegral (VObj.objectAlignment @obj)
