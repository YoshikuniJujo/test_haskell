{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Image.Internal (

	-- * CREATE

	create, recreate, recreate', I(..), Binded(..), CreateInfo(..),

	-- ** Manage Destruction

	manage, create', M.Manager,

	-- * GET MEMORY REQUIREMENTS

	getMemoryRequirements, getMemoryRequirementsBinded,

	-- * MEMORY BARRIER

	MemoryBarrier(..), M.SubresourceRange(..),
	MemoryBarrierListToMiddle(..),

	-- * BLIT

	M.Blit(..), M.SubresourceLayers(..)

	) where

import GHC.TypeLits
import Foreign.Storable.PeekPoke
import Control.Exception
import Data.Kind
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))
import Data.Word

import Gpu.Vulkan.Middle
import Gpu.Vulkan.Enum
import Gpu.Vulkan.Image.Type
import Gpu.Vulkan.Image.Enum hiding (Type)

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.QueueFamily.Middle as QueueFamily
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Memory.Middle as Memory
import qualified Gpu.Vulkan.Image.Middle as M
import qualified Gpu.Vulkan.Sample.Enum as Sample
import qualified Gpu.Vulkan.Image.Enum as I

create :: (
	WithPoked (TMaybe.M mn), T.FormatToValue fmt,
	AllocationCallbacks.ToMiddle mac ) =>
	Device.D sd -> CreateInfo mn fmt ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	(forall s . I s nm fmt -> IO a) -> IO a
create (Device.D mdvc) ci (AllocationCallbacks.toMiddle -> macd) f = bracket
	(M.create mdvc (createInfoToMiddle ci) macd)
	(\i -> M.destroy mdvc i macd)
	(f .I)

manage :: AllocationCallbacks.ToMiddle mac =>
	Device.D sd -> TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	(forall s . M.Manager s -> IO a) -> IO a
manage (Device.D mdvc) (AllocationCallbacks.toMiddle -> mac) =
	M.manage mdvc mac

create' :: (
	WithPoked (TMaybe.M mn), T.FormatToValue fmt,
	AllocationCallbacks.ToMiddle mac ) =>
	Device.D sd -> M.Manager sm -> CreateInfo mn fmt ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac -> IO (I sm nm fmt)
create' (Device.D mdvc) mngr ci (AllocationCallbacks.toMiddle -> macd) =
	I <$> M.create' mdvc mngr (createInfoToMiddle ci) macd

recreate :: (
	WithPoked (TMaybe.M mn), AllocationCallbacks.ToMiddle mac,
	T.FormatToValue fmt ) =>
	Device.D sd -> CreateInfo mn fmt ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	Binded sm si nm fmt -> IO ()
recreate (Device.D mdvc) ci
	(AllocationCallbacks.toMiddle -> macc) (Binded i) =
	M.recreate mdvc (createInfoToMiddle ci) macc macc i

recreate' :: (
	WithPoked (TMaybe.M mn), AllocationCallbacks.ToMiddle mac,
	T.FormatToValue fmt ) =>
	Device.D sd -> CreateInfo mn fmt ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	Binded sm si nm fmt -> IO a -> IO ()
recreate' (Device.D mdvc) ci
	(AllocationCallbacks.toMiddle -> macc) (Binded i) =
	M.recreate' mdvc (createInfoToMiddle ci) macc macc i

getMemoryRequirements :: Device.D sd -> I si nm fmt -> IO Memory.Requirements
getMemoryRequirements (Device.D dvc) (I img) =
	M.getMemoryRequirements dvc img

getMemoryRequirementsBinded :: Device.D sd -> Binded sm si nm fmt -> IO Memory.Requirements
getMemoryRequirementsBinded (Device.D dvc) (Binded img) =
	M.getMemoryRequirements dvc img

data MemoryBarrier mn sm si nm fmt = MemoryBarrier {
	memoryBarrierNext :: TMaybe.M mn,
	memoryBarrierSrcAccessMask :: AccessFlags,
	memoryBarrierDstAccessMask :: AccessFlags,
	memoryBarrierOldLayout :: Layout,
	memoryBarrierNewLayout :: Layout,
	memoryBarrierSrcQueueFamilyIndex :: QueueFamily.Index,
	memoryBarrierDstQueueFamilyIndex :: QueueFamily.Index,
	memoryBarrierImage :: Binded sm si nm fmt,
	memoryBarrierSubresourceRange :: M.SubresourceRange }

memoryBarrierToMiddle :: MemoryBarrier n si sm nm fmt -> M.MemoryBarrier n
memoryBarrierToMiddle MemoryBarrier {
	memoryBarrierNext = mnxt,
	memoryBarrierSrcAccessMask = sam,
	memoryBarrierDstAccessMask = dam,
	memoryBarrierOldLayout = olyt,
	memoryBarrierNewLayout = nlyt,
	memoryBarrierSrcQueueFamilyIndex = sqfi,
	memoryBarrierDstQueueFamilyIndex = dqfi,
	memoryBarrierImage = Binded img,
	memoryBarrierSubresourceRange = srr } = M.MemoryBarrier {
	M.memoryBarrierNext = mnxt,
	M.memoryBarrierSrcAccessMask = sam,
	M.memoryBarrierDstAccessMask = dam,
	M.memoryBarrierOldLayout = olyt,
	M.memoryBarrierNewLayout = nlyt,
	M.memoryBarrierSrcQueueFamilyIndex = sqfi,
	M.memoryBarrierDstQueueFamilyIndex = dqfi,
	M.memoryBarrierImage = img,
	M.memoryBarrierSubresourceRange = srr }

class MemoryBarrierListToMiddle
	(mbargs :: [(Maybe Type, Type, Type, Symbol, T.Format)])  where
	memoryBarrierListToMiddle ::
		HeteroParList.PL (U5 MemoryBarrier) mbargs ->
		HeteroParList.PL M.MemoryBarrier (TMapIndex.M0_5 mbargs)

instance MemoryBarrierListToMiddle '[] where
	memoryBarrierListToMiddle HeteroParList.Nil = HeteroParList.Nil

instance  MemoryBarrierListToMiddle mbargs =>
	MemoryBarrierListToMiddle ('(mn, si, sm, nm, fmt) ': mbargs) where
	memoryBarrierListToMiddle (U5 mb :** mbs) =
		memoryBarrierToMiddle mb :** memoryBarrierListToMiddle mbs

data CreateInfo mn (fmt :: T.Format) = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoImageType :: I.Type,
	createInfoExtent :: Extent3d,
	createInfoMipLevels :: Word32,
	createInfoArrayLayers :: Word32,
	createInfoSamples :: Sample.CountFlagBits,
	createInfoTiling :: Tiling,
	createInfoUsage :: UsageFlags,
	createInfoSharingMode :: SharingMode,
	createInfoQueueFamilyIndices :: [Word32],
	createInfoInitialLayout :: Layout }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn fmt)

createInfoToMiddle :: forall n fmt .
	T.FormatToValue fmt => CreateInfo n fmt -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoImageType = itp,
	createInfoExtent = ext,
	createInfoMipLevels = mls,
	createInfoArrayLayers = als,
	createInfoSamples = smps,
	createInfoTiling = tl,
	createInfoUsage = usg,
	createInfoSharingMode = sm,
	createInfoQueueFamilyIndices = qfis,
	createInfoInitialLayout =ilyt } = M.CreateInfo {
	M.createInfoNext = mnxt,
	M.createInfoFlags = flgs,
	M.createInfoImageType = itp,
	M.createInfoFormat = T.formatToValue @fmt,
	M.createInfoExtent = ext,
	M.createInfoMipLevels = mls,
	M.createInfoArrayLayers = als,
	M.createInfoSamples = smps,
	M.createInfoTiling = tl,
	M.createInfoUsage = usg,
	M.createInfoSharingMode = sm,
	M.createInfoQueueFamilyIndices = qfis,
	M.createInfoInitialLayout = ilyt }
