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

module Gpu.Vulkan.Image (
	INew, Binded, createNew, recreateNew, CreateInfoNew(..),

	getMemoryRequirementsNew, getMemoryRequirementsBindedNew,
	M.SubresourceRange(..), MemoryBarrier(..),
	MemoryBarrierListToMiddle(..),

	M.SubresourceLayers(..), M.Blit(..)
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

createNew :: (
	WithPoked (TMaybe.M mn), T.FormatToValue fmt,
	AllocationCallbacks.ToMiddle msn2n2 ) =>
	Device.D sd -> CreateInfoNew mn fmt ->
	TPMaybe.M (U2 AllocationCallbacks.A) msn2n2 ->
	(forall s . INew s nm fmt -> IO a) -> IO a
createNew dvc@(Device.D mdvc) ci
	macc@(AllocationCallbacks.toMiddle -> macd) f =
	bracket (createNewM dvc ci macc) (\(INew i) -> M.destroy mdvc i macd) f

recreateNew :: (
	WithPoked (TMaybe.M mn), T.FormatToValue fmt,
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd -> CreateInfoNew mn fmt ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	Binded si sm nm fmt -> IO ()
recreateNew dvc ci macc i = recreateNewM dvc ci macc macc i

getMemoryRequirementsNew :: Device.D sd -> INew s nm fmt -> IO Memory.Requirements
getMemoryRequirementsNew (Device.D dvc) (INew img) =
	M.getMemoryRequirements dvc img

getMemoryRequirementsBindedNew :: Device.D sd -> Binded sm si nm fmt -> IO Memory.Requirements
getMemoryRequirementsBindedNew (Device.D dvc) (Binded img) =
	M.getMemoryRequirements dvc img

data MemoryBarrier mn si sm nm fmt = MemoryBarrier {
	memoryBarrierNext :: TMaybe.M mn,
	memoryBarrierSrcAccessMask :: AccessFlags,
	memoryBarrierDstAccessMask :: AccessFlags,
	memoryBarrierOldLayout :: Layout,
	memoryBarrierNewLayout :: Layout,
	memoryBarrierSrcQueueFamilyIndex :: QueueFamily.Index,
	memoryBarrierDstQueueFamilyIndex :: QueueFamily.Index,
	memoryBarrierImage :: Binded si sm nm fmt,
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
	(nsismnmfmts :: [(Maybe Type, Type, Type, Symbol, T.Format)])  where
	memoryBarrierListToMiddle ::
		HeteroParList.PL (U5 MemoryBarrier) nsismnmfmts ->
		HeteroParList.PL M.MemoryBarrier (TMapIndex.M0_5 nsismnmfmts)

instance MemoryBarrierListToMiddle '[] where
	memoryBarrierListToMiddle HeteroParList.Nil = HeteroParList.Nil

instance (WithPoked (TMaybe.M mn), MemoryBarrierListToMiddle nsismnmfmts) =>
	MemoryBarrierListToMiddle ('(mn, si, sm, nm, fmt) ': nsismnmfmts) where
	memoryBarrierListToMiddle (U5 mb :** mbs) =
		memoryBarrierToMiddle mb :** memoryBarrierListToMiddle mbs

createNewM :: (
	WithPoked (TMaybe.M mn), T.FormatToValue fmt,
	AllocationCallbacks.ToMiddle msn'n' ) =>
	Device.D sd -> CreateInfoNew mn fmt ->
	TPMaybe.M (U2 AllocationCallbacks.A) msn'n' -> IO (INew si nm fmt)
createNewM (Device.D mdvc) ci (AllocationCallbacks.toMiddle -> mac) =
	INew <$> M.create mdvc (createInfoFromNew ci) mac

recreateNewM :: (
	T.FormatToValue fmt, WithPoked (TMaybe.M mn),
	AllocationCallbacks.ToMiddle mscc,
	AllocationCallbacks.ToMiddle msdd ) =>
	Device.D sd -> CreateInfoNew mn fmt ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	TPMaybe.M (U2 AllocationCallbacks.A) msdd ->
	Binded si sm nm fmt -> IO ()
recreateNewM (Device.D mdvc) ci
	(AllocationCallbacks.toMiddle -> macc)
	(AllocationCallbacks.toMiddle -> macd) (Binded i) =
	M.recreate mdvc (createInfoFromNew ci) macc macd i

data CreateInfoNew mn (fmt :: T.Format) = CreateInfoNew {
	createInfoNextNew :: TMaybe.M mn,
	createInfoFlagsNew :: CreateFlags,
	createInfoImageTypeNew :: I.Type,
	createInfoExtentNew :: Extent3d,
	createInfoMipLevelsNew :: Word32,
	createInfoArrayLayersNew :: Word32,
	createInfoSamplesNew :: Sample.CountFlagBits,
	createInfoTilingNew :: Tiling,
	createInfoUsageNew :: UsageFlags,
	createInfoSharingModeNew :: SharingMode,
	createInfoQueueFamilyIndicesNew :: [Word32],
	createInfoInitialLayoutNew :: Layout }

deriving instance Show (TMaybe.M mn) => Show (CreateInfoNew mn fmt)

createInfoFromNew :: forall n fmt .
	T.FormatToValue fmt => CreateInfoNew n fmt -> M.CreateInfo n
createInfoFromNew CreateInfoNew {
	createInfoNextNew = mnxt,
	createInfoFlagsNew = flgs,
	createInfoImageTypeNew = itp,
	createInfoExtentNew = ext,
	createInfoMipLevelsNew = mls,
	createInfoArrayLayersNew = als,
	createInfoSamplesNew = smps,
	createInfoTilingNew = tl,
	createInfoUsageNew = usg,
	createInfoSharingModeNew = sm,
	createInfoQueueFamilyIndicesNew = qfis,
	createInfoInitialLayoutNew =ilyt } = M.CreateInfo {
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
