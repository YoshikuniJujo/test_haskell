{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Image (
	INew, BindedNew, createNew, recreateNew, CreateInfoNew(..),
	getMemoryRequirementsNew, getMemoryRequirementsBindedNew,

	I, Binded, create, M.CreateInfo(..), getMemoryRequirements,
	M.SubresourceRange(..), MemoryBarrier(..),

	memoryBarrierToMiddle, MemoryBarrierListToMiddle(..), FirstOfFives
	) where

import GHC.TypeLits
import Foreign.Storable.PeekPoke
import Control.Exception
import Data.Kind
import Data.TypeLevel
import Data.HeteroParList
import Data.Word

import Gpu.Vulkan.Core
import Gpu.Vulkan.Enum
import Gpu.Vulkan.Image.Type
import Gpu.Vulkan.Image.Enum hiding (Type)

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.QueueFamily.EnumManual as QueueFamily
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Memory.Middle as Memory
import qualified Gpu.Vulkan.Image.Middle as M
import qualified Gpu.Vulkan.Sample.Enum as Sample
import qualified Gpu.Vulkan.Image.Enum as I

createNew :: (Pokable n, Pokable n2, Pokable n3, T.FormatToValue fmt) =>
	Device.D sd -> CreateInfoNew n fmt ->
	Maybe (AllocationCallbacks.A n2) -> Maybe (AllocationCallbacks.A n3) ->
	(forall s . INew s nm fmt -> IO a) -> IO a
createNew dvc@(Device.D mdvc) ci macc macd f =
	bracket (createNewM dvc ci macc) (\(INew i) -> M.destroy mdvc i macd) f

recreateNew :: (
	Pokable n, Pokable c, Pokable d, T.FormatToValue fmt ) =>
	Device.D sd -> CreateInfoNew n fmt ->
	Maybe (AllocationCallbacks.A c) ->
	Maybe (AllocationCallbacks.A d) ->
	BindedNew si sm nm fmt -> IO ()
recreateNew dvc ci macc macd i = recreateNewM dvc ci macc macd i

create :: (Pokable n, Pokable n2, Pokable n3) =>
	Device.D sd -> M.CreateInfo n ->
	Maybe (AllocationCallbacks.A n2) -> Maybe (AllocationCallbacks.A n3) ->
	(forall s . I s -> IO a) -> IO a
create (Device.D dvc) ci macc macd f =
	bracket (M.create dvc ci macc) (\i -> M.destroy dvc i macd) (f . I)

getMemoryRequirementsNew :: Device.D sd -> INew s nm fmt -> IO Memory.Requirements
getMemoryRequirementsNew (Device.D dvc) (INew img) =
	M.getMemoryRequirements dvc img

getMemoryRequirementsBindedNew :: Device.D sd -> BindedNew sm si nm fmt -> IO Memory.Requirements
getMemoryRequirementsBindedNew (Device.D dvc) (BindedNew img) =
	M.getMemoryRequirements dvc img

getMemoryRequirements :: Device.D sd -> I s -> IO Memory.Requirements
getMemoryRequirements (Device.D dvc) (I img) = M.getMemoryRequirements dvc img

data MemoryBarrier n si sm nm fmt = MemoryBarrier {
	memoryBarrierNext :: Maybe n,
	memoryBarrierSrcAccessMask :: AccessFlags,
	memoryBarrierDstAccessMask :: AccessFlags,
	memoryBarrierOldLayout :: Layout,
	memoryBarrierNewLayout :: Layout,
	memoryBarrierSrcQueueFamilyIndex :: QueueFamily.Index,
	memoryBarrierDstQueueFamilyIndex :: QueueFamily.Index,
	memoryBarrierImage :: BindedNew si sm nm fmt,
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
	memoryBarrierImage = BindedNew img,
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

type family FirstOfFives (tpl :: [(i, j, k, l, m)]) :: [i] where
	FirstOfFives '[] = '[]
	FirstOfFives ('(x, y, z, w, v) ': xyzwvs) = x ': FirstOfFives xyzwvs

class MemoryBarrierListToMiddle
	(nsismnmfmts :: [(Type, Type, Type, Symbol, T.Format)])  where
	memoryBarrierListToMiddle ::
		HeteroParList (V5 MemoryBarrier) nsismnmfmts ->
		HeteroParList M.MemoryBarrier (FirstOfFives nsismnmfmts)

instance MemoryBarrierListToMiddle '[] where
	memoryBarrierListToMiddle HNil = HNil

instance (Pokable n, MemoryBarrierListToMiddle nsismnmfmts) =>
	MemoryBarrierListToMiddle ('(n, si, sm, nm, fmt) ': nsismnmfmts) where
	memoryBarrierListToMiddle (V5 mb :** mbs) =
		memoryBarrierToMiddle mb :** memoryBarrierListToMiddle mbs

createNewM :: (Pokable n, Pokable n', T.FormatToValue fmt) =>
	Device.D sd -> CreateInfoNew n fmt ->
	Maybe (AllocationCallbacks.A n') -> IO (INew si nm fmt)
createNewM (Device.D mdvc) ci mac =
	INew <$> M.create mdvc (createInfoFromNew ci) mac

recreateNewM :: (
	T.FormatToValue fmt,
	Pokable n, Pokable c, Pokable d ) =>
	Device.D sd -> CreateInfoNew n fmt ->
	Maybe (AllocationCallbacks.A c) ->
	Maybe (AllocationCallbacks.A d) ->
	BindedNew si sm nm fmt -> IO ()
recreateNewM (Device.D mdvc) ci macc macd (BindedNew i) =
	M.recreate mdvc (createInfoFromNew ci) macc macd i

data CreateInfoNew n (fmt :: T.Format) = CreateInfoNew {
	createInfoNextNew :: Maybe n,
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
	deriving Show

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
