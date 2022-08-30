{-# LANGUAGE BlockArguments #-}
{-# LANGUAGe ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Image.Middle where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.IORef
import Data.Word

import Gpu.Vulkan.Core
import Gpu.Vulkan.Enum
import Gpu.Vulkan.Exception
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Image.Enum

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import {-# SOURCE #-} qualified Gpu.Vulkan.Device.Middle as Device
import qualified Gpu.Vulkan.Memory.Middle as Memory
import qualified Gpu.Vulkan.Image.Core as C
import qualified Gpu.Vulkan.Sample.Enum as Sample
import qualified Gpu.Vulkan.QueueFamily.EnumManual as QueueFamily

data SubresourceRange = SubresourceRange {
	subresourceRangeAspectMask :: AspectFlags,
	subresourceRangeBaseMipLevel :: Word32,
	subresourceRangeLevelCount :: Word32,
	subresourceRangeBaseArrayLayer :: Word32,
	subresourceRangeLayerCount :: Word32 }
	deriving Show

subresourceRangeToCore :: SubresourceRange -> C.SubresourceRange
subresourceRangeToCore SubresourceRange {
	subresourceRangeAspectMask = AspectFlagBits am,
	subresourceRangeBaseMipLevel = bmlv,
	subresourceRangeLevelCount = lvc,
	subresourceRangeBaseArrayLayer = baly,
	subresourceRangeLayerCount = lyc } = C.SubresourceRange {
		C.subresourceRangeAspectMask = am,
		C.subresourceRangeBaseMipLevel = bmlv,
		C.subresourceRangeLevelCount = lvc,
		C.subresourceRangeBaseArrayLayer = baly,
		C.subresourceRangeLayerCount = lyc }

newtype I = I (IORef C.I)

data CreateInfoNew n (fmt :: T.Format) = CreateInfoNew {
	createInfoNextNew :: Maybe n,
	createInfoFlagsNew :: CreateFlags,
	createInfoImageTypeNew :: Type,
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
	T.FormatToValue fmt => CreateInfoNew n fmt -> CreateInfo n
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
	createInfoInitialLayoutNew =ilyt } = CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoImageType = itp,
	createInfoFormat = T.formatToValue @fmt,
	createInfoExtent = ext,
	createInfoMipLevels = mls,
	createInfoArrayLayers = als,
	createInfoSamples = smps,
	createInfoTiling = tl,
	createInfoUsage = usg,
	createInfoSharingMode = sm,
	createInfoQueueFamilyIndices = qfis,
	createInfoInitialLayout = ilyt }

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoImageType :: Type,
	createInfoFormat :: Format,
	createInfoExtent :: Extent3d,
	createInfoMipLevels :: Word32,
	createInfoArrayLayers :: Word32,
	createInfoSamples :: Sample.CountFlagBits,
	createInfoTiling :: Tiling,
	createInfoUsage :: UsageFlags,
	createInfoSharingMode :: SharingMode,
	createInfoQueueFamilyIndices :: [Word32],
	createInfoInitialLayout :: Layout }
	deriving Show

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoImageType = Type tp,
	createInfoFormat = Format fmt,
	createInfoExtent = ext,
	createInfoMipLevels = mls,
	createInfoArrayLayers = als,
	createInfoSamples = Sample.CountFlagBits smpls,
	createInfoTiling = Tiling tlng,
	createInfoUsage = UsageFlagBits usg,
	createInfoSharingMode = SharingMode sm,
	createInfoQueueFamilyIndices = length &&& id -> (qfic, qfis),
	createInfoInitialLayout = Layout lyt
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pqfis <- ContT $ allocaArray qfic
	lift $ pokeArray pqfis qfis
	let	C.CreateInfo_ fci = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
			C.createInfoFlags = flgs,
			C.createInfoImageType = tp,
			C.createInfoFormat = fmt,
			C.createInfoExtent = ext,
			C.createInfoMipLevels = mls,
			C.createInfoArrayLayers = als,
			C.createInfoSamples = smpls,
			C.createInfoTiling = tlng,
			C.createInfoUsage = usg,
			C.createInfoSharingMode = sm,
			C.createInfoQueueFamilyIndexCount = fromIntegral qfic,
			C.createInfoPQueueFamilyIndices = pqfis,
			C.createInfoInitialLayout = lyt }
	ContT $ withForeignPtr fci

createNew :: (Pointable n, Pointable n', T.FormatToValue fmt) =>
	Device.D -> CreateInfoNew n fmt -> Maybe (AllocationCallbacks.A n') -> IO I
createNew dvc = create dvc . createInfoFromNew

create :: (Pointable n, Pointable n') =>
	Device.D -> CreateInfo n -> Maybe (AllocationCallbacks.A n') -> IO I
create (Device.D dvc) ci mac = (I <$>) . ($ pure) . runContT $ do
	pci <- createInfoToCore ci
	pac <- AllocationCallbacks.maybeToCore mac
	pimg <- ContT alloca
	lift do	r <- C.create dvc pci pac pimg
		throwUnlessSuccess $ Result r
		newIORef =<< peek pimg

getMemoryRequirements :: Device.D -> I -> IO Memory.Requirements
getMemoryRequirements (Device.D dvc) (I ri) =
	(Memory.requirementsFromCore <$>) . ($ pure) $ runContT do
		pr <- ContT alloca
		lift do	i <- readIORef ri
			C.getMemoryRequirements dvc i pr
			peek pr

bindMemory :: Device.D -> I -> Device.MemoryImage -> IO ()
bindMemory (Device.D dvc) (I rimg) (Device.MemoryImage _ mem) = do
	img <- readIORef rimg
	r <- C.bindMemory dvc img mem 0
	throwUnlessSuccess $ Result r

data MemoryBarrier n = MemoryBarrier {
	memoryBarrierNext :: Maybe n,
	memoryBarrierSrcAccessMask :: AccessFlags,
	memoryBarrierDstAccessMask :: AccessFlags,
	memoryBarrierOldLayout :: Layout,
	memoryBarrierNewLayout :: Layout,
	memoryBarrierSrcQueueFamilyIndex :: QueueFamily.Index,
	memoryBarrierDstQueueFamilyIndex :: QueueFamily.Index,
	memoryBarrierImage :: I,
	memoryBarrierSubresourceRange :: SubresourceRange }

memoryBarrierToCore :: Pointable n =>
	MemoryBarrier n -> ContT r IO C.MemoryBarrier
memoryBarrierToCore MemoryBarrier {
	memoryBarrierNext = mnxt,
	memoryBarrierSrcAccessMask = AccessFlagBits sam,
	memoryBarrierDstAccessMask = AccessFlagBits dam,
	memoryBarrierOldLayout = Layout ol,
	memoryBarrierNewLayout = Layout nl,
	memoryBarrierSrcQueueFamilyIndex = QueueFamily.Index sqfi,
	memoryBarrierDstQueueFamilyIndex = QueueFamily.Index dqfi,
	memoryBarrierImage = I rimg,
	memoryBarrierSubresourceRange = srr } = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	img <- lift $ readIORef rimg
	pure C.MemoryBarrier {
		C.memoryBarrierSType = (),
		C.memoryBarrierPNext = pnxt,
		C.memoryBarrierSrcAccessMask = sam,
		C.memoryBarrierDstAccessMask = dam,
		C.memoryBarrierOldLayout = ol,
		C.memoryBarrierNewLayout = nl,
		C.memoryBarrierSrcQueueFamilyIndex = sqfi,
		C.memoryBarrierDstQueueFamilyIndex = dqfi,
		C.memoryBarrierImage = img,
		C.memoryBarrierSubresourceRange = subresourceRangeToCore srr }

data SubresourceLayers = SubresourceLayers {
	subresourceLayersAspectMask :: AspectFlags,
	subresourceLayersMipLevel :: Word32,
	subresourceLayersBaseArrayLayer :: Word32,
	subresourceLayersLayerCount :: Word32 }
	deriving Show

subresourceLayersToCore :: SubresourceLayers -> C.SubresourceLayers
subresourceLayersToCore SubresourceLayers {
	subresourceLayersAspectMask = AspectFlagBits am,
	subresourceLayersMipLevel = ml,
	subresourceLayersBaseArrayLayer = bal,
	subresourceLayersLayerCount = lc } = C.SubresourceLayers {
		C.subresourceLayersAspectMask = am,
		C.subresourceLayersMipLevel = ml,
		C.subresourceLayersBaseArrayLayer = bal,
		C.subresourceLayersLayerCount = lc }

destroy :: Pointable n =>
	Device.D -> I -> Maybe (AllocationCallbacks.A n) -> IO ()
destroy (Device.D dvc) (I rimg) mac = ($ pure) $ runContT do
	pac <- AllocationCallbacks.maybeToCore mac
	lift do	img <- readIORef rimg
		C.destroy dvc img pac

data Blit = Blit {
	blitSrcSubresource :: SubresourceLayers,
	blitSrcOffsetFrom :: Offset3d,
	blitSrcOffsetTo :: Offset3d,
	blitDstSubresource :: SubresourceLayers,
	blitDstOffsetFrom :: Offset3d,
	blitDstOffsetTo :: Offset3d }
	deriving Show

blitToCore :: Blit -> C.Blit
blitToCore Blit {
	blitSrcSubresource = ssr,
	blitSrcOffsetFrom = sof,
	blitSrcOffsetTo = sot,
	blitDstSubresource = dsr,
	blitDstOffsetFrom = dof,
	blitDstOffsetTo = dot } = C.Blit {
	C.blitSrcSubresource = subresourceLayersToCore ssr,
	C.blitSrcOffsets = [sof, sot],
	C.blitDstSubresource = subresourceLayersToCore dsr,
	C.blitDstOffsets = [dof, dot] }
