{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Buffer.Middle.Internal (
	B(..), CreateInfo(..), create, destroy,
	bindMemory, getMemoryRequirements,

	ImageCopy(..), imageCopyToCore,
	MemoryBarrier(..), memoryBarrierToCore',

	C.Copy, pattern C.Copy, C.copySrcOffset, C.copyDstOffset, C.copySize
	) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.PeekPoke (WithPoked, withPokedMaybe', withPtrS)
import Control.Arrow
import Control.Monad.Cont
import Data.IORef
import Data.Word

import Gpu.Vulkan.Core
import Gpu.Vulkan.Enum
import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Buffer.Enum

import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks (A, maybeToCore)
import qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.Buffer.Core as C
import qualified Gpu.Vulkan.Memory.Middle.Internal as Memory
import qualified Gpu.Vulkan.QueueFamily.EnumManual as QueueFamily
import qualified Gpu.Vulkan.Image.Middle.Internal as Image

#include <vulkan/vulkan.h>

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoSize :: Device.Size,
	createInfoUsage :: UsageFlags,
	createInfoSharingMode :: SharingMode,
	createInfoQueueFamilyIndices :: [QueueFamily.Index] }
	deriving Show

createInfoToCore' ::
	WithPoked n => CreateInfo n -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore' CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoSize = Device.Size sz,
	createInfoUsage = UsageFlagBits usg,
	createInfoSharingMode = SharingMode sm,
	createInfoQueueFamilyIndices =
		length &&& (QueueFamily.unIndex <$>) -> (qfic, qfis) } f =
	allocaArray qfic \pqfis -> do
	pokeArray pqfis qfis
	withPokedMaybe' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') -> do
		let	C.CreateInfo_ fci = C.CreateInfo {
				C.createInfoSType = (),
				C.createInfoPNext = pnxt',
				C.createInfoFlags = flgs,
				C.createInfoSize = sz,
				C.createInfoUsage = usg,
				C.createInfoSharingMode = sm,
				C.createInfoQueueFamilyIndexCount = fromIntegral qfic,
				C.createInfoPQueueFamilyIndices = pqfis }
		withForeignPtr fci $ (() <$) . f

newtype B = B C.B deriving (Show, Eq, Storable)

create :: (WithPoked n, WithPoked c) =>
	Device.D -> CreateInfo n -> Maybe (AllocationCallbacks.A c) -> IO B
create (Device.D dvc) ci mac = B <$> alloca \pb -> do
	createInfoToCore' ci \pci ->
		AllocationCallbacks.maybeToCore mac \pac ->
			throwUnlessSuccess . Result =<< C.create dvc pci pac pb
	peek pb

destroy :: WithPoked d =>
	Device.D -> B -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (Device.D dvc) (B b) mac =
	AllocationCallbacks.maybeToCore mac $ C.destroy dvc b

getMemoryRequirements :: Device.D -> B -> IO Memory.Requirements
getMemoryRequirements (Device.D dvc) (B b) =
	(Memory.requirementsFromCore <$>) . ($ pure) $ runContT do
		pr <- ContT alloca
		lift do	C.getMemoryRequirements dvc b pr
			peek pr

bindMemory :: Device.D -> B -> Memory.M -> Device.Size -> IO ()
bindMemory (Device.D dvc) (B b) (Memory.M mem) (Device.Size sz) = do
	m <- readIORef mem
	throwUnlessSuccess . Result =<< C.bindMemory dvc b m sz

data MemoryBarrier n = MemoryBarrier {
	memoryBarrierNext :: Maybe n,
	memoryBarrierSrcAccessMask :: AccessFlags,
	memoryBarrierDstAccessMask :: AccessFlags,
	memoryBarrierSrcQueueFamilyIndex :: QueueFamily.Index,
	memoryBarrierDstQueueFamilyIndex :: QueueFamily.Index,
	memoryBarrierBuffer :: B,
	memoryBarrierOffset :: Device.Size,
	memoryBarrierSize :: Device.Size }
	deriving Show

memoryBarrierToCore' :: WithPoked n =>
	MemoryBarrier n -> (C.MemoryBarrier -> IO a) -> IO ()
memoryBarrierToCore' MemoryBarrier {
	memoryBarrierNext = mnxt,
	memoryBarrierSrcAccessMask = AccessFlagBits sam,
	memoryBarrierDstAccessMask = AccessFlagBits dam,
	memoryBarrierSrcQueueFamilyIndex = QueueFamily.Index sqfi,
	memoryBarrierDstQueueFamilyIndex = QueueFamily.Index dqfi,
	memoryBarrierBuffer = B b,
	memoryBarrierOffset = Device.Size ofst,
	memoryBarrierSize = Device.Size sz } f =
	withPokedMaybe' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	f C.MemoryBarrier {
		C.memoryBarrierSType = (),
		C.memoryBarrierPNext = pnxt',
		C.memoryBarrierSrcAccessMask = sam,
		C.memoryBarrierDstAccessMask = dam,
		C.memoryBarrierSrcQueueFamilyIndex = sqfi,
		C.memoryBarrierDstQueueFamilyIndex = dqfi,
		C.memoryBarrierBuffer = b,
		C.memoryBarrierOffset = ofst,
		C.memoryBarrierSize = sz }

data ImageCopy = ImageCopy {
	imageCopyBufferOffset :: Device.Size,
	imageCopyBufferRowLength :: Word32,
	imageCopyBufferImageHeight :: Word32,
	imageCopyImageSubresource :: Image.SubresourceLayers,
	imageCopyImageOffset :: Offset3d,
	imageCopyImageExtent :: Extent3d }
	deriving Show

imageCopyToCore :: ImageCopy -> C.ImageCopy
imageCopyToCore ImageCopy {
	imageCopyBufferOffset = Device.Size bo,
	imageCopyBufferRowLength = brl,
	imageCopyBufferImageHeight = bih,
	imageCopyImageSubresource = isr,
	imageCopyImageOffset = io,
	imageCopyImageExtent = ie } = C.ImageCopy {
	C.imageCopyBufferOffset = bo,
	C.imageCopyBufferRowLength = brl,
	C.imageCopyBufferImageHeight = bih,
	C.imageCopyImageSubresource = Image.subresourceLayersToCore isr,
	C.imageCopyImageOffset = io,
	C.imageCopyImageExtent = ie }