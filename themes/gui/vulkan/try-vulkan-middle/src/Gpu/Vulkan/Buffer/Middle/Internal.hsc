{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Buffer.Middle.Internal (
	B(..), CreateInfo(..), create, destroy,
	bindMemory, getMemoryRequirements,

	ImageCopy(..), imageCopyToCore,
	MemoryBarrier(..), memoryBarrierToCore',

	C.Copy, pattern C.Copy, C.copySrcOffset, C.copyDstOffset, C.copySize
	) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.PeekPoke (WithPoked, withPoked, withPoked', withPtrS)
import Control.Arrow
import Control.Monad.Cont
import Data.Kind
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.Word
import Data.IORef

import Gpu.Vulkan.Core
import Gpu.Vulkan.Enum
import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Buffer.Enum

import Gpu.Vulkan.AllocationCallbacks.Middle.Internal qualified as
	AllocationCallbacks (A, mToCore)
import qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.Buffer.Core as C
import qualified Gpu.Vulkan.Memory.Middle.Internal as Memory
import qualified Gpu.Vulkan.QueueFamily.EnumManual as QueueFamily
import qualified Gpu.Vulkan.Image.Middle.Internal as Image

#include <vulkan/vulkan.h>

data CreateInfo (mn :: Maybe Type) = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoSize :: Device.Size,
	createInfoUsage :: UsageFlags,
	createInfoSharingMode :: SharingMode,
	createInfoQueueFamilyIndices :: [QueueFamily.Index] }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn)

createInfoToCore ::
	WithPoked (TMaybe.M mn) => CreateInfo mn -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoSize = Device.Size sz,
	createInfoUsage = UsageFlagBits usg,
	createInfoSharingMode = SharingMode sm,
	createInfoQueueFamilyIndices =
		length &&& (QueueFamily.unIndex <$>) -> (qfic, qfis) } f =
	allocaArray qfic \pqfis -> do
	pokeArray pqfis qfis
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') -> do
		withPoked C.CreateInfo {
				C.createInfoSType = (),
				C.createInfoPNext = pnxt',
				C.createInfoFlags = flgs,
				C.createInfoSize = sz,
				C.createInfoUsage = usg,
				C.createInfoSharingMode = sm,
				C.createInfoQueueFamilyIndexCount = fromIntegral qfic,
				C.createInfoPQueueFamilyIndices = pqfis } f

newtype B = B C.B deriving (Show, Eq, Storable)

create :: WithPoked (TMaybe.M mn) =>
	Device.D -> CreateInfo mn -> TPMaybe.M AllocationCallbacks.A mc -> IO B
create (Device.D dvc) ci mac = B <$> alloca \pb -> do
	createInfoToCore ci \pci ->
		AllocationCallbacks.mToCore mac \pac ->
			throwUnlessSuccess . Result =<< C.create dvc pci pac pb
	peek pb

destroy :: Device.D -> B -> TPMaybe.M AllocationCallbacks.A md -> IO ()
destroy (Device.D dvc) (B b) mac =
	AllocationCallbacks.mToCore mac $ C.destroy dvc b

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

data MemoryBarrier mn = MemoryBarrier {
	memoryBarrierNext :: TMaybe.M mn,
	memoryBarrierSrcAccessMask :: AccessFlags,
	memoryBarrierDstAccessMask :: AccessFlags,
	memoryBarrierSrcQueueFamilyIndex :: QueueFamily.Index,
	memoryBarrierDstQueueFamilyIndex :: QueueFamily.Index,
	memoryBarrierBuffer :: B,
	memoryBarrierOffset :: Device.Size,
	memoryBarrierSize :: Device.Size }

deriving instance Show (TMaybe.M mn) => Show (MemoryBarrier mn)

memoryBarrierToCore' :: WithPoked (TMaybe.M mn) =>
	MemoryBarrier mn -> (C.MemoryBarrier -> IO a) -> IO ()
memoryBarrierToCore' MemoryBarrier {
	memoryBarrierNext = mnxt,
	memoryBarrierSrcAccessMask = AccessFlagBits sam,
	memoryBarrierDstAccessMask = AccessFlagBits dam,
	memoryBarrierSrcQueueFamilyIndex = QueueFamily.Index sqfi,
	memoryBarrierDstQueueFamilyIndex = QueueFamily.Index dqfi,
	memoryBarrierBuffer = B b,
	memoryBarrierOffset = Device.Size ofst,
	memoryBarrierSize = Device.Size sz } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
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
