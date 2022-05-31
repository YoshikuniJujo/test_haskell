{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Buffer.Middle where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Word

import Vulkan.Core
import Vulkan.Enum
import Vulkan.Exception
import Vulkan.Exception.Enum
import Vulkan.Buffer.Enum

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device.Middle as Device
import qualified Vulkan.Buffer.Core as C
import qualified Vulkan.Memory.Middle as Memory
import qualified Vulkan.QueueFamily.EnumManual as QueueFamily
import qualified Vulkan.Image as Image

#include <vulkan/vulkan.h>

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoSize :: Device.Size,
	createInfoUsage :: UsageFlags,
	createInfoSharingMode :: SharingMode,
	createInfoQueueFamilyIndices :: [#{type uint32_t}] }
	deriving Show

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoSize = Device.Size sz,
	createInfoUsage = UsageFlagBits usg,
	createInfoSharingMode = SharingMode sm,
	createInfoQueueFamilyIndices = length &&& id -> (qfic, qfis)
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pqfis <- ContT $ allocaArray qfic
	lift $ pokeArray pqfis qfis
	let	C.CreateInfo_ fci = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
			C.createInfoFlags = flgs,
			C.createInfoSize = sz,
			C.createInfoUsage = usg,
			C.createInfoSharingMode = sm,
			C.createInfoQueueFamilyIndexCount = fromIntegral qfic,
			C.createInfoPQueueFamilyIndices = pqfis }
	ContT $ withForeignPtr fci


newtype B = B C.B deriving (Show, Storable)

create :: (Pointable n, Pointable n') =>
	Device.D -> CreateInfo n -> Maybe (AllocationCallbacks.A n') -> IO B
create (Device.D dvc) ci mac = (B <$>) . ($ pure) $ runContT do
	pci <- createInfoToCore ci
	pac <- AllocationCallbacks.maybeToCore mac
	pb <- ContT alloca
	lift do	r <- C.create dvc pci pac pb
		throwUnlessSuccess $ Result r
		peek pb

destroy :: Pointable n =>
	Device.D -> B -> Maybe (AllocationCallbacks.A n) -> IO ()
destroy (Device.D dvc) (B b) mac = ($ pure) $ runContT do
	pac <- AllocationCallbacks.maybeToCore mac
	lift $ C.destroy dvc b pac

getMemoryRequirements :: Device.D -> B -> IO Memory.Requirements
getMemoryRequirements (Device.D dvc) (B b) =
	(Memory.requirementsFromCore <$>) . ($ pure) $ runContT do
		pr <- ContT alloca
		lift do	C.getMemoryRequirements dvc b pr
			peek pr

bindMemory :: Device.D -> B -> Device.Memory -> Device.Size -> IO ()
bindMemory (Device.D dvc) (B b) (Device.Memory mem) (Device.Size sz) =
	throwUnlessSuccess . Result =<< C.bindMemory dvc b mem sz

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

memoryBarrierToCore :: Pointable n =>
	MemoryBarrier n -> ContT r IO C.MemoryBarrier
memoryBarrierToCore MemoryBarrier {
	memoryBarrierNext = mnxt,
	memoryBarrierSrcAccessMask = AccessFlagBits sam,
	memoryBarrierDstAccessMask = AccessFlagBits dam,
	memoryBarrierSrcQueueFamilyIndex = QueueFamily.Index sqfi,
	memoryBarrierDstQueueFamilyIndex = QueueFamily.Index dqfi,
	memoryBarrierBuffer = B b,
	memoryBarrierOffset = Device.Size ofst,
	memoryBarrierSize = Device.Size sz } = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pure C.MemoryBarrier {
		C.memoryBarrierSType = (),
		C.memoryBarrierPNext = pnxt,
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
