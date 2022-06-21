{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.BufferView.Middle where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Pointable
import Control.Monad.Cont

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Exception
import Gpu.Vulkan.Exception.Enum

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle as Device
import qualified Gpu.Vulkan.BufferView.Core as C
import qualified Gpu.Vulkan.Buffer.Middle as Buffer

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: C.CreateFlags,
	createInfoBuffer :: Buffer.B,
	createInfoFormat :: Format,
	createInfoOffset :: Device.Size,
	createInfoRange :: Device.Size }
	deriving Show

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = C.CreateFlags flgs,
	createInfoBuffer = Buffer.B bf,
	createInfoFormat = Format fmt,
	createInfoOffset = Device.Size os,
	createInfoRange = Device.Size rng
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	let	C.CreateInfo_ fci = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
			C.createInfoFlags = flgs,
			C.createInfoBuffer = bf,
			C.createInfoFormat = fmt,
			C.createInfoOffset = os,
			C.createInfoRange = rng }
	ContT $ withForeignPtr fci

newtype B = B C.B deriving Show

create :: (Pointable n, Pointable c) =>
	Device.D -> CreateInfo n -> Maybe (AllocationCallbacks.A c) -> IO B
create (Device.D dvc) ci mac = (B <$>) . ($ pure) $ runContT do
	pac <- AllocationCallbacks.maybeToCore mac
	pci <- createInfoToCore ci
	pb <- ContT alloca
	lift do	r <- C.create dvc pci pac pb
		throwUnlessSuccess $ Result r
		peek pb
