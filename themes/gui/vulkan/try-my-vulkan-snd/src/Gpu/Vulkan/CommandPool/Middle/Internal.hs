{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandPool.Middle.Internal (
	C(..), CreateInfo(..), create, destroy ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Pointable
import Control.Monad.Cont

import Gpu.Vulkan.Exception
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.CommandPool.Enum

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import {-# SOURCE #-} qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.QueueFamily.EnumManual as QueueFamily
import qualified Gpu.Vulkan.CommandPool.Core as C

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoQueueFamilyIndex :: QueueFamily.Index }
	deriving Show

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoQueueFamilyIndex = QueueFamily.Index qfi
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	let C.CreateInfo_ fCreateInfo = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
			C.createInfoFlags = flgs,
			C.createInfoQueueFamilyIndex = qfi }
	ContT $ withForeignPtr fCreateInfo

newtype C = C C.C deriving Show

create :: (Pointable n, Pointable n') =>
	Device.D -> CreateInfo n -> Maybe (AllocationCallbacks.A n') -> IO C
create (Device.D dvc) ci mac = ($ pure) . runContT $ C <$> do
	pci <- createInfoToCore ci
	pac <- AllocationCallbacks.maybeToCore mac
	pc <- ContT alloca
	lift do r <- C.create dvc pci pac pc
		throwUnlessSuccess $ Result r
		peek pc

destroy :: Pointable n =>
	Device.D -> C -> Maybe (AllocationCallbacks.A n) -> IO ()
destroy (Device.D dvc) (C c) mac = ($ pure) $ runContT do
	pac <- AllocationCallbacks.maybeToCore mac
	lift $ C.destroy dvc c pac
