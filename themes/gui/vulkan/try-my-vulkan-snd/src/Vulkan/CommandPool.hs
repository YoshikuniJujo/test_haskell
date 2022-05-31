{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.CommandPool where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Pointable
import Control.Monad.Cont
import Data.Word

import Vulkan.Exception
import Vulkan.Exception.Enum
import Vulkan.CommandPool.Enum

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device.Middle as Device
import qualified Vulkan.CommandPool.Core as C

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoQueueFamilyIndex :: Word32 }
	deriving Show

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoQueueFamilyIndex = qfi
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
