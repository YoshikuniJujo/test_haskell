{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Fence where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Pointable
import Control.Monad.Cont

import Vulkan.Exception
import Vulkan.Exception.Enum
import Vulkan.Fence.Enum

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device as Device
import qualified Vulkan.Fence.Core as C

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags }
	deriving Show

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	let	C.CreateInfo_ fCreateInfo = C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt,
		C.createInfoFlags = flgs }
	ContT $ withForeignPtr fCreateInfo

newtype F = F C.F deriving Show

create :: (Pointable n, Pointable n') =>
	Device.D -> CreateInfo n -> Maybe (AllocationCallbacks.A n') -> IO F
create (Device.D dvc) ci mac = ($ pure) . runContT $ F <$> do
	pci <- createInfoToCore ci
	pac <- AllocationCallbacks.maybeToCore mac
	pf <- ContT alloca
	lift do	r <- C.create dvc pci pac pf
		throwUnlessSuccess $ Result r
		peek pf

destroy :: Pointable n =>
	Device.D -> F -> Maybe (AllocationCallbacks.A n) -> IO ()
destroy (Device.D dvc) (F f) mac = ($ pure) $ runContT do
	pac <- AllocationCallbacks.maybeToCore mac
	lift $ C.destroy dvc f pac
