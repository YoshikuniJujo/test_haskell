{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Fence where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Word

import Vulkan.Base
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

waitForFs :: Device.D -> [F] -> Bool -> Word64 -> IO ()
waitForFs (Device.D dvc) (length &&& ((\(F f) -> f) <$>) -> (fc, fs))
	(boolToBool32 -> wa) to = ($ pure) $ runContT do
	pfs <- ContT $ allocaArray fc
	lift do	pokeArray pfs fs
		r <- C.waitForFs dvc (fromIntegral fc) pfs wa to
		throwUnlessSuccess $ Result r

resetFs :: Device.D -> [F] -> IO ()
resetFs (Device.D dvc)
	(length &&& ((\(F f) -> f) <$>) -> (fc, fs)) = ($ pure) $ runContT do
	pfs <- ContT $ allocaArray fc
	lift do	pokeArray pfs fs
		r <- C.resetFs dvc (fromIntegral fc) pfs
		throwUnlessSuccess $ Result r
