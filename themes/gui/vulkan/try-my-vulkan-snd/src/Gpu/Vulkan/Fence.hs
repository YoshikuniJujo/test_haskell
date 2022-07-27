{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Fence where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Default
import Data.Bits
import Data.Word

import Gpu.Vulkan.Base
import Gpu.Vulkan.Exception
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Fence.Enum

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle as Device
import qualified Gpu.Vulkan.Fence.Core as C

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags }
	deriving Show

instance Default (CreateInfo n) where
	def = CreateInfo {
		createInfoNext = Nothing, createInfoFlags = zeroBits }

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

fToCore :: F -> C.F
fToCore (F f) = f

maybeFToCore :: Maybe F -> C.F
maybeFToCore Nothing = NullHandle
maybeFToCore (Just f) = fToCore f

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
