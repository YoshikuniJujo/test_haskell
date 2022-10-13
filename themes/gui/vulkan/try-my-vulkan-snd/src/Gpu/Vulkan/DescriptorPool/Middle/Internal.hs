{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorPool.Middle.Internal (
	D(..), CreateInfo(..), Size(..), create, destroy ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Word

import Gpu.Vulkan.Exception
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Descriptor.Enum
import Gpu.Vulkan.DescriptorPool.Enum

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle as Device
import qualified Gpu.Vulkan.DescriptorPool.Core as C

data Size = Size { sizeType :: Type, sizeDescriptorCount :: Word32 }
	deriving Show

sizeToCore :: Size -> C.Size
sizeToCore Size { sizeType = Type tp, sizeDescriptorCount = dc } =
	C.Size { C.sizeType = tp, C.sizeDescriptorCount = dc }

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoMaxSets :: Word32,
	createInfoPoolSizes :: [Size] }
	deriving Show

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoMaxSets = ms,
	createInfoPoolSizes = (length &&& (sizeToCore <$>) -> (psc, pss))
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	ppss <- ContT $ allocaArray psc
	lift $ pokeArray ppss pss
	let	C.CreateInfo_ fci = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
			C.createInfoFlags = flgs,
			C.createInfoMaxSets = ms,
			C.createInfoPoolSizeCount = fromIntegral psc,
			C.createInfoPPoolSizes = ppss }
	ContT $ withForeignPtr fci

newtype D = D C.P deriving Show

create :: (Pointable n, Pointable c) =>
	Device.D -> CreateInfo n -> Maybe (AllocationCallbacks.A c) -> IO D
create (Device.D dvc) ci mac = (D <$>) . ($ pure) $ runContT do
	pci <- createInfoToCore ci
	pac <- AllocationCallbacks.maybeToCore mac
	pp <- ContT alloca
	lift do	r <- C.create dvc pci pac pp
		throwUnlessSuccess $ Result r
		peek pp

destroy :: Pointable d =>
	Device.D -> D -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (Device.D dvc) (D p) mac = ($ pure) $ runContT do
	pac <- AllocationCallbacks.maybeToCore mac
	lift $ C.destroy dvc p pac
