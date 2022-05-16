{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Descriptor.Pool where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Word

import Vulkan.Descriptor.Enum
import Vulkan.Descriptor.Pool.Enum

import qualified Vulkan.Descriptor.Pool.Core as C

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

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO C.CreateInfo
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoMaxSets = ms,
	createInfoPoolSizes = (length &&& (sizeToCore <$>) -> (psc, pss))
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	ppss <- ContT $ allocaArray psc
	lift $ pokeArray ppss pss
	pure C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt,
		C.createInfoFlags = flgs,
		C.createInfoMaxSets = ms,
		C.createInfoPoolSizeCount = fromIntegral psc,
		C.createInfoPPoolSizes = ppss }
