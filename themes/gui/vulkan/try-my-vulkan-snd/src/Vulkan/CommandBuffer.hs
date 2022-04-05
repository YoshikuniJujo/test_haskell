{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.CommandBuffer where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Monad.Cont
import Data.Word

import Vulkan.Exception
import Vulkan.Exception.Enum
import Vulkan.CommandBuffer.Enum

import qualified Vulkan.Device as Device
import qualified Vulkan.CommandPool as CommandPool
import qualified Vulkan.CommandBuffer.Core as C

data AllocateInfo n = AllocateInfo {
	allocateInfoNext :: Maybe n,
	allocateInfoCommandPool :: CommandPool.C,
	allocateInfoLevel :: Level,
	allocateInfoCommandBufferCount :: Word32 }
	deriving Show

allocateInfoToCore :: Pointable n =>
	AllocateInfo n -> ContT r IO (Ptr C.AllocateInfo)
allocateInfoToCore AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoCommandPool = CommandPool.C cp,
	allocateInfoLevel = Level lvl,
	allocateInfoCommandBufferCount = cbc
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	let	C.AllocateInfo_ fAllocateInfo = C.AllocateInfo {
			C.allocateInfoSType = (),
			C.allocateInfoPNext = pnxt,
			C.allocateInfoCommandPool = cp,
			C.allocateInfoLevel = lvl,
			C.allocateInfoCommandBufferCount = cbc }
	ContT $ withForeignPtr fAllocateInfo

newtype C = C C.C deriving Show

allocate :: Pointable n => Device.D -> AllocateInfo n -> IO [C]
allocate (Device.D dvc) ai = ($ pure) . runContT $ (C <$>) <$> do
	pai <- allocateInfoToCore ai
	pc <- ContT $ allocaArray cbc
	lift do	r <- C.allocate dvc pai pc
		throwUnlessSuccess $ Result r
		peekArray cbc pc
	where cbc = fromIntegral $ allocateInfoCommandBufferCount ai
