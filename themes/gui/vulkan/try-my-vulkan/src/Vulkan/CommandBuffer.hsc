{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.CommandBuffer where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Storable
import Control.Monad.Cont
import Data.Word

import Vulkan.Base
import Vulkan.Exception
import Vulkan.Device
import Vulkan.CommandPool
import Vulkan.CommandBufferLevel

import qualified Vulkan.CommandBuffer.Internal as I

data AllocateInfo n = AllocateInfo {
	allocateInfoNext :: Maybe n,
	allocateInfoCommandPool :: CommandPool,
	allocateInfoLevel :: CommandBufferLevel,
	allocateInfoCommandBufferCount :: Word32 }
	deriving Show

allocateInfoToC :: Pointable n => AllocateInfo n -> ContT r IO I.AllocateInfo
allocateInfoToC AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoCommandPool = cp,
	allocateInfoLevel = lv,
	allocateInfoCommandBufferCount = word32ToUint32T -> cbc } = do
	(castPtr -> pnxt) <- ContT $ withMaybePointer mnxt
	pure I.AllocateInfo {
		I.allocateInfoSType = (),
		I.allocateInfoPNext = pnxt,
		I.allocateInfoCommandPool = cp,
		I.allocateInfoLevel = lv,
		I.allocateInfoCommandBufferCount = cbc }

data CommandBufferTag
newtype CommandBuffer = CommandBuffer (Ptr CommandBufferTag)
	deriving (Show, Storable)

allocate :: Storable n => Device -> AllocateInfo n -> IO [CommandBuffer]
allocate dvc ai = ($ pure) $ runContT do
	I.AllocateInfo_ fai <- allocateInfoToC ai
	pai <- ContT $ withForeignPtr fai
	pcbs <- ContT $ allocaArray n
	lift do	r <- c_vkAllocateCommandBuffers dvc pai pcbs
		throwUnlessSuccess r
		peekArray n pcbs
	where n = fromIntegral $ allocateInfoCommandBufferCount ai

foreign import ccall "vkAllocateCommandBuffers" c_vkAllocateCommandBuffers ::
	Device -> Ptr I.AllocateInfo -> Ptr CommandBuffer -> IO Result
