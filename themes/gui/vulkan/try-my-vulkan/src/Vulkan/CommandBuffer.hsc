{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.CommandBuffer where

import Foreign.Ptr
import Control.Monad.Cont
import Data.Word

import Vulkan.Base
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
