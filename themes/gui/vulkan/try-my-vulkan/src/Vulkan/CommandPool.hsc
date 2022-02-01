{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.CommandPool where

import Foreign.Ptr
import Control.Monad.Cont
import Data.Word

import Vulkan.Base
import Vulkan.CommandPoolCreateFlagBits

import qualified Vulkan.CommandPool.Internal as I

#include <vulkan/vulkan.h>

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CommandPoolCreateFlags,
	createInfoQueueFamilyIndex :: Word32 }
	deriving Show

word32ToUint32T :: Word32 -> #{type uint32_t}
word32ToUint32T = fromIntegral

createInfoToC :: Pointable n => CreateInfo n -> ContT r IO I.CreateInfo
createInfoToC CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoQueueFamilyIndex = word32ToUint32T -> qfi } = do
	(castPtr -> pnxt) <- ContT $ withMaybePointer mnxt
	pure I.CreateInfo {
		I.createInfoSType = (),
		I.createInfoPNext = pnxt,
		I.createInfoFlags = flgs,
		I.createInfoQueueFamilyIndex = qfi }
