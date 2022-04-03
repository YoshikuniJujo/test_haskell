{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.CommandPool where

import Foreign.Ptr
import Foreign.Pointable
import Control.Monad.Cont
import Data.Word

import Vulkan.CommandPool.Enum

import qualified Vulkan.CommandPool.Core as C

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoQueueFamilyIndex :: Word32 }
	deriving Show

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO C.CreateInfo
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoQueueFamilyIndex = qfi
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pure C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt,
		C.createInfoFlags = flgs,
		C.createInfoQueueFamilyIndex = qfi
		}
