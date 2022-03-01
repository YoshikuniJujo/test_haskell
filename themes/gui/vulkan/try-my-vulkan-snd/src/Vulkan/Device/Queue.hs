{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Device.Queue where

import Foreign.Ptr
import Foreign.Marshal.Array
import Control.Monad.Cont
import Data.List
import Data.Word

import Vulkan.Base
import Vulkan.Device.Queue.Enum

import qualified Vulkan.Device.Queue.Core as C

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoQueueFamilyIndex :: Word32,
	createInfoQueuePriorities :: [Float] }
	deriving Show

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO C.CreateInfo
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoQueueFamilyIndex = qfi,
	createInfoQueuePriorities = qps
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pqps <- ContT $ allocaArray (length qps)
	lift $ pokeArray pqps qps
	pure C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt,
		C.createInfoFlags = flgs,
		C.createInfoQueueFamilyIndex = qfi,
		C.createInfoQueueCount = genericLength qps,
		C.createInfoPQueuePriorities = pqps }
