{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Queue where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Monad.Cont
import Data.List

import Gpu.Vulkan.Device.Queue.Enum

import qualified Gpu.Vulkan.Device.Queue.Core as C
import qualified Gpu.Vulkan.QueueFamily.EnumManual as QueueFamily

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoQueueFamilyIndex :: QueueFamily.Index,
	createInfoQueuePriorities :: [Float] }
	deriving Show

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO C.CreateInfo
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoQueueFamilyIndex = QueueFamily.Index qfi,
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
