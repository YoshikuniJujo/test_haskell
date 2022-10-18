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

import qualified Gpu.Vulkan.Device.Core as C
import qualified Gpu.Vulkan.QueueFamily.EnumManual as QueueFamily

data QueueCreateInfo n = QueueCreateInfo {
	queueCreateInfoNext :: Maybe n,
	queueCreateInfoFlags :: CreateFlags,
	queueCreateInfoQueueFamilyIndex :: QueueFamily.Index,
	queueCreateInfoQueuePriorities :: [Float] }
	deriving Show

queueCreateInfoToCore :: Pointable n => QueueCreateInfo n -> ContT r IO C.QueueCreateInfo
queueCreateInfoToCore QueueCreateInfo {
	queueCreateInfoNext = mnxt,
	queueCreateInfoFlags = CreateFlagBits flgs,
	queueCreateInfoQueueFamilyIndex = QueueFamily.Index qfi,
	queueCreateInfoQueuePriorities = qps
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pqps <- ContT $ allocaArray (length qps)
	lift $ pokeArray pqps qps
	pure C.QueueCreateInfo {
		C.queueCreateInfoSType = (),
		C.queueCreateInfoPNext = pnxt,
		C.queueCreateInfoFlags = flgs,
		C.queueCreateInfoQueueFamilyIndex = qfi,
		C.queueCreateInfoQueueCount = genericLength qps,
		C.queueCreateInfoPQueuePriorities = pqps }
