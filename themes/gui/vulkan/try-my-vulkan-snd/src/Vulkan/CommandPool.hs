{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.CommandPool where

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
	} = do
	pure C.CreateInfo {
		C.createInfoSType = ()
		}
