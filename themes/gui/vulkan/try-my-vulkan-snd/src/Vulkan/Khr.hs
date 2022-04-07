{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr where

import Foreign.Marshal.Alloc
import Foreign.Storable
import Control.Monad.Cont
import Data.Word

import qualified Data.Text as T

import Vulkan.Base
import Vulkan.Exception
import Vulkan.Exception.Enum

import qualified Vulkan.Device as Device
import qualified Vulkan.Semaphore as Semaphore
import qualified Vulkan.Fence as Fence
import qualified Vulkan.Khr.Swapchain as Swapchain
import qualified Vulkan.Khr.Core as C

validationLayerName :: T.Text
validationLayerName = "VK_LAYER_KHRONOS_validation"

acquireNextImage :: Device.D ->
	Swapchain.S -> Word64 -> Maybe Semaphore.S -> Maybe Fence.F -> IO Word32
acquireNextImage
	(Device.D dvc) (Swapchain.S sc) to msmp mfnc = ($ pure) $ runContT do
	let	smp = maybe NullHandle (\(Semaphore.S s) -> s) msmp
		fnc = maybe NullHandle (\(Fence.F f) -> f) mfnc
	pii <- ContT alloca
	lift do	r <- C.acquireNextImage dvc sc to smp fnc pii
		throwUnlessSuccess $ Result r
		peek pii
