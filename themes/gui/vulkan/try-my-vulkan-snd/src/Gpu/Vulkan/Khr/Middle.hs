{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Middle where

import Foreign.Marshal.Alloc
import Foreign.Storable
import Control.Monad.Cont
import Data.Word

import Gpu.Vulkan.Base
import Gpu.Vulkan.Exception
import Gpu.Vulkan.Exception.Enum

import qualified Gpu.Vulkan.Device.Middle.Internal as Device.M
import qualified Gpu.Vulkan.Khr.Core as C
import qualified Gpu.Vulkan.Khr.Swapchain.Middle.Internal as Swapchain.M
import qualified Gpu.Vulkan.Fence.Middle.Internal as Fence
import qualified Gpu.Vulkan.Semaphore.Middle as Semaphore.M

acquireNextImage :: Device.M.D ->
	Swapchain.M.S -> Word64 -> Maybe Semaphore.M.S -> Maybe Fence.F -> IO Word32
acquireNextImage = acquireNextImageResult [Success]

acquireNextImageResult :: [Result] -> Device.M.D ->
	Swapchain.M.S -> Word64 -> Maybe Semaphore.M.S -> Maybe Fence.F -> IO Word32
acquireNextImageResult sccs
	(Device.M.D dvc) sc to msmp mfnc = ($ pure) $ runContT do
	let	smp = maybe NullHandle (\(Semaphore.M.S s) -> s) msmp
		fnc = maybe NullHandle (\(Fence.F f) -> f) mfnc
	pii <- ContT alloca
	sc' <- lift $ Swapchain.M.sToCore sc
	lift do	r <- C.acquireNextImage dvc sc' to smp fnc pii
		throwUnless sccs $ Result r
		peek pii
