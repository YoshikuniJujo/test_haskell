{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Memory.Image (allocate, M.AllocateInfo(..)) where

import Foreign.Pointable
import Control.Exception

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device.Type as Device
import qualified Vulkan.Image.Type as Image
import qualified Vulkan.Memory.Image.Middle as M

allocate :: (Pointable n, Pointable n2, Pointable n3) =>
	Device.D sd -> Image.I si -> M.AllocateInfo n ->
	Maybe (AllocationCallbacks.A n2) -> Maybe (AllocationCallbacks.A n3) ->
	(forall s . Device.MemoryImage s -> IO a) -> IO a
allocate (Device.D dvc) (Image.I img) ai macc macd f = bracket
	(M.allocate dvc img ai macc) (\mi -> M.free dvc mi macd)
	(f . Device.MemoryImage)
