{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Memory.List where

import Foreign.Pointable

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device as Device
import qualified Vulkan.Memory.Middle as M

allocate :: (Pointable n, Pointable n') =>
	Device.D -> M.AllocateInfo n -> Maybe (AllocationCallbacks.A n') ->
	IO (Device.MemoryList v)
allocate dvc ai mac =
	(\(Device.Memory m) -> Device.MemoryList m) <$> M.allocate dvc ai mac

free :: Pointable n =>
	Device.D -> Device.MemoryList v -> Maybe (AllocationCallbacks.A n) ->
	IO ()
free dvc (Device.MemoryList m) mac = M.free dvc (Device.Memory m) mac
