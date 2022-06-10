{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Memory.List (allocate, M.AllocateInfo(..)) where

import Foreign.Pointable
import Control.Exception

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device.Type as Device
import qualified Vulkan.Buffer.List.Type as Buffer.List
import qualified Vulkan.Memory.List.Middle as M

allocate :: (Pointable n, Pointable c, Pointable d) =>
	Device.D sd -> Buffer.List.L sb v -> M.AllocateInfo n ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . Device.MemoryList s v -> IO a) -> IO a
allocate (Device.D dvc) (Buffer.List.L bf) ai macc macd f = bracket
	(M.allocate dvc bf ai macc) (\mem -> M.free dvc mem macd)
	(f . Device.MemoryList)
