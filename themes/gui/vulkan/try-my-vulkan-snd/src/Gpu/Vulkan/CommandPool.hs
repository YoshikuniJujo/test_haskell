{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandPool (
	C, create, M.CreateInfo(..),

	M.CreateFlags, M.CreateFlagBits,
	pattern M.CreateTransientBit, pattern M.CreateResetCommandBufferBit,
	pattern M.CreateProtectedBit, pattern M.CreateFlagBitsMaxEnum ) where

import Foreign.Pointable
import Control.Exception

import Gpu.Vulkan.CommandPool.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.CommandPool.Middle as M

create :: (Pointable n, Pointable n2, Pointable n3) =>
	Device.D sd -> M.CreateInfo n ->
	Maybe (AllocationCallbacks.A n2) -> Maybe (AllocationCallbacks.A n3) ->
	(forall s . C s -> IO a) -> IO a
create (Device.D dvc) ci macc macd f =
	bracket (M.create dvc ci macc) (\c -> M.destroy dvc c macd) (f . C)
