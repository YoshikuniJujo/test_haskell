{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.CommandPool (C, create, M.CreateInfo(..)) where

import Foreign.Pointable
import Control.Exception

import Vulkan.CommandPool.Type

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device.Type as Device
import qualified Vulkan.CommandPool.Middle as M

create :: (Pointable n, Pointable n2, Pointable n3) =>
	Device.D sd -> M.CreateInfo n ->
	Maybe (AllocationCallbacks.A n2) -> Maybe (AllocationCallbacks.A n3) ->
	(forall s . C s -> IO a) -> IO a
create (Device.D dvc) ci macc macd f =
	bracket (M.create dvc ci macc) (\c -> M.destroy dvc c macd) (f . C)
