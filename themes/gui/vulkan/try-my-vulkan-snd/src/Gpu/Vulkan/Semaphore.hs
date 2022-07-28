{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Semaphore where

import Foreign.Pointable
import Control.Exception

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Semaphore.Middle as M

newtype S ss = S M.S deriving Show

create :: (Pointable n, Pointable c, Pointable d) =>
	Device.D sd -> M.CreateInfo n ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall ss . S ss -> IO a) -> IO a
create (Device.D dvc) ci macc macd f = bracket
	(M.create dvc ci macc) (\s -> M.destroy dvc s macd) (f . S)
