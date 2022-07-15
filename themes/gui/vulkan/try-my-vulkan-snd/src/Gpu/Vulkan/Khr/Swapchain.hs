{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Swapchain (create, recreate, S, M.CreateInfo(..)) where

import Foreign.Pointable
import Control.Exception

import Gpu.Vulkan.Khr.Swapchain.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Khr.Swapchain.Middle as M

create :: (Pointable n, Pointable c, Pointable d) =>
	Device.D sd -> M.CreateInfo n ssfc ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall ssc . S ssc -> IO a) -> IO a
create (Device.D dvc) ci macc macd f =
	bracket (M.create dvc ci macc) (\sc -> M.destroy dvc sc macd) (f . S)

recreate :: (Pointable n, Pointable c, Pointable d) =>
	Device.D sd -> M.CreateInfo n ssfc ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	S ssc -> IO ()
recreate (Device.D dvc) ci macc macd (S sc) = M.recreate dvc ci macc macd sc

-- getImages :: Device.D sd -> S ss -> IO
