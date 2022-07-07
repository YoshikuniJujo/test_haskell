{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Swapchain (create, S, M.CreateInfo(..)) where

import Control.Exception

import Gpu.Vulkan.Khr.Swapchain.Type

import qualified Gpu.Vulkan.Khr.Swapchain.Middle as M

create ::
	Device.D sd -> M.CreateInfo n ssfc ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall ssc . S ssc -> IO a) -> IO a
create (Device.D dvc) ci macc macd f =
	bracket (M.create dvc ci macc) (\sc -> M.destroy dvc sc macd) (f . S)
