{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PipelineCache (
	C, create, getData
	) where

import Foreign.Storable.PeekPoke
import Control.Exception

import Gpu.Vulkan.PipelineCache.Type

import Gpu.Vulkan.AllocationCallbacks qualified as AllocationCallbacks
import Gpu.Vulkan.Device qualified as Device
import Gpu.Vulkan.Device.Type qualified as Device
import Gpu.Vulkan.PipelineCache.Middle qualified as M

create :: (WithPoked n, WithPoked c, WithPoked d) =>
	Device.D sd -> M.CreateInfo n ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . C s -> IO a) -> IO a
create (Device.D dv) ci macc macd f =
	bracket (M.create dv ci macc) (\c -> M.destroy dv c macd) (f . C)

getData :: Device.D sd -> C s -> IO M.Data
getData (Device.D dv) (C c) = M.getData dv c
