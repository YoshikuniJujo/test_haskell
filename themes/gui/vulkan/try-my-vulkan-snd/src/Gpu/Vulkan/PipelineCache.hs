{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PipelineCache (
	C, create, getData
	) where

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe

import Gpu.Vulkan.PipelineCache.Type

import Gpu.Vulkan.AllocationCallbacks qualified as AllocationCallbacks
import Gpu.Vulkan.AllocationCallbacks.Type qualified as AllocationCallbacks
import Gpu.Vulkan.Device qualified as Device
import Gpu.Vulkan.Device.Type qualified as Device
import Gpu.Vulkan.PipelineCache.Middle qualified as M

create :: (WithPoked (TMaybe.M mn), WithPoked c, WithPoked d) =>
	Device.D sd -> M.CreateInfo mn ->
	Maybe (AllocationCallbacks.A sc c) -> Maybe (AllocationCallbacks.A sd' d) ->
	(forall s . C s -> IO a) -> IO a
create (Device.D dv) ci
	((AllocationCallbacks.toMiddle <$>) -> macc)
	((AllocationCallbacks.toMiddle <$>) -> macd) f =
	bracket (M.create dv ci macc) (\c -> M.destroy dv c macd) (f . C)

getData :: Device.D sd -> C s -> IO M.Data
getData (Device.D dv) (C c) = M.getData dv c
