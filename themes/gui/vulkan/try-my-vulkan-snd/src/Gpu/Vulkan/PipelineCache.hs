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
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Uncurry

import Gpu.Vulkan.PipelineCache.Type

import Gpu.Vulkan.AllocationCallbacks qualified as AllocationCallbacks
import Gpu.Vulkan.AllocationCallbacks.Type qualified as AllocationCallbacks
import Gpu.Vulkan.Device qualified as Device
import Gpu.Vulkan.Device.Type qualified as Device
import Gpu.Vulkan.PipelineCache.Middle qualified as M

create :: (WithPoked (TMaybe.M mn), AllocationCallbacks.ToMiddle' mscc) =>
	Device.D sd -> M.CreateInfo mn ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc -> (forall s . C s -> IO a) -> IO a
create (Device.D dv) ci
	(AllocationCallbacks.toMiddle' -> mac) f =
	bracket (M.create dv ci mac) (\c -> M.destroy dv c mac) (f . C)

getData :: Device.D sd -> C s -> IO M.Data
getData (Device.D dv) (C c) = M.getData dv c
