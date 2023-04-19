{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandPool (
	C, create, reset, M.CreateInfo(..),

	M.CreateFlags, M.CreateFlagBits,
	pattern M.CreateTransientBit, pattern M.CreateResetCommandBufferBit,
	pattern M.CreateProtectedBit, pattern M.CreateFlagBitsMaxEnum ) where

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe

import Gpu.Vulkan.CommandPool.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.CommandPool.Middle as M
import qualified Gpu.Vulkan.CommandPool.Enum as M

create :: (WithPoked (TMaybe.M mn), WithPoked c, WithPoked d) =>
	Device.D sd -> M.CreateInfo mn ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . C s -> IO a) -> IO a
create (Device.D dvc) ci macc macd f =
	bracket (M.create dvc ci macc) (\c -> M.destroy dvc c macd) (f . C)

reset :: Device.D sd -> C s -> M.ResetFlags -> IO ()
reset (Device.D dv) (C c) fs = M.reset dv c fs
