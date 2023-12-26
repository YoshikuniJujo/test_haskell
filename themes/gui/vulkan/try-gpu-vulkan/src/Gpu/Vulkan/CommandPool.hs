{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandPool (

	-- * CREATE

	create, reset, C, M.CreateInfo(..),

	-- * ENUM

	module Gpu.Vulkan.CommandPool.Enum

	) where

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry

import Gpu.Vulkan.CommandPool.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.CommandPool.Middle as M
import qualified Gpu.Vulkan.CommandPool.Enum as Enum
import Gpu.Vulkan.CommandPool.Enum

create :: (WithPoked (TMaybe.M mn), AllocationCallbacks.ToMiddle ma) =>
	Device.D sd -> M.CreateInfo mn ->
	TPMaybe.M (U2 AllocationCallbacks.A) ma ->
	(forall s . C s -> IO a) -> IO a
create (Device.D dvc) ci
	(AllocationCallbacks.toMiddle -> macc) f =
	bracket (M.create dvc ci macc) (\c -> M.destroy dvc c macc) (f . C)

reset :: Device.D sd -> C s -> Enum.ResetFlags -> IO ()
reset (Device.D dv) (C c) fs = M.reset dv c fs
