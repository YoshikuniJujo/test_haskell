{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Sampler (

	-- * CREATE

	create, S, M.CreateInfo(..),

	-- * ENUM

	module Gpu.Vulkan.Sampler.Enum

	) where

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Sampler.Middle as M
import Gpu.Vulkan.Sampler.Enum

import Gpu.Vulkan.Sampler.Type

create :: (WithPoked (TMaybe.M mn), AllocationCallbacks.ToMiddle mac) =>
	Device.D sd -> M.CreateInfo mn ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	(forall s . S s -> IO a) -> IO a
create (Device.D dvc) ci
	(AllocationCallbacks.toMiddle -> mac) f =
	bracket (M.create dvc ci mac) (\s -> M.destroy dvc s mac) (f . S)
