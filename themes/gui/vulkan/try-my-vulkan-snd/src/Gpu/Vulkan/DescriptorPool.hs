{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorPool (
	P, create, M.CreateInfo(..), M.Size(..),

	M.CreateFlags,
	pattern M.CreateFreeDescriptorSetBit, pattern M.CreateUpdateAfterBindBit,
	pattern M.CreateHostOnlyBitValve, pattern M.CreateUpdateAfterBindBitExt,
	pattern M.CreateFlagBitsMaxEnum ) where

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe

import Gpu.Vulkan.DescriptorPool.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.DescriptorPool.Middle as M
import qualified Gpu.Vulkan.DescriptorPool.Enum as M

create :: (WithPoked (TMaybe.M mn)) =>
	Device.D sd -> M.CreateInfo mn -> Maybe (AllocationCallbacks.A sc c) ->
	(forall s . P s -> IO a) -> IO a
create (Device.D dvc) ci
	((AllocationCallbacks.toMiddle <$>) -> macc) f =
	bracket (M.create dvc ci macc) (\p -> M.destroy dvc p macc) (f . P)
