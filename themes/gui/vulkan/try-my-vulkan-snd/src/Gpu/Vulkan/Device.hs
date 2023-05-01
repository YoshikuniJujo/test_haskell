{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device (
	D, create, M.CreateInfo(..), M.QueueCreateInfo(..), M.CreateFlags,
	getQueue,
	waitIdle
	) where

import Foreign.Storable.PeekPoke
import Foreign.Storable.HeteroList
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.Word

import Gpu.Vulkan.Device.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.PhysicalDevice as PhysicalDevice
import qualified Gpu.Vulkan.Device.Middle as M
import qualified Gpu.Vulkan.QueueFamily.Middle as QueueFamily
import qualified Gpu.Vulkan.Queue as Queue

create :: (WithPoked (TMaybe.M mn), WithPokedHeteroToListM' TMaybe.M mns, Pokable n3, Pokable n4) =>
	PhysicalDevice.P -> M.CreateInfo mn mns ->
	Maybe (AllocationCallbacks.A sn3 n3) -> Maybe (AllocationCallbacks.A sn4 n4) ->
	(forall s . D s -> IO a) -> IO a
create phdvc ci
	((AllocationCallbacks.toMiddle <$>) -> macc)
	((AllocationCallbacks.toMiddle <$>) -> macd) f =
	bracket (M.create phdvc ci macc) (`M.destroy` macd) (f . D)

getQueue :: D s -> QueueFamily.Index -> Word32 -> IO Queue.Q
getQueue (D dvc) (QueueFamily.Index qfi) qi = M.getQueue dvc qfi qi

waitIdle :: D s -> IO ()
waitIdle (D d) = M.waitIdle d
