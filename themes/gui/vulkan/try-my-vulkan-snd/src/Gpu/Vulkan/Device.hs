{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device (
	D, create, M.CreateInfo(..), M.QueueCreateInfo(..), M.CreateFlags,
	getQueue,
	waitIdle
	) where

import Foreign.Storable.PeekPoke
import Foreign.Storable.HeteroList
import Control.Exception
import Data.Word

import Gpu.Vulkan.Device.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.PhysicalDevice as PhysicalDevice
import qualified Gpu.Vulkan.Device.Middle as M
import qualified Gpu.Vulkan.QueueFamily.Middle as QueueFamily
import qualified Gpu.Vulkan.Queue as Queue

create :: (WithPoked n, WithPokedHeteroToListM ns, Pokable n3, Pokable n4) =>
	PhysicalDevice.P -> M.CreateInfo n ns ->
	Maybe (AllocationCallbacks.A n3) -> Maybe (AllocationCallbacks.A n4) ->
	(forall s . D s -> IO a) -> IO a
create phdvc ci macc macd f =
	bracket (M.create phdvc ci macc) (`M.destroy` macd) (f . D)

getQueue :: D s -> QueueFamily.Index -> Word32 -> IO Queue.Q
getQueue (D dvc) (QueueFamily.Index qfi) qi = M.getQueue dvc qfi qi

waitIdle :: D s -> IO ()
waitIdle (D d) = M.waitIdle d
