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
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry
import Data.Word

import Gpu.Vulkan.Device.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.PhysicalDevice as PhysicalDevice
import qualified Gpu.Vulkan.Device.Middle as M
import qualified Gpu.Vulkan.QueueFamily.Middle as QueueFamily
import qualified Gpu.Vulkan.Queue as Queue

import Data.HeteroParList qualified as HeteroParList

create :: (
	WithPoked (TMaybe.M mn), HeteroParList.ToListWithCM' WithPoked TMaybe.M mns,
	AllocationCallbacks.ToMiddle msn3n3 ) =>
	PhysicalDevice.P -> M.CreateInfo mn mns ->
	TPMaybe.M (U2 AllocationCallbacks.A) msn3n3 ->
	(forall s . D s -> IO a) -> IO a
create phdvc ci
	(AllocationCallbacks.toMiddle -> macc) f =
	bracket (M.create phdvc ci macc) (`M.destroy` macc) (f . D)

getQueue :: D s -> QueueFamily.Index -> Word32 -> IO Queue.Q
getQueue (D dvc) (QueueFamily.Index qfi) qi = M.getQueue dvc qfi qi

waitIdle :: D s -> IO ()
waitIdle (D d) = M.waitIdle d
