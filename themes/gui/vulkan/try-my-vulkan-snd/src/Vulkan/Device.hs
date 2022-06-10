{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Device (
	D, create, M.CreateInfo(..), M.CreateFlags, pattern M.CreateFlagsZero,
	getQueue, MemoryImage, MemoryList
	) where

import Foreign.Pointable
import Control.Exception
import Data.Word

import Vulkan.Device.Type

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.PhysicalDevice as PhysicalDevice
import qualified Vulkan.Device.Middle as M
import qualified Vulkan.QueueFamily.EnumManual as QueueFamily
import qualified Vulkan.Queue as Queue

create :: (Pointable n, Pointable n2, Pointable n3, Pointable n4) =>
	PhysicalDevice.P -> M.CreateInfo n n2 ->
	Maybe (AllocationCallbacks.A n3) -> Maybe (AllocationCallbacks.A n4) ->
	(forall s . D s -> IO a) -> IO a
create phdvc ci macc macd f =
	bracket (M.create phdvc ci macc) (`M.destroy` macd) (f . D)

getQueue :: D s -> QueueFamily.Index -> Word32 -> IO Queue.Q
getQueue (D dvc) (QueueFamily.Index qfi) qi = M.getQueue dvc qfi qi
