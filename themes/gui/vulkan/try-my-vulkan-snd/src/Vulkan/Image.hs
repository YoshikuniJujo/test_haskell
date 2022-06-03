{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Image (
	I, create, M.CreateInfo(..), getMemoryRequirements, bindMemory ) where

import Foreign.Pointable
import Control.Exception

import Vulkan.Image.Type

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device.Type as Device
import qualified Vulkan.Memory.Middle as Memory
import qualified Vulkan.Image.Middle as M

create :: (Pointable n, Pointable n2, Pointable n3) =>
	Device.D sd -> M.CreateInfo n ->
	Maybe (AllocationCallbacks.A n2) -> Maybe (AllocationCallbacks.A n3) ->
	(forall s . I s -> IO a) -> IO a
create (Device.D dvc) ci macc macd f =
	bracket (M.create dvc ci macc) (\i -> M.destroy dvc i macd) (f . I)

getMemoryRequirements :: Device.D sd -> I s -> IO Memory.Requirements
getMemoryRequirements (Device.D dvc) (I img) = M.getMemoryRequirements dvc img

bindMemory :: Device.D sd -> I si -> Device.MemoryImage sm -> IO ()
bindMemory (Device.D dvc) (I img) (Device.MemoryImage mem) =
	M.bindMemory dvc img mem
