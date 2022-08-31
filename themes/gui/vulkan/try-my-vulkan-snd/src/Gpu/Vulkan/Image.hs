{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Image (
	INew, BindedNew, createNew, M.CreateInfoNew(..),

	I, Binded, create, M.CreateInfo(..), getMemoryRequirements, bindMemory,
	M.SubresourceRange(..) ) where

import Foreign.Pointable
import Control.Exception

import Gpu.Vulkan.Image.Type

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Memory.Middle as Memory
import qualified Gpu.Vulkan.Image.Middle as M

createNew :: (Pointable n, Pointable n2, Pointable n3, T.FormatToValue fmt) =>
	Device.D sd -> M.CreateInfoNew n fmt ->
	Maybe (AllocationCallbacks.A n2) -> Maybe (AllocationCallbacks.A n3) ->
	(forall s . INew s fmt -> IO a) -> IO a
createNew (Device.D dvc) ci macc macd f =
	bracket (M.createNew dvc ci macc) (\i -> M.destroy dvc i macd) (f . INew)

create :: (Pointable n, Pointable n2, Pointable n3) =>
	Device.D sd -> M.CreateInfo n ->
	Maybe (AllocationCallbacks.A n2) -> Maybe (AllocationCallbacks.A n3) ->
	(forall s . I s -> IO a) -> IO a
create (Device.D dvc) ci macc macd f =
	bracket (M.create dvc ci macc) (\i -> M.destroy dvc i macd) (f . I)

getMemoryRequirements :: Device.D sd -> I s -> IO Memory.Requirements
getMemoryRequirements (Device.D dvc) (I img) = M.getMemoryRequirements dvc img

bindMemory :: Device.D sd -> I si -> Device.MemoryImage sm -> IO (Binded si sm)
bindMemory (Device.D dvc) (I img) (Device.MemoryImage mem) = do
	M.bindMemory dvc img mem
	pure $ Binded img
