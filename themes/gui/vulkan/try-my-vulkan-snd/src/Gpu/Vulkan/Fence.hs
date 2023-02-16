{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Fence (F, create, M.CreateInfo(..), waitForFs, resetFs) where

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.HeteroParList
import Data.Word

import Gpu.Vulkan.Fence.Type

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Fence.Middle as M

create :: (Pokable n, Pokable c, Pokable d) =>
	Device.D sd -> M.CreateInfo n ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall sf . F sf -> IO a) -> IO a
create (Device.D dvc) ci macc macd f = bracket
	(M.create dvc ci macc) (\fnc -> M.destroy dvc fnc macd) (f . F)

waitForFs :: Device.D sd -> HeteroParList F sfs -> Bool -> Word64 -> IO ()
waitForFs (Device.D dvc) fs wa to =
	M.waitForFs dvc (heteroParListToList (\(F f) -> f) fs) wa to

resetFs :: Device.D sd -> HeteroParList F sfs -> IO ()
resetFs (Device.D dvc) = M.resetFs dvc . heteroParListToList \(F f) -> f
