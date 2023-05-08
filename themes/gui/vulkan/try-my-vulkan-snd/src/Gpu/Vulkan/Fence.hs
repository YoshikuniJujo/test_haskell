{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Fence (F, create, M.CreateInfo(..), waitForFs, resetFs) where

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.HeteroParList qualified as HeteroParList
import Data.Word

import Gpu.Vulkan.Fence.Type

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Fence.Middle as M

create :: (WithPoked (TMaybe.M mn)) =>
	Device.D sd -> M.CreateInfo mn ->
	Maybe (AllocationCallbacks.A sc c) ->
	(forall sf . F sf -> IO a) -> IO a
create (Device.D dvc) ci
	((AllocationCallbacks.toMiddle <$>) -> macc) f = bracket
	(M.create dvc ci macc) (\fnc -> M.destroy dvc fnc macc) (f . F)

waitForFs :: Device.D sd -> HeteroParList.PL F sfs -> Bool -> Word64 -> IO ()
waitForFs (Device.D dvc) fs wa to =
	M.waitForFs dvc (HeteroParList.toList (\(F f) -> f) fs) wa to

resetFs :: Device.D sd -> HeteroParList.PL F sfs -> IO ()
resetFs (Device.D dvc) = M.resetFs dvc . HeteroParList.toList \(F f) -> f
