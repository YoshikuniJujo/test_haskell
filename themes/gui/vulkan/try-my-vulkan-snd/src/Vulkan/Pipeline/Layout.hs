{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Layout (
	L, create, M.CreateInfo(..),
	M.CreateFlags, pattern M.CreateFlagsZero ) where

import Foreign.Pointable
import Control.Exception

import Vulkan.Pipeline.Layout.Type

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device.Type as Device
import qualified Vulkan.Pipeline.Layout.Middle as M

-- data CreateInfo n s =

create :: (Pointable n, Pointable n2, Pointable n3) =>
	Device.D sd -> M.CreateInfo n ->
	Maybe (AllocationCallbacks.A n2) -> Maybe (AllocationCallbacks.A n3) ->
	(forall s . L s -> IO a) -> IO a
create (Device.D dvc) ci macc macd f =
	bracket (M.create dvc ci macc) (\l -> M.destroy dvc l macd) (f . L)
