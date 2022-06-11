{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Descriptor.Set.Layout (
	L, create, M.Binding(..), M.CreateInfo(..) ) where

import Foreign.Pointable
import Control.Exception

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device.Type as Device
import qualified Vulkan.Descriptor.Set.Layout.Middle as M

newtype L s = L M.L deriving Show

create :: (Pointable n, Pointable c, Pointable d) =>
	Device.D sd -> M.CreateInfo n ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . L s -> IO a) -> IO a
create (Device.D dvc) ci macc macd f =
	bracket (M.create dvc ci macc) (\l -> M.destroy dvc l macd) (f . L)
