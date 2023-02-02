{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Instance (
	I, create, M.CreateInfo(..), -- M.createInfoNil,
	M.enumerateLayerProperties, M.enumerateExtensionProperties ) where

import Foreign.Storable.PeekPoke
import Control.Exception

import Gpu.Vulkan.Instance.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Instance.Middle as M

create :: (WithPoked n, Pokable n2, Pokable n3, Pokable n4) =>
	M.CreateInfo n n2 ->
	Maybe (AllocationCallbacks.A n3) -> Maybe (AllocationCallbacks.A n4) ->
	(forall s . I s -> IO a) -> IO a
create ci macc macd f = bracket (M.create ci macc) (`M.destroy` macd) (f . I)
