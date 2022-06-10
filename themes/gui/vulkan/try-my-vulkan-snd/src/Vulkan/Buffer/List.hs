{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Buffer.List (
	L, create, M.CreateInfo(..), getMemoryRequirements ) where

import Foreign.Storable
import Foreign.Pointable
import Control.Exception

import qualified Foreign.Storable.Generic

import Vulkan.Buffer.List.Type

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device.Type as Device
import qualified Vulkan.Memory.Middle as Memory
import qualified Vulkan.Buffer.List.Middle as M

create :: forall ds n v c d a . (
	Storable (Foreign.Storable.Generic.Wrap v),
	Pointable n, Pointable c, Pointable d ) =>
	Device.D ds -> M.CreateInfo n v ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . L s v -> IO a) -> IO a
create (Device.D dvc) ci macc macd f =
	bracket (M.create dvc ci macc) (\b -> M.destroy dvc b macd) (f . L)

getMemoryRequirements :: Device.D ds -> L s v -> IO Memory.Requirements
getMemoryRequirements (Device.D dvc) (L b) = M.getMemoryRequirements dvc b
