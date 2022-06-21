{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Buffer.List (
	L, Binded, create, M.CreateInfo(..), getMemoryRequirements, bindMemory
	) where

import Foreign.Storable
import Foreign.Pointable
import Control.Exception

import qualified Foreign.Storable.Generic

import Gpu.Vulkan.Buffer.List.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Memory.Middle as Memory
import qualified Gpu.Vulkan.Buffer.List.Middle as M

create :: forall ds n v c d a . (
	Storable (Foreign.Storable.Generic.Wrap v),
	Pointable n, Pointable c, Pointable d ) =>
	Device.D ds -> M.CreateInfo n v ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . L s v -> IO a) -> IO a
create (Device.D dvc) ci macc macd f =
	bracket (M.create dvc ci macc) (\b -> M.destroy dvc b macd) (f . L)

getMemoryRequirements :: Device.D sd -> L s v -> IO Memory.Requirements
getMemoryRequirements (Device.D dvc) (L b) = M.getMemoryRequirements dvc b

bindMemory ::
	Device.D sd -> L sl v -> Device.MemoryList sm v -> IO (Binded sl sm v)
bindMemory (Device.D dvc) (L bf) (Device.MemoryList mem) =
	Binded bf <$ M.bindMemory dvc bf mem
