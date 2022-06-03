{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.RenderPass (create, M.CreateInfo(..)) where

import Foreign.Pointable
import Control.Exception

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device.Type as Device
import qualified Vulkan.RenderPass.Middle as M

newtype R s = R M.R deriving Show

create :: (Pointable n, Pointable n2, Pointable n3) =>
	Device.D sd -> M.CreateInfo n ->
	Maybe (AllocationCallbacks.A n2) -> Maybe (AllocationCallbacks.A n3) ->
	(forall s . R s -> IO a) -> IO a
create (Device.D dvc) ci macc macd f =
	bracket (M.create dvc ci macc) (\r -> M.destroy dvc r macd) (f . R)
