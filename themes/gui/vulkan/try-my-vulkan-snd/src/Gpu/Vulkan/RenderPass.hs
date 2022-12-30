{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.RenderPass (
	R, createNew, M.CreateInfoNew(..),
	BeginInfo(..), BeginInfoNew(..) ) where

import Foreign.Pointable
import Control.Exception

import Gpu.Vulkan.RenderPass.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.RenderPass.Middle as M
import qualified Gpu.Vulkan.Attachment as Attachment

createNew :: (
	Attachment.DescriptionsToCoreNew fmts,
	Pointable n, Pointable c, Pointable d ) =>
	Device.D sd -> M.CreateInfoNew n fmts ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . R s -> IO a) -> IO a
createNew (Device.D dvc) ci macc macd f =
	bracket (M.createNew dvc ci macc) (\r -> M.destroy dvc r macd) (f . R)
