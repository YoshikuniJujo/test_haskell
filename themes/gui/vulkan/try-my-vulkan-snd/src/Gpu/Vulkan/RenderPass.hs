{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.RenderPass (
	R, createNew, create, M.CreateInfo(..), BeginInfo(..), BeginInfoNew(..) ) where

import Foreign.Pointable
import Control.Exception

import Gpu.Vulkan.RenderPass.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.RenderPass.Middle as M
import qualified Gpu.Vulkan.Attachment as Attachment

create :: (Pointable n, Pointable n2, Pointable n3) =>
	Device.D sd -> M.CreateInfo n ->
	Maybe (AllocationCallbacks.A n2) -> Maybe (AllocationCallbacks.A n3) ->
	(forall s . R s -> IO a) -> IO a
create (Device.D dvc) ci macc macd f =
	bracket (M.create dvc ci macc) (\r -> M.destroy dvc r macd) (f . R)

createNew :: (
	Attachment.DescriptionsToCoreNew fmts,
	Pointable n, Pointable c, Pointable d ) =>
	Device.D sd -> M.CreateInfoNew n fmts ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . R s -> IO a) -> IO a
createNew (Device.D dvc) ci macc macd f =
	bracket (M.createNew dvc ci macc) (\r -> M.destroy dvc r macd) (f . R)
