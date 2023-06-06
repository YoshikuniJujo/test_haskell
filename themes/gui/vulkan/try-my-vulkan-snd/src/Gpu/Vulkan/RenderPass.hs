{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.RenderPass (
	R, createNew, M.CreateInfoNew(..), BeginInfo(..) ) where

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Uncurry

import Gpu.Vulkan.RenderPass.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.RenderPass.Middle as M
import qualified Gpu.Vulkan.RenderPass.Tmp as M
import qualified Gpu.Vulkan.Attachment as Attachment

createNew :: (
	Attachment.DescriptionListToMiddle fmts, WithPoked (TMaybe.M mn),
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd -> M.CreateInfoNew mn fmts ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	(forall s . R s -> IO a) -> IO a
createNew (Device.D dvc) ci macc@(AllocationCallbacks.toMiddle -> macd) f =
	bracket (M.createNew dvc ci macc) (\r -> M.destroy dvc r macd) (f . R)
