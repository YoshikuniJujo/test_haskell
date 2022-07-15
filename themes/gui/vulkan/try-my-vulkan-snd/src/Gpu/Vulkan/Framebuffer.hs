{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Framebuffer (F, create, recreate, CreateInfo(..)
	, createInfoToMiddle -- <-- temporary
	) where

import Foreign.Pointable
import Control.Exception
import Data.HeteroList
import Data.Word

import Gpu.Vulkan.Framebuffer.Enum
import Gpu.Vulkan.Framebuffer.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.RenderPass.Type as RenderPass
import qualified Gpu.Vulkan.ImageView as ImageView
import qualified Gpu.Vulkan.Framebuffer.Middle as M

data CreateInfo n sr sis = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoRenderPass :: RenderPass.R sr,
	createInfoAttachments :: HeteroVarList ImageView.I sis,
	createInfoWidth :: Word32,
	createInfoHeight :: Word32,
	createInfoLayers :: Word32 }

deriving instance (Show n, Show (HeteroVarList ImageView.I sis)) =>
	Show (CreateInfo n sr sis)

createInfoToMiddle :: CreateInfo n sr si -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoRenderPass = RenderPass.R rp,
	createInfoAttachments = heteroVarListToList (\(ImageView.I iv) -> iv) -> ivs,
	createInfoWidth = w,
	createInfoHeight = h,
	createInfoLayers = lyrs } = M.CreateInfo {
		M.createInfoNext = mnxt,
		M.createInfoFlags = flgs,
		M.createInfoRenderPass = rp,
		M.createInfoAttachments = ivs,
		M.createInfoWidth = w,
		M.createInfoHeight = h,
		M.createInfoLayers = lyrs }

create :: (Pointable n, Pointable c, Pointable d) =>
	Device.D sd -> CreateInfo n sr si ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . F s -> IO a) -> IO a
create (Device.D dvc) ci macc macd f = bracket
	(M.create dvc (createInfoToMiddle ci) macc)
	(\fb -> M.destroy dvc fb macd) (f . F)

recreate :: (Pointable n, Pointable c, Pointable d) =>
	Device.D sd -> CreateInfo n sr si ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	F sf -> IO ()
recreate (Device.D dvc) ci macc macd (F fb) =
	M.recreate dvc (createInfoToMiddle ci) macc macd fb
