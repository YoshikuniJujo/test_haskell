{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Framebuffer (F, create, CreateInfo(..)) where

import Foreign.Pointable
import Control.Exception
import Data.Word

import Vulkan.Framebuffer.Enum

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device.Type as Device
import qualified Vulkan.RenderPass.Type as RenderPass
import qualified Vulkan.ImageView as ImageView
import qualified Vulkan.Framebuffer.Middle as M

newtype F s = F M.F deriving Show

data CreateInfo n sr si = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoRenderPass :: RenderPass.R sr,
	createInfoAttachments :: [ImageView.I si],
	createInfoWidth :: Word32,
	createInfoHeight :: Word32,
	createInfoLayers :: Word32 }
	deriving Show

createInfoToMiddle :: CreateInfo n sr si -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoRenderPass = RenderPass.R rp,
	createInfoAttachments = ((\(ImageView.I iv) -> iv) <$>) -> ivs,
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
