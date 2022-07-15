{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.RenderPass.Type where

import Gpu.Vulkan.Middle
import Gpu.Vulkan.Core

import qualified Gpu.Vulkan.Framebuffer.Type as Framebuffer
import qualified Gpu.Vulkan.RenderPass.Middle as M

newtype R s = R M.R deriving Show

data BeginInfo n sr sf ct = BeginInfo {
	beginInfoNext :: Maybe n,
	beginInfoRenderPass :: R sr,
	beginInfoFramebuffer :: Framebuffer.F sf,
	beginInfoRenderArea :: Rect2d,
	beginInfoClearValues :: [ClearValue ct] }

beginInfoToMiddle :: BeginInfo n sr sf ct -> M.BeginInfo n ct
beginInfoToMiddle BeginInfo {
	beginInfoNext = mnxt,
	beginInfoRenderPass = R rp,
	beginInfoFramebuffer = Framebuffer.F fb,
	beginInfoRenderArea = ra,
	beginInfoClearValues = cvs } = M.BeginInfo {
		M.beginInfoNext = mnxt,
		M.beginInfoRenderPass = rp,
		M.beginInfoFramebuffer = fb,
		M.beginInfoRenderArea = ra,
		M.beginInfoClearValues = cvs }
