{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Framebuffer where

import Vulkan.Framebuffer.Enum

import qualified Vulkan.RenderPass as RenderPass

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoRenderPass :: RenderPass.R
--	createInfoAttachments :: [ImageView.I]
	}
	deriving Show
