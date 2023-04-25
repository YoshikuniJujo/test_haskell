{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.RenderPass.Type where

import Data.TypeLevel.Maybe qualified as TMaybe
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:*), pattern (:**))

import Gpu.Vulkan.Middle

import qualified Gpu.Vulkan.Framebuffer.Type as Framebuffer
import qualified Gpu.Vulkan.RenderPass.Middle as M

newtype R s = R M.R deriving Show

data BeginInfo mn sr sf cts = BeginInfo {
	beginInfoNext :: TMaybe.M mn,
	beginInfoRenderPass :: R sr,
	beginInfoFramebuffer :: Framebuffer.F sf,
	beginInfoRenderArea :: Rect2d,
	beginInfoClearValues :: HeteroParList.PL ClearValue cts }

beginInfoToMiddle :: BeginInfo n sr sf cts -> M.BeginInfo n cts
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
