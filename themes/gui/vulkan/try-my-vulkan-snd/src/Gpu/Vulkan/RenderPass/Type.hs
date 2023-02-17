{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.RenderPass.Type where

import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:*), pattern (:**))

import Gpu.Vulkan.Middle
import Gpu.Vulkan.Core

import qualified Gpu.Vulkan.Framebuffer.Type as Framebuffer
import qualified Gpu.Vulkan.RenderPass.Middle as M

newtype R s = R M.R deriving Show

data BeginInfoNew n sr fmt sf cts = BeginInfoNew {
	beginInfoNextNew :: Maybe n,
	beginInfoRenderPassNew :: R sr,
	beginInfoFramebufferNew :: Framebuffer.FNew fmt sf,
	beginInfoRenderAreaNew :: Rect2d,
	beginInfoClearValuesNew :: HeteroParList.PL ClearValue cts }

beginInfoToMiddleNew :: BeginInfoNew n sr fmt sf cts -> M.BeginInfo n cts
beginInfoToMiddleNew BeginInfoNew {
	beginInfoNextNew = mnxt,
	beginInfoRenderPassNew = R rp,
	beginInfoFramebufferNew = Framebuffer.FNew fb,
	beginInfoRenderAreaNew = ra,
	beginInfoClearValuesNew = cvs } = M.BeginInfo {
		M.beginInfoNext = mnxt,
		M.beginInfoRenderPass = rp,
		M.beginInfoFramebuffer = fb,
		M.beginInfoRenderArea = ra,
		M.beginInfoClearValues = cvs }

data BeginInfo n sr sf cts = BeginInfo {
	beginInfoNext :: Maybe n,
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
