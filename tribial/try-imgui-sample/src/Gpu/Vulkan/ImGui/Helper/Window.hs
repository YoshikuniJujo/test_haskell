{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.Window where

import Data.TypeLevel.Tuple.Uncurry
import Data.List qualified as L
import Data.Word
import Data.Int
import Data.HeteroParList qualified as HPList
import Text.Show.ToolsYj

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Pipeline.Graphics.Type qualified as Vk.Ppl.Gr
import Gpu.Vulkan.RenderPass.Type qualified as Vk.RndrPss

import Gpu.Vulkan.Khr.Swapchain.Type qualified as Vk.Swpch
import Gpu.Vulkan.Khr.Surface qualified as Vk.Sfc
import Gpu.Vulkan.Khr.Surface.Internal qualified as Vk.Sfc

import Gpu.Vulkan.ImGui.Helper.Frame qualified as Frame
import Gpu.Vulkan.ImGui.Helper.FrameSemaphores qualified as FrameSemaphores

import Gpu.Vulkan.ImGui.Helper.Window.Middle qualified as M

data WC scfmt ssc ssfc srp sppl vibs vias lyta ct
		bnm bfmt bvnm bvfmt fras frsmas = WC {
	wCWidth :: Int32, wCHeight :: Int32,
	wCSwapchain :: Vk.Swpch.S scfmt ssc,
	wCSurface :: Vk.Sfc.S ssfc,
	wCSurfaceFormat :: Vk.Sfc.Format scfmt,
	wCPresentMode :: Vk.Sfc.PresentMode,
	wCRenderPass :: Vk.RndrPss.R srp,
	wCPipeline :: Vk.Ppl.Gr.G sppl vibs vias lyta,
	wCUseDynamicRendering :: Bool,
	wCClearEnable :: Bool,
	wCClearValue :: Vk.ClearValue ct,
	wCFrameIndex :: Word32,
	wCImageCount :: Word32,
	wCSemaphoreCount :: Word32,
	wCSemaphoreIndex :: Word32,
	wCFrames :: HPList.PL (U7 (Frame.FC bnm bfmt bvnm bvfmt)) fras,
	wCFrameSemaphores :: HPList.PL (U2 FrameSemaphores.FC) frsmas }

instance Vk.T.FormatToValue scfmt => ShowIO
	(WC scfmt ssc ssfc srp sppl vibs vias lyta ct
		bnm bfmt bvnm bvfmt fras frsmas) where
	showIO wc = do
		sppl <- showIO $ wCPipeline wc
		sfrs <- sequence $ HPList.toList showIO (wCFrames wc)
		sfrsms <- sequence $ HPList.toList showIO (wCFrameSemaphores wc)
		pure $ "WC { " ++
			"wCWidth = " ++ show (wCWidth wc) ++ ", " ++
			"wCHeight = " ++ show (wCHeight wc) ++ ", " ++
			"wCSwapchain = " ++ show (wCSwapchain wc) ++ ", " ++
			"wCSurface = " ++ show (wCSurface wc) ++ ", " ++
			"wCSurfaceFormat = " ++
				show (wCSurfaceFormat wc) ++ ", " ++
			"wCPresentMode = " ++ show (wCPresentMode wc) ++ ", " ++
			"wCRenderPass = " ++ show (wCRenderPass wc) ++ ", " ++
			"wCPipeline = " ++ sppl ++ ", " ++
			"wCUseDynamicRendering = " ++
				show (wCUseDynamicRendering wc) ++ ", " ++
			"wCClearEnable = " ++ show (wCClearEnable wc) ++ ", " ++
			"wCClearValue = " ++ show (wCClearValue wc) ++ ", " ++
			"wCFrameIndex = " ++ show (wCFrameIndex wc) ++ ", " ++
			"wCImageCount = " ++ show (wCImageCount wc) ++ ", " ++
			"wCSemaphoreCount = " ++
				show (wCSemaphoreCount wc) ++ ", " ++
			"wCSemaphoreIndex = " ++
				show (wCSemaphoreIndex wc) ++ ", " ++
			"wCFrame = "  ++
				L.intercalate " :** " sfrs ++ "HPList.Nil, " ++
			"wCFrameSemaphores = " ++
				L.intercalate " :** " sfrsms ++ "HPList.Nil }"

wCToMiddle :: WC scfmt ssc ssfc srp sppl vibs vias lyta ct
		bnm bfmt bvnm bvfmt fras frsmas -> M.WC ct
wCToMiddle WC {
	} = M.WC {
	}
