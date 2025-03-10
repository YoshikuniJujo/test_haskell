{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.Window where

import Data.TypeLevel.Tuple.Uncurry
import Data.Word
import Data.Int
import Data.HeteroParList qualified as HPList

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.Pipeline.Graphics.Type qualified as Vk.Ppl.Gr
import Gpu.Vulkan.RenderPass.Type qualified as Vk.RndrPss

import Gpu.Vulkan.Khr.Swapchain.Type qualified as Vk.Swpch
import Gpu.Vulkan.Khr.Surface qualified as Vk.Sfc
import Gpu.Vulkan.Khr.Surface.Internal qualified as Vk.Sfc

import Gpu.Vulkan.ImGui.Helper.Frame qualified as Frame
import Gpu.Vulkan.ImGui.Helper.FrameSemaphores qualified as FrameSemaphores

data WC scfmt ssc ssfc srp sppl vibs vias lyta ct
		bnm bfmt bvnm bvfmt fras frsmas = WC {
	wCWidth :: Int32, wCHeight :: Int32,
	wCSwapchain :: Vk.Swpch.S scfmt ssc,
	wCSurface :: Vk.Sfc.S ssfc,
	wCSurfaceFormat :: Vk.Sfc.Format scfmt,
	wCPresentMode :: Vk.Sfc.PresentMode,
	wCRenderPasss :: Vk.RndrPss.R srp,
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

-- instance ShowIO
