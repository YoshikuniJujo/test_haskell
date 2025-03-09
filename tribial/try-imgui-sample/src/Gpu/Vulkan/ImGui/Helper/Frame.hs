{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.Frame where

import Text.Show.ToolsYj

import Gpu.Vulkan.CommandPool.Type qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer.Type qualified as Vk.CmdBffr
import Gpu.Vulkan.Fence.Internal qualified as Vk.Fnc
import Gpu.Vulkan.Image.Internal qualified as Vk.Img
import Gpu.Vulkan.ImageView.Type qualified as Vk.ImgVw
import Gpu.Vulkan.Framebuffer.Type qualified as Vk.Frmbffr

data FC scp scb sf sbm sbi bnm bfmt bvnm bvfmt sbvi sfb = FC {
	fCCommandPool :: Vk.CmdPl.C scp,
	fCCommandBuffer :: Vk.CmdBffr.C scb,
	fCFence :: Vk.Fnc.F sf,
	fCBackbuffer :: Vk.Img.Binded sbm sbi bnm bfmt,
	fCBackbufferView :: Vk.ImgVw.I bvnm bvfmt sbvi,
	fCFramebuffer :: Vk.Frmbffr.F sfb }

instance ShowIO (FC scp scb sf sbm sbi bnm bfmt bvnm bvfmt sbvi sfb) where
	showIO FC {
		fCCommandPool = cp,
		fCCommandBuffer = cb,
		fCFence = fnc,
		fCBackbuffer = bb,
		fCBackbufferView = bbv,
		fCFramebuffer = fb
		} = do
		sbb <- showIO bb
		sbbv <- showIO bbv
		sfb <- showIO fb
		pure $ "FC { " ++
			"fCCommandPool = " ++ show cp ++ ", " ++
			"fCCommandBuffer = " ++ show cb ++ ", " ++
			"fCFence = " ++ show fnc ++ ", " ++
			"fCBackbuffer = " ++ sbb ++ ", " ++
			"fCBackbufferView = " ++ sbbv ++ ", " ++
			"fCFramebuffer = " ++ sfb ++ " }"
