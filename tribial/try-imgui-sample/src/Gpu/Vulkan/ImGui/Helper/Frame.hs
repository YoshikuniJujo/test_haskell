{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.Frame (FC(..), fcToMiddle, fcFromMiddle) where

import Text.Show.ToolsYj

import Gpu.Vulkan.CommandPool.Type qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer.Type qualified as Vk.CmdBffr
import Gpu.Vulkan.Fence.Internal qualified as Vk.Fnc
import Gpu.Vulkan.Image.Internal qualified as Vk.Img
import Gpu.Vulkan.ImageView.Type qualified as Vk.ImgVw
import Gpu.Vulkan.Framebuffer.Type qualified as Vk.Frmbffr

import Gpu.Vulkan.ImGui.Helper.Frame.Middle qualified as M

data FC bnm bfmt bvnm bvfmt scp scb sf sbm sbi sbvi sfb = FC {
	fCCommandPool :: Vk.CmdPl.C scp,
	fCCommandBuffer :: Vk.CmdBffr.C scb,
	fCFence :: Vk.Fnc.F sf,
	fCBackbuffer :: Vk.Img.Binded sbm sbi bnm bfmt,
	fCBackbufferView :: Vk.ImgVw.I bvnm bvfmt sbvi,
	fCFramebuffer :: Vk.Frmbffr.F sfb }

instance ShowIO (FC bnm bfmt bvnm bvfmt scp scb sf sbm sbi sbvi sfb) where
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

fcToMiddle :: FC bnm bfmt bvnm bvfmt scp scb sf sbm sbi sbvi sfb -> M.FC
fcToMiddle FC {
	fCCommandPool = Vk.CmdPl.C cp,
	fCCommandBuffer = Vk.CmdBffr.C cb,
	fCFence = Vk.Fnc.F fnc,
	fCBackbuffer = Vk.Img.Binded bb,
	fCBackbufferView = Vk.ImgVw.I bbv,
	fCFramebuffer = Vk.Frmbffr.F fb } = M.FC {
	M.fCCommandPool = cp,
	M.fCCommandBuffer = cb,
	M.fCFence = fnc,
	M.fCBackbuffer = bb,
	M.fCBackbufferView = bbv,
	M.fCFramebuffer = fb }

fcFromMiddle :: M.FC -> FC bnm bfmt bvnm bvfmt scp scb sf sbm sbi sbvi sfb
fcFromMiddle M.FC {
	M.fCCommandPool = cp,
	M.fCCommandBuffer = cb,
	M.fCFence = fnc,
	M.fCBackbuffer = bb,
	M.fCBackbufferView = bbv,
	M.fCFramebuffer = fb } = FC {
	fCCommandPool = Vk.CmdPl.C cp,
	fCCommandBuffer = Vk.CmdBffr.C cb,
	fCFence = Vk.Fnc.F fnc,
	fCBackbuffer = Vk.Img.Binded bb,
	fCBackbufferView = Vk.ImgVw.I bbv,
	fCFramebuffer = Vk.Frmbffr.F fb }
