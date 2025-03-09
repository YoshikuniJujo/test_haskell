{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.Frame.Middle (

	-- * DATA TYPE

	FC(..),

	-- * TO/FROM CORE

	fcToCore, fcFromCore
	
	) where

import Foreign.Ptr
import Data.IORef
import Text.Show.ToolsYj

import Gpu.Vulkan.Middle qualified as Vk
import Gpu.Vulkan.CommandPool.Middle.Internal qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer.Middle.Internal qualified as Vk.CmdBffr
import Gpu.Vulkan.Fence.Middle.Internal qualified as Vk.Fnc
import Gpu.Vulkan.Image.Middle.Internal qualified as Vk.Img
import Gpu.Vulkan.ImageView.Middle.Internal qualified as Vk.ImgVw
import Gpu.Vulkan.Framebuffer.Middle.Internal qualified as Vk.Frmbffr

import Gpu.Vulkan.ImGui.Helper.Frame.Core qualified as C

data FC = FC {
	fCCommandPool :: Vk.CmdPl.C,
	fCCommandBuffer :: Vk.CmdBffr.C,
	fCFence :: Vk.Fnc.F,
	fCBackbuffer :: Vk.Img.I,
	fCBackbufferView :: Vk.ImgVw.I,
	fCFramebuffer :: Vk.Frmbffr.F }

instance ShowIO FC where
	showIO fc = do
		sb <- showIO $ fCBackbuffer fc
		sbv <- showIO $ fCBackbufferView fc
		sf <- showIO $ fCFramebuffer fc
		pure $	"FC { fCCommandPool = " ++ show (fCCommandPool fc) ++
			", fCCommandBuffer = " ++ show (fCCommandBuffer fc) ++
			", fCFence = " ++ show (fCFence fc) ++
			", fCBackbuffer = " ++ sb ++
			", fCBackbufferView = " ++ sbv ++
			", fCFramebuffer = " ++ sf ++ " }"

fcToCore :: FC -> IO C.FC
fcToCore FC {
	fCCommandPool = Vk.CmdPl.C cp,
	fCCommandBuffer = Vk.CmdBffr.C _ cb,
	fCFence = Vk.Fnc.F f,
	fCBackbuffer = Vk.Img.I rbb,
	fCBackbufferView = Vk.ImgVw.I rbbv,
	fCFramebuffer = Vk.Frmbffr.F rfb } = do
	(_, bb) <- readIORef rbb
	bbv <- readIORef rbbv
	fb <- readIORef rfb
	pure C.FC {
		C.fCCommandPool = cp,
		C.fCCommandBuffer = cb,
		C.fCFence = f,
		C.fCBackbuffer = bb,
		C.fCBackbufferView = bbv,
		C.fCFramebuffer = fb }

fcFromCore :: C.FC -> IO FC
fcFromCore C.FC {
	C.fCCommandPool = cp,
	C.fCCommandBuffer = cb,
	C.fCFence = fnc,
	C.fCBackbuffer = bb,
	C.fCBackbufferView = bbv,
	C.fCFramebuffer = fb
	} = do
	rppl <- newIORef nullPtr
	rbb <- newIORef (Vk.Extent3d 0 0 0, bb)
	rbbv <- newIORef bbv
	rfb <- newIORef fb
	pure FC {
		fCCommandPool = Vk.CmdPl.C cp,
		fCCommandBuffer = Vk.CmdBffr.C rppl cb,
		fCFence = Vk.Fnc.F fnc,
		fCBackbuffer = Vk.Img.I rbb,
		fCBackbufferView = Vk.ImgVw.I rbbv,
		fCFramebuffer = Vk.Frmbffr.F rfb }
