{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.Frame (

	FC(..), fCToMiddle, fCFromMiddle, FCListFromMiddle(..), fCListFromMiddle'

	) where

import Data.TypeLevel.Tuple.Uncurry
import Data.HeteroParList (pattern (:**))
import Data.HeteroParList qualified as HPList
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

instance ShowIO (U7 (FC bnm bfmt bvnm bvfmt) fras) where
	showIO (U7 f) = do
		sf <- showIO f
		pure $ "(U7 " ++ sf ++ ")"

fCToMiddle :: FC bnm bfmt bvnm bvfmt scp scb sf sbm sbi sbvi sfb -> M.FC
fCToMiddle FC {
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

fCFromMiddle :: M.FC -> FC bnm bfmt bvnm bvfmt scp scb sf sbm sbi sbvi sfb
fCFromMiddle M.FC {
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

class FCListFromMiddle ss where
	fCListFromMiddle :: [M.FC] -> HPList.PL (U7 (FC bnm bfmt bvnm bvfmt)) ss

instance FCListFromMiddle '[] where
	fCListFromMiddle [] = HPList.Nil
	fCListFromMiddle _ = error "bad"

instance FCListFromMiddle ss => FCListFromMiddle ('(s1, s2, s3, s4, s5, s6, s7) ': ss) where
	fCListFromMiddle (f : fs) = U7 (fCFromMiddle f) :** fCListFromMiddle fs
	fCListFromMiddle _ = error "bad"

fCListFromMiddle' ::
	[M.FC] -> (forall ss . HPList.PL (U7 (FC bnm bfmt bvnm bvfmt)) ss -> a) -> a
fCListFromMiddle' [] g = g HPList.Nil
fCListFromMiddle' (f : fs) g = fCListFromMiddle' fs \fs' -> g $ U7 (fCFromMiddle f) :** fs'
