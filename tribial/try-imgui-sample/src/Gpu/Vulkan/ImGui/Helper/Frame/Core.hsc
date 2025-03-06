{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.Frame.Core (

	-- * DATA TYPE

	FC, PtrFC, pattern FC,
	fCCommandPool, fCCommandBuffer, fCFence,
	fCBackbuffer, fCBackbufferView, fCFramebuffer,

	-- * MUTABLE

	FCPrim, FCST, FCIO,

	fCFreeze, fCThaw, fCCopy,

	-- * CXX TO/FROM C

	F(..), FTag, toC, fromC

	) where

#include "imgui_impl_vulkan_helper_c.h"

import Foreign.Ptr
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.C.Struct
import Control.Monad.Primitive
import Data.Word

import Gpu.Vulkan.CommandPool.Core qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer.Core qualified as Vk.CmdBffr
import Gpu.Vulkan.Image.Core qualified as Vk.Img
import Gpu.Vulkan.Fence.Core qualified as Vk.Fnc
import Gpu.Vulkan.Framebuffer.Core qualified as Vk.Frmbffr

struct "FC" #{size ImGui_ImplVulkanH_Frame_C}
	#{alignment ImGui_ImplVulkanH_Frame_C} [
	("CommandPool", ''Vk.CmdPl.C,
		[| #{peek ImGui_ImplVulkanH_Frame_C, CommandPool} |],
		[| #{poke ImGui_ImplVulkanH_Frame_C, CommandPool} |]),
	("CommandBuffer", ''Vk.CmdBffr.C,
		[| #{peek ImGui_ImplVulkanH_Frame_C, CommandBuffer} |],
		[| #{poke ImGui_ImplVulkanH_Frame_C, CommandBuffer} |]),
	("Fence", ''Vk.Fnc.F,
		[| #{peek ImGui_ImplVulkanH_Frame_C, Fence} |],
		[| #{poke ImGui_ImplVulkanH_Frame_C, Fence} |]),
	("Backbuffer", ''Vk.Img.I,
		[| #{peek ImGui_ImplVulkanH_Frame_C, Backbuffer} |],
		[| #{poke ImGui_ImplVulkanH_Frame_C, Backbuffer} |]),
	("BackbufferView", ''Vk.Img.I,
		[| #{peek ImGui_ImplVulkanH_Frame_C, BackbufferView} |],
		[| #{poke ImGui_ImplVulkanH_Frame_C, BackbufferView} |]),
	("Framebuffer", ''Vk.Frmbffr.F,
		[| #{peek ImGui_ImplVulkanH_Frame_C, Framebuffer} |],
		[| #{poke ImGui_ImplVulkanH_Frame_C, Framebuffer} |]) ]
	[''Show, ''Storable]

type PtrFC = Ptr FC

foreign import ccall "copyImguiImplVulkanHFrameC"
	cxx_copyImguiImplVulkanHFrameC :: Ptr FC -> IO (Ptr FC)

foreign import ccall "freeImguiImplVulkanHFrameC"
	cxx_freeImguiImplVulkanHFrameC :: Ptr FC -> IO ()

structPrim "FC"
	'cxx_copyImguiImplVulkanHFrameC 'cxx_freeImguiImplVulkanHFrameC [''Show]

toC :: PrimMonad m => F -> m (FCPrim (PrimState m))
toC (F pf) = unsafeIOToPrim $ alloca \pfc -> do
	cxx_imguiImplVulkanHFrameToC pf pfc
	FCPrim <$> newForeignPtr pfc (free pfc)

fromC :: FCPrim s -> (F -> IO a) -> IO a
fromC (FCPrim ffc) a =
	allocaBytesAligned
		(fromIntegral cxx_sizeofImguiImplVulkanHFrame)
		(fromIntegral cxx_alignofImguiImplVulkanHFrame) \pf ->
	withForeignPtr ffc \pfc -> do
	cxx_imguiImplVulkanHFrameFromC pfc pf
	a $ F pf

newtype F = F (Ptr FTag) deriving Show
data FTag

foreign import ccall "sizeOfImguiImplVulkanHFrame"
	cxx_sizeofImguiImplVulkanHFrame :: #{type size_t}

foreign import ccall "alignofImguiImplVulkanHFrame"
	cxx_alignofImguiImplVulkanHFrame :: #{type size_t}

foreign import ccall "imguiImplVulkanHFrameFromC"
	cxx_imguiImplVulkanHFrameFromC :: Ptr FC -> Ptr FTag -> IO ()

foreign import ccall "imguiImplVulkanHFrameToC"
	cxx_imguiImplVulkanHFrameToC :: Ptr FTag -> Ptr FC -> IO ()
