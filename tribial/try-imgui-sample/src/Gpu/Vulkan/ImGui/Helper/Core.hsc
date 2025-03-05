{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.Core where

#include <vulkan/vulkan.h>
#include "imgui_impl_vulkan_helper_c.h"

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Gpu.Vulkan.CommandPool.Core qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer.Core qualified as Vk.CmdBffr
import Gpu.Vulkan.Image.Core qualified as Vk.Img
import Gpu.Vulkan.ImageView.Core qualified as Vk.ImgVw
import Gpu.Vulkan.Fence.Core qualified as Vk.Fnc
import Gpu.Vulkan.Framebuffer.Core qualified as Vk.Frmbffr

struct "ImGuiImplVulkanHFrameC" #{size ImGui_ImplVulkanH_Frame_C}
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
	("BackbufferView", ''Vk.ImgVw.I,
		[| #{peek ImGui_ImplVulkanH_Frame_C, BackbufferView} |],
		[| #{poke ImGui_ImplVulkanH_Frame_C, BackbufferView} |]),
	("Framebuffer", ''Vk.Frmbffr.F,
		[| #{peek ImGui_ImplVulkanH_Frame_C, Framebuffer} |],
		[| #{poke ImGui_ImplVulkanH_Frame_C, Framebuffer} |]) ]
	[''Show, ''Storable]

foreign import ccall "copyImguiImplVulkanHFrameC"
	cxx_copyImguiImplVulkanHFrameC ::
		Ptr ImGuiImplVulkanHFrameC -> IO (Ptr ImGuiImplVulkanHFrameC)

foreign import ccall "freeImguiImplVulkanHFrameC"
	cxx_freeImguiImplVulkanHFrameC :: Ptr ImGuiImplVulkanHFrameC -> IO ()

structPrim "ImGuiImplVulkanHFrameC"
	'cxx_copyImguiImplVulkanHFrameC 'cxx_freeImguiImplVulkanHFrameC [''Show]

data ImGuiImplVulkanHFrameTag

foreign import ccall "sizeOfImguiImplVulkanHFrame"
	cxx_sizeOfImguiImplVulkanHFrame :: #{type size_t}

foreign import ccall "alignofImguiImplVulkanHFrame"
	cxx_alignOfImguiImplVulkanHFrame :: #{type size_t}

foreign import ccall "imguiImplVulkanHFrameFromC"
	cxx_imguiImplVulkanHFrameFromC ::
		Ptr ImGuiImplVulkanHFrameC -> Ptr ImGuiImplVulkanHFrameTag ->
		IO ()

foreign import ccall "imguiImplVulkanHFrameToC"
	cxx_imguiImplVulkanHFrameToC ::
		Ptr ImGuiImplVulkanHFrameTag -> Ptr ImGuiImplVulkanHFrameC ->
		IO ()
