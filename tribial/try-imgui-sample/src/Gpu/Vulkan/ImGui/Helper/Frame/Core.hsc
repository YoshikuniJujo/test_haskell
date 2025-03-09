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

	) where

#include "imgui_impl_vulkan_helper_c.h"

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct

import Gpu.Vulkan.CommandPool.Core qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer.Core qualified as Vk.CmdBffr
import Gpu.Vulkan.Image.Core qualified as Vk.Img
import Gpu.Vulkan.ImageView.Core qualified as Vk.ImgVw
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
	("BackbufferView", ''Vk.ImgVw.I,
		[| #{peek ImGui_ImplVulkanH_Frame_C, BackbufferView} |],
		[| #{poke ImGui_ImplVulkanH_Frame_C, BackbufferView} |]),
	("Framebuffer", ''Vk.Frmbffr.F,
		[| #{peek ImGui_ImplVulkanH_Frame_C, Framebuffer} |],
		[| #{poke ImGui_ImplVulkanH_Frame_C, Framebuffer} |]) ]
	[''Show, ''Storable]

type PtrFC = Ptr FC
