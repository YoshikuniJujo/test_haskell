{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.Window.Core where

import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Gpu.Vulkan.Pipeline.Core qualified as Vk.Ppl
import Gpu.Vulkan.RenderPass.Core qualified as Vk.RndrPss

import Gpu.Vulkan.Khr.Swapchain.Core qualified as Vk.Swpch
import Gpu.Vulkan.Khr.Surface.Core qualified as Vk.Sfc

#include "imgui_impl_vulkan_helper_c.h"

struct "WC" #{size ImGui_ImplVulkanH_Window_C}
	#{alignment ImGui_ImplVulkanH_Window_C} [
	("Width", ''Int,
		[| #{peek ImGui_ImplVulkanH_Window_C, Width} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, Width} |]),
	("Height", ''Int,
		[| #{peek ImGui_ImplVulkanH_Window_C, Height} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, Height} |]),
	("Swapchain", ''Vk.Swpch.S,
		[| #{peek ImGui_ImplVulkanH_Window_C, Swapchain} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, Swapchain} |]),
	("Surface", ''Vk.Sfc.S,
		[| #{peek ImGui_ImplVulkanH_Window_C, Surface} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, Surface} |]),
	("SurfaceFormat", ''Vk.Sfc.Format,
		[| #{peek ImGui_ImplVulkanH_Window_C, SurfaceFormat} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, SurfaceFormat} |]),
	("PresentMode", ''#{type VkPresentModeKHR},
		[| #{peek ImGui_ImplVulkanH_Window_C, PresentMode} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, PresentMode} |]),
	("RenderPass", ''Vk.RndrPss.R,
		[| #{peek ImGui_ImplVulkanH_Window_C, RenderPass} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, RenderPass} |]),
	("Pipeline", ''Vk.Ppl.P,
		[| #{peek ImGui_ImplVulkanH_Window_C, Pipeline} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, Pipeline} |])
	]
	[''Show, ''Storable]
