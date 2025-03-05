{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.Window.Core where

import Foreign.Storable
import Foreign.C.Struct

#include "imgui_impl_vulkan_helper_c.h"

struct "WC" #{size ImGui_ImplVulkanH_Window_C}
	#{alignment ImGui_ImplVulkanH_Window_C} [
	("Width", ''Int,
		[| #{peek ImGui_ImplVulkanH_Window_C, Width} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, Width} |]),
	("Height", ''Int,
		[| #{peek ImGui_ImplVulkanH_Window_C, Height} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, Height} |])
	]
	[''Show, ''Storable]
