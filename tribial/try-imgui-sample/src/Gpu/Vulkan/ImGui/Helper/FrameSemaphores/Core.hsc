{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.FrameSemaphores.Core (

	-- * DATA TYPE

	FC, PtrFC, pattern FC,
	fCImageAcquiredSemaphore, fCRenderCompleteSemaphore,

	-- * CXX TO/FROM C

	F(..), FTag

	) where

#include "imgui_impl_vulkan_helper_c.h"

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct

import Gpu.Vulkan.Semaphore.Core qualified as Vk.Smph

struct "FC" #{size ImGui_ImplVulkanH_FrameSemaphores_C}
	#{alignment ImGui_ImplVulkanH_FrameSemaphores_C} [
	("ImageAcquiredSemaphore", ''Vk.Smph.S,
		[| #{peek ImGui_ImplVulkanH_FrameSemaphores_C,
			ImageAcquiredSemaphore} |],
		[| #{poke ImGui_ImplVulkanH_FrameSemaphores_C,
			ImageAcquiredSemaphore} |]),
	("RenderCompleteSemaphore", ''Vk.Smph.S,
		[| #{peek ImGui_ImplVulkanH_FrameSemaphores_C,
			RenderCompleteSemaphore} |],
		[| #{poke ImGui_ImplVulkanH_FrameSemaphores_C,
			RenderCompleteSemaphore} |])
	]
	[''Show, ''Storable]

type PtrFC = Ptr FC

newtype F = F (Ptr FTag) deriving Show
data FTag
