{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.Core (

	imGuiImplVulkanHSelectSurfaceFormat,
	imGuiImplVulkanHSelectPresentMode

	) where

import Foreign.Ptr
import Foreign.Concurrent
import Data.Word
import Data.Int

import Gpu.Vulkan.PhysicalDevice.Core qualified as Vk.Phd

import Gpu.Vulkan.Khr.Surface.Core qualified as Vk.Sfc

#include <vulkan/vulkan.h>

imGuiImplVulkanHSelectSurfaceFormat ::
	Vk.Phd.P -> Vk.Sfc.S -> Ptr #{type VkFormat} -> #{type int} ->
	#{type VkColorSpaceKHR} -> IO Vk.Sfc.Format
imGuiImplVulkanHSelectSurfaceFormat pd sfc pfmts fmtc cs = do
	psfmt <- cxx_ImGui_ImplVulkanH_SelectSurfaceFormat pd sfc pfmts fmtc cs
	Vk.Sfc.Format_ <$> newForeignPtr psfmt (pure ())

foreign import ccall "ImGui_ImplVulkanH_SelectSurfaceFormat2"
	cxx_ImGui_ImplVulkanH_SelectSurfaceFormat ::
	Vk.Phd.P -> Vk.Sfc.S -> Ptr #{type VkFormat} -> #{type int} ->
	#{type VkColorSpaceKHR} -> IO (Ptr Vk.Sfc.Format)

imGuiImplVulkanHSelectPresentMode ::
	Vk.Phd.P -> Vk.Sfc.S -> Ptr #{type VkPresentModeKHR} -> #{type int} ->
	IO #{type VkPresentModeKHR}
imGuiImplVulkanHSelectPresentMode = cxx_ImGui_ImplVulkanH_SelectPresentMode

foreign import ccall "ImGui_ImplVulkanH_SelectPresentMode"
	cxx_ImGui_ImplVulkanH_SelectPresentMode ::
	Vk.Phd.P -> Vk.Sfc.S -> Ptr #{type VkPresentModeKHR} -> #{type int} ->
	IO #{type VkPresentModeKHR}
