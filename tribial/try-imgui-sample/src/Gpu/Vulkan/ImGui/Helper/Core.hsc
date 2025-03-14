{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.Core (

	selectSurfaceFormat,
	selectPresentMode,
	createOrResizeWindow

	) where

import Foreign.Ptr
import Foreign.Concurrent
import Data.Word
import Data.Int

import Gpu.Vulkan.Instance.Core qualified as Vk.Ist
import Gpu.Vulkan.AllocationCallbacks.Core qualified as Vk.AllocCallbacks
import Gpu.Vulkan.PhysicalDevice.Core qualified as Vk.Phd
import Gpu.Vulkan.Device.Core qualified as Vk.Dvc

import Gpu.Vulkan.Khr.Surface.Core qualified as Vk.Sfc

import Gpu.Vulkan.ImGui.Helper.Window.Core qualified as Vk.ImGui.H.Win

#include <vulkan/vulkan.h>

selectSurfaceFormat ::
	Vk.Phd.P -> Vk.Sfc.S -> Ptr #{type VkFormat} -> #{type int} ->
	#{type VkColorSpaceKHR} -> IO Vk.Sfc.Format
selectSurfaceFormat pd sfc pfmts fmtc cs = do
	psfmt <- cxx_ImGui_ImplVulkanH_SelectSurfaceFormat pd sfc pfmts fmtc cs
	Vk.Sfc.Format_ <$> newForeignPtr psfmt (pure ())

foreign import ccall "ImGui_ImplVulkanH_SelectSurfaceFormat2"
	cxx_ImGui_ImplVulkanH_SelectSurfaceFormat ::
	Vk.Phd.P -> Vk.Sfc.S -> Ptr #{type VkFormat} -> #{type int} ->
	#{type VkColorSpaceKHR} -> IO (Ptr Vk.Sfc.Format)

selectPresentMode ::
	Vk.Phd.P -> Vk.Sfc.S -> Ptr #{type VkPresentModeKHR} -> #{type int} ->
	IO #{type VkPresentModeKHR}
selectPresentMode = cxx_ImGui_ImplVulkanH_SelectPresentMode

foreign import ccall "ImGui_ImplVulkanH_SelectPresentMode"
	cxx_ImGui_ImplVulkanH_SelectPresentMode ::
	Vk.Phd.P -> Vk.Sfc.S -> Ptr #{type VkPresentModeKHR} -> #{type int} ->
	IO #{type VkPresentModeKHR}

createOrResizeWindow ::
	Vk.Ist.I -> Vk.Phd.P -> Vk.Dvc.D -> Vk.ImGui.H.Win.W ->
	#{type uint32_t} -> Ptr Vk.AllocCallbacks.A ->
	#{type int} -> #{type int} -> #{type uint32_t} -> IO ()
createOrResizeWindow =
	cxx_ImGui_ImplVulkanH_CreateOrResizeWindow

foreign import ccall "ImGui_ImplVulkanH_CreateOrResizeWindow"
	cxx_ImGui_ImplVulkanH_CreateOrResizeWindow ::
	Vk.Ist.I -> Vk.Phd.P -> Vk.Dvc.D -> Vk.ImGui.H.Win.W ->
	#{type uint32_t} -> Ptr Vk.AllocCallbacks.A ->
	#{type int} -> #{type int} -> #{type uint32_t} -> IO ()
