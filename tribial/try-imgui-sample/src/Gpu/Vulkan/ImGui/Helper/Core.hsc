{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.Core (

	selectSurfaceFormat,
	selectPresentMode,
	createWindowSwapChain, createWindowCommandBuffers,

	destroyBeforeCreateSwapChain,

	createSwapChain, onlyCreateSwapChain,

	onlyCreateSwapChainNoWd, copySwapChainToWd, setSize,

	createSwapChainModifyWd,

	createWindowRenderPass, createWindowImageViews, createWindowFramebuffer

	) where

import Foreign.Ptr
import Foreign.Concurrent
import Data.Word
import Data.Int

import Gpu.Vulkan.AllocationCallbacks.Core qualified as Vk.AllocCallbacks
import Gpu.Vulkan.PhysicalDevice.Core qualified as Vk.Phd
import Gpu.Vulkan.Device.Core qualified as Vk.Dvc
import Gpu.Vulkan.Image.Core qualified as Vk.Img

import Gpu.Vulkan.Khr.Swapchain.Core qualified as Vk.Swpch
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

createWindowSwapChain ::
	Vk.Dvc.D -> Vk.ImGui.H.Win.W ->
	Ptr Vk.AllocCallbacks.A ->
	#{type int} -> #{type int} -> #{type uint32_t} -> Vk.Swpch.S -> IO ()
createWindowSwapChain = cxx_im_gui_impl_vulkan_h_create_window_swap_chain

foreign import ccall "im_gui_impl_vulkan_h_create_window_swap_chain"
	cxx_im_gui_impl_vulkan_h_create_window_swap_chain ::
	Vk.Dvc.D -> Vk.ImGui.H.Win.W ->
	Ptr Vk.AllocCallbacks.A ->
	#{type int} -> #{type int} -> #{type uint32_t} -> Vk.Swpch.S -> IO ()

destroyBeforeCreateSwapChain ::
	Vk.Dvc.D -> Vk.ImGui.H.Win.W -> Ptr Vk.AllocCallbacks.A -> IO ()
destroyBeforeCreateSwapChain =
	cxx_im_gui_impl_vulkan_h_destroy_before_create_swap_chain

foreign import ccall "im_gui_impl_vulkan_h_destroy_before_create_swap_chain"
	cxx_im_gui_impl_vulkan_h_destroy_before_create_swap_chain ::
	Vk.Dvc.D -> Vk.ImGui.H.Win.W -> Ptr Vk.AllocCallbacks.A -> IO ()

createWindowCommandBuffers ::
	Vk.Phd.P -> Vk.Dvc.D -> Vk.ImGui.H.Win.W -> #{type uint32_t} ->
	Ptr Vk.AllocCallbacks.A -> IO ()
createWindowCommandBuffers =
	cxx_im_gui_impl_vulkan_h_create_window_command_buffers

foreign import ccall "im_gui_impl_vulkan_h_create_window_command_buffers"
	cxx_im_gui_impl_vulkan_h_create_window_command_buffers ::
	Vk.Phd.P -> Vk.Dvc.D -> Vk.ImGui.H.Win.W -> #{type uint32_t} ->
	Ptr Vk.AllocCallbacks.A -> IO ()

createSwapChain ::
	Vk.Dvc.D -> Vk.ImGui.H.Win.W -> #{type uint32_t} -> IO ()
createSwapChain = cxx_im_gui_impl_vulkan_h_create_swap_chain

foreign import ccall "im_gui_impl_vulkan_h_create_swap_chain"
	cxx_im_gui_impl_vulkan_h_create_swap_chain ::
	Vk.Dvc.D -> Vk.ImGui.H.Win.W -> #{type uint32_t} -> IO ()

onlyCreateSwapChain ::
	Vk.Dvc.D -> Vk.ImGui.H.Win.W -> Ptr Vk.AllocCallbacks.A ->
	#{type int} -> #{type int} -> #{type uint32_t} -> Vk.Swpch.S ->
	Ptr Vk.Sfc.Capabilities -> IO ()
onlyCreateSwapChain = cxx_im_gui_impl_vulkan_h_only_create_swap_chain

foreign import ccall "im_gui_impl_vulkan_h_only_create_swap_chain"
	cxx_im_gui_impl_vulkan_h_only_create_swap_chain ::
	Vk.Dvc.D -> Vk.ImGui.H.Win.W -> Ptr Vk.AllocCallbacks.A ->
	#{type int} -> #{type int} -> #{type uint32_t} -> Vk.Swpch.S ->
	Ptr Vk.Sfc.Capabilities -> IO ()

onlyCreateSwapChainNoWd ::
	Vk.Dvc.D -> Ptr Vk.AllocCallbacks.A ->
	#{type uint32_t} -> Vk.Swpch.S ->
	Ptr Vk.Sfc.Capabilities ->
	Vk.Sfc.S -> Ptr Vk.Sfc.Format -> #{type VkPresentModeKHR} ->
	#{type int} -> #{type int} -> IO (Ptr Vk.Swpch.S)
onlyCreateSwapChainNoWd =
	cxx_im_gui_impl_vulkan_h_only_create_swap_chain_no_wd

foreign import ccall "im_gui_impl_vulkan_h_only_create_swap_chain_no_wd"
	cxx_im_gui_impl_vulkan_h_only_create_swap_chain_no_wd ::
	Vk.Dvc.D -> Ptr Vk.AllocCallbacks.A ->
	#{type uint32_t} -> Vk.Swpch.S ->
	Ptr Vk.Sfc.Capabilities ->
	Vk.Sfc.S -> Ptr Vk.Sfc.Format -> #{type VkPresentModeKHR} ->
	#{type int} -> #{type int} -> IO (Ptr Vk.Swpch.S)

copySwapChainToWd :: Vk.ImGui.H.Win.W -> Ptr Vk.Swpch.S -> IO ()
copySwapChainToWd =
	cxx_im_gui_impl_vulkan_h_copy_swapchain_to_wd

foreign import ccall "im_gui_impl_vulkan_h_copy_swap_chain_to_wd"
	cxx_im_gui_impl_vulkan_h_copy_swapchain_to_wd ::
	Vk.ImGui.H.Win.W -> Ptr Vk.Swpch.S -> IO ()

setSize :: Vk.ImGui.H.Win.W -> #{type int} -> #{type int} ->
	Ptr Vk.Sfc.Capabilities -> IO ()
setSize = cxx_im_gui_impl_vulkan_h_set_size

foreign import ccall "im_gui_impl_vulkan_h_set_size"
	cxx_im_gui_impl_vulkan_h_set_size ::
	Vk.ImGui.H.Win.W -> #{type int} -> #{type int} ->
	Ptr Vk.Sfc.Capabilities -> IO ()

createSwapChainModifyWd :: Vk.ImGui.H.Win.W -> Ptr Vk.Img.I -> #{type int} -> IO ()
createSwapChainModifyWd =
	cxx_im_gui_impl_vulkan_h_create_swap_chain_modify_wd

foreign import ccall "im_gui_impl_vulkan_h_create_swap_chain_modify_wd"
	cxx_im_gui_impl_vulkan_h_create_swap_chain_modify_wd ::
	Vk.ImGui.H.Win.W -> Ptr Vk.Img.I -> #{type int} -> IO ()

createWindowRenderPass = cxx_im_gui_impl_vulkan_h_create_window_render_pass

foreign import ccall "im_gui_impl_vulkan_h_create_window_render_pass"
	cxx_im_gui_impl_vulkan_h_create_window_render_pass ::
	Vk.Dvc.D -> Vk.ImGui.H.Win.W -> Ptr Vk.AllocCallbacks.A -> IO ()

createWindowImageViews = cxx_im_gui_impl_vulkan_h_create_window_image_views

foreign import ccall "im_gui_impl_vulkan_h_create_window_image_views"
	cxx_im_gui_impl_vulkan_h_create_window_image_views ::
	Vk.Dvc.D -> Vk.ImGui.H.Win.W -> Ptr Vk.AllocCallbacks.A -> IO ()

createWindowFramebuffer = cxx_im_gui_impl_vulkan_h_create_window_framebuffer

foreign import ccall "im_gui_impl_vulkan_h_create_window_framebuffer"
	cxx_im_gui_impl_vulkan_h_create_window_framebuffer ::
	Vk.Dvc.D -> Vk.ImGui.H.Win.W -> Ptr Vk.AllocCallbacks.A -> IO ()
