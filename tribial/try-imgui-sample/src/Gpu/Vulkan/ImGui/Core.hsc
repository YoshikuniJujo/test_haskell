{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Core (
	checkVersion,
	createContextNoArg, Context,

	InitInfo, pattern InitInfo,
	initInfoApiVersion, initInfoInstance, initInfoPhysicalDevice,
	initInfoDevice, initInfoQueueFamily, initInfoQueue,
	initInfoDescriptorPool, initInfoRenderPass, initInfoMinImageCount,
	initInfoImageCount, initInfoMSAASamples, initInfoPipelineCache,
	initInfoSubpass, initInfoDescriptorPoolSize,
	initInfoUseDynamicRendering, initInfoAllocator, initInfoCheckVkResultFn,
	initInfoMinAllocationSize,

	PtrA, PtrCheckVkResultFn, CheckVkResultFn,

	InitInfoCxx, initInfoFromCxx, copyInitInfoToCxx,

	cxx_imgui_impl_vulkan_init

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word
import Data.Int

import Gpu.Vulkan.AllocationCallbacks.Core qualified as Vk.AllocCallbacks
import Gpu.Vulkan.Instance.Core qualified as Vk.Ist
import Gpu.Vulkan.PhysicalDevice.Core qualified as Vk.Phd
import Gpu.Vulkan.Device.Core qualified as Vk.Dvc
import Gpu.Vulkan.Queue.Core qualified as Vk.Q
import Gpu.Vulkan.DescriptorPool.Core qualified as Vk.DscPl
import Gpu.Vulkan.RenderPass.Core qualified as Vk.RndrPss
import Gpu.Vulkan.PipelineCache.Core qualified as Vk.PplCch

#include "imgui_c.h"

checkVersion :: IO ()
checkVersion = cxx_imgui_check_version

createContextNoArg :: IO Context
createContextNoArg = cxx_create_context_no_arg

newtype Context = Context (Ptr ContextTag)
data ContextTag

foreign import ccall "imgui_check_version" cxx_imgui_check_version :: IO ()
foreign import ccall "create_context_no_arg" cxx_create_context_no_arg :: IO Context

type PtrA = Ptr Vk.AllocCallbacks.A
type CheckVkResultFn = #{type VkResult} -> IO ()
type PtrCheckVkResultFn = FunPtr CheckVkResultFn

struct "InitInfo" #{size struct ImGui_ImplVulkan_InitInfo}
	#{alignment struct ImGui_ImplVulkan_InitInfo} [
	("ApiVersion", ''#{type uint32_t},
		[| #{peek struct ImGui_ImplVulkan_InitInfo, ApiVersion} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo, ApiVersion} |]),
	("Instance", ''Vk.Ist.I,
		[| #{peek struct ImGui_ImplVulkan_InitInfo, Instance} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo, Instance} |]),
	("PhysicalDevice", ''Vk.Phd.P,
		[| #{peek struct ImGui_ImplVulkan_InitInfo, PhysicalDevice} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo, PhysicalDevice} |]),
	("Device", ''Vk.Dvc.D,
		[| #{peek struct ImGui_ImplVulkan_InitInfo, Device} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo, Device} |]),
	("QueueFamily", ''#{type uint32_t},
		[| #{peek struct ImGui_ImplVulkan_InitInfo, QueueFamily} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo, QueueFamily} |]),
	("Queue", ''Vk.Q.Q,
		[| #{peek struct ImGui_ImplVulkan_InitInfo, Queue} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo, Queue} |]),
	("DescriptorPool", ''Vk.DscPl.D,
		[| #{peek struct ImGui_ImplVulkan_InitInfo, DescriptorPool} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo, DescriptorPool} |]),
	("RenderPass", ''Vk.RndrPss.R,
		[| #{peek struct ImGui_ImplVulkan_InitInfo, RenderPass} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo, RenderPass} |]),
	("MinImageCount", ''#{type uint32_t},
		[| #{peek struct ImGui_ImplVulkan_InitInfo, MinImageCount} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo, MinImageCount} |]),
	("ImageCount", ''#{type uint32_t},
		[| #{peek struct ImGui_ImplVulkan_InitInfo, ImageCount} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo, ImageCount} |]),
	("MSAASamples", ''#{type VkSampleCountFlagBits},
		[| #{peek struct ImGui_ImplVulkan_InitInfo, MSAASamples} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo, MSAASamples} |]),
	("PipelineCache", ''Vk.PplCch.P,
		[| #{peek struct ImGui_ImplVulkan_InitInfo, PipelineCache} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo, PipelineCache} |]),
	("Subpass", ''#{type uint32_t},
		[| #{peek struct ImGui_ImplVulkan_InitInfo, Subpass} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo, Subpass} |]),
	("DescriptorPoolSize", ''#{type uint32_t},
		[| #{peek struct ImGui_ImplVulkan_InitInfo,
			DescriptorPoolSize} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo,
			DescriptorPoolSize} |]),
	("UseDynamicRendering", ''#{type bool},
		[| #{peek struct ImGui_ImplVulkan_InitInfo,
			UseDynamicRendering} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo,
			UseDynamicRendering} |]),
	-- PipelineRenderingCreateInfo
	("Allocator", ''PtrA,
		[| #{peek struct ImGui_ImplVulkan_InitInfo, Allocator} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo, Allocator} |]),
	("CheckVkResultFn", ''PtrCheckVkResultFn,
		[| #{peek struct ImGui_ImplVulkan_InitInfo,
			CheckVkResultFn} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo,
			CheckVkResultFn} |]),
	("MinAllocationSize", ''#{type VkDeviceSize},
		[| #{peek struct ImGui_ImplVulkan_InitInfo,
			MinAllocationSize} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo,
			MinAllocationSize} |]) ]
	[''Show, ''Storable]

type InitInfoCxx = Ptr InitInfo

initInfoFromCxx :: InitInfoCxx -> IO InitInfo
initInfoFromCxx = peek

copyInitInfoToCxx :: InitInfo -> InitInfoCxx -> IO ()
copyInitInfoToCxx = flip poke

foreign import ccall "imgui_impl_vulkan_init" cxx_imgui_impl_vulkan_init ::
	InitInfoCxx -> IO #{type bool}
