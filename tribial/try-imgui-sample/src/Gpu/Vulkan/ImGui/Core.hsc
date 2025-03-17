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

struct "InitInfo" #{size struct ImGui_ImplVulkan_InitInfo_C}
	#{alignment struct ImGui_ImplVulkan_InitInfo_C} [
	("ApiVersion", ''#{type uint32_t},
		[| #{peek struct ImGui_ImplVulkan_InitInfo_C, ApiVersion} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo_C, ApiVersion} |]),
	("Instance", ''Vk.Ist.I,
		[| #{peek struct ImGui_ImplVulkan_InitInfo_C, Instance} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo_C, Instance} |]),
	("PhysicalDevice", ''Vk.Phd.P,
		[| #{peek struct ImGui_ImplVulkan_InitInfo_C, PhysicalDevice} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo_C, PhysicalDevice} |]),
	("Device", ''Vk.Dvc.D,
		[| #{peek struct ImGui_ImplVulkan_InitInfo_C, Device} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo_C, Device} |]),
	("QueueFamily", ''#{type uint32_t},
		[| #{peek struct ImGui_ImplVulkan_InitInfo_C, QueueFamily} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo_C, QueueFamily} |]),
	("Queue", ''Vk.Q.Q,
		[| #{peek struct ImGui_ImplVulkan_InitInfo_C, Queue} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo_C, Queue} |]),
	("DescriptorPool", ''Vk.DscPl.D,
		[| #{peek struct ImGui_ImplVulkan_InitInfo_C, DescriptorPool} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo_C, DescriptorPool} |]),
	("RenderPass", ''Vk.RndrPss.R,
		[| #{peek struct ImGui_ImplVulkan_InitInfo_C, RenderPass} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo_C, RenderPass} |]),
	("MinImageCount", ''#{type uint32_t},
		[| #{peek struct ImGui_ImplVulkan_InitInfo_C, MinImageCount} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo_C, MinImageCount} |]),
	("ImageCount", ''#{type uint32_t},
		[| #{peek struct ImGui_ImplVulkan_InitInfo_C, ImageCount} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo_C, ImageCount} |]),
	("MSAASamples", ''#{type VkSampleCountFlagBits},
		[| #{peek struct ImGui_ImplVulkan_InitInfo_C, MSAASamples} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo_C, MSAASamples} |]),
	("PipelineCache", ''Vk.PplCch.P,
		[| #{peek struct ImGui_ImplVulkan_InitInfo_C, PipelineCache} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo_C, PipelineCache} |]),
	("Subpass", ''#{type uint32_t},
		[| #{peek struct ImGui_ImplVulkan_InitInfo_C, Subpass} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo_C, Subpass} |]),
	("DescriptorPoolSize", ''#{type uint32_t},
		[| #{peek struct ImGui_ImplVulkan_InitInfo_C,
			DescriptorPoolSize} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo_C,
			DescriptorPoolSize} |]),
	("UseDynamicRendering", ''#{type bool},
		[| #{peek struct ImGui_ImplVulkan_InitInfo_C,
			UseDynamicRendering} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo_C,
			UseDynamicRendering} |]),
	-- PipelineRenderingCreateInfo
	("Allocator", ''PtrA,
		[| #{peek struct ImGui_ImplVulkan_InitInfo_C, Allocator} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo_C, Allocator} |]),
	("CheckVkResultFn", ''PtrCheckVkResultFn,
		[| #{peek struct ImGui_ImplVulkan_InitInfo_C,
			CheckVkResultFn} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo_C,
			CheckVkResultFn} |]),
	("MinAllocationSize", ''#{type VkDeviceSize},
		[| #{peek struct ImGui_ImplVulkan_InitInfo_C,
			MinAllocationSize} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo_C,
			MinAllocationSize} |]) ]
	[''Show, ''Storable]

type InitInfoCxx = Ptr InitInfo

initInfoFromCxx :: InitInfoCxx -> IO InitInfo
initInfoFromCxx = peek

copyInitInfoToCxx :: InitInfo -> InitInfoCxx -> IO ()
copyInitInfoToCxx = flip poke

foreign import ccall "imgui_impl_vulkan_init" cxx_imgui_impl_vulkan_init ::
	InitInfoCxx -> IO #{type bool}
