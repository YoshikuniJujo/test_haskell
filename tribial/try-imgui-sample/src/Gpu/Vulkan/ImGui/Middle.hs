{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Middle (
	C.checkVersion,
	C.createContextNoArg, C.Context,
	InitInfo(..)
	) where

import Data.Word

import Gpu.Vulkan.Middle.Internal qualified as Vk
import Gpu.Vulkan.Exception.Enum qualified as Vk
import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as Vk.AllocCallbacks
import Gpu.Vulkan.Instance.Middle.Internal qualified as Vk.Ist
import Gpu.Vulkan.PhysicalDevice.Middle.Internal qualified as Vk.Phd
import Gpu.Vulkan.Device.Middle.Internal qualified as Vk.Dvc
import Gpu.Vulkan.QueueFamily.Middle qualified as Vk.QFam
import Gpu.Vulkan.Queue.Middle.Internal qualified as Vk.Q
import Gpu.Vulkan.DescriptorPool.Middle.Internal qualified as Vk.DscPl
import Gpu.Vulkan.PipelineCache.Middle.Internal qualified as Vk.PplCch
import Gpu.Vulkan.RenderPass.Middle.Internal qualified as Vk.RndrPss
import Gpu.Vulkan.Sample.Enum qualified as Vk.Smpl
import Gpu.Vulkan.Subpass.Middle.Internal qualified as Vk.Sbpss

import Gpu.Vulkan.ImGui.Core qualified as C

data InitInfo ac = InitInfo {
	initInfoApiVersion :: Vk.ApiVersion,
	initInfoInstance :: Vk.Ist.I,
	initInfoPhysicalDevice :: Vk.Phd.P,
	initInfoDevice :: Vk.Dvc.D,
	initInfoQueueFamily :: Vk.QFam.Index,
	initInfoQueue :: Vk.Q.Q,
	initInfoDescriptorPool :: Vk.DscPl.D,
	initInfoRenderPass :: Vk.RndrPss.R,
	initInfoMinImageCount :: Word32,
	initInfoImageCount :: Word32,
	initInfoMSAASamples :: Vk.Smpl.CountFlagBits,
	initInfoPipelineCache :: Vk.PplCch.P,
	initInfoSubpass :: Vk.Sbpss.S,
	initInfoDescriptorPoolSize :: Word32,
	initInfoUseDynamicRendering :: Bool,
	initInfoAllocator :: Vk.AllocCallbacks.A ac,
	initInfoCheckVkResultFn :: Vk.Result -> IO (),
	initInfoMinAllocationSize :: Vk.Dvc.Size }
