{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui (

	-- * VERSION AND CONTEXT

	M.checkVersion,
	M.createContextNoArg, M.Context,

	-- * INITIALIZATION

	InitInfo(..),

	-- * ENUMS

	module Gpu.Vulkan.ImGui.Enum

	) where

import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.Word

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.Exception qualified as Vk
import Gpu.Vulkan.AllocationCallbacks.Internal qualified as Vk.AllocCallbacks
import Gpu.Vulkan.Instance.Internal qualified as Vk.Ist
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.Device.Internal qualified as Vk.Dvc
import Gpu.Vulkan.QueueFamily qualified as Vk.QFam
import Gpu.Vulkan.Queue qualified as Vk.Q
import Gpu.Vulkan.PipelineCache.Type qualified as Vk.PplCch
import Gpu.Vulkan.DescriptorPool.Type qualified as Vk.DscPl
import Gpu.Vulkan.RenderPass.Type qualified as Vk.RndrPss
import Gpu.Vulkan.Sample qualified as Vk.Smpl
import Gpu.Vulkan.Subpass qualified as Vk.Sbpss

import Gpu.Vulkan.ImGui.Enum
import Gpu.Vulkan.ImGui.Middle qualified as M

data InitInfo si sd sdp srp spc mac = InitInfo {
	initInfoApiVersion :: Vk.ApiVersion,
	initInfoInstance :: Vk.Ist.I si,
	initInfoPhysicalDevice :: Vk.Phd.P,
	initInfoDevice :: Vk.Dvc.D sd,
	initInfoQueueFamily :: Vk.QFam.Index,
	initInfoQueue :: Vk.Q.Q,
	initInfoDescriptorPool :: Vk.DscPl.P sdp,
	initInfoRenderPass :: Vk.RndrPss.R srp,
	initInfoMinImageCount :: Word32,
	initInfoImageCount :: Word32,
	initInfoMSAASamples :: Vk.Smpl.CountFlagBits,
	initInfoPipelineCache :: Vk.PplCch.P spc,
	initInfoSubpass :: Vk.Sbpss.S,
	initInfoDescriptorPoolSize :: Word32,
	initInfoUseDynamicRendering :: Bool,
	initInfoAllocator :: TPMaybe.M (U2 Vk.AllocCallbacks.A) mac,
	initInfoCheckVkResultFn :: Maybe (Vk.Result -> IO ()),
	initInfoMinAllocationSize :: Vk.Dvc.Size }
