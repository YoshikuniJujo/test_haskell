{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui (

	-- * VERSION AND CONTEXT

	M.checkVersion,
	M.createContextNoArg, M.Context,

	-- * INITIALIZATION

	InitInfo(..), initInfoFromMiddle,

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

initInfoFromMiddle :: Vk.AllocCallbacks.ToMiddle mac =>
	M.InitInfo (Vk.AllocCallbacks.Snd mac) -> InitInfo si sd sdp srp spc mac
initInfoFromMiddle M.InitInfo {
	M.initInfoApiVersion = av,
	M.initInfoInstance = ist,
	M.initInfoPhysicalDevice = phd,
	M.initInfoDevice = dvc,
	M.initInfoQueueFamily = qfi,
	M.initInfoQueue = gq,
	M.initInfoDescriptorPool = dpl,
	M.initInfoRenderPass = rp,
	M.initInfoMinImageCount = mic,
	M.initInfoImageCount = ic,
	M.initInfoMSAASamples = mss,
	M.initInfoPipelineCache = pc,
	M.initInfoSubpass = sp,
	M.initInfoDescriptorPoolSize = dps,
	M.initInfoUseDynamicRendering = udr,
	M.initInfoAllocator = ac,
	M.initInfoCheckVkResultFn = crfn,
	M.initInfoMinAllocationSize = mas } = InitInfo {
	initInfoApiVersion = av,
	initInfoInstance = Vk.Ist.I ist,
	initInfoPhysicalDevice = phd,
	initInfoDevice = Vk.Dvc.D dvc,
	initInfoQueueFamily = qfi,
	initInfoQueue = gq,
	initInfoDescriptorPool = Vk.DscPl.P dpl,
	initInfoRenderPass = Vk.RndrPss.R rp,
	initInfoMinImageCount = mic,
	initInfoImageCount = ic,
	initInfoMSAASamples = mss,
	initInfoPipelineCache = Vk.PplCch.P pc,
	initInfoSubpass = sp,
	initInfoDescriptorPoolSize = dps,
	initInfoUseDynamicRendering = udr,
	initInfoAllocator = Vk.AllocCallbacks.fromMiddle ac,
	initInfoCheckVkResultFn = crfn,
	initInfoMinAllocationSize = mas }
