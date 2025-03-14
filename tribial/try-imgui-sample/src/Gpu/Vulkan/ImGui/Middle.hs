{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Middle (
	C.checkVersion,
	C.createContextNoArg, C.Context,
	InitInfo(..), initInfoFromCore
	) where

import Foreign.Ptr
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.Bool
import Data.Word
import Text.Show.ToolsYj

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

data InitInfo mac = InitInfo {
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
	initInfoAllocator :: TPMaybe.M Vk.AllocCallbacks.A mac,
	initInfoCheckVkResultFn :: Maybe (Vk.Result -> IO ()),
	initInfoMinAllocationSize :: Vk.Dvc.Size }

instance Show (TPMaybe.M Vk.AllocCallbacks.A ac) => ShowIO (InitInfo ac) where
	showIO ii = do
		pure $ "InitInfo { " ++
			"initInfoApiVersion = " ++
				show (initInfoApiVersion ii) ++ ", " ++
			"initInfoInstance = " ++
				show (initInfoInstance ii) ++ ", " ++
			"initInfoPhysicalDevice = " ++
				show (initInfoPhysicalDevice ii) ++ ", " ++
			"initInfoDevice = " ++
				show (initInfoDevice ii) ++ ", " ++
			"initInfoQueueFamily = " ++
				show (initInfoQueueFamily ii) ++ ", " ++
			"initInfoQueue = " ++ show (initInfoQueue ii) ++ ", " ++
			"initInfoDescriptorPool = " ++
				show (initInfoDescriptorPool ii) ++ ", " ++
			"initInfoRenderPass = " ++
				show (initInfoRenderPass ii) ++ ", " ++
			"initInfoMinImageCount = " ++
				show (initInfoMinImageCount ii) ++ ", " ++
			"initInfoImageCount = " ++
				show (initInfoImageCount ii) ++ ", " ++
			"initInfoMSAASamples = " ++
				show (initInfoMSAASamples ii) ++ ", " ++
			"initInfoPipelineCache = " ++
				show (initInfoPipelineCache ii) ++ ", " ++
			"initInfoSubpass = " ++
				show (initInfoSubpass ii) ++ ", " ++
			"initInfoDescriptorPoolSize = " ++
				show (initInfoDescriptorPoolSize ii) ++ ", " ++
			"initInfoUseDynamicRendering = " ++
				show (initInfoUseDynamicRendering ii) ++ ", " ++
			"initInfoAllocator = " ++
				show (initInfoAllocator ii) ++ ", " ++
			"initInfoCheckVkResultFn = " ++ maybe "Nothing"
				(const "Just <function>")
				(initInfoCheckVkResultFn ii) ++ ", " ++
			"initInfoMinAllocationSize = " ++
				show (initInfoMinAllocationSize ii) ++ " }"

initInfoFromCore ::
	Vk.AllocCallbacks.MFromCore ac => C.InitInfo -> IO (InitInfo ac)
initInfoFromCore C.InitInfo {
	C.initInfoApiVersion = av,
	C.initInfoInstance = ist,
	C.initInfoPhysicalDevice = phd,
	C.initInfoDevice = dvc,
	C.initInfoQueueFamily = qfi,
	C.initInfoQueue = q,
	C.initInfoDescriptorPool = dpl,
	C.initInfoRenderPass = rp,
	C.initInfoMinImageCount = mic,
	C.initInfoImageCount = ic,
	C.initInfoMSAASamples = mss,
	C.initInfoPipelineCache = pc,
	C.initInfoSubpass = sp,
	C.initInfoDescriptorPoolSize = dps,
	C.initInfoUseDynamicRendering = udr,
	C.initInfoAllocator = ac,
	C.initInfoCheckVkResultFn = crf,
	C.initInfoMinAllocationSize = mas } = do
	acm <- Vk.AllocCallbacks.mFromCore ac
	pure InitInfo {
		initInfoApiVersion = Vk.ApiVersion av,
		initInfoInstance = Vk.Ist.I ist,
		initInfoPhysicalDevice = Vk.Phd.P phd,
		initInfoDevice = Vk.Dvc.D dvc,
		initInfoQueueFamily = Vk.QFam.Index qfi,
		initInfoQueue = Vk.Q.Q q,
		initInfoDescriptorPool = Vk.DscPl.D dpl,
		initInfoRenderPass = Vk.RndrPss.R rp,
		initInfoMinImageCount = mic,
		initInfoImageCount = ic,
		initInfoMSAASamples = Vk.Smpl.CountFlagBits mss,
		initInfoPipelineCache = Vk.PplCch.P pc,
		initInfoSubpass = Vk.Sbpss.S sp,
		initInfoDescriptorPoolSize = dps,
		initInfoUseDynamicRendering = udr /= 0,
		initInfoAllocator = acm,
		initInfoCheckVkResultFn = bool
			Nothing
			(Just $ mkCheckResultFn crf . (\(Vk.Result r) -> r))
			(crf /= nullFunPtr),
		initInfoMinAllocationSize = Vk.Dvc.Size mas }

foreign import ccall "dynamic" mkCheckResultFn ::
	FunPtr C.CheckVkResultFn -> C.CheckVkResultFn
