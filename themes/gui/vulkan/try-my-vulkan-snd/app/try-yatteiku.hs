{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Default
import Data.Bits
import Data.List
import Data.Word

import Vulkan.Base

import qualified Vulkan as Vk
import qualified Vulkan.Enum as Vk
import qualified Vulkan.Core as Vk.C
import qualified Vulkan.Instance as Vk.Instance
import qualified Vulkan.PhysicalDevice as Vk.PhysicalDevice
import qualified Vulkan.Device as Vk.Device
import qualified Vulkan.QueueFamily as Vk.QueueFamily
import qualified Vulkan.QueueFamily.EnumManual as Vk.QueueFamily
import qualified Vulkan.Device.Queue as Vk.Device.Queue
import qualified Vulkan.Device.Queue.Enum as Vk.Device.Queue
import qualified Vulkan.CommandPool as Vk.CommandPool
import qualified Vulkan.CommandPool.Enum as Vk.CommandPool
import qualified Vulkan.CommandBuffer as Vk.CommandBuffer
import qualified Vulkan.CommandBuffer.Enum as Vk.CommandBuffer
import qualified Vulkan.Queue as Vk.Queue
import qualified Vulkan.Queue.Enum as Vk.Queue
import qualified Vulkan.Image as Vk.Image
import qualified Vulkan.Image.Enum as Vk.Image
import qualified Vulkan.Format.Enum as Vk.Format
import qualified Vulkan.Sample.Enum as Vk.Sample
import qualified Vulkan.Memory.Middle as Vk.Memory.M
import qualified Vulkan.Memory.Image as Vk.Memory.Image
import qualified Vulkan.Attachment as Vk.Attachment
import qualified Vulkan.Attachment.Enum as Vk.Attachment
import qualified Vulkan.Subpass as Vk.Subpass
import qualified Vulkan.Subpass.Enum as Vk.Subpass
import qualified Vulkan.Pipeline.Enum as Vk.Pipeline
import qualified Vulkan.RenderPass as Vk.RenderPass
import qualified Vulkan.RenderPass.Enum as Vk.RenderPass

import qualified Vulkan.Khr as Vk.Khr

screenWidth, screenHeight :: Word32
(screenWidth, screenHeight) = (640, 480)

main :: IO ()
main = do
	let	createInfo :: Vk.Instance.CreateInfo () ()
		createInfo = def {
			Vk.Instance.createInfoEnabledLayerNames =
				[Vk.Khr.validationLayerName] }
	Vk.Instance.create createInfo nil nil \inst -> do
		(physicalDevice, graphicsQueueFamilyIndex) <-
			selectPhysicalDeviceAndQueueFamily
				=<< Vk.PhysicalDevice.enumerate inst
		print physicalDevice
		print graphicsQueueFamilyIndex
		let	queueCreateInfo = Vk.Device.Queue.CreateInfo {
				Vk.Device.Queue.createInfoNext = Nothing,
				Vk.Device.Queue.createInfoFlags =
					Vk.Device.Queue.CreateFlagsZero,
				Vk.Device.Queue.createInfoQueueFamilyIndex =
					graphicsQueueFamilyIndex,
				Vk.Device.Queue.createInfoQueuePriorities =
					[1.0] }
			devCreateInfo :: Vk.Device.CreateInfo () ()
			devCreateInfo = Vk.Device.CreateInfo {
				Vk.Device.createInfoNext = Nothing,
				Vk.Device.createInfoFlags =
					Vk.Device.CreateFlagsZero,
				Vk.Device.createInfoQueueCreateInfos =
					[queueCreateInfo],
				Vk.Device.createInfoEnabledLayerNames =
					[Vk.Khr.validationLayerName],
				Vk.Device.createInfoEnabledExtensionNames = [],
				Vk.Device.createInfoEnabledFeatures = Nothing }
		Vk.Device.create physicalDevice devCreateInfo nil nil \dvc ->
			runDevice physicalDevice dvc graphicsQueueFamilyIndex

runDevice :: Vk.PhysicalDevice.P -> Vk.Device.D sd -> Vk.QueueFamily.Index -> IO ()
runDevice phdvc device graphicsQueueFamilyIndex = do
	graphicsQueue <- Vk.Device.getQueue
		device graphicsQueueFamilyIndex 0
	print graphicsQueue
	let	cmdPoolCreateInfo :: Vk.CommandPool.CreateInfo ()
		cmdPoolCreateInfo = Vk.CommandPool.CreateInfo {
			Vk.CommandPool.createInfoNext = Nothing,
			Vk.CommandPool.createInfoFlags =
				Vk.CommandPool.CreateFlagsZero,
			Vk.CommandPool.createInfoQueueFamilyIndex =
				graphicsQueueFamilyIndex }
	makeImage phdvc device
	makeRenderPass device
	Vk.CommandPool.create device cmdPoolCreateInfo nil nil
			\(cmdPool :: Vk.CommandPool.C s) -> do
		let	cmdBufAllocInfo :: Vk.CommandBuffer.AllocateInfo () s
			cmdBufAllocInfo = Vk.CommandBuffer.AllocateInfo {
				Vk.CommandBuffer.allocateInfoNext = Nothing,
				Vk.CommandBuffer.allocateInfoCommandPool =
					cmdPool,
				Vk.CommandBuffer.allocateInfoLevel =
					Vk.CommandBuffer.LevelPrimary,
				Vk.CommandBuffer.allocateInfoCommandBufferCount
					= 1 }
		Vk.CommandBuffer.allocate device cmdBufAllocInfo \case
			[cmdBuf] -> do
				Vk.CommandBuffer.begin cmdBuf
						Vk.CommandBuffer.beginInfoNil do
					pure ()
				let	submitInfo = Vk.SubmitInfo {
						Vk.submitInfoNext = Nothing,
						Vk.submitInfoWaitSemaphoreDstStageMasks = [],
						Vk.submitInfoCommandBuffers = [cmdBuf],
						Vk.submitInfoSignalSemaphores = [] }
				Vk.Queue.submit @() graphicsQueue [submitInfo] Nothing
				Vk.Queue.waitIdle graphicsQueue
			_ -> error "never occur"

makeImage :: Vk.PhysicalDevice.P -> Vk.Device.D sd -> IO ()
makeImage phdvc dvc = do
	let	imgCreateInfo = Vk.Image.CreateInfo {
			Vk.Image.createInfoNext = Nothing,
			Vk.Image.createInfoFlags = Vk.Image.CreateFlagsZero,
			Vk.Image.createInfoImageType = Vk.Image.Type2d,
			Vk.Image.createInfoExtent =
				Vk.C.Extent3d screenWidth screenHeight 1,
			Vk.Image.createInfoMipLevels = 1,
			Vk.Image.createInfoArrayLayers = 1,
			Vk.Image.createInfoFormat = Vk.Format.R8g8b8a8Unorm,
			Vk.Image.createInfoTiling = Vk.Image.TilingLinear,
			Vk.Image.createInfoInitialLayout =
				Vk.Image.LayoutUndefined,
			Vk.Image.createInfoUsage =
				Vk.Image.UsageColorAttachmentBit,
			Vk.Image.createInfoSharingMode =
				Vk.SharingModeExclusive,
			Vk.Image.createInfoSamples = Vk.Sample.Count1Bit,
			Vk.Image.createInfoQueueFamilyIndices = [] }
	memProps <- Vk.PhysicalDevice.getMemoryProperties phdvc
	print memProps
	Vk.Image.create @() dvc imgCreateInfo nil nil \image -> do
		print image
		imgMemReq <- Vk.Image.getMemoryRequirements dvc image
		print imgMemReq
		let	imgMemReqTypes =
				Vk.Memory.M.requirementsMemoryTypeBits imgMemReq
			memPropTypes = fst <$>
				Vk.PhysicalDevice.memoryPropertiesMemoryTypes
					memProps
			memoryTypeIndex = case filter
				(`Vk.Memory.M.elemTypeIndex` imgMemReqTypes)
				memPropTypes of
				[] -> error "No available memory types"
				i : _ -> i
			imgMemAllocInfo = Vk.Memory.Image.AllocateInfo {
				Vk.Memory.Image.allocateInfoNext = Nothing,
				Vk.Memory.Image.allocateInfoMemoryTypeIndex =
					memoryTypeIndex }
		Vk.Memory.Image.allocate @()
			dvc image imgMemAllocInfo nil nil \imgMem -> do
			print imgMem
			Vk.Image.bindMemory dvc image imgMem

selectPhysicalDeviceAndQueueFamily ::
	[Vk.PhysicalDevice.P] -> IO (Vk.PhysicalDevice.P, Vk.QueueFamily.Index)
selectPhysicalDeviceAndQueueFamily = \case
	[] -> error "no suitable QueueFamilies"
	phdvc : phdvcs -> do
		queueProps <- Vk.PhysicalDevice.getQueueFamilyProperties phdvc
		case pickGraphicsQueueFamilyIndex queueProps of
			Nothing -> selectPhysicalDeviceAndQueueFamily phdvcs
			Just idx -> pure (phdvc, idx)

pickGraphicsQueueFamilyIndex ::
	[(Vk.QueueFamily.Index, Vk.QueueFamily.Properties)] -> Maybe Vk.QueueFamily.Index
pickGraphicsQueueFamilyIndex ps = fst <$> find
	((/= zeroBits) . (Vk.Queue.GraphicsBit .&.)
		. Vk.QueueFamily.propertiesQueueFlags . snd) ps

printQueueProps :: Vk.QueueFamily.Properties -> IO ()
printQueueProps qps = do
	print qps
	print . breakBits $ Vk.QueueFamily.propertiesQueueFlags qps

breakBits :: FiniteBits b => b -> [b]
breakBits = bb (bit 0 `rotateR` 1)
	where
	bb i n	| i == zeroBits = []
		| i .&. n == zeroBits = bs
		| otherwise = i : bs
		where
		bs = bb (i `shiftR` 1) n

makeRenderPass :: Vk.Device.D sd -> IO ()
makeRenderPass dvc = do
	let	attachment = Vk.Attachment.Description {
			Vk.Attachment.descriptionFlags =
				Vk.Attachment.DescriptionFlagsZero,
			Vk.Attachment.descriptionFormat =
				Vk.Format.R8g8b8a8Unorm,
			Vk.Attachment.descriptionSamples =
				Vk.Sample.Count1Bit,
			Vk.Attachment.descriptionLoadOp =
				Vk.Attachment.LoadOpDontCare,
			Vk.Attachment.descriptionStoreOp =
				Vk.Attachment.StoreOpDontCare,
			Vk.Attachment.descriptionStencilLoadOp =
				Vk.Attachment.LoadOpDontCare,
			Vk.Attachment.descriptionStencilStoreOp =
				Vk.Attachment.StoreOpDontCare,
			Vk.Attachment.descriptionInitialLayout =
				Vk.Image.LayoutUndefined,
			Vk.Attachment.descriptionFinalLayout =
				Vk.Image.LayoutGeneral }
		subpass0AttachmentRef = Vk.Attachment.Reference {
			Vk.Attachment.referenceAttachment = 0,
			Vk.Attachment.referenceLayout =
				Vk.Image.LayoutColorAttachmentOptimal }
		subpass = Vk.Subpass.Description {
			Vk.Subpass.descriptionFlags =
				Vk.Subpass.DescriptionFlagsZero,
			Vk.Subpass.descriptionPipelineBindPoint =
				Vk.Pipeline.BindPointGraphics,
			Vk.Subpass.descriptionInputAttachments = [],
			Vk.Subpass.descriptionColorAndResolveAttachments =
				Left [subpass0AttachmentRef],
			Vk.Subpass.descriptionDepthStencilAttachment = Nothing,
			Vk.Subpass.descriptionPreserveAttachments = [] }
		renderPassCreateInfo = Vk.RenderPass.CreateInfo {
			Vk.RenderPass.createInfoNext = Nothing,
			Vk.RenderPass.createInfoFlags =
				Vk.RenderPass.CreateFlagsZero,
			Vk.RenderPass.createInfoAttachments = [attachment],
			Vk.RenderPass.createInfoSubpasses = [subpass],
			Vk.RenderPass.createInfoDependencies = [] }
	Vk.RenderPass.create @() dvc renderPassCreateInfo nil nil \rp ->
		print rp
