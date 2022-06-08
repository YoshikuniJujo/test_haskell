{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-partial-type-signatures #-}

module Main where

import Data.Default
import Data.Bits
import Data.Maybe
import Data.List
import Data.Word
import Data.Color

import Shaderc.TH
import Shaderc.EnumAuto

import Vulkan.Base

import qualified Vulkan as Vk
import qualified Vulkan.Enum as Vk
import qualified Vulkan.Middle as Vk.M
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
import qualified Vulkan.Image as Vk.Img
import qualified Vulkan.Image.Enum as Vk.Img
import qualified Vulkan.Format.Enum as Vk.Format
import qualified Vulkan.Sample as Vk.Sample
import qualified Vulkan.Sample.Enum as Vk.Sample
import qualified Vulkan.Memory.Enum as Vk.Memory
import qualified Vulkan.Memory.Middle as Vk.Memory.M
import qualified Vulkan.Memory.Image as Vk.Memory.Image
import qualified Vulkan.Attachment as Vk.Attachment
import qualified Vulkan.Attachment.Enum as Vk.Attachment
import qualified Vulkan.Subpass as Vk.Subpass
import qualified Vulkan.Subpass.Enum as Vk.Subpass
import qualified Vulkan.Pipeline.Enum as Vk.Ppl
import qualified Vulkan.RenderPass as Vk.RenderPass
import qualified Vulkan.RenderPass.Enum as Vk.RenderPass
import qualified Vulkan.Pipeline.ViewportState as Vk.Ppl.ViewportState
import qualified Vulkan.Pipeline.VertexInputState as
	Vk.Ppl.VertexInputState
import qualified Vulkan.Pipeline.VertexInputState.Middle as
	Vk.Ppl.VertexInputState.M
import qualified Vulkan.Pipeline.InputAssemblyState as Vk.Ppl.InpAssSt
import qualified Vulkan.Pipeline.RasterizationState as Vk.Ppl.RstSt
import qualified Vulkan.Pipeline.MultisampleState as Vk.Ppl.MulSmplSt
import qualified Vulkan.Pipeline.ColorBlendAttachment as Vk.Ppl.ClrBlndAtt
import qualified Vulkan.ColorComponent.Enum as Vk.ColorComponent
import qualified Vulkan.Pipeline.ColorBlendState as Vk.Ppl.ClrBlndSt
import qualified Vulkan.Pipeline.Layout as Vk.Ppl.Lyt
import qualified Vulkan.Pipeline.Graphics as Vk.Ppl.Gr
import qualified Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShSt
import qualified Vulkan.Pipeline.ShaderStage.Enum as Vk.Ppl.ShSt
import qualified Vulkan.Shader.Module as Vk.Shader.Module
import qualified Vulkan.Shader.Stage.Enum as Vk.Shader.Stage
import qualified Vulkan.ImageView as Vk.ImgView
import qualified Vulkan.ImageView.Enum as Vk.ImgView
import qualified Vulkan.Component as Vk.Component
import qualified Vulkan.Component.Enum as Vk.Component
import qualified Vulkan.Framebuffer as Vk.Framebuffer
import qualified Vulkan.Framebuffer.Enum as Vk.Framebuffer
import qualified Vulkan.Command as Vk.Cmd

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
runDevice phdvc device graphicsQueueFamilyIndex =
	makeRenderPass device \rp ->
	makePipeline device rp \ppl ->
	makeImage phdvc device \bimg mi ->
	makeImageView device bimg \iv ->
	makeFramebuffer device rp iv \fb ->
	makeCommandBuffer device graphicsQueueFamilyIndex \cb -> do
	print cb
	let	renderpassBeginInfo = Vk.RenderPass.BeginInfo {
			Vk.RenderPass.beginInfoNext = Nothing,
			Vk.RenderPass.beginInfoRenderPass = rp,
			Vk.RenderPass.beginInfoFramebuffer = fb,
			Vk.RenderPass.beginInfoRenderArea = Vk.C.Rect2d {
				Vk.C.rect2dOffset = Vk.C.Offset2d 0 0,
				Vk.C.rect2dExtent = Vk.C.Extent2d
					screenWidth screenHeight
				},
			Vk.RenderPass.beginInfoClearValues = [] }
	Vk.Cmd.beginRenderPass @() @('Vk.M.ClearTypeColor 'Vk.M.ClearColorTypeFloat32)
		cb renderpassBeginInfo Vk.Subpass.ContentsInline do
		Vk.Cmd.bindPipeline cb Vk.Ppl.BindPointGraphics ppl
		Vk.Cmd.draw cb 3 1 0 0

makeCommandBuffer :: Vk.Device.D sd -> Vk.QueueFamily.Index ->
	(forall s . Vk.CommandBuffer.C s vs -> IO a) -> IO a
makeCommandBuffer device graphicsQueueFamilyIndex f = do
	graphicsQueue <- Vk.Device.getQueue device graphicsQueueFamilyIndex 0
	print graphicsQueue
	let	cmdPoolCreateInfo :: Vk.CommandPool.CreateInfo ()
		cmdPoolCreateInfo = Vk.CommandPool.CreateInfo {
			Vk.CommandPool.createInfoNext = Nothing,
			Vk.CommandPool.createInfoFlags =
				Vk.CommandPool.CreateFlagsZero,
			Vk.CommandPool.createInfoQueueFamilyIndex =
				graphicsQueueFamilyIndex }
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
				r <- Vk.CommandBuffer.begin cmdBuf
						Vk.CommandBuffer.beginInfoNil $ f cmdBuf
				let	submitInfo = Vk.SubmitInfo {
						Vk.submitInfoNext = Nothing,
						Vk.submitInfoWaitSemaphoreDstStageMasks = [],
						Vk.submitInfoCommandBuffers = [cmdBuf],
						Vk.submitInfoSignalSemaphores = [] }
				Vk.Queue.submit @() graphicsQueue [submitInfo] Nothing
				Vk.Queue.waitIdle graphicsQueue
				pure r
			_ -> error "never occur"

makeImage :: Vk.PhysicalDevice.P -> Vk.Device.D sd ->
	(forall si sm . Vk.Img.Binded si sm -> Vk.Device.MemoryImage sm -> IO a) ->
	IO a
makeImage phdvc dvc f = do
	let	imgCreateInfo = Vk.Img.CreateInfo {
			Vk.Img.createInfoNext = Nothing,
			Vk.Img.createInfoFlags = Vk.Img.CreateFlagsZero,
			Vk.Img.createInfoImageType = Vk.Img.Type2d,
			Vk.Img.createInfoExtent =
				Vk.C.Extent3d screenWidth screenHeight 1,
			Vk.Img.createInfoMipLevels = 1,
			Vk.Img.createInfoArrayLayers = 1,
			Vk.Img.createInfoFormat = Vk.Format.R8g8b8a8Unorm,
			Vk.Img.createInfoTiling = Vk.Img.TilingLinear,
			Vk.Img.createInfoInitialLayout =
				Vk.Img.LayoutUndefined,
			Vk.Img.createInfoUsage =
				Vk.Img.UsageColorAttachmentBit,
			Vk.Img.createInfoSharingMode =
				Vk.SharingModeExclusive,
			Vk.Img.createInfoSamples = Vk.Sample.Count1Bit,
			Vk.Img.createInfoQueueFamilyIndices = [] }
	memProps <- Vk.PhysicalDevice.getMemoryProperties phdvc
	print memProps
	Vk.Img.create @() dvc imgCreateInfo nil nil \image -> do
		print image
		imgMemReq <- Vk.Img.getMemoryRequirements dvc image
		print imgMemReq
		let	imgMemReqTypes =
				Vk.Memory.M.requirementsMemoryTypeBits imgMemReq
			memPropTypes = (fst <$>)
				. filter (const True
					. (/= zeroBits)
					. (.&. Vk.Memory.PropertyHostVisibleBit)
					. Vk.Memory.M.mTypePropertyFlags . snd)
				$ Vk.PhysicalDevice.memoryPropertiesMemoryTypes
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
			bimg <- Vk.Img.bindMemory dvc image imgMem
			f bimg imgMem

makeImageView :: Vk.Device.D sd -> Vk.Img.Binded si sm ->
	(forall s . Vk.ImgView.I s -> IO a) -> IO a
makeImageView dvc bimg f = do
	let	imgViewCreateInfo = Vk.ImgView.CreateInfo {
			Vk.ImgView.createInfoNext = Nothing,
			Vk.ImgView.createInfoFlags =
				Vk.ImgView.CreateFlagsZero,
			Vk.ImgView.createInfoImage = bimg,
			Vk.ImgView.createInfoViewType = Vk.ImgView.Type2d,
			Vk.ImgView.createInfoFormat = Vk.Format.R8g8b8a8Unorm,
			Vk.ImgView.createInfoComponents =
				Vk.Component.Mapping {
					Vk.Component.mappingR =
						Vk.Component.SwizzleIdentity,
					Vk.Component.mappingG =
						Vk.Component.SwizzleIdentity,
					Vk.Component.mappingB =
						Vk.Component.SwizzleIdentity,
					Vk.Component.mappingA =
						Vk.Component.SwizzleIdentity },
			Vk.ImgView.createInfoSubresourceRange =
				Vk.Img.SubresourceRange {
					Vk.Img.subresourceRangeAspectMask =
						Vk.Img.AspectColorBit,
					Vk.Img.subresourceRangeBaseMipLevel = 0,
					Vk.Img.subresourceRangeLevelCount = 1,
					Vk.Img.subresourceRangeBaseArrayLayer =
						0,
					Vk.Img.subresourceRangeLayerCount = 1 }
			}
	Vk.ImgView.create @() dvc imgViewCreateInfo nil nil \imgView -> do
		putStrLn $ "imgView: " ++ show imgView
		f imgView

makeFramebuffer :: Vk.Device.D sd -> Vk.RenderPass.R sr -> Vk.ImgView.I si ->
	(forall s . Vk.Framebuffer.F s -> IO a) -> IO a
makeFramebuffer dvc rp iv f = do
	let	frameBufCreateInfo = Vk.Framebuffer.CreateInfo {
			Vk.Framebuffer.createInfoNext = Nothing,
			Vk.Framebuffer.createInfoFlags =
				Vk.Framebuffer.CreateFlagsZero,
			Vk.Framebuffer.createInfoRenderPass = rp,
			Vk.Framebuffer.createInfoAttachments = [iv],
			Vk.Framebuffer.createInfoWidth = screenWidth,
			Vk.Framebuffer.createInfoHeight = screenHeight,
			Vk.Framebuffer.createInfoLayers = 1 }
	Vk.Framebuffer.create @() dvc frameBufCreateInfo nil nil \fb ->
		print fb >> f fb

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

makeRenderPass ::
	Vk.Device.D sd -> (forall s . Vk.RenderPass.R s -> IO a) -> IO a
makeRenderPass dvc f = do
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
				Vk.Img.LayoutUndefined,
			Vk.Attachment.descriptionFinalLayout =
				Vk.Img.LayoutGeneral }
		subpass0AttachmentRef = Vk.Attachment.Reference {
			Vk.Attachment.referenceAttachment = 0,
			Vk.Attachment.referenceLayout =
				Vk.Img.LayoutColorAttachmentOptimal }
		subpass = Vk.Subpass.Description {
			Vk.Subpass.descriptionFlags =
				Vk.Subpass.DescriptionFlagsZero,
			Vk.Subpass.descriptionPipelineBindPoint =
				Vk.Ppl.BindPointGraphics,
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
	Vk.RenderPass.create @() dvc renderPassCreateInfo nil nil f

makePipeline :: Vk.Device.D sd -> Vk.RenderPass.R sr ->
	(forall s . Vk.Ppl.Gr.G s () '[] -> IO a) -> IO a
makePipeline dvc rp f = do
	let	viewport = Vk.C.Viewport {
			Vk.C.viewportX = 0,
			Vk.C.viewportY = 0,
			Vk.C.viewportMinDepth = 0,
			Vk.C.viewportMaxDepth = 1,
			Vk.C.viewportWidth = fromIntegral screenWidth,
			Vk.C.viewportHeight = fromIntegral screenHeight }
		scissor = Vk.C.Rect2d {
			Vk.C.rect2dOffset = Vk.C.Offset2d 0 0,
			Vk.C.rect2dExtent =
				Vk.C.Extent2d screenWidth screenHeight }
		viewportState :: Vk.Ppl.ViewportState.CreateInfo ()
		viewportState = Vk.Ppl.ViewportState.CreateInfo {
			Vk.Ppl.ViewportState.createInfoNext = Nothing,
			Vk.Ppl.ViewportState.createInfoFlags =
				Vk.Ppl.ViewportState.CreateFlagsZero,
			Vk.Ppl.ViewportState.createInfoViewports =
				[viewport],
			Vk.Ppl.ViewportState.createInfoScissors =
				[scissor] }
		vertexInputInfo = Vk.Ppl.VertexInputState.CreateInfo {
			Vk.Ppl.VertexInputState.createInfoNext = Nothing,
			Vk.Ppl.VertexInputState.createInfoFlags =
				Vk.Ppl.VertexInputState.M.CreateFlagsZero }
		inputAssembly :: Vk.Ppl.InpAssSt.CreateInfo ()
		inputAssembly = Vk.Ppl.InpAssSt.CreateInfo {
			Vk.Ppl.InpAssSt.createInfoNext = Nothing,
			Vk.Ppl.InpAssSt.createInfoFlags =
				Vk.Ppl.InpAssSt.CreateFlagsZero,
			Vk.Ppl.InpAssSt.createInfoTopology =
				Vk.PrimitiveTopologyTriangleList,
			Vk.Ppl.InpAssSt.createInfoPrimitiveRestartEnable =
				False }
		rasterizer = Vk.Ppl.RstSt.CreateInfo {
			Vk.Ppl.RstSt.createInfoNext = Nothing,
			Vk.Ppl.RstSt.createInfoFlags =
				Vk.Ppl.RstSt.CreateFlagsZero,
			Vk.Ppl.RstSt.createInfoDepthClampEnable = False,
			Vk.Ppl.RstSt.createInfoRasterizerDiscardEnable = False,
			Vk.Ppl.RstSt.createInfoPolygonMode = Vk.PolygonModeFill,
			Vk.Ppl.RstSt.createInfoLineWidth = 1,
			Vk.Ppl.RstSt.createInfoCullMode = Vk.CullModeBackBit,
			Vk.Ppl.RstSt.createInfoFrontFace =
				Vk.FrontFaceClockwise,
			Vk.Ppl.RstSt.createInfoDepthBiasEnable = False,
			Vk.Ppl.RstSt.createInfoDepthBiasConstantFactor = 0,
			Vk.Ppl.RstSt.createInfoDepthBiasClamp = 0,
			Vk.Ppl.RstSt.createInfoDepthBiasSlopeFactor = 0 }
		multisample = Vk.Ppl.MulSmplSt.CreateInfo {
			Vk.Ppl.MulSmplSt.createInfoNext = Nothing,
			Vk.Ppl.MulSmplSt.createInfoFlags =
				Vk.Ppl.MulSmplSt.CreateFlagsZero,
			Vk.Ppl.MulSmplSt.createInfoRasterizationSamplesAndMask =
				Vk.Sample.CountAndMask
					Vk.Sample.Count1Bit Nothing,
			Vk.Ppl.MulSmplSt.createInfoSampleShadingEnable = False,
			Vk.Ppl.MulSmplSt.createInfoMinSampleShading = 0,
			Vk.Ppl.MulSmplSt.createInfoAlphaToCoverageEnable =
				False,
			Vk.Ppl.MulSmplSt.createInfoAlphaToOneEnable = False }
		blendattachment = Vk.Ppl.ClrBlndAtt.State {
			Vk.Ppl.ClrBlndAtt.stateColorWriteMask =
				Vk.ColorComponent.ABit .|.
				Vk.ColorComponent.RBit .|.
				Vk.ColorComponent.GBit .|.
				Vk.ColorComponent.BBit,
			Vk.Ppl.ClrBlndAtt.stateBlendEnable = False,
			Vk.Ppl.ClrBlndAtt.stateSrcColorBlendFactor =
				Vk.BlendFactorZero,
			Vk.Ppl.ClrBlndAtt.stateDstColorBlendFactor =
				Vk.BlendFactorZero,
			Vk.Ppl.ClrBlndAtt.stateColorBlendOp = Vk.BlendOpAdd,
			Vk.Ppl.ClrBlndAtt.stateSrcAlphaBlendFactor =
				Vk.BlendFactorZero,
			Vk.Ppl.ClrBlndAtt.stateDstAlphaBlendFactor =
				Vk.BlendFactorZero,
			Vk.Ppl.ClrBlndAtt.stateAlphaBlendOp = Vk.BlendOpAdd }
		blend = Vk.Ppl.ClrBlndSt.CreateInfo {
			Vk.Ppl.ClrBlndSt.createInfoNext = Nothing,
			Vk.Ppl.ClrBlndSt.createInfoFlags =
				Vk.Ppl.ClrBlndSt.CreateFlagsZero,
			Vk.Ppl.ClrBlndSt.createInfoLogicOpEnable = False,
			Vk.Ppl.ClrBlndSt.createInfoLogicOp = Vk.LogicOpClear,
			Vk.Ppl.ClrBlndSt.createInfoAttachments =
				[blendattachment],
			Vk.Ppl.ClrBlndSt.createInfoBlendConstants =
				fromJust $ rgbaDouble 0 0 0 0 }
		layoutCreateInfo = Vk.Ppl.Lyt.CreateInfo {
			Vk.Ppl.Lyt.createInfoNext = Nothing,
			Vk.Ppl.Lyt.createInfoFlags = Vk.Ppl.Lyt.CreateFlagsZero,
			Vk.Ppl.Lyt.createInfoSetLayouts = [],
			Vk.Ppl.Lyt.createInfoPushConstantRanges = [] }
		vertShaderCreateInfo = Vk.Shader.Module.CreateInfo {
			Vk.Shader.Module.createInfoNext = Nothing,
			Vk.Shader.Module.createInfoFlags =
				Vk.Shader.Module.CreateFlagsZero,
			Vk.Shader.Module.createInfoCode = glslVertexShaderMain }
		vertShaderStage = Vk.Ppl.ShSt.CreateInfo {
			Vk.Ppl.ShSt.createInfoNext = Nothing,
			Vk.Ppl.ShSt.createInfoFlags =
				Vk.Ppl.ShSt.CreateFlagsZero,
			Vk.Ppl.ShSt.createInfoStage = Vk.Shader.Stage.VertexBit,
			Vk.Ppl.ShSt.createInfoModule =
				Vk.Shader.Module.M vertShaderCreateInfo nil nil,
			Vk.Ppl.ShSt.createInfoName = "main",
			Vk.Ppl.ShSt.createInfoSpecializationInfo = Nothing }
		fragShaderCreateInfo = Vk.Shader.Module.CreateInfo {
			Vk.Shader.Module.createInfoNext = Nothing,
			Vk.Shader.Module.createInfoFlags =
				Vk.Shader.Module.CreateFlagsZero,
			Vk.Shader.Module.createInfoCode =
				glslFragmentShaderMain }
		fragShaderStage = Vk.Ppl.ShSt.CreateInfo {
			Vk.Ppl.ShSt.createInfoNext = Nothing,
			Vk.Ppl.ShSt.createInfoFlags =
				Vk.Ppl.ShSt.CreateFlagsZero,
			Vk.Ppl.ShSt.createInfoStage =
				Vk.Shader.Stage.FragmentBit,
			Vk.Ppl.ShSt.createInfoModule =
				Vk.Shader.Module.M fragShaderCreateInfo nil nil,
			Vk.Ppl.ShSt.createInfoName = "main",
			Vk.Ppl.ShSt.createInfoSpecializationInfo = Nothing }
	Vk.Ppl.Lyt.create @() dvc layoutCreateInfo nil nil \pipelineLayout -> do
		let	pipelineCreateInfo :: Vk.Ppl.Gr.CreateInfo
				() () ()
				'[ 'GlslVertexShader, 'GlslFragmentShader] () ()
				'[(), ()] ()
				() '[]
				() () () () () () () () _ _ _ _ '[]
			pipelineCreateInfo = Vk.Ppl.Gr.CreateInfo {
				Vk.Ppl.Gr.createInfoNext = Nothing,
				Vk.Ppl.Gr.createInfoFlags =
					Vk.Ppl.CreateFlagsZero,
				Vk.Ppl.Gr.createInfoStages =
					vertShaderStage
					`Vk.Ppl.ShSt.CreateInfoCons`
					fragShaderStage
					`Vk.Ppl.ShSt.CreateInfoCons`
					Vk.Ppl.ShSt.CreateInfoNil,
				Vk.Ppl.Gr.createInfoVertexInputState =
					Just vertexInputInfo,
				Vk.Ppl.Gr.createInfoInputAssemblyState =
					Just inputAssembly,
				Vk.Ppl.Gr.createInfoTessellationState = Nothing,
				Vk.Ppl.Gr.createInfoViewportState =
					Just viewportState,
				Vk.Ppl.Gr.createInfoRasterizationState =
					Just rasterizer,
				Vk.Ppl.Gr.createInfoMultisampleState =
					Just multisample,
				Vk.Ppl.Gr.createInfoDepthStencilState = Nothing,
				Vk.Ppl.Gr.createInfoColorBlendState =
					Just blend,
				Vk.Ppl.Gr.createInfoDynamicState = Nothing,
				Vk.Ppl.Gr.createInfoLayout = pipelineLayout,
				Vk.Ppl.Gr.createInfoRenderPass = rp,
				Vk.Ppl.Gr.createInfoSubpass = 0,
				Vk.Ppl.Gr.createInfoBasePipelineHandle = Nothing,
				Vk.Ppl.Gr.createInfoBasePipelineIndex = - 1 }
		Vk.Ppl.Gr.createGs dvc Nothing (
			pipelineCreateInfo `Vk.Ppl.Gr.CreateInfoCons`
			Vk.Ppl.Gr.CreateInfoNil ) nil nil \case
				(g `Vk.Ppl.Gr.GCons` _) -> print g >> f g
				_ -> error "never occur"

[glslVertexShader|

#version 450
#extension GL_ARB_separate_shader_objects : enable

void
main()
{
	if (gl_VertexIndex == 0)
	{
		gl_Position = vec4(0.0, -0.5, 0.0, 1.0);
	}
	else if (gl_VertexIndex == 1)
	{
		gl_Position = vec4(0.5, 0.5, 0.0, 1.0);
	}
	else if (gl_VertexIndex == 2)
	{
		gl_Position = vec4(-0.5, 0.5, 0.0, 1.0);
	}
}

|]

[glslFragmentShader|

#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(location = 0) out vec4 outColor;

void
main()
{
	outColor = vec4(1.0, 0.0, 0.0, 1.0);
}

|]
