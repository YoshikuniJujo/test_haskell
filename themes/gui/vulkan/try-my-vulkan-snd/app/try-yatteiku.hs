{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-partial-type-signatures #-}

module Main where

import qualified Gpu.Vulkan.Memory as Vk.Mem

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Gpu.Vulkan.Object.Base qualified as KObj
import Gpu.Vulkan.Object qualified as VObj
import Data.Default
import Data.Bits
import Data.Maybe
import Data.List
import Data.Array
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Word
import Data.Color
import Codec.Picture

import Shaderc.TH
import Shaderc.EnumAuto

import Gpu.Vulkan.Misc

import qualified Gpu.Vulkan as Vk
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Enum as Vk
import qualified Gpu.Vulkan.TypeEnum as Vk.T
import qualified Gpu.Vulkan.Instance as Vk.Instance
import qualified Gpu.Vulkan.PhysicalDevice as Vk.PhysicalDevice
import qualified Gpu.Vulkan.Device as Vk.Device
import qualified Gpu.Vulkan.Device.Middle as Vk.Dvc.M
import qualified Gpu.Vulkan.QueueFamily as Vk.QueueFamily

import qualified Gpu.Vulkan.CommandPool as Vk.CommandPool
import qualified Gpu.Vulkan.CommandBuffer as Vk.CommandBuffer
import qualified "try-my-vulkan-snd" Gpu.Vulkan.CommandBuffer.Enum as Vk.CommandBuffer
import qualified Gpu.Vulkan.CommandBuffer.Middle as Vk.CommandBuffer.M
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.Queue.Enum as Vk.Queue
import qualified Gpu.Vulkan.Image as Vk.Img
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Image.Enum as Vk.Img
import qualified Gpu.Vulkan.Image as Vk.Img.M
import qualified Gpu.Vulkan.Sample as Vk.Sample
import qualified Gpu.Vulkan.Sample.Enum as Vk.Sample
import qualified Gpu.Vulkan.Memory.Enum as Vk.Memory
import qualified Gpu.Vulkan.Memory.Middle as Vk.Memory.M
import qualified Gpu.Vulkan.Memory as Vk.Memory
import qualified Gpu.Vulkan.Attachment as Vk.Attachment
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Attachment.Enum as Vk.Attachment
import qualified Gpu.Vulkan.Subpass as Vk.Subpass
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Subpass.Enum as Vk.Subpass
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Pipeline.Enum as Vk.Ppl
import qualified Gpu.Vulkan.RenderPass as Vk.RenderPass
import qualified Gpu.Vulkan.RenderPass.Enum as Vk.RenderPass
import qualified Gpu.Vulkan.Pipeline.ViewportState as Vk.Ppl.ViewportState
import qualified Gpu.Vulkan.Pipeline.VertexInputState as
	Vk.Ppl.VertexInputState
import qualified Gpu.Vulkan.Pipeline.InputAssemblyState as Vk.Ppl.InpAssSt
import qualified Gpu.Vulkan.Pipeline.RasterizationState as Vk.Ppl.RstSt
import qualified Gpu.Vulkan.Pipeline.MultisampleState as Vk.Ppl.MulSmplSt
import qualified Gpu.Vulkan.Pipeline.ColorBlendAttachment as Vk.Ppl.ClrBlndAtt
import qualified Gpu.Vulkan.ColorComponent.Enum as Vk.ColorComponent
import qualified Gpu.Vulkan.Pipeline.ColorBlendState as Vk.Ppl.ClrBlndSt
import qualified Gpu.Vulkan.PipelineLayout as Vk.Ppl.Lyt
import qualified Gpu.Vulkan.Pipeline.Graphics as Vk.Ppl.Gr
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShSt
import qualified Gpu.Vulkan.Pipeline.ShaderStage.Enum as Vk.Ppl.ShSt
import qualified Gpu.Vulkan.ShaderModule as Vk.Shader.Module
import qualified Gpu.Vulkan.ImageView as Vk.ImgView
import qualified Gpu.Vulkan.ImageView.Enum as Vk.ImgView
import qualified Gpu.Vulkan.Component as Vk.Component
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Component.Enum as Vk.Component
import qualified Gpu.Vulkan.Framebuffer as Vk.Framebuffer
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Framebuffer.Enum as Vk.Framebuffer
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import qualified Gpu.Vulkan.Khr as Vk.Khr

import qualified Gpu.Vulkan.Buffer as Vk.Bffr
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Buffer.Enum as Vk.Bffr
import qualified Gpu.Vulkan.PushConstant as Vk.PushConstant

import Tools

screenWidth, screenHeight :: Word32
(screenWidth, screenHeight) = (640, 480)

main :: IO ()
main = do
	let	createInfo :: Vk.Instance.CreateInfo 'Nothing 'Nothing
		createInfo = def {
			Vk.Instance.createInfoEnabledLayerNames =
				[Vk.Khr.validationLayerName] }
	Vk.Instance.create createInfo nil' \inst -> do
		(physicalDevice, graphicsQueueFamilyIndex) <-
			selectPhysicalDeviceAndQueueFamily
				=<< Vk.PhysicalDevice.enumerate inst
		print physicalDevice
		print graphicsQueueFamilyIndex
		let	queueCreateInfo = Vk.Device.QueueCreateInfo {
				Vk.Device.queueCreateInfoNext = TMaybe.N,
				Vk.Device.queueCreateInfoFlags = zeroBits,
				Vk.Device.queueCreateInfoQueueFamilyIndex =
					graphicsQueueFamilyIndex,
				Vk.Device.queueCreateInfoQueuePriorities =
					[1.0] }
			devCreateInfo :: Vk.Device.CreateInfo 'Nothing '[ 'Nothing]
			devCreateInfo = Vk.Device.CreateInfo {
				Vk.Device.createInfoNext = TMaybe.N,
				Vk.Device.createInfoFlags = zeroBits,
				Vk.Device.createInfoQueueCreateInfos =
					HeteroParList.Singleton queueCreateInfo,
				Vk.Device.createInfoEnabledLayerNames =
					[Vk.Khr.validationLayerName],
				Vk.Device.createInfoEnabledExtensionNames = [],
				Vk.Device.createInfoEnabledFeatures = Nothing }
		Vk.Device.create physicalDevice devCreateInfo nil' \dvc ->
			runDevice physicalDevice dvc graphicsQueueFamilyIndex

runDevice :: Vk.PhysicalDevice.P -> Vk.Device.D sd -> Vk.QueueFamily.Index -> IO ()
runDevice phdvc device graphicsQueueFamilyIndex =
	makeRenderPass device \rp ->
	makePipelineNew device rp \ppl ->
	makeImage' phdvc device \bimg' _mi' ->
	makeBuffer phdvc device screenWidth screenHeight \b bm -> do
		makeImageView device bimg' \iv ->
			makeFramebuffer device rp iv \fb ->
			makeCommandBufferEtc device graphicsQueueFamilyIndex \gq cp -> do
				makeCommandBuffer device gq cp \cb -> do
					let	renderpassBeginInfo = Vk.RenderPass.BeginInfo {
							Vk.RenderPass.beginInfoNext = TMaybe.N,
							Vk.RenderPass.beginInfoRenderPass = rp,
							Vk.RenderPass.beginInfoFramebuffer = fb,
							Vk.RenderPass.beginInfoRenderArea = Vk.Rect2d {
								Vk.rect2dOffset = Vk.Offset2d 0 0,
								Vk.rect2dExtent = Vk.Extent2d
									screenWidth screenHeight },
							Vk.RenderPass.beginInfoClearValues = HeteroParList.Nil }
					Vk.Cmd.beginRenderPass @'Nothing @'[]
						cb renderpassBeginInfo Vk.Subpass.ContentsInline $
						Vk.Cmd.bindPipelineGraphics cb Vk.Ppl.BindPointGraphics ppl \cbb ->
						Vk.Cmd.draw cbb 3 1 0 0
				transitionImageLayout device gq cp bimg'
					Vk.Img.LayoutUndefined Vk.Img.LayoutTransferSrcOptimal
				copyBufferToImage device gq cp bimg' b screenWidth screenHeight
		print screenWidth
		print screenHeight
		MyImage img <- Vk.Memory.read @"image-buffer" @(VObj.Image 1 MyImage "") device bm def
		writePng "yatteiku.png" (img :: Image PixelRGBA8)

makeCommandBufferEtc :: Vk.Device.D sd -> Vk.QueueFamily.Index ->
	(forall scp . Vk.Queue.Q -> Vk.CommandPool.C scp -> IO a) -> IO a
makeCommandBufferEtc device graphicsQueueFamilyIndex f = do
	graphicsQueue <- Vk.Device.getQueue device graphicsQueueFamilyIndex 0
	print graphicsQueue
	let	cmdPoolCreateInfo :: Vk.CommandPool.CreateInfo 'Nothing
		cmdPoolCreateInfo = Vk.CommandPool.CreateInfo {
			Vk.CommandPool.createInfoNext = TMaybe.N,
			Vk.CommandPool.createInfoFlags = zeroBits,
			Vk.CommandPool.createInfoQueueFamilyIndex =
				graphicsQueueFamilyIndex }
	Vk.CommandPool.create device cmdPoolCreateInfo nil'
			\(cmdPool :: Vk.CommandPool.C s) -> f graphicsQueue cmdPool

makeCommandBuffer :: forall sd scp a . Vk.Device.D sd -> Vk.Queue.Q -> Vk.CommandPool.C scp ->
	(forall s . Vk.CommandBuffer.C s -> IO a) -> IO a
makeCommandBuffer device graphicsQueue cmdPool f = do
		let	cmdBufAllocInfo :: Vk.CommandBuffer.AllocateInfo 'Nothing scp '[ '()]
			cmdBufAllocInfo = Vk.CommandBuffer.AllocateInfo {
				Vk.CommandBuffer.allocateInfoNext = TMaybe.N,
				Vk.CommandBuffer.allocateInfoCommandPool =
					cmdPool,
				Vk.CommandBuffer.allocateInfoLevel =
					Vk.CommandBuffer.LevelPrimary }
		Vk.CommandBuffer.allocate device cmdBufAllocInfo \(cmdBuf :*. HeteroParList.Nil) -> do
				r <- Vk.CommandBuffer.begin cmdBuf
					(def :: Vk.CommandBuffer.BeginInfo 'Nothing 'Nothing) $ f cmdBuf
				let	submitInfo :: Vk.SubmitInfo 'Nothing _ _ _
					submitInfo = Vk.SubmitInfo {
						Vk.submitInfoNext = TMaybe.N,
						Vk.submitInfoWaitSemaphoreDstStageMasks = HeteroParList.Nil,
						Vk.submitInfoCommandBuffers = cmdBuf :** HeteroParList.Nil,
						Vk.submitInfoSignalSemaphores = HeteroParList.Nil }
				Vk.Queue.submit graphicsQueue (U4 submitInfo :** HeteroParList.Nil) Nothing
				Vk.Queue.waitIdle graphicsQueue
				pure r

makeImage' :: Vk.PhysicalDevice.P -> Vk.Device.D sd ->
	(forall si sm .
		Vk.Img.Binded sm si nm 'Vk.T.FormatR8g8b8a8Unorm ->
		Vk.Memory.M sm '[ '(si, 'Vk.Memory.ImageArg nm 'Vk.T.FormatR8g8b8a8Unorm)] -> IO a) ->
	IO a
makeImage' phdvc dvc f = do
	let	imgCreateInfo = Vk.Img.CreateInfo {
			Vk.Img.createInfoNext = TMaybe.N,
			Vk.Img.createInfoFlags = Vk.Img.CreateFlagsZero,
			Vk.Img.createInfoImageType = Vk.Img.Type2d,
			Vk.Img.createInfoExtent =
				Vk.Extent3d screenWidth screenHeight 1,
			Vk.Img.createInfoMipLevels = 1,
			Vk.Img.createInfoArrayLayers = 1,
			Vk.Img.createInfoTiling = Vk.Img.TilingLinear,
			Vk.Img.createInfoInitialLayout =
				Vk.Img.LayoutUndefined,
--				Vk.Img.LayoutTransferSrcOptimal,
			Vk.Img.createInfoUsage =
				Vk.Img.UsageColorAttachmentBit .|.
				Vk.Img.UsageTransferSrcBit,
			Vk.Img.createInfoSharingMode =
				Vk.SharingModeExclusive,
			Vk.Img.createInfoSamples = Vk.Sample.Count1Bit,
			Vk.Img.createInfoQueueFamilyIndices = [] }
	memProps <- Vk.PhysicalDevice.getMemoryProperties phdvc
	print memProps
	Vk.Img.create @'Nothing dvc imgCreateInfo nil' \image -> do
		imgMemReq <- Vk.Img.getMemoryRequirements dvc image
		print imgMemReq
		let	imgMemReqTypes =
				Vk.Memory.M.requirementsMemoryTypeBits imgMemReq
			memPropTypes = (fst <$>)
				. filter ((/= zeroBits)
					. (.&. Vk.Memory.PropertyHostVisibleBit)
					. Vk.Memory.M.mTypePropertyFlags . snd)
				$ Vk.PhysicalDevice.memoryPropertiesMemoryTypes
					memProps
			memoryTypeIndex = case filter
				(`Vk.Memory.M.elemTypeIndex` imgMemReqTypes)
				memPropTypes of
				[] -> error "No available memory types"
				i : _ -> i
			imgMemAllocInfo = Vk.Memory.AllocateInfo {
				Vk.Memory.allocateInfoNext = TMaybe.N,
				Vk.Memory.allocateInfoMemoryTypeIndex =
					memoryTypeIndex }
		Vk.Memory.allocateBind @'Nothing
			dvc (HeteroParList.Singleton . U2 $ Vk.Memory.Image image)
			imgMemAllocInfo nil' \(HeteroParList.Singleton (U2 (Vk.Memory.ImageBinded bimg))) imgMem -> do
			f bimg imgMem

makeBuffer :: Vk.PhysicalDevice.P -> Vk.Device.D sd -> Word32 -> Word32 ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb "image-buffer" '[ VObj.Image 1 MyImage ""] ->
		Vk.Memory.M sm '[ '(
			sb,
			'Vk.Memory.BufferArg "image-buffer" '[ VObj.Image 1 MyImage ""])] ->
			IO a) -> IO a
makeBuffer phdvc dvc wdt hgt f =
	createBufferImage phdvc dvc
		(fromIntegral wdt, fromIntegral wdt, fromIntegral hgt, 1) 
		Vk.Bffr.UsageTransferDstBit
		(	Vk.Memory.PropertyHostVisibleBit .|.
			Vk.Memory.PropertyHostCoherentBit ) f

copyBufferToImage :: forall sd sc sm sb nm img inm si sm' nm' .
	Storable (KObj.ImagePixel img) =>
	Vk.Device.D sd -> Vk.Queue.Q -> Vk.CommandPool.C sc ->
--	Vk.Img.Binded sm' si nm' (Vk.Bffr.ImageFormat img) ->
	Vk.Img.Binded sm' si nm' (KObj.ImageFormat img) ->
	Vk.Bffr.Binded sm sb nm '[ VObj.Image 1 img inm]  ->
	Word32 -> Word32 -> IO ()
copyBufferToImage dvc gq cp img bf wdt hgt =
	beginSingleTimeCommands dvc gq cp \cb -> do
	let	region :: Vk.Bffr.ImageCopy img inm
		region = Vk.Bffr.ImageCopy {
			Vk.Bffr.imageCopyImageSubresource = isr,
			Vk.Bffr.imageCopyImageOffset = Vk.Offset3d 0 0 0,
			Vk.Bffr.imageCopyImageExtent = Vk.Extent3d wdt hgt 1 }
		isr = Vk.Img.M.SubresourceLayers {
			Vk.Img.M.subresourceLayersAspectMask =
				Vk.Img.AspectColorBit,
			Vk.Img.M.subresourceLayersMipLevel = 0,
			Vk.Img.M.subresourceLayersBaseArrayLayer = 0,
			Vk.Img.M.subresourceLayersLayerCount = 1 }
	Vk.Cmd.copyImageToBuffer @1
		cb img Vk.Img.LayoutTransferSrcOptimal bf (HeteroParList.Singleton region)

transitionImageLayout :: forall sd sc si sm nm fmt .
	Vk.Device.D sd -> Vk.Queue.Q -> Vk.CommandPool.C sc ->
	Vk.Img.Binded sm si nm fmt -> Vk.Img.Layout -> Vk.Img.Layout ->
	IO ()
transitionImageLayout dvc gq cp img olyt nlyt =
	beginSingleTimeCommands dvc gq cp \cb -> do
	let	barrier :: Vk.Img.MemoryBarrier 'Nothing sm si nm fmt
		barrier = Vk.Img.MemoryBarrier {
			Vk.Img.memoryBarrierNext = TMaybe.N,
			Vk.Img.memoryBarrierOldLayout = olyt,
			Vk.Img.memoryBarrierNewLayout = nlyt,
			Vk.Img.memoryBarrierSrcQueueFamilyIndex =
				Vk.QueueFamily.Ignored,
			Vk.Img.memoryBarrierDstQueueFamilyIndex =
				Vk.QueueFamily.Ignored,
			Vk.Img.memoryBarrierImage = img,
			Vk.Img.memoryBarrierSubresourceRange = srr,
			Vk.Img.memoryBarrierSrcAccessMask = sam,
			Vk.Img.memoryBarrierDstAccessMask = dam }
		srr = Vk.Img.SubresourceRange {
			Vk.Img.subresourceRangeAspectMask =
				Vk.Img.AspectColorBit,
			Vk.Img.subresourceRangeBaseMipLevel = 0,
			Vk.Img.subresourceRangeLevelCount = 1,
			Vk.Img.subresourceRangeBaseArrayLayer = 0,
			Vk.Img.subresourceRangeLayerCount = 1 }
	Vk.Cmd.pipelineBarrier cb
		sstg dstg zeroBits HeteroParList.Nil HeteroParList.Nil (HeteroParList.Singleton $ U5 barrier)
	where (sam, dam, sstg, dstg) = case (olyt, nlyt) of
		(Vk.Img.LayoutUndefined, Vk.Img.LayoutTransferDstOptimal) -> (
			zeroBits, Vk.AccessTransferWriteBit,
			Vk.Ppl.StageTopOfPipeBit, Vk.Ppl.StageTransferBit )
		(Vk.Img.LayoutTransferDstOptimal,
			Vk.Img.LayoutShaderReadOnlyOptimal ) -> (
			Vk.AccessTransferWriteBit, Vk.AccessShaderReadBit,
			Vk.Ppl.StageTransferBit, Vk.Ppl.StageFragmentShaderBit )
		(Vk.Img.LayoutUndefined, Vk.Img.LayoutTransferSrcOptimal) -> (
			zeroBits, Vk.AccessTransferWriteBit,
			Vk.Ppl.StageTopOfPipeBit, Vk.Ppl.StageTransferBit )
--			Vk.Ppl.StageTransferBit, Vk.Ppl.StageTransferBit )
		_ -> error "unsupported layout transition!"


beginSingleTimeCommands :: forall sd sc a .
	Vk.Device.D sd -> Vk.Queue.Q -> Vk.CommandPool.C sc ->
	(forall s . Vk.CommandBuffer.C s -> IO a) -> IO a
beginSingleTimeCommands dvc gq cp cmd = do
	Vk.CommandBuffer.allocate
		dvc allocInfo \((cb :: Vk.CommandBuffer.C s) :*. HeteroParList.Nil) -> do
		let	submitInfo :: Vk.SubmitInfo 'Nothing '[] '[s] '[]
			submitInfo = Vk.SubmitInfo {
				Vk.submitInfoNext = TMaybe.N,
				Vk.submitInfoWaitSemaphoreDstStageMasks = HeteroParList.Nil,
				Vk.submitInfoCommandBuffers = HeteroParList.Singleton cb,
				Vk.submitInfoSignalSemaphores = HeteroParList.Nil }
		Vk.CommandBuffer.begin @'Nothing @'Nothing cb beginInfo (cmd cb) <* do
			Vk.Queue.submit gq (HeteroParList.Singleton $ U4 submitInfo) Nothing
			Vk.Queue.waitIdle gq
	where
	allocInfo :: Vk.CommandBuffer.AllocateInfo 'Nothing sc '[ '()]
	allocInfo = Vk.CommandBuffer.AllocateInfo {
		Vk.CommandBuffer.allocateInfoNext = TMaybe.N,
		Vk.CommandBuffer.allocateInfoCommandPool = cp,
		Vk.CommandBuffer.allocateInfoLevel = Vk.CommandBuffer.LevelPrimary }
	beginInfo = Vk.CommandBuffer.M.BeginInfo {
		Vk.CommandBuffer.beginInfoNext = TMaybe.N,
		Vk.CommandBuffer.beginInfoFlags = Vk.CommandBuffer.UsageOneTimeSubmitBit,
		Vk.CommandBuffer.beginInfoInheritanceInfo = Nothing }

createBufferImage :: Storable (KObj.ImagePixel t) =>
	Vk.PhysicalDevice.P -> Vk.Device.D sd -> (Vk.Dvc.M.Size, Vk.Dvc.M.Size, Vk.Dvc.M.Size, Vk.Dvc.M.Size) ->
	Vk.Bffr.UsageFlags -> Vk.Memory.PropertyFlags ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb nm '[ VObj.Image 1 t inm] ->
		Vk.Memory.M sm '[ '(
			sb,
			'Vk.Memory.BufferArg nm '[ VObj.Image 1 t inm])] ->
		IO a) -> IO a
createBufferImage p dv (r, w, h, d) usg props =
	createBuffer p dv (VObj.LengthImage r w h d) usg props

createBuffer :: forall sd nm o a . VObj.SizeAlignment o =>
	Vk.PhysicalDevice.P -> Vk.Device.D sd -> VObj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Memory.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[o] ->
		Vk.Memory.M sm
			'[ '(sb, 'Vk.Memory.BufferArg nm '[o])] ->
		IO a) -> IO a
createBuffer p dv ln usg props f = Vk.Bffr.create dv bffrInfo nil' \b -> do
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMemoryType p (Vk.Memory.M.requirementsMemoryTypeBits reqs) props
	Vk.Memory.allocateBind dv (HeteroParList.Singleton . U2 $ Vk.Memory.Buffer b)
		(allcInfo mt) nil'
		$ f . \(HeteroParList.Singleton (U2 (Vk.Memory.BufferBinded bnd))) -> bnd
	where
	bffrInfo :: Vk.Bffr.CreateInfo 'Nothing '[o]
	bffrInfo = Vk.Bffr.CreateInfo {
		Vk.Bffr.createInfoNext = TMaybe.N,
		Vk.Bffr.createInfoFlags = zeroBits,
		Vk.Bffr.createInfoLengths = HeteroParList.Singleton ln,
		Vk.Bffr.createInfoUsage = usg,
		Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Bffr.createInfoQueueFamilyIndices = [] }
	allcInfo :: Vk.Memory.M.TypeIndex -> Vk.Mem.AllocateInfo 'Nothing
	allcInfo mt = Vk.Mem.AllocateInfo {
		Vk.Mem.allocateInfoNext = TMaybe.N,
		Vk.Mem.allocateInfoMemoryTypeIndex = mt }

findMemoryType :: Vk.PhysicalDevice.P -> Vk.Memory.M.TypeBits -> Vk.Memory.PropertyFlags ->
	IO Vk.Memory.M.TypeIndex
findMemoryType phdvc flt props =
	fromMaybe (error msg) . suitable <$> Vk.PhysicalDevice.getMemoryProperties phdvc
	where
	msg = "failed to find suitable memory type!"
	suitable props1 = fst <$> find ((&&)
		<$> (`Vk.Memory.M.elemTypeIndex` flt) . fst
		<*> checkBits props . Vk.Memory.M.mTypePropertyFlags . snd) tps
		where tps = Vk.PhysicalDevice.memoryPropertiesMemoryTypes props1

newtype MyImage = MyImage (Image PixelRGBA8)

-- type instance Vk.Bffr.ImageFormat MyImage = 'Vk.T.FormatR8g8b8a8Unorm

newtype MyRgba8 = MyRgba8 { unMyRgba8 :: PixelRGBA8 }

instance Storable MyRgba8 where
	sizeOf _ = 4 * sizeOf @Pixel8 undefined
	alignment _ = alignment @Pixel8 undefined
	peek p = MyRgba8 . (\(r, g, b, a) -> PixelRGBA8 r g b a) . listToTuple4
		<$> peekArray 4 (castPtr p)
	poke p (MyRgba8 (PixelRGBA8 r g b a)) =
		pokeArray (castPtr p) [r, g, b, a]

listToTuple4 :: [a] -> (a, a, a, a)
listToTuple4 [r, g, b, a] = (r, g, b, a)
listToTuple4 _ = error "The length of the list is not 4"

instance KObj.IsImage MyImage where
	type ImagePixel MyImage = MyRgba8
	type ImageFormat MyImage = 'Vk.T.FormatR8g8b8a8Unorm
	imageRow = KObj.imageWidth
	imageWidth (MyImage img) = fromIntegral $ imageWidth img
	imageHeight (MyImage img) = fromIntegral $ imageHeight img
	imageDepth _ = 1
	imageBody (MyImage img) = (<$> [0 .. imageHeight img - 1]) \y ->
		(<$> [0 .. imageWidth img - 1]) \x -> MyRgba8 $ pixelAt img x y
	imageMake w h _d pss = MyImage
		$ generateImage (\x y -> let MyRgba8 p = (pss' ! y) ! x in p) (fromIntegral w) (fromIntegral h)
		where pss' = listArray (0, fromIntegral h - 1) (listArray (0, fromIntegral w - 1) <$> pss)

makeImageView :: Vk.Device.D sd -> Vk.Img.Binded sm si nm fmt ->
	(forall s . Vk.ImgView.I nm Vk.T.FormatR8g8b8a8Unorm s -> IO a) -> IO a
makeImageView dvc bimg f =
	Vk.ImgView.create dvc imgViewCreateInfo nil' \imgView -> do
		putStrLn $ "imgView: " ++ show imgView
		f imgView
	where	imgViewCreateInfo = Vk.ImgView.CreateInfo {
			Vk.ImgView.createInfoNext = TMaybe.N,
			Vk.ImgView.createInfoFlags =
				Vk.ImgView.CreateFlagsZero,
			Vk.ImgView.createInfoImage = bimg,
			Vk.ImgView.createInfoViewType = Vk.ImgView.Type2d,
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
					Vk.Img.subresourceRangeLayerCount = 1 } }

makeFramebuffer :: Vk.Device.D sd -> Vk.RenderPass.R sr -> Vk.ImgView.I nm fmt si ->
	(forall s . Vk.Framebuffer.F s -> IO a) -> IO a
makeFramebuffer dvc rp iv f =
	Vk.Framebuffer.create @'Nothing dvc frameBufCreateInfo nil' f
	where	frameBufCreateInfo = Vk.Framebuffer.CreateInfo {
			Vk.Framebuffer.createInfoNext = TMaybe.N,
			Vk.Framebuffer.createInfoFlags =
				Vk.Framebuffer.CreateFlagsZero,
			Vk.Framebuffer.createInfoRenderPass = rp,
			Vk.Framebuffer.createInfoAttachments = U3 iv :** HeteroParList.Nil,
			Vk.Framebuffer.createInfoWidth = screenWidth,
			Vk.Framebuffer.createInfoHeight = screenHeight,
			Vk.Framebuffer.createInfoLayers = 1 }

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
	let	attachmentNew :: Vk.Attachment.Description 'Vk.T.FormatR8g8b8a8Unorm
		attachmentNew = Vk.Attachment.Description {
			Vk.Attachment.descriptionFlags =
				Vk.Attachment.DescriptionFlagsZero,
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
--				Vk.Img.LayoutTransferSrcOptimal,
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
		renderPassCreateInfoNew :: Vk.RenderPass.CreateInfoNew 'Nothing _
		renderPassCreateInfoNew = Vk.RenderPass.CreateInfoNew {
			Vk.RenderPass.createInfoNextNew = TMaybe.N,
			Vk.RenderPass.createInfoFlagsNew =
				Vk.RenderPass.CreateFlagsZero,
			Vk.RenderPass.createInfoAttachmentsNew =
				attachmentNew :** HeteroParList.Nil,
			Vk.RenderPass.createInfoSubpassesNew = [subpass],
			Vk.RenderPass.createInfoDependenciesNew = [] }
	Vk.RenderPass.createNew dvc renderPassCreateInfoNew nil' f

makePipelineNew :: Vk.Device.D sd -> Vk.RenderPass.R sr ->
	(forall s sl . Vk.Ppl.Gr.G s '[] '[] '(sl, '[], '[]) -> IO a) -> IO a
makePipelineNew dvc rp f = do
	let	viewport = Vk.Viewport {
			Vk.viewportX = 0,
			Vk.viewportY = 0,
			Vk.viewportMinDepth = 0,
			Vk.viewportMaxDepth = 1,
			Vk.viewportWidth = fromIntegral screenWidth,
			Vk.viewportHeight = fromIntegral screenHeight }
		scissor = Vk.Rect2d {
			Vk.rect2dOffset = Vk.Offset2d 0 0,
			Vk.rect2dExtent =
				Vk.Extent2d screenWidth screenHeight }
		viewportState :: Vk.Ppl.ViewportState.CreateInfo 'Nothing
		viewportState = Vk.Ppl.ViewportState.CreateInfo {
			Vk.Ppl.ViewportState.createInfoNext = TMaybe.N,
			Vk.Ppl.ViewportState.createInfoFlags = zeroBits,
			Vk.Ppl.ViewportState.createInfoViewports =
				[viewport],
			Vk.Ppl.ViewportState.createInfoScissors =
				[scissor] }
		vertexInputInfo = Vk.Ppl.VertexInputState.CreateInfo {
			Vk.Ppl.VertexInputState.createInfoNext = TMaybe.N,
			Vk.Ppl.VertexInputState.createInfoFlags = zeroBits }
		inputAssembly :: Vk.Ppl.InpAssSt.CreateInfo 'Nothing
		inputAssembly = Vk.Ppl.InpAssSt.CreateInfo {
			Vk.Ppl.InpAssSt.createInfoNext = TMaybe.N,
			Vk.Ppl.InpAssSt.createInfoFlags = zeroBits,
			Vk.Ppl.InpAssSt.createInfoTopology =
				Vk.PrimitiveTopologyTriangleList,
			Vk.Ppl.InpAssSt.createInfoPrimitiveRestartEnable =
				False }
		rasterizer = Vk.Ppl.RstSt.CreateInfo {
			Vk.Ppl.RstSt.createInfoNext = TMaybe.N,
			Vk.Ppl.RstSt.createInfoFlags = zeroBits,
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
			Vk.Ppl.MulSmplSt.createInfoNext = TMaybe.N,
			Vk.Ppl.MulSmplSt.createInfoFlags = zeroBits,
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
			Vk.Ppl.ClrBlndSt.createInfoNext = TMaybe.N,
			Vk.Ppl.ClrBlndSt.createInfoFlags = zeroBits,
			Vk.Ppl.ClrBlndSt.createInfoLogicOpEnable = False,
			Vk.Ppl.ClrBlndSt.createInfoLogicOp = Vk.LogicOpClear,
			Vk.Ppl.ClrBlndSt.createInfoAttachments =
				[blendattachment],
			Vk.Ppl.ClrBlndSt.createInfoBlendConstants =
				fromJust $ rgbaDouble 0 0 0 0 }
		layoutCreateInfoNew :: Vk.Ppl.Lyt.CreateInfoNew 'Nothing '[]
			('Vk.PushConstant.PushConstantLayout '[] '[])
		layoutCreateInfoNew = Vk.Ppl.Lyt.CreateInfoNew {
			Vk.Ppl.Lyt.createInfoNextNew = TMaybe.N,
			Vk.Ppl.Lyt.createInfoFlagsNew = zeroBits,
			Vk.Ppl.Lyt.createInfoSetLayoutsNew = HeteroParList.Nil }
		vertShaderCreateInfo = Vk.Shader.Module.CreateInfo {
			Vk.Shader.Module.createInfoNext = TMaybe.N,
			Vk.Shader.Module.createInfoFlags = zeroBits,
			Vk.Shader.Module.createInfoCode = glslVertexShaderMain }
		vertShaderStage = Vk.Ppl.ShSt.CreateInfo {
			Vk.Ppl.ShSt.createInfoNextNew = TMaybe.N,
			Vk.Ppl.ShSt.createInfoFlagsNew =
				Vk.Ppl.ShSt.CreateFlagsZero,
			Vk.Ppl.ShSt.createInfoStageNew = Vk.ShaderStageVertexBit,
			Vk.Ppl.ShSt.createInfoModuleNew =
				Vk.Shader.Module.M vertShaderCreateInfo nil',
			Vk.Ppl.ShSt.createInfoNameNew = "main",
			Vk.Ppl.ShSt.createInfoSpecializationInfoNew = Nothing }
		fragShaderCreateInfo = Vk.Shader.Module.CreateInfo {
			Vk.Shader.Module.createInfoNext = TMaybe.N,
			Vk.Shader.Module.createInfoFlags = zeroBits,
			Vk.Shader.Module.createInfoCode =
				glslFragmentShaderMain }
		fragShaderStage = Vk.Ppl.ShSt.CreateInfo {
			Vk.Ppl.ShSt.createInfoNextNew = TMaybe.N,
			Vk.Ppl.ShSt.createInfoFlagsNew =
				Vk.Ppl.ShSt.CreateFlagsZero,
			Vk.Ppl.ShSt.createInfoStageNew =
				Vk.ShaderStageFragmentBit,
			Vk.Ppl.ShSt.createInfoModuleNew =
				Vk.Shader.Module.M fragShaderCreateInfo nil',
			Vk.Ppl.ShSt.createInfoNameNew = "main",
			Vk.Ppl.ShSt.createInfoSpecializationInfoNew = Nothing }
	Vk.Ppl.Lyt.createNew dvc layoutCreateInfoNew nil' \plyt -> do
		let	pipelineCreateInfo :: Vk.Ppl.Gr.CreateInfo 'Nothing '[
					'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
					'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
				'(	'Nothing, '[], '[] )
				'Nothing 'Nothing 'Nothing 'Nothing 'Nothing
				'Nothing 'Nothing 'Nothing '(_, _, _) _ '(_, '[], _, _)
			pipelineCreateInfo = Vk.Ppl.Gr.CreateInfo {
				Vk.Ppl.Gr.createInfoNext = TMaybe.N,
				Vk.Ppl.Gr.createInfoFlags =
					Vk.Ppl.CreateFlagsZero,
				Vk.Ppl.Gr.createInfoStages =
					U5 vertShaderStage :**
					U5 fragShaderStage :** HeteroParList.Nil,
				Vk.Ppl.Gr.createInfoVertexInputState =
					Just $ U3 vertexInputInfo,
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
				Vk.Ppl.Gr.createInfoLayout = U3 plyt,
				Vk.Ppl.Gr.createInfoRenderPass = rp,
				Vk.Ppl.Gr.createInfoSubpass = 0,
				Vk.Ppl.Gr.createInfoBasePipelineHandle = Nothing,
				Vk.Ppl.Gr.createInfoBasePipelineIndex = - 1 }
		Vk.Ppl.Gr.createGs dvc Nothing (
			U14 pipelineCreateInfo :** HeteroParList.Nil ) nil'
				\(U3 g :** HeteroParList.Nil) -> f g

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
