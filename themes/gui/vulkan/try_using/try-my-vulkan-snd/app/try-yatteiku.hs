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

module Main (main) where

import GHC.TypeNats
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Gpu.Vulkan.Object.Base qualified as BObj
import Gpu.Vulkan.Object qualified as VObj
import Data.Default
import Data.Bits
import Data.Maybe
import Data.List
import Data.Array
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import qualified Data.HeteroParList as HPList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Word
import Data.Color
import Codec.Picture

import Language.SpirV.Shaderc.TH
import Language.SpirV.ShaderKind

import Data.TypeLevel.ParMaybe (nil)

import qualified Gpu.Vulkan as Vk
import qualified Gpu.Vulkan.TypeEnum as Vk.T
import qualified Gpu.Vulkan.Instance as Vk.Ist
import qualified Gpu.Vulkan.PhysicalDevice as Vk.Phd
import qualified Gpu.Vulkan.Device as Vk.Dvc
import qualified Gpu.Vulkan.QueueFamily as Vk.QFam

import qualified Gpu.Vulkan.CommandPool as Vk.CmdPl
import qualified Gpu.Vulkan.CommandBuffer as Vk.CommandBuffer
import qualified Gpu.Vulkan.CommandBuffer as Vk.CommandBuffer.M
import qualified Gpu.Vulkan.Queue as Vk.Q
import qualified Gpu.Vulkan.Image as Vk.Img
import qualified Gpu.Vulkan.Image as Vk.Img.M
import qualified Gpu.Vulkan.Sample as Vk.Sample
import qualified Gpu.Vulkan.Memory as Vk.Mm.M
import qualified Gpu.Vulkan.Memory as Vk.Mm
import qualified Gpu.Vulkan.Attachment as Vk.Attachment
import qualified Gpu.Vulkan.Subpass as Vk.Subpass
import qualified "try-gpu-vulkan" Gpu.Vulkan.Pipeline as Vk.Ppl
import qualified Gpu.Vulkan.RenderPass as Vk.RenderPass
import qualified Gpu.Vulkan.Pipeline.ViewportState as Vk.Ppl.ViewportState
import Gpu.Vulkan.Pipeline.VertexInputState as
	Vk.Ppl.VertexInputState
import qualified Gpu.Vulkan.Pipeline.InputAssemblyState as Vk.Ppl.InpAssSt
import qualified Gpu.Vulkan.Pipeline.RasterizationState as Vk.Ppl.RstSt
import qualified Gpu.Vulkan.Pipeline.MultisampleState as Vk.Ppl.MulSmplSt
import qualified Gpu.Vulkan.Pipeline.ColorBlendAttachment as Vk.Ppl.ClrBlndAtt
import qualified Gpu.Vulkan.ColorComponent as Vk.ColorComponent
import qualified Gpu.Vulkan.Pipeline.ColorBlendState as Vk.Ppl.ClrBlndSt
import qualified Gpu.Vulkan.PipelineLayout as Vk.Ppl.Lyt
import qualified Gpu.Vulkan.Pipeline.Graphics as Vk.Ppl.Gr
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShSt
import qualified Gpu.Vulkan.ShaderModule as Vk.ShaderModule
import qualified Gpu.Vulkan.ImageView as Vk.ImgView
import qualified Gpu.Vulkan.Component as Vk.Component
import qualified Gpu.Vulkan.Framebuffer as Vk.Framebuffer
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import qualified Gpu.Vulkan.Buffer as Vk.Bffr
import qualified Gpu.Vulkan.PushConstant as Vk.PushConstant

import Data.Proxy
import Data.Bits.ToolsYj
import Data.Maybe.ToolsYj
import Data.List.ToolsYj

main :: IO ()
main =	createIst \it -> pickPhd it >>= \(pd, qfi) -> createLgDvc pd qfi \dv ->
	createRsltBffr pd dv imgWidth imgHeight
		\(b :: RsltBffr sm sb nm img) bm -> do
	Vk.Dvc.getQueue dv qfi 0 >>= \gq -> createCmdPl qfi dv \cp ->
		body pd dv gq cp \bimg' ->
		copyBufferToImage dv gq cp bimg' b imgWidth imgHeight
	ImageRgba8 img <- Vk.Mm.read @nm @img dv bm zeroBits
	writePng "yatteiku.png" img

imgWidth, imgHeight :: Word32
(imgWidth, imgHeight) = (640, 480)

createIst :: (forall si . Vk.Ist.I si -> IO a) -> IO a
createIst f = Vk.Ist.create info nil f
	where
	info :: Vk.Ist.CreateInfo 'Nothing 'Nothing
	info = def { Vk.Ist.createInfoEnabledLayerNames = vldLayers }

vldLayers :: [Vk.LayerName]
vldLayers = [Vk.layerKhronosValidation]

pickPhd :: Vk.Ist.I si -> IO (Vk.Phd.P, Vk.QFam.Index)
pickPhd ist = Vk.Phd.enumerate ist >>= \case
	[] -> error "failed to find GPUs with Gpu.Vulkan support!"
	pds -> findMaybeM suit pds >>= \case
		Nothing -> error "failed to find a suitable GPU!"
		Just pdqfi -> pure pdqfi
	where suit pd = findQFams <$> Vk.Phd.getQueueFamilyProperties pd

findQFams :: [(Vk.QFam.Index, Vk.QFam.Properties)] -> Maybe Vk.QFam.Index
findQFams ps = fst <$> find (grbit . snd) ps
	where grbit = checkBits Vk.Q.GraphicsBit . Vk.QFam.propertiesQueueFlags

createLgDvc :: Vk.Phd.P -> Vk.QFam.Index ->
	(forall sd . Vk.Dvc.D sd -> IO a) -> IO a
createLgDvc pd gqfi a =
	Vk.Dvc.create pd info nil \dv -> a dv
	where
	info = Vk.Dvc.CreateInfo {
		Vk.Dvc.createInfoNext = TMaybe.N,
		Vk.Dvc.createInfoFlags = zeroBits,
		Vk.Dvc.createInfoQueueCreateInfos = HPList.Singleton qinfo,
		Vk.Dvc.createInfoEnabledLayerNames = vldLayers,
		Vk.Dvc.createInfoEnabledExtensionNames = [],
		Vk.Dvc.createInfoEnabledFeatures = Just def }
	qinfo = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = zeroBits,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = gqfi,
		Vk.Dvc.queueCreateInfoQueuePriorities = [1.0] }

createRsltBffr :: Vk.Phd.P -> Vk.Dvc.D sd -> Word32 -> Word32 ->
	(forall al sm sb . KnownNat al =>
		RsltBffr sm sb bnmi (RsltObjImg al nmi) ->
		RsltMm sm sb bnmi (RsltObjImg al nmi) -> IO a) -> IO a
createRsltBffr pd dv = createBffrImg pd dv Vk.Bffr.UsageTransferDstBit
	(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit)

type RsltBffr sm sb bnmi oimg = Vk.Bffr.Binded sm sb bnmi '[oimg]

type RsltMm sm sb bnmi oimg =
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg bnmi '[oimg])]

type RsltObjImg al nmi = VObj.Image al ImageRgba8 nmi
type RsltFmt = BObj.ImageFormat ImageRgba8

createBffrImg :: forall sd img nm inm a . BObj.IsImage img =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags ->
	Word32 -> Word32 -> (forall sm sb al . KnownNat al =>
		Vk.Bffr.Binded sm sb nm '[VObj.Image al img inm] ->
		Vk.Mm.M sm '[
			'(sb, 'Vk.Mm.BufferArg nm '[VObj.Image al img inm]) ] ->
		IO a) -> IO a
createBffrImg p dv us prs (fromIntegral -> w) (fromIntegral -> h) a =
	bffrAlgn @(VObj.Image 256 img inm) dv ln us \(_ :: Proxy al) ->
	createBffr @_ @_ @(VObj.Image al img inm) p dv ln us prs a
	where
	ln :: VObj.Length (VObj.Image al img inm)
	ln = VObj.LengthImage w w h 1

bffrAlgn :: forall o sd a . VObj.SizeAlignment o =>
	Vk.Dvc.D sd -> VObj.Length o -> Vk.Bffr.UsageFlags ->
	(forall al . KnownNat al => Proxy al -> IO a) -> IO a
bffrAlgn dv ln us f = Vk.Bffr.create dv (bffrInfo ln us) nil \b ->
	(\(SomeNat p) -> f p) . someNatVal . fromIntegral =<<
	Vk.Mm.requirementsAlignment <$> Vk.Bffr.getMemoryRequirements dv b

createBffr :: forall sd bnm o a . VObj.SizeAlignment o =>
	Vk.Phd.P -> Vk.Dvc.D sd -> VObj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb bnm '[o] -> Vk.Mm.M sm
			'[ '(sb, 'Vk.Mm.BufferArg bnm '[o])] -> IO a) -> IO a
createBffr p dv ln us prs f = Vk.Bffr.create dv (bffrInfo ln us) nil \b -> do
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMmType p (Vk.Mm.requirementsMemoryTypeBits reqs) prs
	Vk.Mm.allocateBind dv (HPList.Singleton . U2 $ Vk.Mm.Buffer b)
		(ainfo mt) nil
		$ f . \(HPList.Singleton (U2 (Vk.Mm.BufferBinded bd))) -> bd
	where ainfo mt = Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mt }

bffrInfo ::
	VObj.Length o -> Vk.Bffr.UsageFlags -> Vk.Bffr.CreateInfo 'Nothing '[o]
bffrInfo ln us = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N, Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths = HPList.Singleton ln,
	Vk.Bffr.createInfoUsage = us,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

findMmType :: Vk.Phd.P ->
	Vk.Mm.TypeBits -> Vk.Mm.PropertyFlags -> IO Vk.Mm.TypeIndex
findMmType pd flt prs =
	fromMaybe (error msg) . suit <$> Vk.Phd.getMemoryProperties pd
	where
	msg = "failed to find suitable memory type!"
	suit prs1 = fst <$> find ((&&)
		<$> (`Vk.Mm.elemTypeIndex` flt) . fst
		<*> checkBits prs . Vk.Mm.mTypePropertyFlags . snd)
			(Vk.Phd.memoryPropertiesMemoryTypes prs1)

createCmdPl :: Vk.QFam.Index -> Vk.Dvc.D sd ->
	(forall sc . Vk.CmdPl.C sc -> IO a) -> IO a
createCmdPl gqfi dv = Vk.CmdPl.create dv info nil
	where info = Vk.CmdPl.CreateInfo {
		Vk.CmdPl.createInfoNext = TMaybe.N,
		Vk.CmdPl.createInfoFlags = zeroBits,
		Vk.CmdPl.createInfoQueueFamilyIndex = gqfi }

body :: Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	(forall sm' si . Vk.Img.Binded sm' si nm' RsltFmt -> IO a) -> IO a
body pd dv gq cp f =
	makeImage' pd dv \bimg' _mi' -> do
	makeImageView dv bimg' \iv ->
		makeRenderPass dv \rp ->
		makeFramebuffer dv rp iv \fb ->
		makePipelineNew dv rp \ppl ->
		makeCommandBuffer dv gq cp \cb ->
			Vk.Cmd.beginRenderPass @'Nothing @'[]
				cb (binfo rp fb) Vk.Subpass.ContentsInline $
			Vk.Cmd.bindPipelineGraphics
				cb Vk.Ppl.BindPointGraphics ppl \cbb ->
			Vk.Cmd.draw cbb 3 1 0 0
	transitionImageLayout dv gq cp bimg'
		Vk.Img.LayoutUndefined Vk.Img.LayoutTransferSrcOptimal
	f bimg'
	where binfo rp fb = Vk.RenderPass.BeginInfo {
		Vk.RenderPass.beginInfoNext = TMaybe.N,
		Vk.RenderPass.beginInfoRenderPass = rp,
		Vk.RenderPass.beginInfoFramebuffer = fb,
		Vk.RenderPass.beginInfoRenderArea = Vk.Rect2d {
			Vk.rect2dOffset = Vk.Offset2d 0 0,
			Vk.rect2dExtent = Vk.Extent2d imgWidth imgHeight },
		Vk.RenderPass.beginInfoClearValues = HPList.Nil }

makeImage' :: Vk.Phd.P -> Vk.Dvc.D sd ->
	(forall si sm .
		Vk.Img.Binded sm si nm 'Vk.T.FormatR8g8b8a8Unorm ->
		Vk.Mm.M sm '[ '(si, 'Vk.Mm.ImageArg nm 'Vk.T.FormatR8g8b8a8Unorm)] -> IO a) ->
	IO a
makeImage' phdvc dvc f = do
	let	imgCreateInfo = Vk.Img.CreateInfo {
			Vk.Img.createInfoNext = TMaybe.N,
			Vk.Img.createInfoFlags = Vk.Img.CreateFlagsZero,
			Vk.Img.createInfoImageType = Vk.Img.Type2d,
			Vk.Img.createInfoExtent =
				Vk.Extent3d imgWidth imgHeight 1,
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
	memProps <- Vk.Phd.getMemoryProperties phdvc
	print memProps
	Vk.Img.create @'Nothing dvc imgCreateInfo nil \image -> do
		imgMemReq <- Vk.Img.getMemoryRequirements dvc image
		print imgMemReq
		let	imgMemReqTypes =
				Vk.Mm.M.requirementsMemoryTypeBits imgMemReq
			memPropTypes = (fst <$>)
				. filter ((/= zeroBits)
					. (.&. Vk.Mm.PropertyHostVisibleBit)
					. Vk.Mm.M.mTypePropertyFlags . snd)
				$ Vk.Phd.memoryPropertiesMemoryTypes
					memProps
			memoryTypeIndex = case filter
				(`Vk.Mm.M.elemTypeIndex` imgMemReqTypes)
				memPropTypes of
				[] -> error "No available memory types"
				i : _ -> i
			imgMemAllocInfo = Vk.Mm.AllocateInfo {
				Vk.Mm.allocateInfoNext = TMaybe.N,
				Vk.Mm.allocateInfoMemoryTypeIndex =
					memoryTypeIndex }
		Vk.Mm.allocateBind @'Nothing
			dvc (HPList.Singleton . U2 $ Vk.Mm.Image image)
			imgMemAllocInfo nil \(HPList.Singleton (U2 (Vk.Mm.ImageBinded bimg))) imgMem -> do
			f bimg imgMem

makeImageView :: Vk.Dvc.D sd -> Vk.Img.Binded sm si nm fmt ->
	(forall s . Vk.ImgView.I nm Vk.T.FormatR8g8b8a8Unorm s -> IO a) -> IO a
makeImageView dvc bimg f =
	Vk.ImgView.create dvc imgViewCreateInfo nil \imgView -> do
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

makeCommandBuffer :: forall sd scp a . Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scp ->
	(forall s . Vk.CommandBuffer.C s -> IO a) -> IO a
makeCommandBuffer device graphicsQueue cmdPool f = do
		let	cmdBufAllocInfo :: Vk.CommandBuffer.AllocateInfo 'Nothing scp '[ '()]
			cmdBufAllocInfo = Vk.CommandBuffer.AllocateInfo {
				Vk.CommandBuffer.allocateInfoNext = TMaybe.N,
				Vk.CommandBuffer.allocateInfoCommandPool =
					cmdPool,
				Vk.CommandBuffer.allocateInfoLevel =
					Vk.CommandBuffer.LevelPrimary }
		Vk.CommandBuffer.allocate device cmdBufAllocInfo \(cmdBuf :*. HPList.Nil) -> do
				r <- Vk.CommandBuffer.begin cmdBuf
					(def :: Vk.CommandBuffer.BeginInfo 'Nothing 'Nothing) $ f cmdBuf
				let	submitInfo :: Vk.SubmitInfo 'Nothing _ _ _
					submitInfo = Vk.SubmitInfo {
						Vk.submitInfoNext = TMaybe.N,
						Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
						Vk.submitInfoCommandBuffers = cmdBuf :** HPList.Nil,
						Vk.submitInfoSignalSemaphores = HPList.Nil }
				Vk.Q.submit graphicsQueue (U4 submitInfo :** HPList.Nil) Nothing
				Vk.Q.waitIdle graphicsQueue
				pure r

copyBufferToImage :: forall sd sc sm sb nm img inm si sm' nm' al . KnownNat al =>
	Storable (BObj.ImagePixel img) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
--	Vk.Img.Binded sm' si nm' (Vk.Bffr.ImageFormat img) ->
	Vk.Img.Binded sm' si nm' (BObj.ImageFormat img) ->
	Vk.Bffr.Binded sm sb nm '[ VObj.Image al img inm]  ->
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
	Vk.Cmd.copyImageToBuffer @al
		cb img Vk.Img.LayoutTransferSrcOptimal bf (HPList.Singleton region)

transitionImageLayout :: forall sd sc si sm nm fmt .
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
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
				Vk.QFam.Ignored,
			Vk.Img.memoryBarrierDstQueueFamilyIndex =
				Vk.QFam.Ignored,
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
		sstg dstg zeroBits HPList.Nil HPList.Nil (HPList.Singleton $ U5 barrier)
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
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	(forall s . Vk.CommandBuffer.C s -> IO a) -> IO a
beginSingleTimeCommands dvc gq cp cmd = do
	Vk.CommandBuffer.allocate
		dvc allocInfo \((cb :: Vk.CommandBuffer.C s) :*. HPList.Nil) -> do
		let	submitInfo :: Vk.SubmitInfo 'Nothing '[] '[s] '[]
			submitInfo = Vk.SubmitInfo {
				Vk.submitInfoNext = TMaybe.N,
				Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
				Vk.submitInfoCommandBuffers = HPList.Singleton cb,
				Vk.submitInfoSignalSemaphores = HPList.Nil }
		Vk.CommandBuffer.begin @'Nothing @'Nothing cb beginInfo (cmd cb) <* do
			Vk.Q.submit gq (HPList.Singleton $ U4 submitInfo) Nothing
			Vk.Q.waitIdle gq
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

newtype ImageRgba8 = ImageRgba8 (Image PixelRGBA8)

newtype MyRgba8 = MyRgba8 PixelRGBA8

instance Storable MyRgba8 where
	sizeOf _ = 4 * sizeOf @Pixel8 undefined
	alignment _ = alignment @Pixel8 undefined
	peek p = MyRgba8 . (\(r, g, b, a) -> PixelRGBA8 r g b a) . listToTuple4
		<$> peekArray 4 (castPtr p)
	poke p (MyRgba8 (PixelRGBA8 r g b a)) =
		pokeArray (castPtr p) [r, g, b, a]

instance BObj.IsImage ImageRgba8 where
	type ImagePixel ImageRgba8 = MyRgba8
	type ImageFormat ImageRgba8 = 'Vk.T.FormatR8g8b8a8Unorm
	imageRow = BObj.imageWidth
	imageWidth (ImageRgba8 img) = fromIntegral $ imageWidth img
	imageHeight (ImageRgba8 img) = fromIntegral $ imageHeight img
	imageDepth _ = 1
	imageBody (ImageRgba8 img) = (<$> [0 .. imageHeight img - 1]) \y ->
		(<$> [0 .. imageWidth img - 1]) \x -> MyRgba8 $ pixelAt img x y
	imageMake w h _d pss = ImageRgba8
		$ generateImage (\x y -> let MyRgba8 p = (pss' ! y) ! x in p) (fromIntegral w) (fromIntegral h)
		where pss' = listArray (0, fromIntegral h - 1) (listArray (0, fromIntegral w - 1) <$> pss)

makeFramebuffer :: Vk.Dvc.D sd -> Vk.RenderPass.R sr -> Vk.ImgView.I nm fmt si ->
	(forall s . Vk.Framebuffer.F s -> IO a) -> IO a
makeFramebuffer dvc rp iv f =
	Vk.Framebuffer.create @'Nothing dvc frameBufCreateInfo nil f
	where	frameBufCreateInfo = Vk.Framebuffer.CreateInfo {
			Vk.Framebuffer.createInfoNext = TMaybe.N,
			Vk.Framebuffer.createInfoFlags =
				Vk.Framebuffer.CreateFlagsZero,
			Vk.Framebuffer.createInfoRenderPass = rp,
			Vk.Framebuffer.createInfoAttachments = U3 iv :** HPList.Nil,
			Vk.Framebuffer.createInfoWidth = imgWidth,
			Vk.Framebuffer.createInfoHeight = imgHeight,
			Vk.Framebuffer.createInfoLayers = 1 }

makeRenderPass ::
	Vk.Dvc.D sd -> (forall s . Vk.RenderPass.R s -> IO a) -> IO a
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
		renderPassCreateInfoNew :: Vk.RenderPass.CreateInfo 'Nothing _
		renderPassCreateInfoNew = Vk.RenderPass.CreateInfo {
			Vk.RenderPass.createInfoNext = TMaybe.N,
			Vk.RenderPass.createInfoFlags =
				Vk.RenderPass.CreateFlagsZero,
			Vk.RenderPass.createInfoAttachments =
				attachmentNew :** HPList.Nil,
			Vk.RenderPass.createInfoSubpasses = [subpass],
			Vk.RenderPass.createInfoDependencies = [] }
	Vk.RenderPass.create dvc renderPassCreateInfoNew nil f

makePipelineNew :: Vk.Dvc.D sd -> Vk.RenderPass.R sr ->
	(forall s sl . Vk.Ppl.Gr.G s '[] '[] '(sl, '[], '[]) -> IO a) -> IO a
makePipelineNew dvc rp f = do
	let	viewport = Vk.Viewport {
			Vk.viewportX = 0,
			Vk.viewportY = 0,
			Vk.viewportMinDepth = 0,
			Vk.viewportMaxDepth = 1,
			Vk.viewportWidth = fromIntegral imgWidth,
			Vk.viewportHeight = fromIntegral imgHeight }
		scissor = Vk.Rect2d {
			Vk.rect2dOffset = Vk.Offset2d 0 0,
			Vk.rect2dExtent =
				Vk.Extent2d imgWidth imgHeight }
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
		layoutCreateInfoNew :: Vk.Ppl.Lyt.CreateInfo 'Nothing '[]
			('Vk.PushConstant.Layout '[] '[])
		layoutCreateInfoNew = Vk.Ppl.Lyt.CreateInfo {
			Vk.Ppl.Lyt.createInfoNext = TMaybe.N,
			Vk.Ppl.Lyt.createInfoFlags = zeroBits,
			Vk.Ppl.Lyt.createInfoSetLayouts = HPList.Nil }
		vertShaderCreateInfo = Vk.ShaderModule.CreateInfo {
			Vk.ShaderModule.createInfoNext = TMaybe.N,
			Vk.ShaderModule.createInfoFlags = zeroBits,
			Vk.ShaderModule.createInfoCode = glslVertexShaderMain }
		vertShaderStage = Vk.Ppl.ShSt.CreateInfo {
			Vk.Ppl.ShSt.createInfoNext = TMaybe.N,
			Vk.Ppl.ShSt.createInfoFlags =
				Vk.Ppl.ShSt.CreateFlagsZero,
			Vk.Ppl.ShSt.createInfoStage = Vk.ShaderStageVertexBit,
			Vk.Ppl.ShSt.createInfoModule =
				(vertShaderCreateInfo, nil),
			Vk.Ppl.ShSt.createInfoName = "main",
			Vk.Ppl.ShSt.createInfoSpecializationInfo = Nothing }
		fragShaderCreateInfo = Vk.ShaderModule.CreateInfo {
			Vk.ShaderModule.createInfoNext = TMaybe.N,
			Vk.ShaderModule.createInfoFlags = zeroBits,
			Vk.ShaderModule.createInfoCode =
				glslFragmentShaderMain }
		fragShaderStage = Vk.Ppl.ShSt.CreateInfo {
			Vk.Ppl.ShSt.createInfoNext = TMaybe.N,
			Vk.Ppl.ShSt.createInfoFlags =
				Vk.Ppl.ShSt.CreateFlagsZero,
			Vk.Ppl.ShSt.createInfoStage =
				Vk.ShaderStageFragmentBit,
			Vk.Ppl.ShSt.createInfoModule =
				(fragShaderCreateInfo, nil),
			Vk.Ppl.ShSt.createInfoName = "main",
			Vk.Ppl.ShSt.createInfoSpecializationInfo = Nothing }
	Vk.Ppl.Lyt.create dvc layoutCreateInfoNew nil \plyt -> do
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
					U5 fragShaderStage :** HPList.Nil,
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
			U14 pipelineCreateInfo :** HPList.Nil ) nil
				\(U3 g :** HPList.Nil) -> f g

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
	outColor = vec4(0.0, 0.0, 1.0, 1.0);
}

|]
