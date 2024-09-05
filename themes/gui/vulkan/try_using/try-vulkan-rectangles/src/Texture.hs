{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Texture (

	-- * CREATE AND UPDATE

	createBindImage, createBufferImageForCopy, createBuffer,

	-- * WRITE

	writeBufferImage1, writeBufferImage2,

	-- * CREATE INFOS

	imgVwInfo,

	) where

import Foreign.Storable
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.TypeLevel.Tuple.Uncurry
import Data.Default
import Data.Bits
import Data.Maybe
import Data.List qualified as L
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**), pattern (:*.))
import Data.Word
import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.PhysicalDevice qualified as Vk.PhDvc
import Gpu.Vulkan.Device qualified as Vk.Dvc
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPool
import Gpu.Vulkan.CommandBuffer qualified as Vk.CmdBffr
import Gpu.Vulkan.Cmd qualified as Vk.Cmd
import Gpu.Vulkan.Queue qualified as Vk.Queue
import Gpu.Vulkan.QueueFamily qualified as Vk.QueueFamily
import Gpu.Vulkan.Pipeline qualified as Vk.Ppl
import Gpu.Vulkan.Descriptor qualified as Vk.Dsc
import Gpu.Vulkan.DescriptorSet qualified as Vk.DscSet
import Gpu.Vulkan.Sample qualified as Vk.Sample
import Gpu.Vulkan.Sampler qualified as Vk.Smplr
import Gpu.Vulkan.Image qualified as Vk.Img
import Gpu.Vulkan.ImageView qualified as Vk.ImgVw
import Gpu.Vulkan.Buffer qualified as Vk.Bffr
import Gpu.Vulkan.Memory qualified as Vk.Mem
import Gpu.Vulkan.Object qualified as VObj
import Gpu.Vulkan.Object.Base qualified as BObj

import Data.Bits.ToolsYj

createBindImage :: forall nmt bis img sd sds sdsc ss a . (
	BObj.IsImage img,
	Vk.DscSet.BindingAndArrayElemImage bis
		'[ '(nmt, BObj.ImageFormat img)] 0 ) =>
	Vk.PhDvc.P -> Vk.Dvc.D sd ->
	Vk.DscSet.D sds '(sdsc, bis) ->
	Vk.Smplr.S ss -> img ->
	(forall sm si . Vk.Img.Binded sm si nmt (BObj.ImageFormat img) -> IO a) -> IO a
createBindImage phdv dv ubds txsmplr img f =
	createTextureImage phdv dv img \tximg ->
	Vk.ImgVw.create @_ @(BObj.ImageFormat img)
		dv (imgVwInfo tximg) nil \tximgvw ->
	updateDescriptorSetTex dv ubds tximgvw txsmplr >> f tximg

imgVwInfo :: Vk.Img.Binded sm si nm ifmt ->
	Vk.ImgVw.CreateInfo 'Nothing sm si nm ifmt vfmt
imgVwInfo i = Vk.ImgVw.CreateInfo {
	Vk.ImgVw.createInfoNext = TMaybe.N,
	Vk.ImgVw.createInfoFlags = zeroBits,
	Vk.ImgVw.createInfoImage = i,
	Vk.ImgVw.createInfoViewType = Vk.ImgVw.Type2d,
	Vk.ImgVw.createInfoComponents = def,
	Vk.ImgVw.createInfoSubresourceRange = Vk.Img.SubresourceRange {
		Vk.Img.subresourceRangeAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.subresourceRangeBaseMipLevel = 0,
		Vk.Img.subresourceRangeLevelCount = 1,
		Vk.Img.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.subresourceRangeLayerCount = 1 } }

createTextureImage :: forall nm sd img a . (
	BObj.IsImage img
	) => Vk.PhDvc.P -> Vk.Dvc.D sd ->
	img -> (forall smm sim . Vk.Img.Binded smm sim nm (BObj.ImageFormat img) -> IO a) -> IO a
createTextureImage phdvc dvc img f =
	let	wdt = fromIntegral $ BObj.imageWidth img
		hgt = fromIntegral $ BObj.imageHeight img in
	createImage @(BObj.ImageFormat img) phdvc dvc
		wdt hgt Vk.Img.TilingOptimal
		(Vk.Img.UsageTransferDstBit .|. Vk.Img.UsageSampledBit)
		Vk.Mem.PropertyDeviceLocalBit \(tximg, _txmem) ->
	f tximg

createBufferImageForCopy :: forall img sd inm' . BObj.IsImage img =>
	Vk.PhDvc.P -> Vk.Dvc.D sd ->
	img ->
	(forall sm' sb' .
		Vk.Bffr.Binded sm' sb' "texture-buffer" '[VObj.Image 1 img inm'] ->
		Vk.Mem.M sm' '[ '(sb', Vk.Mem.BufferArg "texture-buffer" '[VObj.Image 1 img inm'])] -> IO ()) ->
	IO ()
createBufferImageForCopy phdvc dvc img f =
	createBufferImage @img @_ phdvc dvc (wdt, wdt, hgt, 1)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mem.PropertyHostVisibleBit .|.
			Vk.Mem.PropertyHostCoherentBit )
		\(sb :: Vk.Bffr.Binded
			sm' sb' "texture-buffer" '[ VObj.Image 1 a inm]) sbm -> f sb sbm
	where
	wdt = BObj.imageWidth img
	hgt = BObj.imageHeight img

writeBufferImage1 :: forall img sd sm' sb' inm' . BObj.IsImage img =>
	Vk.Dvc.D sd ->
	Vk.Mem.M sm' '[ '(sb', Vk.Mem.BufferArg "texture-buffer" '[VObj.Image 1 img inm'])] ->
	img -> IO ()
writeBufferImage1 dvc sbm img = do
	Vk.Mem.write @"texture-buffer"
		@(VObj.Image 1 img inm') @0 dvc sbm zeroBits img

writeBufferImage2 :: forall img sd sc sm si nm sm' sb' inm' . BObj.IsImage img =>
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
	Vk.Img.Binded sm si nm (BObj.ImageFormat img) ->
	Vk.Bffr.Binded sm' sb' "texture-buffer" '[VObj.Image 1 img inm'] ->
	Word32 -> Word32 -> IO ()
writeBufferImage2 dvc gq cp tximg sb wdt hgt = do
		transitionImageLayout dvc gq cp tximg
			Vk.Img.LayoutUndefined
			Vk.Img.LayoutTransferDstOptimal

		copyBufferToImage dvc gq cp sb tximg wdt hgt

		transitionImageLayout dvc gq cp tximg
			Vk.Img.LayoutTransferDstOptimal
			Vk.Img.LayoutShaderReadOnlyOptimal


copyBufferToImage :: forall sd sc sm sb nm img inm si sm' nm' .
	Storable (BObj.ImagePixel img) =>
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
	Vk.Bffr.Binded sm sb nm '[ VObj.Image 1 img inm]  ->
	Vk.Img.Binded sm' si nm' (BObj.ImageFormat img) ->
	Word32 -> Word32 -> IO ()
copyBufferToImage dvc gq cp bf img wdt hgt =
	beginSingleTimeCommands dvc gq cp \cb -> do
	let	region :: Vk.Bffr.ImageCopy img inm
		region = Vk.Bffr.ImageCopy {
			Vk.Bffr.imageCopyImageSubresource = isr,
			Vk.Bffr.imageCopyImageOffset = Vk.Offset3d 0 0 0,
			Vk.Bffr.imageCopyImageExtent = Vk.Extent3d wdt hgt 1 }
		isr = Vk.Img.SubresourceLayers {
			Vk.Img.subresourceLayersAspectMask =
				Vk.Img.AspectColorBit,
			Vk.Img.subresourceLayersMipLevel = 0,
			Vk.Img.subresourceLayersBaseArrayLayer = 0,
			Vk.Img.subresourceLayersLayerCount = 1 }
	Vk.Cmd.copyBufferToImage @1
		cb bf img Vk.Img.LayoutTransferDstOptimal (HeteroParList.Singleton region)

createBufferImage :: Storable (BObj.ImagePixel t) =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> (Vk.Dvc.Size, Vk.Dvc.Size, Vk.Dvc.Size, Vk.Dvc.Size) ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb nm '[ VObj.Image 1 t inm] ->
		Vk.Mem.M sm '[ '(
			sb,
			'Vk.Mem.BufferArg nm '[ VObj.Image 1 t inm])] ->
		IO a) -> IO a
createBufferImage p dv (r, w, h, d) usg props f =
	putStrLn "createBufferImage begin" >>
	createBuffer p dv (VObj.LengthImage r w h d) usg props f
		<* putStrLn "createBufferImage end"

createBuffer :: forall sd nm o a . VObj.SizeAlignment o =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> VObj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[o] ->
		Vk.Mem.M sm
			'[ '(sb, 'Vk.Mem.BufferArg nm '[o])] ->
		IO a) -> IO a
createBuffer p dv ln usg props f = Vk.Bffr.create dv bffrInfo nil \b -> do
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMemoryType p (Vk.Mem.requirementsMemoryTypeBits reqs) props
	Vk.Mem.allocateBind dv (HeteroParList.Singleton . U2 $ Vk.Mem.Buffer b)
		(allcInfo mt) nil
		$ f . \(HeteroParList.Singleton (U2 (Vk.Mem.BufferBinded bnd))) -> bnd
	where
	bffrInfo :: Vk.Bffr.CreateInfo 'Nothing '[o]
	bffrInfo = Vk.Bffr.CreateInfo {
		Vk.Bffr.createInfoNext = TMaybe.N,
		Vk.Bffr.createInfoFlags = zeroBits,
		Vk.Bffr.createInfoLengths = HeteroParList.Singleton ln,
		Vk.Bffr.createInfoUsage = usg,
		Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Bffr.createInfoQueueFamilyIndices = [] }
	allcInfo :: Vk.Mem.TypeIndex -> Vk.Mem.AllocateInfo 'Nothing
	allcInfo mt = Vk.Mem.AllocateInfo {
		Vk.Mem.allocateInfoNext = TMaybe.N,
		Vk.Mem.allocateInfoMemoryTypeIndex = mt }

createImage :: forall fmt nm sd a . Vk.T.FormatToValue fmt =>
	Vk.PhDvc.P -> Vk.Dvc.D sd ->
	Word32 -> Word32 -> Vk.Img.Tiling ->
	Vk.Img.UsageFlagBits -> Vk.Mem.PropertyFlagBits -> (
		forall sm si .
		(Vk.Img.Binded sm si nm fmt,
		Vk.Mem.M sm
			'[ '(si, 'Vk.Mem.ImageArg nm fmt)]) -> IO a ) -> IO a
createImage pd dvc wdt hgt tlng usg prps f =
	Vk.Img.create dvc imageInfo nil \img ->
	Vk.Img.getMemoryRequirements dvc img >>= \reqs ->
	findMemoryType pd (Vk.Mem.requirementsMemoryTypeBits reqs) prps >>= \mt ->
	Vk.Mem.allocateBind dvc
		(HeteroParList.Singleton . U2 $ Vk.Mem.Image img) (memInfo mt) nil
		\(HeteroParList.Singleton (U2 (Vk.Mem.ImageBinded bnd))) m ->
	f (bnd, m)
	where
	imageInfo = Vk.Img.CreateInfo {
		Vk.Img.createInfoNext = TMaybe.N,
		Vk.Img.createInfoImageType = Vk.Img.Type2d,
		Vk.Img.createInfoExtent = Vk.Extent3d {
			Vk.extent3dWidth = wdt,
			Vk.extent3dHeight = hgt,
			Vk.extent3dDepth = 1 },
		Vk.Img.createInfoMipLevels = 1,
		Vk.Img.createInfoArrayLayers = 1,
		Vk.Img.createInfoTiling = tlng,
		Vk.Img.createInfoInitialLayout = Vk.Img.LayoutUndefined,
		Vk.Img.createInfoUsage = usg,
		Vk.Img.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Img.createInfoSamples = Vk.Sample.Count1Bit,
		Vk.Img.createInfoFlags = zeroBits,
		Vk.Img.createInfoQueueFamilyIndices = [] }
	memInfo mt = Vk.Mem.AllocateInfo {
		Vk.Mem.allocateInfoNext = TMaybe.N,
		Vk.Mem.allocateInfoMemoryTypeIndex = mt }

findMemoryType :: Vk.PhDvc.P -> Vk.Mem.TypeBits -> Vk.Mem.PropertyFlags ->
	IO Vk.Mem.TypeIndex
findMemoryType phdvc flt props =
	fromMaybe (error msg) . suitable <$> Vk.PhDvc.getMemoryProperties phdvc
	where
	msg = "failed to find suitable memory type!"
	suitable props1 = fst <$> L.find ((&&)
		<$> (`Vk.Mem.elemTypeIndex` flt) . fst
		<*> checkBits props . Vk.Mem.mTypePropertyFlags . snd) tps
		where tps = Vk.PhDvc.memoryPropertiesMemoryTypes props1

transitionImageLayout :: forall sd sc si sm nm fmt .
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
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
		_ -> error "unsupported layout transition!"

updateDescriptorSetTex ::
	Vk.DscSet.BindingAndArrayElemImage bis
		'[ '(nmt, fmt)] 0 =>
	Vk.Dvc.D sd -> Vk.DscSet.D sds '(sdsc, bis) ->
	Vk.ImgVw.I nmt fmt siv  -> Vk.Smplr.S ss -> IO ()
updateDescriptorSetTex dvc dscs tximgvw txsmp = do
	Vk.DscSet.updateDs dvc (
		U5 (descriptorWrite1 dscs tximgvw txsmp) :** HeteroParList.Nil )
		HeteroParList.Nil

descriptorWrite1 ::
	Vk.DscSet.D sds slbts -> Vk.ImgVw.I nm fmt si -> Vk.Smplr.S ss ->
	Vk.DscSet.Write 'Nothing sds slbts
		('Vk.DscSet.WriteSourcesArgImage '[ '(ss, nm, fmt, si) ]) 0
descriptorWrite1 dscs tiv tsmp = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = dscs,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeCombinedImageSampler,
	Vk.DscSet.writeSources = Vk.DscSet.ImageInfos . HeteroParList.Singleton
		$ U4 Vk.Dsc.ImageInfo {
			Vk.Dsc.imageInfoImageLayout =
				Vk.Img.LayoutShaderReadOnlyOptimal,
			Vk.Dsc.imageInfoImageView = tiv,
			Vk.Dsc.imageInfoSampler = tsmp }
	}

beginSingleTimeCommands :: forall sd sc a .
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
	(forall s . Vk.CmdBffr.C s -> IO a) -> IO a
beginSingleTimeCommands dvc gq cp cmd =
	Vk.CmdBffr.allocate
		dvc allocInfo \((cb :: Vk.CmdBffr.C s) :*. HeteroParList.Nil) -> do
	let	submitInfo :: Vk.SubmitInfo 'Nothing '[] '[s] '[]
		submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = TMaybe.N,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HeteroParList.Nil,
			Vk.submitInfoCommandBuffers = HeteroParList.Singleton cb,
			Vk.submitInfoSignalSemaphores = HeteroParList.Nil }
	Vk.CmdBffr.begin @'Nothing @'Nothing cb beginInfo (cmd cb) <* do
		Vk.Queue.submit gq (HeteroParList.Singleton $ U4 submitInfo) Nothing
		Vk.Queue.waitIdle gq
	where
	allocInfo :: Vk.CmdBffr.AllocateInfo 'Nothing sc '[ '()]
	allocInfo = Vk.CmdBffr.AllocateInfo {
		Vk.CmdBffr.allocateInfoNext = TMaybe.N,
		Vk.CmdBffr.allocateInfoCommandPool = cp,
		Vk.CmdBffr.allocateInfoLevel = Vk.CmdBffr.LevelPrimary }
	beginInfo = Vk.CmdBffr.BeginInfo {
		Vk.CmdBffr.beginInfoNext = TMaybe.N,
		Vk.CmdBffr.beginInfoFlags = Vk.CmdBffr.UsageOneTimeSubmitBit,
		Vk.CmdBffr.beginInfoInheritanceInfo = Nothing }
