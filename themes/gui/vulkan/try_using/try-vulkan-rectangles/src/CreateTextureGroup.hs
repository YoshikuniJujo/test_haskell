{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CreateTextureGroup (

	-- * GROUP

	txGroup, TextureGroup,

	-- * CREATE AND DESTROY

	createTx, destroyTx,
	createImgVw, recreateImgVw, createBffr, createBffr',

	-- * BEGIN SINGLE TIME COMMANDS

	singleTimeCmds,

	-- * SAMPLER

	createTxSmplr,

	-- * IMAGE TYPE

	ImageRgba8(..)

	) where

import Foreign.Storable
import Data.Foldable
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.TypeLevel.Tuple.Uncurry
import Data.Default
import Data.Bits
import Data.Maybe
import Data.List qualified as L
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList qualified as HPList
import Data.HeteroParList (pattern (:**), pattern (:*.))
import Data.Word
import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.Device qualified as Vk.Dvc
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer qualified as Vk.CmdBffr
import Gpu.Vulkan.Cmd qualified as Vk.Cmd
import Gpu.Vulkan.Queue qualified as Vk.Q
import Gpu.Vulkan.QueueFamily qualified as Vk.QFamily
import Gpu.Vulkan.Pipeline qualified as Vk.Ppl
import Gpu.Vulkan.Descriptor qualified as Vk.Dsc
import Gpu.Vulkan.DescriptorSet qualified as Vk.DscSet
import Gpu.Vulkan.Sample qualified as Vk.Sample
import Gpu.Vulkan.Sampler qualified as Vk.Smplr
import Gpu.Vulkan.Image qualified as Vk.Img
import Gpu.Vulkan.ImageView qualified as Vk.ImgVw
import Gpu.Vulkan.Buffer qualified as Vk.Bffr
import Gpu.Vulkan.Memory qualified as Vk.Mm
import Gpu.Vulkan.Object qualified as Vk.Obj
import Gpu.Vulkan.Object.Base qualified as BObj

import Tools

import Codec.Picture

import Foreign.Ptr
import Foreign.Marshal.Array
import Data.List.ToolsYj
import Data.Array

import Data.Either.ToolsYj

----------------------------------------------------------------------
--
-- * GROUP
-- * CREATE AND DESTROY
-- * BUFFER
-- * IMAGE
-- * IMAGE VIEW
-- * DESCRIPTOR SET
-- * BEGIN SINGLE TIME COMMANDS
-- * SAMPLER
-- * IMAGE TYPE
--
----------------------------------------------------------------------

-- GROUP

txGroup :: Vk.Dvc.D sd ->
	(forall si sm sv . TextureGroup sd si sm sv fmt nmt k -> IO a) -> IO a
txGroup dv f =
	Vk.Img.group dv nil \ig -> Vk.Mm.group dv nil \mg ->
	Vk.ImgVw.group dv nil \vg -> f (ig, mg, vg)

type TextureGroup sd si sm sv fmt nmt k = (
	Vk.Img.Group sd 'Nothing si k nmt fmt,
	Vk.Mm.Group sd 'Nothing sm k '[ '(si, 'Vk.Mm.ImageArg nmt fmt)],
	Vk.ImgVw.Group sd 'Nothing sv k nmt fmt )

-- CREATE AND DESTROY

createTx :: forall bis nmt sd scp sds k sdp sdsl si sm sv img ssmp . (
	Vk.DscSet.BindingAndArrayElemImage bis
		'[ '(nmt, BObj.ImageFormat img)] 0,
	Ord k, BObj.IsImage img ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scp ->
	Vk.DscSet.Group sd sds k sdp '[ '(sdsl, bis)] ->
	TextureGroup sd si sm sv (BObj.ImageFormat img) nmt k ->
	Vk.Smplr.S ssmp -> img -> k -> IO ()
createTx pd dv gq cp dsg (ig, mg, vg) smp img k =
	createTxImg pd dv ig mg gq cp k img >>= \i ->
	createImgVw vg k i >>= \v ->
	Vk.DscSet.lookup dsg k >>= \(fromJust -> HPList.Singleton ds) ->
	updateDscStTx dv ds v smp

destroyTx :: Ord k => TextureGroup sd si sm sv fmt nmt k -> k -> IO ()
destroyTx (ig, mg, vg) = for_ [
	Vk.Img.unsafeDestroy ig, Vk.Mm.unsafeFree mg,
	Vk.ImgVw.unsafeDestroy vg ] . flip ($)

createTxImg :: forall k sim nm sd smm sc img . (
	BObj.IsImage img, Ord k
	) => Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Img.Group sd 'Nothing sim k nm (BObj.ImageFormat img) ->
	Vk.Mm.Group sd 'Nothing smm k
		'[ '(sim, 'Vk.Mm.ImageArg nm (BObj.ImageFormat img))] ->
	Vk.Q.Q -> Vk.CmdPl.C sc -> k -> img ->
	IO (Vk.Img.Binded smm sim nm (BObj.ImageFormat img))
createTxImg phdvc dvc mng mmng gq cp k img = do
	let	wdt = fromIntegral $ BObj.imageWidth img
		hgt = fromIntegral $ BObj.imageHeight img
	(tximg, _txmem) <- createImage' @(BObj.ImageFormat img) phdvc dvc mng mmng k
		wdt hgt Vk.Img.TilingOptimal
		(Vk.Img.UsageTransferDstBit .|. Vk.Img.UsageSampledBit)
		Vk.Mm.PropertyDeviceLocalBit
	putStrLn "before createBufferImage"
	createBufferImage @img @_ phdvc dvc
		(fromIntegral wdt, fromIntegral wdt, fromIntegral hgt, 1)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit )
		\(sb :: Vk.Bffr.Binded
			sm sb "texture-buffer" '[ Vk.Obj.Image 1 a inm]) sbm -> do
		Vk.Mm.write @"texture-buffer"
			@(Vk.Obj.Image 1 img inm) @0 dvc sbm zeroBits img -- (MyImage img)
		print sb
		transitionImageLayout dvc gq cp tximg
			Vk.Img.LayoutUndefined
			Vk.Img.LayoutTransferDstOptimal
		copyBufferToImage dvc gq cp sb tximg wdt hgt
		transitionImageLayout dvc gq cp tximg
			Vk.Img.LayoutTransferDstOptimal
			Vk.Img.LayoutShaderReadOnlyOptimal
	pure tximg

copyBufferToImage :: forall sd sc sm sb nm img inm si sm' nm' .
	Storable (BObj.ImagePixel img) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Bffr.Binded sm sb nm '[ Vk.Obj.Image 1 img inm]  ->
	Vk.Img.Binded sm' si nm' (BObj.ImageFormat img) ->
	Word32 -> Word32 -> IO ()
copyBufferToImage dvc gq cp bf img wdt hgt =
	singleTimeCmds dvc gq cp \cb -> do
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

-- BUFFER

createBufferImage :: Storable (BObj.ImagePixel t) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> (Vk.Dvc.Size, Vk.Dvc.Size, Vk.Dvc.Size, Vk.Dvc.Size) ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb nm '[ Vk.Obj.Image 1 t inm] ->
		Vk.Mm.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg nm '[ Vk.Obj.Image 1 t inm])] ->
		IO a) -> IO a
createBufferImage p dv (r, w, h, d) usg props f =
	putStrLn "createBufferImage begin" >>
	createBffr p dv (Vk.Obj.LengthImage r w h d) usg props f
		<* putStrLn "createBufferImage end"

createBffr :: forall sd nm o a . Vk.Obj.SizeAlignment o =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Obj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[o] ->
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[o])] -> IO a) -> IO a
createBffr p dv ln us prs f = Vk.Bffr.create dv (bffrInfo ln us) nil \b -> do
	rqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMmType p (Vk.Mm.requirementsMemoryTypeBits rqs) prs
	Vk.Mm.allocateBind dv (HPList.Singleton . U2 $ Vk.Mm.Buffer b)
		(ainfo mt) nil
		$ f . \(HPList.Singleton (U2 (Vk.Mm.BufferBinded bb))) -> bb
	where ainfo mt = Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mt }

createBffr' :: forall sd sm sb k nm o . (Ord k, Vk.Obj.SizeAlignment o) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Bffr.Group sd 'Nothing sb k nm '[o] ->
	Vk.Mm.Group sd 'Nothing sm k '[ '(sb, 'Vk.Mm.BufferArg nm '[o])] -> k ->
	Vk.Obj.Length o -> Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> IO (
		Vk.Bffr.Binded sm sb nm '[o],
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[o])] )
createBffr' p dv bg mg k ln us prs =
	Vk.Bffr.create' bg k binfo >>= \(forceRight' -> b) -> do
		rqs <- Vk.Bffr.getMemoryRequirements dv b
		mt <- findMmType p (Vk.Mm.requirementsMemoryTypeBits rqs) prs
		(<$> Vk.Mm.allocateBind' mg k
			(HPList.Singleton . U2 $ Vk.Mm.Buffer b) (ainfo mt))
			\(forceRight' -> (HPList.Singleton
				(U2 (Vk.Mm.BufferBinded bnd)), m)) -> (bnd, m)
	where
	binfo :: Vk.Bffr.CreateInfo 'Nothing '[o]
	binfo = Vk.Bffr.CreateInfo {
		Vk.Bffr.createInfoNext = TMaybe.N,
		Vk.Bffr.createInfoFlags = zeroBits,
		Vk.Bffr.createInfoLengths = HPList.Singleton ln,
		Vk.Bffr.createInfoUsage = us,
		Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Bffr.createInfoQueueFamilyIndices = [] }
	ainfo :: Vk.Mm.TypeIndex -> Vk.Mm.AllocateInfo 'Nothing
	ainfo mt = Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mt }

findMmType ::
	Vk.Phd.P -> Vk.Mm.TypeBits -> Vk.Mm.PropertyFlags -> IO Vk.Mm.TypeIndex
findMmType pd flt prs =
	fromMaybe (error msg) . suit <$> Vk.Phd.getMemoryProperties pd
	where
	msg = "failed to find suitable memory type!"
	suit prs1 = fst <$> L.find ((&&)
		<$> (`Vk.Mm.elemTypeIndex` flt) . fst
		<*> checkBits prs . Vk.Mm.mTypePropertyFlags . snd)
			(Vk.Phd.memoryPropertiesMemoryTypes prs1)

bffrInfo ::
	Vk.Obj.Length s -> Vk.Bffr.UsageFlags -> Vk.Bffr.CreateInfo Nothing '[s]
bffrInfo ln us = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N,
	Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths = HPList.Singleton ln,
	Vk.Bffr.createInfoUsage = us,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

-- IMAGE

createImage' :: forall fmt sim smm nm sd k . Ord k => Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Img.Group sd 'Nothing sim k nm fmt ->
	Vk.Mm.Group sd 'Nothing smm k '[ '(sim, 'Vk.Mm.ImageArg nm fmt)] ->
	k ->
	Word32 -> Word32 -> Vk.Img.Tiling ->
	Vk.Img.UsageFlagBits -> Vk.Mm.PropertyFlagBits -> IO (
		Vk.Img.Binded smm sim nm fmt,
		Vk.Mm.M smm
			'[ '(sim, 'Vk.Mm.ImageArg nm fmt)] )
createImage' pd dvc mng mmng k wdt hgt tlng usg prps = do
	AlwaysRight img <- Vk.Img.create' @_ @'Nothing mng k imageInfo
	reqs <- Vk.Img.getMemoryRequirements dvc img
	print reqs
	mt <- findMmType pd (Vk.Mm.requirementsMemoryTypeBits reqs) prps
	print mt
	Right (HeteroParList.Singleton (U2 (Vk.Mm.ImageBinded bnd)), m) <-
		Vk.Mm.allocateBind' @_ @'Nothing mmng k
			(HeteroParList.Singleton . U2 $ Vk.Mm.Image img) (memInfo mt)
	pure (bnd :: Vk.Img.Binded smm sim nm fmt, m)
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
	memInfo mt = Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mt }

transitionImageLayout :: forall sd sc si sm nm fmt .
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Img.Binded sm si nm fmt -> Vk.Img.Layout -> Vk.Img.Layout ->
	IO ()
transitionImageLayout dvc gq cp img olyt nlyt =
	singleTimeCmds dvc gq cp \cb -> do
	let	barrier :: Vk.Img.MemoryBarrier 'Nothing sm si nm fmt
		barrier = Vk.Img.MemoryBarrier {
			Vk.Img.memoryBarrierNext = TMaybe.N,
			Vk.Img.memoryBarrierOldLayout = olyt,
			Vk.Img.memoryBarrierNewLayout = nlyt,
			Vk.Img.memoryBarrierSrcQueueFamilyIndex =
				Vk.QFamily.Ignored,
			Vk.Img.memoryBarrierDstQueueFamilyIndex =
				Vk.QFamily.Ignored,
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

-- IMAGE VIEW

createImgVw :: forall sd sv k nm vfmt sm si ifmt .
	(Ord k, Vk.T.FormatToValue vfmt) =>
	Vk.ImgVw.Group sd 'Nothing sv k nm vfmt ->
	k -> Vk.Img.Binded sm si nm ifmt -> IO (Vk.ImgVw.I nm vfmt sv)
createImgVw vg k i = forceRight' <$> Vk.ImgVw.create' vg k (imgVwInfo i)

recreateImgVw :: Vk.T.FormatToValue ivfmt => Vk.Dvc.D sd ->
	Vk.Img.Binded sm si nm ifmt -> Vk.ImgVw.I nm ivfmt sv -> IO ()
recreateImgVw dv i = Vk.ImgVw.unsafeRecreate dv (imgVwInfo i) nil

imgVwInfo ::
	Vk.Img.Binded sm si nm ifmt ->
	Vk.ImgVw.CreateInfo 'Nothing sm si nm ifmt ivfmt
imgVwInfo i = imgVwInfo' i Vk.Img.AspectColorBit

imgVwInfo' :: Vk.Img.Binded sm si nm ifmt -> Vk.Img.AspectFlags ->
	Vk.ImgVw.CreateInfo 'Nothing sm si nm ifmt vfmt
imgVwInfo' i a = Vk.ImgVw.CreateInfo {
	Vk.ImgVw.createInfoNext = TMaybe.N,
	Vk.ImgVw.createInfoFlags = zeroBits,
	Vk.ImgVw.createInfoImage = i,
	Vk.ImgVw.createInfoViewType = Vk.ImgVw.Type2d,
	Vk.ImgVw.createInfoComponents = def,
	Vk.ImgVw.createInfoSubresourceRange = Vk.Img.SubresourceRange {
		Vk.Img.subresourceRangeAspectMask = a,
		Vk.Img.subresourceRangeBaseMipLevel = 0,
		Vk.Img.subresourceRangeLevelCount = 1,
		Vk.Img.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.subresourceRangeLayerCount = 1 } }

-- DESCRIPTOR SET

updateDscStTx ::
	Vk.DscSet.BindingAndArrayElemImage bis
		'[ '(nmt, fmt)] 0 =>
	Vk.Dvc.D sd -> Vk.DscSet.D sds '(sdsc, bis) ->
	Vk.ImgVw.I nmt fmt siv  -> Vk.Smplr.S ss -> IO ()
updateDscStTx dvc dscs tximgvw txsmp = do
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

-- BEGIN SINGLE TIME COMMANDS

singleTimeCmds :: forall sd sc a . Vk.Dvc.D sd -> Vk.Q.Q ->
	Vk.CmdPl.C sc -> (forall s . Vk.CmdBffr.C s -> IO a) -> IO a
singleTimeCmds dv gq cp cmd =
	Vk.CmdBffr.allocate dv ainfo \(cb :*. HPList.Nil) ->
	Vk.CmdBffr.begin @_ @'Nothing cb binfo (cmd cb) <* do
	Vk.Q.submit gq (HPList.Singleton . U4 $ sinfo cb) Nothing
	Vk.Q.waitIdle gq
	where
	ainfo :: Vk.CmdBffr.AllocateInfo 'Nothing sc '[ '()]
	ainfo = Vk.CmdBffr.AllocateInfo {
		Vk.CmdBffr.allocateInfoNext = TMaybe.N,
		Vk.CmdBffr.allocateInfoCommandPool = cp,
		Vk.CmdBffr.allocateInfoLevel = Vk.CmdBffr.LevelPrimary }
	binfo = Vk.CmdBffr.BeginInfo {
		Vk.CmdBffr.beginInfoNext = TMaybe.N,
		Vk.CmdBffr.beginInfoFlags = Vk.CmdBffr.UsageOneTimeSubmitBit,
		Vk.CmdBffr.beginInfoInheritanceInfo = Nothing }
	sinfo cb = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Nil }

-- SAMPLER

createTxSmplr ::
	Vk.Phd.P -> Vk.Dvc.D sd -> (forall ss . Vk.Smplr.S ss -> IO a) -> IO a
createTxSmplr phdv dvc f = do
	prp <- Vk.Phd.getProperties phdv
	print . Vk.Phd.limitsMaxSamplerAnisotropy $ Vk.Phd.propertiesLimits prp
	let	samplerInfo = Vk.Smplr.CreateInfo {
			Vk.Smplr.createInfoNext = TMaybe.N,
			Vk.Smplr.createInfoFlags = zeroBits,
			Vk.Smplr.createInfoMagFilter = Vk.FilterLinear,
			Vk.Smplr.createInfoMinFilter = Vk.FilterLinear,
			Vk.Smplr.createInfoMipmapMode =
				Vk.Smplr.MipmapModeLinear,
			Vk.Smplr.createInfoAddressModeU =
				Vk.Smplr.AddressModeRepeat,
			Vk.Smplr.createInfoAddressModeV =
				Vk.Smplr.AddressModeRepeat,
			Vk.Smplr.createInfoAddressModeW =
				Vk.Smplr.AddressModeRepeat,
			Vk.Smplr.createInfoMipLodBias = 0,
			Vk.Smplr.createInfoAnisotropyEnable = True,
			Vk.Smplr.createInfoMaxAnisotropy =
				Vk.Phd.limitsMaxSamplerAnisotropy
					$ Vk.Phd.propertiesLimits prp,
			Vk.Smplr.createInfoCompareEnable = False,
			Vk.Smplr.createInfoCompareOp = Vk.CompareOpAlways,
			Vk.Smplr.createInfoMinLod = 0,
			Vk.Smplr.createInfoMaxLod = 0,
			Vk.Smplr.createInfoBorderColor =
				Vk.BorderColorIntOpaqueBlack,
			Vk.Smplr.createInfoUnnormalizedCoordinates = False }
	Vk.Smplr.create @'Nothing dvc samplerInfo nil f

{-# COMPLETE AlwaysRight #-}

pattern AlwaysRight :: b -> Either a b
pattern AlwaysRight x <- Right x

-- IMAGE TYPE

newtype ImageRgba8 = ImageRgba8 (Image PixelRGBA8)

newtype PixelRgba8 = PixelRgba8 PixelRGBA8

instance BObj.IsImage ImageRgba8 where
	type ImagePixel ImageRgba8 = PixelRgba8
	type ImageFormat ImageRgba8 = 'Vk.T.FormatR8g8b8a8Srgb
	imageRow = BObj.imageWidth
	imageWidth (ImageRgba8 i) = fromIntegral $ imageWidth i
	imageHeight (ImageRgba8 i) = fromIntegral $ imageHeight i
	imageDepth _ = 1
	imageBody (ImageRgba8 i) = (<$> [0 .. imageHeight i - 1]) \y ->
		(<$> [0 .. imageWidth i - 1]) \x -> PixelRgba8 $ pixelAt i x y
	imageMake (fromIntegral -> w) (fromIntegral -> h) _d pss =
		ImageRgba8 $ generateImage
			(\x y -> let PixelRgba8 p = (pss' ! y) ! x in p) w h
		where pss' = listArray (0, h - 1) (listArray (0, w - 1) <$> pss)

instance Storable PixelRgba8 where
	sizeOf _ = 4 * sizeOf @Pixel8 undefined
	alignment _ = alignment @Pixel8 undefined
	peek p = PixelRgba8 . (\(r, g, b, a) -> PixelRGBA8 r g b a)
		. listToTuple4 <$> peekArray 4 (castPtr p)
	poke p (PixelRgba8 (PixelRGBA8 r g b a)) =
		pokeArray (castPtr p) [r, g, b, a]
