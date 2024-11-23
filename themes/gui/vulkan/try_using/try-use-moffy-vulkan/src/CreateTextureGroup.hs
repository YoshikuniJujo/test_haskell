{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CreateTextureGroup (

	-- * GROUP

	txGroup, TextureGroup,

	-- * CREATE AND DESTROY

	createTx, destroyTx,
	createBffr, createBffr', createImgVw', recreateImgVw,

	-- * BEGIN SINGLE TIME COMMANDS

	singleTimeCmds,

	-- * SAMPLER

	createTxSmplr ) where

import Foreign.Storable
import Control.Arrow
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.TypeLevel.Tuple.Uncurry
import Data.Foldable
import Data.Bits
import Data.Bits.ToolsYj
import Data.Default
import Data.Maybe
import Data.Either.ToolsYj
import Data.List qualified as L
import Data.HeteroParList (pattern (:*.))
import Data.HeteroParList qualified as HPList
import Data.Word

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Object qualified as Vk.Obj
import Gpu.Vulkan.Object.NoAlignment qualified as Vk.ObjNA
import Gpu.Vulkan.Object.Base qualified as Vk.ObjB
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.Queue qualified as Vk.Q
import Gpu.Vulkan.QueueFamily qualified as Vk.QFamily
import Gpu.Vulkan.Device qualified as Vk.Dvc
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer qualified as Vk.CmdBffr
import Gpu.Vulkan.Cmd qualified as Vk.Cmd
import Gpu.Vulkan.Memory qualified as Vk.Mm
import Gpu.Vulkan.Buffer qualified as Vk.Bffr
import Gpu.Vulkan.Image qualified as Vk.Img
import Gpu.Vulkan.ImageView qualified as Vk.ImgVw
import Gpu.Vulkan.Sampler qualified as Vk.Smplr

import Gpu.Vulkan.Pipeline qualified as Vk.Ppl
import Gpu.Vulkan.Sample qualified as Vk.Sample
import Gpu.Vulkan.Descriptor qualified as Vk.Dsc
import Gpu.Vulkan.DescriptorSet qualified as Vk.DscSet

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

createTx :: forall bis nmt sd scp sds k sdsl si sm sv img ssmp . (
	Vk.DscSet.BindingAndArrayElemImage bis
		'[ '(nmt, Vk.ObjB.ImageFormat img)] 0,
	Ord k, Vk.ObjB.IsImage img ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scp ->
	Vk.DscSet.D sds '(sdsl, bis) ->
	TextureGroup sd si sm sv (Vk.ObjB.ImageFormat img) nmt k ->
	Vk.Smplr.S ssmp -> img -> k -> IO ()
createTx pd dv gq cp ds (ig, mg, vg) smp img k =
	createTxImg pd dv gq cp ig mg k img >>= \i ->
	createImgVw' vg k i >>= \v ->
	updateDscStTx dv ds v smp

destroyTx :: Ord k => TextureGroup sd si sm sv fmt nmt k -> k -> IO ()
destroyTx (ig, mg, vg) = for_ [
	Vk.Img.unsafeDestroy ig, Vk.Mm.unsafeFree mg,
	Vk.ImgVw.unsafeDestroy vg ] . flip ($)

createTxImg :: forall sd sc smi si k nmi img . (Ord k, Vk.ObjB.IsImage img) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Img.Group sd 'Nothing si k nmi (Vk.ObjB.ImageFormat img) ->
	Vk.Mm.Group sd 'Nothing smi k
		'[ '(si, 'Vk.Mm.ImageArg nmi (Vk.ObjB.ImageFormat img))] ->
	k -> img -> IO (Vk.Img.Binded smi si nmi (Vk.ObjB.ImageFormat img))
createTxImg pd dv gq cp ig mg k img = do
	(i, _) <- createImg' @(Vk.ObjB.ImageFormat img) pd dv ig mg k w h
		Vk.Img.TilingOptimal
		(Vk.Img.UsageTransferDstBit .|. Vk.Img.UsageSampledBit)
		Vk.Mm.PropertyDeviceLocalBit
	i <$ createBffrImg @img pd dv (w, w, h, 1)
		Vk.Bffr.UsageTransferSrcBit
		(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit)
		\(sb :: Vk.Bffr.Binded sm sb n '[ Vk.Obj.Image 1 a inm]) m -> do
		Vk.Mm.write @n @(Vk.Obj.Image 1 img inm) @0 dv m zeroBits img
		transitionImgLyt dv gq cp i Vk.Img.LayoutUndefined
			Vk.Img.LayoutTransferDstOptimal
		copyBffrToImg dv gq cp sb i w h
		transitionImgLyt dv gq cp i Vk.Img.LayoutTransferDstOptimal
			Vk.Img.LayoutShaderReadOnlyOptimal
	where
	w, h :: Integral n => n
	w = fromIntegral $ Vk.ObjB.imageWidth img
	h = fromIntegral $ Vk.ObjB.imageHeight img

copyBffrToImg :: forall sd sc smb sb bnmi img nmi si smi .
	Storable (Vk.ObjB.ImagePixel img) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Bffr.Binded smb sb bnmi '[ Vk.ObjNA.Image img nmi]  ->
	Vk.Img.Binded smi si nmi (Vk.ObjB.ImageFormat img) ->
	Word32 -> Word32 -> IO ()
copyBffrToImg dv gq cp bf i w h = singleTimeCmds dv gq cp \cb ->
	Vk.Cmd.copyBufferToImage @1 cb bf i
		Vk.Img.LayoutTransferDstOptimal (HPList.Singleton rgn)
	where
	rgn :: Vk.Bffr.ImageCopy img nmi
	rgn = Vk.Bffr.ImageCopy {
		Vk.Bffr.imageCopyImageSubresource = isr,
		Vk.Bffr.imageCopyImageOffset = Vk.Offset3d 0 0 0,
		Vk.Bffr.imageCopyImageExtent = Vk.Extent3d w h 1 }
	isr = Vk.Img.SubresourceLayers {
		Vk.Img.subresourceLayersAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.subresourceLayersMipLevel = 0,
		Vk.Img.subresourceLayersBaseArrayLayer = 0,
		Vk.Img.subresourceLayersLayerCount = 1 }

-- BUFFER

createBffrImg :: Storable (Vk.ObjB.ImagePixel i) =>
	Vk.Phd.P -> Vk.Dvc.D sd ->
	(Vk.Dvc.Size, Vk.Dvc.Size, Vk.Dvc.Size, Vk.Dvc.Size) ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags ->
	(forall m b .
		Vk.Bffr.Binded m b bn '[ Vk.ObjNA.Image i n] ->
		Vk.Mm.M m '[ '(b, 'Vk.Mm.BufferArg bn '[Vk.ObjNA.Image i n])] ->
		IO a) -> IO a
createBffrImg p dv (r, w, h, d) = createBffr p dv (Vk.Obj.LengthImage r w h d)

createBffr :: forall sd o nm a . Vk.Obj.SizeAlignment o =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Obj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[o] ->
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[o])] -> IO a) -> IO a
createBffr p dv ln us prs f = Vk.Bffr.create dv (bffrInfo ln us) nil \b -> do
	rqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMmType p (Vk.Mm.requirementsMemoryTypeBits rqs) prs
	Vk.Mm.allocateBind dv (HPList.Singleton . U2 $ Vk.Mm.Buffer b)
		(mmAllcInfo mt) nil
		$ f . \(HPList.Singleton (U2 (Vk.Mm.BufferBinded bb))) -> bb

createBffr' :: forall sd sm sb k nm o . (Ord k, Vk.Obj.SizeAlignment o) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Bffr.Group sd 'Nothing sb k nm '[o] ->
	Vk.Mm.Group sd 'Nothing sm k '[ '(sb, 'Vk.Mm.BufferArg nm '[o])] -> k ->
	Vk.Obj.Length o -> Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> IO (
		Vk.Bffr.Binded sm sb nm '[o],
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[o])] )
createBffr' p dv bg mg k ln us prs =
	Vk.Bffr.create' bg k (bffrInfo ln us) >>= \(forceRight' -> b) -> do
	rqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMmType p (Vk.Mm.requirementsMemoryTypeBits rqs) prs
	(<$> Vk.Mm.allocateBind' mg k
		(HPList.Singleton . U2 $ Vk.Mm.Buffer b) (mmAllcInfo mt))
		\(forceRight' ->
			(HPList.Singleton (U2 (Vk.Mm.BufferBinded bb)), m)) ->
		(bb, m)

bffrInfo ::
	Vk.Obj.Length s -> Vk.Bffr.UsageFlags -> Vk.Bffr.CreateInfo Nothing '[s]
bffrInfo ln us = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N, Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths = HPList.Singleton ln,
	Vk.Bffr.createInfoUsage = us,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

findMmType ::
	Vk.Phd.P -> Vk.Mm.TypeBits -> Vk.Mm.PropertyFlags -> IO Vk.Mm.TypeIndex
findMmType pd flt prs =
	fromMaybe (error msg) . suit <$> Vk.Phd.getMemoryProperties pd
	where
	msg = "failed to find suitable memory type!"
	suit p = fst <$> L.find (uncurry (&&) .
			((`Vk.Mm.elemTypeIndex` flt) ***
			checkBits prs . Vk.Mm.mTypePropertyFlags))
		(Vk.Phd.memoryPropertiesMemoryTypes p)

mmAllcInfo :: Vk.Mm.TypeIndex -> Vk.Mm.AllocateInfo 'Nothing
mmAllcInfo mt = Vk.Mm.AllocateInfo {
	Vk.Mm.allocateInfoNext = TMaybe.N,
	Vk.Mm.allocateInfoMemoryTypeIndex = mt }

-- IMAGE

createImg' :: forall fmt sd sm si k nmi . (Vk.T.FormatToValue fmt, Ord k) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Img.Group sd 'Nothing si k nmi fmt ->
	Vk.Mm.Group sd 'Nothing sm k '[ '(si, 'Vk.Mm.ImageArg nmi fmt)] ->
	k -> Word32 -> Word32 -> Vk.Img.Tiling -> Vk.Img.UsageFlagBits ->
	Vk.Mm.PropertyFlagBits -> IO (
		Vk.Img.Binded sm si nmi fmt,
		Vk.Mm.M sm '[ '(si, 'Vk.Mm.ImageArg nmi fmt)] )
createImg' pd dv ig mg k w h tl us prs = do
	(forceRight' -> i) <- Vk.Img.create' ig k iinfo
	rqs <- Vk.Img.getMemoryRequirements dv i
	mt <- findMmType pd (Vk.Mm.requirementsMemoryTypeBits rqs) prs
	(forceRight' -> (HPList.Singleton (U2 (Vk.Mm.ImageBinded bi)), m)) <-
		Vk.Mm.allocateBind' mg k
			(HPList.Singleton . U2 $ Vk.Mm.Image i) (minfo mt)
	pure (bi, m)
	where
	iinfo = Vk.Img.CreateInfo {
		Vk.Img.createInfoNext = TMaybe.N,
		Vk.Img.createInfoImageType = Vk.Img.Type2d,
		Vk.Img.createInfoExtent = Vk.Extent3d {
			Vk.extent3dWidth = w, Vk.extent3dHeight = h,
			Vk.extent3dDepth = 1 },
		Vk.Img.createInfoMipLevels = 1,
		Vk.Img.createInfoArrayLayers = 1,
		Vk.Img.createInfoTiling = tl,
		Vk.Img.createInfoInitialLayout = Vk.Img.LayoutUndefined,
		Vk.Img.createInfoUsage = us,
		Vk.Img.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Img.createInfoSamples = Vk.Sample.Count1Bit,
		Vk.Img.createInfoFlags = zeroBits,
		Vk.Img.createInfoQueueFamilyIndices = [] }
	minfo mt = Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mt }

transitionImgLyt :: forall sd sc si sm nm fmt .
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Img.Binded sm si nm fmt -> Vk.Img.Layout -> Vk.Img.Layout -> IO ()
transitionImgLyt dv gq cp i ol nl = singleTimeCmds dv gq cp \cb ->
	Vk.Cmd.pipelineBarrier cb sstg dstg zeroBits
		HPList.Nil HPList.Nil (HPList.Singleton $ U5 brrr)
	where
	brrr :: Vk.Img.MemoryBarrier 'Nothing sm si nm fmt
	brrr = Vk.Img.MemoryBarrier {
		Vk.Img.memoryBarrierNext = TMaybe.N,
		Vk.Img.memoryBarrierOldLayout = ol,
		Vk.Img.memoryBarrierNewLayout = nl,
		Vk.Img.memoryBarrierSrcQueueFamilyIndex = Vk.QFamily.Ignored,
		Vk.Img.memoryBarrierDstQueueFamilyIndex = Vk.QFamily.Ignored,
		Vk.Img.memoryBarrierImage = i,
		Vk.Img.memoryBarrierSubresourceRange = srr,
		Vk.Img.memoryBarrierSrcAccessMask = sam,
		Vk.Img.memoryBarrierDstAccessMask = dam }
	srr = Vk.Img.SubresourceRange {
		Vk.Img.subresourceRangeAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.subresourceRangeBaseMipLevel = 0,
		Vk.Img.subresourceRangeLevelCount = 1,
		Vk.Img.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.subresourceRangeLayerCount = 1 }
	(sam, dam, sstg, dstg) = case (ol, nl) of
		(Vk.Img.LayoutUndefined, Vk.Img.LayoutTransferDstOptimal) -> (
			zeroBits, Vk.AccessTransferWriteBit,
			Vk.Ppl.StageTopOfPipeBit, Vk.Ppl.StageTransferBit )
		(Vk.Img.LayoutTransferDstOptimal,
			Vk.Img.LayoutShaderReadOnlyOptimal) -> (
			Vk.AccessTransferWriteBit, Vk.AccessShaderReadBit,
			Vk.Ppl.StageTransferBit, Vk.Ppl.StageFragmentShaderBit )
		_ -> error "unsupported layout transition!"

-- IMAGE VIEW

createImgVw' :: forall sd sv k nm vfmt sm si ifmt .
	(Ord k, Vk.T.FormatToValue vfmt) =>
	Vk.ImgVw.Group sd 'Nothing sv k nm vfmt ->
	k -> Vk.Img.Binded sm si nm ifmt -> IO (Vk.ImgVw.I nm vfmt sv)
createImgVw' vg k i = forceRight' <$> Vk.ImgVw.create' vg k (imgVwInfo i)

recreateImgVw :: Vk.T.FormatToValue ivfmt => Vk.Dvc.D sd ->
	Vk.Img.Binded sm si nm ifmt -> Vk.ImgVw.I nm ivfmt sv -> IO ()
recreateImgVw dv i = Vk.ImgVw.unsafeRecreate dv (imgVwInfo i) nil

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

-- DESCRIPTOR SET

updateDscStTx :: Vk.DscSet.BindingAndArrayElemImage bis '[ '(nmt, fmt)] 0 =>
	Vk.Dvc.D sd -> Vk.DscSet.D sds '(sdsl, bis) ->
	Vk.ImgVw.I nmt fmt sv  -> Vk.Smplr.S ss -> IO ()
updateDscStTx dv ds v smp = Vk.DscSet.updateDs
	dv (HPList.Singleton . U5 $ dscWrite ds v smp) HPList.Nil

dscWrite :: Vk.DscSet.D sds slbts -> Vk.ImgVw.I nm fmt sv -> Vk.Smplr.S ssmp ->
	Vk.DscSet.Write 'Nothing sds slbts
		('Vk.DscSet.WriteSourcesArgImage '[ '(ssmp, nm, fmt, sv) ]) 0
dscWrite ds v smp = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N, Vk.DscSet.writeDstSet = ds,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeCombinedImageSampler,
	Vk.DscSet.writeSources = Vk.DscSet.ImageInfos . HPList.Singleton
		$ U4 Vk.Dsc.ImageInfo {
			Vk.Dsc.imageInfoImageLayout =
				Vk.Img.LayoutShaderReadOnlyOptimal,
			Vk.Dsc.imageInfoImageView = v,
			Vk.Dsc.imageInfoSampler = smp } }

-- BEGIN SINGLE TIME COMMANDS

singleTimeCmds :: forall sd sc a . Vk.Dvc.D sd -> Vk.Q.Q ->
	Vk.CmdPl.C sc -> (forall scb . Vk.CmdBffr.C scb -> IO a) -> IO a
singleTimeCmds dv gq cp cmds =
	Vk.CmdBffr.allocateCs dv ainfo \(cb :*. HPList.Nil) ->
	Vk.CmdBffr.begin @_ @'Nothing cb binfo (cmds cb) <* do
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
createTxSmplr pd dv f =
	Vk.Phd.getProperties pd >>= \(Vk.Phd.propertiesLimits -> lm) ->
	Vk.Smplr.create @'Nothing dv (info lm) nil f
	where info lm = Vk.Smplr.CreateInfo {
		Vk.Smplr.createInfoNext = TMaybe.N,
		Vk.Smplr.createInfoFlags = zeroBits,
		Vk.Smplr.createInfoMagFilter = Vk.FilterLinear,
		Vk.Smplr.createInfoMinFilter = Vk.FilterLinear,
		Vk.Smplr.createInfoMipmapMode = Vk.Smplr.MipmapModeLinear,
		Vk.Smplr.createInfoAddressModeU = Vk.Smplr.AddressModeRepeat,
		Vk.Smplr.createInfoAddressModeV = Vk.Smplr.AddressModeRepeat,
		Vk.Smplr.createInfoAddressModeW = Vk.Smplr.AddressModeRepeat,
		Vk.Smplr.createInfoMipLodBias = 0,
		Vk.Smplr.createInfoAnisotropyEnable = True,
		Vk.Smplr.createInfoMaxAnisotropy =
			Vk.Phd.limitsMaxSamplerAnisotropy lm,
		Vk.Smplr.createInfoCompareEnable = False,
		Vk.Smplr.createInfoCompareOp = Vk.CompareOpAlways,
		Vk.Smplr.createInfoMinLod = 0,
		Vk.Smplr.createInfoMaxLod = 0,
		Vk.Smplr.createInfoBorderColor = Vk.BorderColorIntOpaqueBlack,
		Vk.Smplr.createInfoUnnormalizedCoordinates = False }
