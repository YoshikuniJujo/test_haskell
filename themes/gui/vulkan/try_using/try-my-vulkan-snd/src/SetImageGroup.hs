{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SetImageGroup (

	-- * GROUP

	imgGroups, ImgGroups,

	-- * CREATE AND UPDATE

	setImg, updateImg, createBffr,

	-- * BEGIN SINGLE TIME COMMANDS

	singleTimeCmds,

	-- * CREATE INFO

	imgVwInfo ) where

import GHC.TypeNats
import Foreign.Storable
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.Default
import Data.Bits
import Data.Bits.ToolsYj
import Data.Maybe
import Data.List
import Data.HeteroParList qualified as HPList
import Data.HeteroParList (pattern (:**), pattern (:*.))
import Data.Bool
import Data.Word

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Object qualified as VObj
import Gpu.Vulkan.Object.Base qualified as BObj
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.Queue qualified as Vk.Q
import Gpu.Vulkan.QueueFamily qualified as Vk.QFam
import Gpu.Vulkan.Device qualified as Vk.Dvc
import Gpu.Vulkan.Memory qualified as Vk.Mm
import Gpu.Vulkan.Buffer qualified as Vk.Bffr
import Gpu.Vulkan.Image qualified as Vk.Img
import Gpu.Vulkan.ImageView qualified as Vk.ImgVw
import Gpu.Vulkan.Sampler qualified as Vk.Smplr
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer qualified as Vk.CBffr
import Gpu.Vulkan.Cmd qualified as Vk.Cmd

import Gpu.Vulkan.Pipeline qualified as Vk.Ppl
import Gpu.Vulkan.Sample qualified as Vk.Sample
import Gpu.Vulkan.Descriptor qualified as Vk.Dsc
import Gpu.Vulkan.DescriptorSet qualified as Vk.DscSet

imgGroups :: Vk.Dvc.D sd ->
	(forall si sm siv . ImgGroups sd sm k si nm fmt siv -> IO a) -> IO a
imgGroups dv f =
	Vk.Img.group dv nil \ig -> Vk.Mm.group dv nil \mg ->
	Vk.ImgVw.group dv nil \vg -> f (ig, mg, vg)

type ImgGroups sd sm k si nm fmt siv = (
	Vk.Img.Group sd 'Nothing si k nm fmt,
	Vk.Mm.Group sd 'Nothing sm k '[ '(si, 'Vk.Mm.ImageArg nm fmt)],
	Vk.ImgVw.Group sd 'Nothing siv k nm fmt )

setImg :: forall sd sc sds sdsl bis sm k si nm i siv ss . (
	Vk.DscSet.BindingAndArrayElemImage bis '[ '(nm, BObj.ImageFormat i)] 0,
	BObj.IsImage i, Ord k ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.DscSet.D sds '(sdsl, bis) ->
	ImgGroups sd sm k si nm (BObj.ImageFormat i) siv ->
	Vk.Smplr.S ss -> i -> k -> IO ()
setImg pd dv gq cp ds (ig, mg, vg) s img k =
	createImg' pd dv gq cp ig mg k img >>= \i ->
	Vk.ImgVw.create' @_ @_ @(BObj.ImageFormat i) vg k
		(imgVwInfo i Vk.Img.AspectColorBit 1) >>= \(AlwaysRight iv) ->
	updateDscStImg dv ds iv s

updateImg :: (Vk.DscSet.BindingAndArrayElemImage bis '[ '(nm, fmt)] 0, Ord k) =>
	Vk.Dvc.D sd -> Vk.DscSet.D sds '(sdsl, bis) -> Vk.Smplr.S ss ->
	Vk.ImgVw.Group sd 'Nothing siv k nm fmt -> k -> IO ()
updateImg dv ds s ib k = maybe (error "no such key in the image view groups")
	(\iv -> updateDscStImg dv ds iv s) =<< Vk.ImgVw.lookup ib k

createImg' :: forall sd sc sm k si nm i . (Ord k, BObj.IsImage i) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Img.Group sd 'Nothing si k nm (BObj.ImageFormat i) ->
	Vk.Mm.Group sd 'Nothing sm k
		'[ '(si, 'Vk.Mm.ImageArg nm (BObj.ImageFormat i))] -> k -> i ->
	IO (Vk.Img.Binded sm si nm (BObj.ImageFormat i))
createImg' pd dv gq cp ig mg k img = do
	(i, _) <- prepareImg' @(BObj.ImageFormat i) pd dv ig mg k
		w h Vk.Img.TilingOptimal
		(Vk.Img.UsageTransferDstBit .|. Vk.Img.UsageSampledBit)
		Vk.Mm.PropertyDeviceLocalBit
	i <$ createBffrImg @i pd dv (w, w, h, 1)
		Vk.Bffr.UsageTransferSrcBit
		(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit)
		\(b :: Vk.Bffr.Binded sm' sb bnm '[ VObj.Image 1 a nm]) bm -> do
		Vk.Mm.write @bnm @(VObj.Image 1 i nm) dv bm zeroBits img
		transitionImgLyt dv gq cp i
			Vk.Img.LayoutUndefined Vk.Img.LayoutTransferDstOptimal 1
		copyBffrToImg dv gq cp b i
		transitionImgLyt dv gq cp i
			Vk.Img.LayoutTransferDstOptimal
			Vk.Img.LayoutShaderReadOnlyOptimal 1
	where
	w, h :: Integral n => n
	w = fromIntegral $ BObj.imageWidth img
	h = fromIntegral $ BObj.imageHeight img

-- UPDATE DESCRIPTOR SETS IMAGE

updateDscStImg ::
	Vk.DscSet.BindingAndArrayElemImage bis '[ '(nm, fmt)] 0 =>
	Vk.Dvc.D sd -> Vk.DscSet.D sds '(sdsc, bis) ->
	Vk.ImgVw.I nm fmt siv  -> Vk.Smplr.S ss -> IO ()
updateDscStImg dv ds iv s =
	Vk.DscSet.updateDs dv (U5 (dscWrite1 ds iv s) :** HPList.Nil) HPList.Nil

dscWrite1 :: Vk.DscSet.D sds slbts -> Vk.ImgVw.I nm fmt si -> Vk.Smplr.S ss ->
	Vk.DscSet.Write 'Nothing sds slbts
		('Vk.DscSet.WriteSourcesArgImage '[ '(ss, nm, fmt, si) ]) 0
dscWrite1 ds v s = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N, Vk.DscSet.writeDstSet = ds,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeCombinedImageSampler,
	Vk.DscSet.writeSources = Vk.DscSet.ImageInfos . HPList.Singleton
		$ U4 Vk.Dsc.ImageInfo {
			Vk.Dsc.imageInfoImageLayout =
				Vk.Img.LayoutShaderReadOnlyOptimal,
			Vk.Dsc.imageInfoImageView = v,
			Vk.Dsc.imageInfoSampler = s } }

-- IMAGE VIEW CREATE INFO

imgVwInfo :: Vk.Img.Binded sm si nm ifmt -> Vk.Img.AspectFlags -> Word32 ->
	Vk.ImgVw.CreateInfo 'Nothing sm si nm ifmt vfmt
imgVwInfo i a ml = Vk.ImgVw.CreateInfo {
	Vk.ImgVw.createInfoNext = TMaybe.N,
	Vk.ImgVw.createInfoFlags = zeroBits,
	Vk.ImgVw.createInfoImage = i,
	Vk.ImgVw.createInfoViewType = Vk.ImgVw.Type2d,
	Vk.ImgVw.createInfoComponents = def,
	Vk.ImgVw.createInfoSubresourceRange = Vk.Img.SubresourceRange {
		Vk.Img.subresourceRangeAspectMask = a,
		Vk.Img.subresourceRangeBaseMipLevel = 0,
		Vk.Img.subresourceRangeLevelCount = ml,
		Vk.Img.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.subresourceRangeLayerCount = 1 } }

-- PREPARE IMAGE

prepareImg' :: forall fmt sd sm k si nm . (Ord k, Vk.T.FormatToValue fmt) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Img.Group sd 'Nothing si k nm fmt ->
	Vk.Mm.Group sd 'Nothing sm k '[ '(si, 'Vk.Mm.ImageArg nm fmt)] ->
	k -> Word32 -> Word32 -> Vk.Img.Tiling ->
	Vk.Img.UsageFlagBits -> Vk.Mm.PropertyFlagBits -> IO (
		Vk.Img.Binded sm si nm fmt,
		Vk.Mm.M sm '[ '(si, 'Vk.Mm.ImageArg nm fmt)] )
prepareImg' pd dv ig mg k w h tl us prs = do
	Right i <- Vk.Img.create' ig k $ imgInfo w h 1 Vk.Sample.Count1Bit tl us
	rqs <- Vk.Img.getMemoryRequirements dv i
	mt <- findMmType pd (Vk.Mm.requirementsMemoryTypeBits rqs) prs
	Right (HPList.Singleton (U2 (Vk.Mm.ImageBinded b)), m) <-
		Vk.Mm.allocateBind' mg k
			(HPList.Singleton . U2 $ Vk.Mm.Image i) (memInfo mt)
	pure (b, m)

imgInfo :: Word32 -> Word32 -> Word32 -> Vk.Sample.CountFlags ->
	Vk.Img.Tiling -> Vk.Img.UsageFlags -> Vk.Img.CreateInfo 'Nothing fmt
imgInfo w h ml spcnt tl us = Vk.Img.CreateInfo {
		Vk.Img.createInfoNext = TMaybe.N,
		Vk.Img.createInfoImageType = Vk.Img.Type2d,
		Vk.Img.createInfoExtent = Vk.Extent3d {
			Vk.extent3dWidth = w, Vk.extent3dHeight = h,
			Vk.extent3dDepth = 1 },
		Vk.Img.createInfoMipLevels = ml,
		Vk.Img.createInfoArrayLayers = 1,
		Vk.Img.createInfoTiling = tl,
		Vk.Img.createInfoInitialLayout = Vk.Img.LayoutUndefined,
		Vk.Img.createInfoUsage = us,
		Vk.Img.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Img.createInfoSamples = spcnt,
		Vk.Img.createInfoFlags = zeroBits,
		Vk.Img.createInfoQueueFamilyIndices = [] }

memInfo :: Vk.Mm.TypeIndex -> Vk.Mm.AllocateInfo 'Nothing
memInfo mt = Vk.Mm.AllocateInfo {
	Vk.Mm.allocateInfoNext = TMaybe.N,
	Vk.Mm.allocateInfoMemoryTypeIndex = mt }

-- COPY BUFFER TO IMAGE

copyBffrToImg :: forall sd sc smb sbb nmb al img imgnm smi si nmi .
	(KnownNat al, Storable (BObj.ImagePixel img)) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Bffr.Binded smb sbb nmb '[ VObj.Image al img imgnm]  ->
	Vk.Img.Binded smi si nmi (BObj.ImageFormat img) -> IO ()
copyBffrToImg dv gq cp bf img = singleTimeCmds dv gq cp \cb ->
	Vk.Cmd.copyBufferToImage @al @img @'[imgnm] cb bf img
		Vk.Img.LayoutTransferDstOptimal
		$ HPList.Singleton Vk.Bffr.ImageCopy {
			Vk.Bffr.imageCopyImageSubresource = isr,
			Vk.Bffr.imageCopyImageOffset = Vk.Offset3d 0 0 0,
			Vk.Bffr.imageCopyImageExtent = Vk.Extent3d w h 1 }
	where
	isr = Vk.Img.SubresourceLayers {
		Vk.Img.subresourceLayersAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.subresourceLayersMipLevel = 0,
		Vk.Img.subresourceLayersBaseArrayLayer = 0,
		Vk.Img.subresourceLayersLayerCount = 1 }
	VObj.LengthImage _r (fromIntegral -> w) (fromIntegral -> h) _d =
		VObj.lengthOf @(VObj.Image al img imgnm) $ Vk.Bffr.lengthBinded bf

-- TRANSITION IMAGE LAYOUT

transitionImgLyt :: forall sd sc si sm nm fmt . Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> Vk.Img.Binded sm si nm fmt ->
	Vk.Img.Layout -> Vk.Img.Layout -> Word32 -> IO ()
transitionImgLyt dv gq cp i ol nl ml = singleTimeCmds dv gq cp \cb ->
	Vk.Cmd.pipelineBarrier cb ss ds
		zeroBits HPList.Nil HPList.Nil . HPList.Singleton $ U5 brrr
	where
	brrr = Vk.Img.MemoryBarrier {
		Vk.Img.memoryBarrierNext = TMaybe.N,
		Vk.Img.memoryBarrierOldLayout = ol,
		Vk.Img.memoryBarrierNewLayout = nl,
		Vk.Img.memoryBarrierSrcQueueFamilyIndex = Vk.QFam.Ignored,
		Vk.Img.memoryBarrierDstQueueFamilyIndex = Vk.QFam.Ignored,
		Vk.Img.memoryBarrierImage = i,
		Vk.Img.memoryBarrierSubresourceRange = srr,
		Vk.Img.memoryBarrierSrcAccessMask = sam,
		Vk.Img.memoryBarrierDstAccessMask = dam }
	srr = Vk.Img.SubresourceRange {
		Vk.Img.subresourceRangeAspectMask = asps,
		Vk.Img.subresourceRangeBaseMipLevel = 0,
		Vk.Img.subresourceRangeLevelCount = ml,
		Vk.Img.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.subresourceRangeLayerCount = 1 }
	asps = case nl of
		Vk.Img.LayoutDepthStencilAttachmentOptimal ->
			Vk.Img.AspectDepthBit .|.
			bool zeroBits Vk.Img.AspectStencilBit stencil
		_ -> Vk.Img.AspectColorBit
	stencil = case Vk.T.formatToValue @fmt of
		Vk.FormatD32SfloatS8Uint -> True
		Vk.FormatD24UnormS8Uint -> True; _ -> False
	(sam, dam, ss, ds) = case (ol, nl) of
		(Vk.Img.LayoutUndefined, Vk.Img.LayoutTransferDstOptimal) -> (
			zeroBits, Vk.AccessTransferWriteBit,
			Vk.Ppl.StageTopOfPipeBit, Vk.Ppl.StageTransferBit )
		(Vk.Img.LayoutTransferDstOptimal,
			Vk.Img.LayoutShaderReadOnlyOptimal) -> (
			Vk.AccessTransferWriteBit, Vk.AccessShaderReadBit,
			Vk.Ppl.StageTransferBit, Vk.Ppl.StageFragmentShaderBit )
		(Vk.Img.LayoutUndefined,
			Vk.Img.LayoutDepthStencilAttachmentOptimal) -> (
			zeroBits,
			Vk.AccessDepthStencilAttachmentReadBit .|.
				Vk.AccessDepthStencilAttachmentWriteBit,
			Vk.Ppl.StageTopOfPipeBit,
			Vk.Ppl.StageEarlyFragmentTestsBit )
		_ -> error "unsupported layout transition!"

-- SINGLE TIME COMMANDS

singleTimeCmds :: forall sd sc a .
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	(forall s . Vk.CBffr.C s -> IO a) -> IO a
singleTimeCmds dv gq cp cmd =
	Vk.CBffr.allocate dv (cmdBffrInfo @'[ '()] cp) \(cb :*. HPList.Nil) ->
	Vk.CBffr.begin @_ @'Nothing cb binfo (cmd cb) <* do
		Vk.Q.submit gq (HPList.Singleton . U4 $ sinfo cb) Nothing
		Vk.Q.waitIdle gq
	where
	sinfo cb = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Nil }
	binfo = Vk.CBffr.BeginInfo {
		Vk.CBffr.beginInfoNext = TMaybe.N,
		Vk.CBffr.beginInfoFlags = Vk.CBffr.UsageOneTimeSubmitBit,
		Vk.CBffr.beginInfoInheritanceInfo = Nothing }

cmdBffrInfo :: forall n scp .
	Vk.CmdPl.C scp -> Vk.CBffr.AllocateInfo 'Nothing scp n
cmdBffrInfo cp = Vk.CBffr.AllocateInfo {
	Vk.CBffr.allocateInfoNext = TMaybe.N,
	Vk.CBffr.allocateInfoCommandPool = cp,
	Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }

-- CREATE BUFFER

createBffrImg :: Storable (BObj.ImagePixel t) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> (Vk.Dvc.Size, Vk.Dvc.Size, Vk.Dvc.Size, Vk.Dvc.Size) ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb nm '[ VObj.Image 1 t inm] ->
		Vk.Mm.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg nm '[ VObj.Image 1 t inm])] ->
		IO a) -> IO a
createBffrImg p dv (r, w, h, d) = createBffr p dv (VObj.LengthImage r w h d)

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

{-# COMPLETE AlwaysRight #-}

pattern AlwaysRight :: b -> Either a b
pattern AlwaysRight x <- Right x
