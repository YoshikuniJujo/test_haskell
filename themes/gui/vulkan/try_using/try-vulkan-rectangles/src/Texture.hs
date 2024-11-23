{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Texture (

	-- * CREATE AND BIND

	createBindImg, imgVwInfo, createBffrImg, createBffr, createBffr', bffrInfo,

	-- * WRITE BUFFER

	writeBffr,

	-- * COPY BETWEEN BUFFER AND IMAGE

	flashImg, copyBffrLst ) where

import GHC.Stack (HasCallStack)
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.TypeLevel.Tuple.Uncurry
import Data.Bits
import Data.Bits.ToolsYj
import Data.Default
import Data.Maybe
import Data.List qualified as L
import Data.HeteroParList (pattern (:*.))
import Data.HeteroParList qualified as HPList
import Data.Word

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Object qualified as Vk.Obj
import Gpu.Vulkan.Object.Base qualified as Vk.ObjB
import Gpu.Vulkan.Object.NoAlignment qualified as Vk.ObjNA
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.Queue qualified as Vk.Q
import Gpu.Vulkan.QueueFamily qualified as Vk.QFam
import Gpu.Vulkan.Device qualified as Vk.Dvc
import Gpu.Vulkan.Memory qualified as Vk.Mm
import Gpu.Vulkan.Buffer qualified as Vk.Bffr
import Gpu.Vulkan.Image qualified as Vk.Img
import Gpu.Vulkan.ImageView qualified as Vk.ImgVw
import Gpu.Vulkan.Sample qualified as Vk.Sample
import Gpu.Vulkan.Sampler qualified as Vk.Smplr
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer qualified as Vk.CmdBffr
import Gpu.Vulkan.Cmd qualified as Vk.Cmd
import Gpu.Vulkan.Pipeline qualified as Vk.Ppl
import Gpu.Vulkan.Descriptor qualified as Vk.Dsc
import Gpu.Vulkan.DescriptorSet qualified as Vk.DscSt

----------------------------------------------------------------------
--
-- * CREATE AND BIND IMAGE
-- * CREATE IMAGE BUFFER
-- * WRITE BUFFER
-- * COPY FROM BUFFER TO IMAGE
--
----------------------------------------------------------------------

-- CREATE AND BIND IMAGE

createBindImg :: forall sd sds sdsl bis ssmp nmt fmt a . (
	Vk.T.FormatToValue fmt,
	Vk.DscSt.BindingAndArrayElemImage bis '[ '(nmt, fmt)] 0 ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.DscSt.D sds '(sdsl, bis) ->
	Vk.Smplr.S ssmp -> (Word32, Word32) ->
	(forall sm si . Vk.Img.Binded sm si nmt fmt -> IO a) -> IO a
createBindImg pd dv ds smp sz f = createImg pd dv sz \i ->
	Vk.ImgVw.create @_ @fmt dv (imgVwInfo i Vk.Img.AspectColorBit) nil \v ->
	updateDscSt dv ds v smp >> f i

createImg :: forall sd nm fmt a . Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd -> (Word32, Word32) ->
	(forall sm si . Vk.Img.Binded sm si nm fmt -> IO a) -> IO a
createImg pd dv (w, h) f = Vk.Img.create dv iinfo nil \i ->
	Vk.Img.getMemoryRequirements dv i >>= \rs ->
	findMmType pd
		(Vk.Mm.requirementsMemoryTypeBits rs)
		Vk.Mm.PropertyDeviceLocalBit >>= \mt ->
	Vk.Mm.allocateBind dv
		(HPList.Singleton . U2 $ Vk.Mm.Image i) (minfo mt) nil
		\(HPList.Singleton (U2 (Vk.Mm.ImageBinded b))) _m -> f b
	where
	iinfo = Vk.Img.CreateInfo {
		Vk.Img.createInfoNext = TMaybe.N,
		Vk.Img.createInfoFlags = zeroBits,
		Vk.Img.createInfoImageType = Vk.Img.Type2d,
		Vk.Img.createInfoExtent = Vk.Extent3d {
			Vk.extent3dWidth = w, Vk.extent3dHeight = h,
			Vk.extent3dDepth = 1 },
		Vk.Img.createInfoMipLevels = 1,
		Vk.Img.createInfoArrayLayers = 1,
		Vk.Img.createInfoTiling = Vk.Img.TilingOptimal,
		Vk.Img.createInfoInitialLayout = Vk.Img.LayoutUndefined,
		Vk.Img.createInfoUsage =
			Vk.Img.UsageTransferDstBit .|. Vk.Img.UsageSampledBit,
		Vk.Img.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Img.createInfoSamples = Vk.Sample.Count1Bit,
		Vk.Img.createInfoQueueFamilyIndices = [] }
	minfo mt = Vk.Mm.AllocateInfo {
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

imgVwInfo :: Vk.Img.Binded sm si nm ifmt -> Vk.Img.AspectFlags ->
	Vk.ImgVw.CreateInfo 'Nothing sm si nm ifmt vfmt
imgVwInfo i a = Vk.ImgVw.CreateInfo {
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

updateDscSt :: forall sd sds sdsl bis nmt fmt siv ssmp .
	Vk.DscSt.BindingAndArrayElemImage bis '[ '(nmt, fmt)] 0 =>
	Vk.Dvc.D sd -> Vk.DscSt.D sds '(sdsl, bis) ->
	Vk.ImgVw.I nmt fmt siv  -> Vk.Smplr.S ssmp -> IO ()
updateDscSt dv ds v smp =
	Vk.DscSt.updateDs dv (HPList.Singleton $ U5 wr) HPList.Nil
	where
	wr :: Vk.DscSt.Write 'Nothing sds '(sdsl, bis)
		('Vk.DscSt.WriteSourcesArgImage '[ '(ssmp, nmt, fmt, siv) ]) 0
	wr = Vk.DscSt.Write {
		Vk.DscSt.writeNext = TMaybe.N, Vk.DscSt.writeDstSet = ds,
		Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeCombinedImageSampler,
		Vk.DscSt.writeSources = Vk.DscSt.ImageInfos . HPList.Singleton
			$ U4 Vk.Dsc.ImageInfo {
				Vk.Dsc.imageInfoImageLayout =
					Vk.Img.LayoutShaderReadOnlyOptimal,
				Vk.Dsc.imageInfoImageView = v,
				Vk.Dsc.imageInfoSampler = smp } }

-- CREATE IMAGE BUFFER

createBffrImg :: forall sd bnm i nm . Vk.ObjB.IsImage i =>
	Vk.Phd.P -> Vk.Dvc.D sd -> (Vk.Dvc.Size, Vk.Dvc.Size) -> (forall sm sb .
		Vk.Bffr.Binded sm sb bnm '[Vk.ObjNA.Image i nm] ->
		Vk.Mm.M sm '[
			'(sb, Vk.Mm.BufferArg bnm '[Vk.ObjNA.Image i nm]) ] ->
		IO ()) -> IO ()
createBffrImg pd dv (w, h) = createBffr pd dv (Vk.Obj.LengthImage w w h 1)
	Vk.Bffr.UsageTransferSrcBit
	(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit)

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
createBffr' p dv bg mg k ln us prs = do
	(rightOrError -> b) <- Vk.Bffr.create' bg k $ bffrInfo ln us
	rqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMmType p (Vk.Mm.requirementsMemoryTypeBits rqs) prs
	(rightOrError -> (HPList.Singleton (U2 (Vk.Mm.BufferBinded bd)), m)) <-
		Vk.Mm.allocateBind'
			mg k (HPList.Singleton . U2 $ Vk.Mm.Buffer b) (ainfo mt)
	pure (bd, m)
	where ainfo mt = Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mt }

rightOrError :: HasCallStack => Either String r -> r
rightOrError = \case Left msg -> error msg; Right v -> v

bffrInfo ::
	Vk.Obj.Length s -> Vk.Bffr.UsageFlags -> Vk.Bffr.CreateInfo Nothing '[s]
bffrInfo ln us = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N,
	Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths = HPList.Singleton ln,
	Vk.Bffr.createInfoUsage = us,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

-- WRITE BUFFER

writeBffr :: forall sd sm sb bnm i nm . Vk.ObjB.IsImage i =>
	Vk.Dvc.D sd ->
	Vk.Mm.M sm '[ '(sb, Vk.Mm.BufferArg bnm '[Vk.ObjNA.Image i nm])] -> i ->
	IO ()
writeBffr dv m = Vk.Mm.write @bnm @(Vk.ObjNA.Image i nm) @0 dv m zeroBits

-- COPY FROM BUFFER TO IMAGE

flashImg :: forall sd sc sim si inm i sbm sb bnmi nmi . Vk.ObjB.IsImage i =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Img.Binded sim si inm (Vk.ObjB.ImageFormat i) ->
	Vk.Bffr.Binded sbm sb bnmi '[Vk.ObjNA.Image i nmi] ->
	(Word32, Word32) -> IO ()
flashImg dv gq cp i b sz = do
	tr Vk.Img.LayoutUndefined Vk.Img.LayoutTransferDstOptimal
	copyBffrToImg dv gq cp b i sz
	tr Vk.Img.LayoutTransferDstOptimal Vk.Img.LayoutShaderReadOnlyOptimal
	where tr = transitionImgLyt dv gq cp i

transitionImgLyt :: forall sd sc sm si nm fmt .
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Img.Binded sm si nm fmt -> Vk.Img.Layout -> Vk.Img.Layout -> IO ()
transitionImgLyt dv gq cp i ol nl = singleTimeCmds dv gq cp \cb ->
	Vk.Cmd.pipelineBarrier cb sstg dstg zeroBits HPList.Nil HPList.Nil brrr
	where
	brrr = HPList.Singleton $ U5 Vk.Img.MemoryBarrier {
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
			Vk.Img.LayoutShaderReadOnlyOptimal ) -> (
			Vk.AccessTransferWriteBit, Vk.AccessShaderReadBit,
			Vk.Ppl.StageTransferBit, Vk.Ppl.StageFragmentShaderBit )
		_ -> error "unsupported layout transition!"

copyBffrToImg :: forall sd sc sbm sb bnm i nmi sim si inm .
	(Storable (Vk.ObjB.ImagePixel i)) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Bffr.Binded sbm sb bnm '[Vk.ObjNA.Image i nmi]  ->
	Vk.Img.Binded sim si inm (Vk.ObjB.ImageFormat i) -> (Word32, Word32) ->
	IO ()
copyBffrToImg dv gq cp b i (w, h) = singleTimeCmds dv gq cp \cb ->
	Vk.Cmd.copyBufferToImage @1 cb b i Vk.Img.LayoutTransferDstOptimal rgns
	where
	rgns :: HPList.PL (Vk.Bffr.ImageCopy i) '[nmi]
	rgns = HPList.Singleton Vk.Bffr.ImageCopy {
		Vk.Bffr.imageCopyImageSubresource = srl,
		Vk.Bffr.imageCopyImageOffset = Vk.Offset3d 0 0 0,
		Vk.Bffr.imageCopyImageExtent = Vk.Extent3d w h 1 }
	srl = Vk.Img.SubresourceLayers {
		Vk.Img.subresourceLayersAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.subresourceLayersMipLevel = 0,
		Vk.Img.subresourceLayersBaseArrayLayer = 0,
		Vk.Img.subresourceLayersLayerCount = 1 }

copyBffrLst :: forall sd sc sm sb bnm t lnm sm' sb' bnm' . Storable' t =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Bffr.Binded sm sb bnm '[Vk.ObjNA.List t lnm] ->
	Vk.Bffr.Binded sm' sb' bnm' '[Vk.ObjNA.List t lnm] -> IO ()
copyBffrLst dv gq cp s d = singleTimeCmds dv gq cp \cb ->
	Vk.Cmd.copyBuffer @'[ '( '[Vk.ObjNA.List t lnm], 0, 0)] cb s d

singleTimeCmds :: forall sd sc a . Vk.Dvc.D sd -> Vk.Q.Q ->
	Vk.CmdPl.C sc -> (forall s . Vk.CmdBffr.C s -> IO a) -> IO a
singleTimeCmds dv gq cp cmd =
	Vk.CmdBffr.allocateCs dv ainfo \(cb :*. HPList.Nil) ->
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
