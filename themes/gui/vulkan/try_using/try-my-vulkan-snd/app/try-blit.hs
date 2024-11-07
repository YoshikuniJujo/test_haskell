{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-partial-type-signatures #-}

module Main (main) where

import GHC.TypeNats
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.TypeLevel.Tuple.Uncurry
import Data.Default
import Data.Bits
import Data.Bits.ToolsYj
import Data.Maybe
import Data.Maybe.ToolsYj
import Data.List
import Data.List.ToolsYj
import Data.HeteroParList (pattern (:*.))
import Data.HeteroParList qualified as HPList
import Data.Array
import Data.Word
import Data.Int
import Codec.Picture

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Object qualified as VObj
import Gpu.Vulkan.Object.Base qualified as BObj
import Gpu.Vulkan.Instance qualified as Vk.Ist
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.Queue qualified as Vk.Q
import Gpu.Vulkan.QueueFamily qualified as Vk.QFam
import Gpu.Vulkan.Device qualified as Vk.Dvc
import Gpu.Vulkan.Memory qualified as Vk.Mm
import Gpu.Vulkan.Buffer qualified as Vk.Bffr
import Gpu.Vulkan.Image qualified as Vk.Img
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer qualified as Vk.CBffr
import Gpu.Vulkan.Cmd qualified as Vk.Cmd

import Gpu.Vulkan.Pipeline qualified as Vk.Ppl
import Gpu.Vulkan.Sample qualified as Vk.Sample

import Data.Bool

saikoro :: FilePath
saikoro = "../../../../../files/images/saikoro.png"

main :: IO ()
main =	createIst \it -> pickPhd it >>= \(pd, qfi) -> createLgDvc pd qfi \dv ->
	either error convertRGBA8 <$> readImage saikoro >>= \(ImageRgba8 -> i) ->
	let	wdt = fromIntegral $ BObj.imageWidth i
		hgt = fromIntegral $ BObj.imageHeight i in
	createRsltBffr pd dv wdt hgt
		\(b :: RsltBffr sm sb nm img) bm -> do
	Vk.Dvc.getQueue dv qfi 0 >>= \gq -> createCmdPl qfi dv \cp ->
		body pd dv gq cp i \cb img ->
		copyImgToBffr @1 cb img b wdt hgt
	ImageRgba8 img <- Vk.Mm.read @nm @img @0 dv bm zeroBits
	writePng "try-blit.png" img

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

createCmdPl :: Vk.QFam.Index -> Vk.Dvc.D sd ->
	(forall sc . Vk.CmdPl.C sc -> IO a) -> IO a
createCmdPl gqfi dv = Vk.CmdPl.create dv info nil
	where info = Vk.CmdPl.CreateInfo {
		Vk.CmdPl.createInfoNext = TMaybe.N,
		Vk.CmdPl.createInfoFlags = zeroBits,
		Vk.CmdPl.createInfoQueueFamilyIndex = gqfi }

body :: Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> ImageRgba8 ->
	(forall scb sm' si . Vk.CBffr.C scb -> Vk.Img.Binded sm' si nm' RsltFmt -> IO a) -> IO a
body pd dv gq cp i f = prepareRsltImg @RsltFmt pd dv wdt hgt \img0 _ ->
	prepareRsltImg pd dv wdt hgt \img _mimg ->
	createBffrImg @1 @_ @ImageRgba8 pd dv
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit ) wdt hgt
		\(b :: Vk.Bffr.Binded sm sb inm '[bimg]) bm ->
	singleTimeCmds dv gq cp \cb -> do
	transitionImgLyt cb img0
		Vk.Img.LayoutUndefined Vk.Img.LayoutTransferDstOptimal
	transitionImgLyt cb img
		Vk.Img.LayoutUndefined Vk.Img.LayoutTransferDstOptimal
	Vk.Mm.write @inm @bimg @0 dv bm zeroBits i
	copyBffrToImg cb b img0
	transitionImgLyt cb img0
		Vk.Img.LayoutUndefined Vk.Img.LayoutTransferSrcOptimal
	copyImgToImg cb img0 img (fromIntegral wdt) (fromIntegral hgt)
	transitionImgLyt cb img
		Vk.Img.LayoutUndefined Vk.Img.LayoutTransferSrcOptimal
	f cb img
	where
	wdt = fromIntegral $ BObj.imageWidth i
	hgt = fromIntegral $ BObj.imageHeight i

createRsltBffr :: Vk.Phd.P -> Vk.Dvc.D sd -> Word32 -> Word32 ->
	(forall sm sb .
		RsltBffr sm sb bnmi (RsltObjImg 1 nmi) ->
		RsltMm sm sb bnmi (RsltObjImg 1 nmi) -> IO a) -> IO a
createRsltBffr pd dv = createBffrImg @1 pd dv Vk.Bffr.UsageTransferDstBit
	(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit)

type RsltBffr sm sb bnmi oimg = Vk.Bffr.Binded sm sb bnmi '[oimg]

type RsltMm sm sb bnmi oimg =
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg bnmi '[oimg])]

type RsltObjImg al nmi = VObj.Image al ImageRgba8 nmi
type RsltFmt = BObj.ImageFormat ImageRgba8

createBffrImg :: forall al sd img nm inm a . (BObj.IsImage img, KnownNat al) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags ->
	Word32 -> Word32 -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[VObj.Image al img inm] ->
		Vk.Mm.M sm '[
			'(sb, 'Vk.Mm.BufferArg nm '[VObj.Image al img inm]) ] ->
		IO a) -> IO a
createBffrImg p dv us prs (fromIntegral -> w) (fromIntegral -> h) a =
	createBffr @_ @_ @(VObj.Image al img inm) p dv ln us prs a
	where
	ln :: VObj.Length (VObj.Image al img inm)
	ln = VObj.LengthImage w w h 1

createBffr :: forall sd bnm o a . VObj.SizeAlignment o =>
	Vk.Phd.P -> Vk.Dvc.D sd -> VObj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb bnm '[o] -> Vk.Mm.M sm
			'[ '(sb, 'Vk.Mm.BufferArg bnm '[o])] -> IO a) -> IO a
createBffr p dv ln us prs f = Vk.Bffr.create dv bffrInfo nil \b -> do
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMmType p (Vk.Mm.requirementsMemoryTypeBits reqs) prs
	Vk.Mm.allocateBind dv (HPList.Singleton . U2 $ Vk.Mm.Buffer b)
		(ainfo mt) nil
		$ f . \(HPList.Singleton (U2 (Vk.Mm.BufferBinded bd))) -> bd
	where
	ainfo mt = Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mt }
	bffrInfo = Vk.Bffr.CreateInfo {
		Vk.Bffr.createInfoNext = TMaybe.N,
		Vk.Bffr.createInfoFlags = zeroBits,
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

prepareRsltImg :: Vk.T.FormatToValue fmt => Vk.Phd.P -> Vk.Dvc.D sd -> Word32 -> Word32 ->
	(forall si sm .
		Vk.Img.Binded sm si nm fmt ->
		Vk.Mm.M sm '[ '(si, 'Vk.Mm.ImageArg nm fmt)] -> IO a) -> IO a
prepareRsltImg pd dv w h = prepareImg pd dv Vk.Img.TilingLinear
	(	Vk.Img.UsageColorAttachmentBit .|.
		Vk.Img.UsageTransferSrcBit .|.
		Vk.Img.UsageTransferDstBit )
	Vk.Mm.PropertyHostVisibleBit w h

prepareImg :: forall sd nm fmt a . Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Img.Tiling ->
	Vk.Img.UsageFlagBits -> Vk.Mm.PropertyFlagBits -> Word32 -> Word32 ->
	(forall si sm .
		Vk.Img.Binded sm si nm fmt ->
		Vk.Mm.M sm '[ '(si, 'Vk.Mm.ImageArg nm fmt)] -> IO a) -> IO a
prepareImg pd dv tl us pr w h a = Vk.Img.create @'Nothing dv iinfo nil \i -> do
	rqs <- Vk.Img.getMemoryRequirements dv i
	mt <- findMmType pd (Vk.Mm.requirementsMemoryTypeBits rqs) pr
	Vk.Mm.allocateBind @'Nothing dv
		(HPList.Singleton . U2 $ Vk.Mm.Image i) (minfo mt) nil
		\(HPList.Singleton (U2 (Vk.Mm.ImageBinded b))) m -> a b m
	where
	iinfo = Vk.Img.CreateInfo {
		Vk.Img.createInfoNext = TMaybe.N,
		Vk.Img.createInfoImageType = Vk.Img.Type2d,
		Vk.Img.createInfoExtent = Vk.Extent3d {
			Vk.extent3dWidth = fromIntegral w,
			Vk.extent3dHeight = fromIntegral h,
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

transitionImgLyt :: Vk.CBffr.C scb ->
	Vk.Img.Binded s2 s3 s4 s5 -> Vk.Img.Layout -> Vk.Img.Layout -> IO ()
transitionImgLyt cb i ol nl =
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
		Vk.Img.subresourceRangeAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.subresourceRangeBaseMipLevel = 0,
		Vk.Img.subresourceRangeLevelCount = 1,
		Vk.Img.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.subresourceRangeLayerCount = 1 }
	(sam, dam, ss, ds) = case (ol, nl) of
		(Vk.Img.LayoutUndefined, Vk.Img.LayoutTransferDstOptimal) -> (
			zeroBits, Vk.AccessTransferWriteBit,
			Vk.Ppl.StageTopOfPipeBit, Vk.Ppl.StageTransferBit )
		(Vk.Img.LayoutTransferDstOptimal,
			Vk.Img.LayoutShaderReadOnlyOptimal) -> (
			Vk.AccessTransferWriteBit, Vk.AccessShaderReadBit,
			Vk.Ppl.StageTransferBit, Vk.Ppl.StageFragmentShaderBit )
		(Vk.Img.LayoutUndefined, Vk.Img.LayoutTransferSrcOptimal) -> (
			zeroBits, Vk.AccessTransferWriteBit,
			Vk.Ppl.StageTopOfPipeBit, Vk.Ppl.StageTransferBit )
		_ -> error "unsupported layout transition!"

copyBffrToImg :: forall smb sbb nmb al img imgnm smi si nmi scb .
	(KnownNat al, Storable (BObj.ImagePixel img)) =>
	Vk.CBffr.C scb ->
	Vk.Bffr.Binded smb sbb nmb '[ VObj.Image al img imgnm]  ->
	Vk.Img.Binded smi si nmi (BObj.ImageFormat img) -> IO ()
copyBffrToImg cb bf img =
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

copyImgToBffr ::
	forall al img sms sis inms smd sbd bnmd nmd scb . KnownNat al =>
	Storable (BObj.ImagePixel img) =>
	Vk.CBffr.C scb ->
	Vk.Img.Binded sms sis inms (BObj.ImageFormat img) ->
	Vk.Bffr.Binded smd sbd bnmd '[ VObj.Image al img nmd] ->
	Word32 -> Word32 -> IO ()
copyImgToBffr cb i bf w h =
	Vk.Cmd.copyImageToBuffer @al cb i Vk.Img.LayoutTransferSrcOptimal bf rgn
	where
	rgn :: HPList.PL (Vk.Bffr.ImageCopy img) '[nmd]
	rgn = HPList.Singleton Vk.Bffr.ImageCopy {
		Vk.Bffr.imageCopyImageSubresource = isr,
		Vk.Bffr.imageCopyImageOffset = Vk.Offset3d 0 0 0,
		Vk.Bffr.imageCopyImageExtent = Vk.Extent3d w h 1 }
	isr = Vk.Img.SubresourceLayers {
		Vk.Img.subresourceLayersAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.subresourceLayersMipLevel = 0,
		Vk.Img.subresourceLayersBaseArrayLayer = 0,
		Vk.Img.subresourceLayersLayerCount = 1 }

copyImgToImg :: Vk.CBffr.C scb ->
	Vk.Img.Binded sms sis nms fmts ->
	Vk.Img.Binded smd sid nmd fmtd -> Int32 -> Int32 -> IO ()
copyImgToImg cb si di w h =
	Vk.Cmd.blitImage cb
		si Vk.Img.LayoutTransferSrcOptimal
		di Vk.Img.LayoutTransferDstOptimal [blit] Vk.FilterLinear
	where
	blit = Vk.Img.Blit {
		Vk.Img.blitSrcSubresource = sr 0,
		Vk.Img.blitSrcOffsetFrom = Vk.Offset3d 0 0 0,
		Vk.Img.blitSrcOffsetTo = Vk.Offset3d w h 1,
		Vk.Img.blitDstSubresource = sr 0,
		Vk.Img.blitDstOffsetFrom = Vk.Offset3d 0 (half h) 0,
		Vk.Img.blitDstOffsetTo = Vk.Offset3d (half w) h 1 }
--		Vk.Img.blitDstOffsetFrom = Vk.Offset3d (half w) (half h) 0,
--		Vk.Img.blitDstOffsetTo = Vk.Offset3d w h 1 }
	sr sd = Vk.Img.SubresourceLayers {
		Vk.Img.subresourceLayersAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.subresourceLayersMipLevel = sd,
		Vk.Img.subresourceLayersBaseArrayLayer = 0,
		Vk.Img.subresourceLayersLayerCount = 1 }

singleTimeCmds :: forall sd sc a .
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	(forall s . Vk.CBffr.C s -> IO a) -> IO a
singleTimeCmds dv gq cp cmd =
	Vk.CBffr.allocate dv cmdBffrInfo \(cb :*. HPList.Nil) ->
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
	cmdBffrInfo :: Vk.CBffr.AllocateInfo 'Nothing sc '[ '()]
	cmdBffrInfo = Vk.CBffr.AllocateInfo {
		Vk.CBffr.allocateInfoNext = TMaybe.N,
		Vk.CBffr.allocateInfoCommandPool = cp,
		Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }

half :: Integral i => i -> i
half n = bool 1 (n `div` 2) (n > 1)

newtype ImageRgba8 = ImageRgba8 (Image PixelRGBA8)
newtype PixelRgba8 = PixelRgba8 PixelRGBA8

instance Storable PixelRgba8 where
	sizeOf _ = 4 * sizeOf @Pixel8 undefined
	alignment _ = alignment @Pixel8 undefined
	peek p = PixelRgba8 . (\(r, g, b, a) -> PixelRGBA8 r g b a)
		. listToTuple4 <$> peekArray 4 (castPtr p)
	poke p (PixelRgba8 (PixelRGBA8 r g b a)) =
		pokeArray (castPtr p) [r, g, b, a]

instance BObj.IsImage ImageRgba8 where
	type ImagePixel ImageRgba8 = PixelRgba8
	type ImageFormat ImageRgba8 = 'Vk.T.FormatR8g8b8a8Unorm
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
