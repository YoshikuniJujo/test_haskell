{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Control.Arrow
import Data.Kind
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.Bits
import Data.Bits.ToolsYj
import Data.Default
import Data.Maybe
import Data.Maybe.ToolsYj
import Data.List qualified as L
import Data.List.ToolsYj
import Data.HeteroParList (pattern (:**), pattern (:*), pattern (:*.))
import Data.HeteroParList qualified as HPList
import Data.Array
import Data.Word
import Data.Int
import Text.Read
import System.Environment
import Codec.Picture

import Language.SpirV qualified as SpirV
import Language.SpirV.Shaderc.TH
import Language.SpirV.ShaderKind

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Object qualified as Vk.Obj
import Gpu.Vulkan.Object.NoAlignment qualified as Vk.ObjNA
import Gpu.Vulkan.Object.Base qualified as Vk.ObjB
import Gpu.Vulkan.Instance qualified as Vk.Ist
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.Queue qualified as Vk.Q
import Gpu.Vulkan.QueueFamily qualified as Vk.QFam
import Gpu.Vulkan.Device qualified as Vk.Dvc
import Gpu.Vulkan.Memory qualified as Vk.Mm
import Gpu.Vulkan.Buffer qualified as Vk.Bffr
import Gpu.Vulkan.Image qualified as Vk.Img
import Gpu.Vulkan.ImageView qualified as Vk.ImgVw
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer qualified as Vk.CBffr
import Gpu.Vulkan.Cmd qualified as Vk.Cmd
import Gpu.Vulkan.Pipeline qualified as Vk.Ppl
import Gpu.Vulkan.Sample qualified as Vk.Sample
import Gpu.Vulkan.Sampler qualified as Vk.Smplr

import Gpu.Vulkan.Descriptor qualified as Vk.Dsc
import Gpu.Vulkan.DescriptorPool qualified as Vk.DscPl
import Gpu.Vulkan.DescriptorSet qualified as Vk.DscSt
import Gpu.Vulkan.DescriptorSetLayout qualified as Vk.DscStLyt
import Gpu.Vulkan.ShaderModule qualified as Vk.ShdrMd

import Gpu.Vulkan.Pipeline.Compute qualified as Vk.Ppl.Cp
import Gpu.Vulkan.Pipeline.ShaderStage qualified as Vk.Ppl.ShdrSt
import Gpu.Vulkan.PipelineLayout qualified as Vk.PplLyt
import Gpu.Vulkan.PushConstant qualified as Vk.PshCnst

-- DATA TYPE IMAGE RGBA8

newtype ImageRgba8 = ImageRgba8 (Image PixelRGBA8)
newtype PixelRgba8 = PixelRgba8 PixelRGBA8 deriving Show

instance Vk.ObjB.IsImage ImageRgba8 where
	type ImagePixel ImageRgba8 = PixelRgba8
	type ImageFormat ImageRgba8 = 'Vk.T.FormatR8g8b8a8Unorm
	imageRow = Vk.ObjB.imageWidth
	imageWidth (ImageRgba8 i) = fromIntegral $ imageWidth i
	imageHeight (ImageRgba8 i) = fromIntegral $ imageHeight i
	imageDepth _ = 1
	imageBody (ImageRgba8 i) = (<$> [0 ..imageHeight i - 1]) \y ->
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

-- MAIN

main :: IO ()
main = getArgs >>= \case
	[ifp, ofp, getFilter -> Just flt,
		readMaybe -> Just n, readMaybe -> Just i] -> do
		img <- either error convertRGBA8 <$> readImage ifp
		ImageRgba8 img' <- realMain (ImageRgba8 img) flt n i
		writePng ofp img'
	_ -> error "Invalid command line arguments"

getFilter :: String -> Maybe Vk.Filter
getFilter = \case
	"nearest" -> Just Vk.FilterNearest; "linear" -> Just Vk.FilterLinear
	_ -> Nothing

realMain :: ImageRgba8 -> Vk.Filter -> Int32 -> Int32 -> IO ImageRgba8
realMain img flt n i = createIst \ist -> pickPhd ist >>= \(pd, qfi) ->
	createLgDvc pd qfi \dv -> Vk.Dvc.getQueue dv qfi 0 >>= \gq ->
	createCmdPl qfi dv \cp -> body pd dv gq cp img flt n i

createIst :: (forall si . Vk.Ist.I si -> IO a) -> IO a
createIst = Vk.Ist.create info nil
	where
	info :: Vk.Ist.CreateInfo 'Nothing 'Nothing
	info = def { Vk.Ist.createInfoEnabledLayerNames = vldLayers }

vldLayers :: [Vk.LayerName]
vldLayers = [Vk.layerKhronosValidation]

pickPhd :: Vk.Ist.I si -> IO (Vk.Phd.P, Vk.QFam.Index)
pickPhd ist = Vk.Phd.enumerate ist >>= \case
	[] -> error "failed to find GPUs with Vulkan support!"
	pds -> findMaybeM suit pds >>= \case
		Nothing -> error "failed to find a suitable GPU!"
		Just pdqfi -> pure pdqfi
	where
	suit pd = findf <$> Vk.Phd.getQueueFamilyProperties pd
	findf ps = fst <$> L.find (grbit . snd) ps
	grbit = checkBits Vk.Q.GraphicsBit . Vk.QFam.propertiesQueueFlags

createLgDvc ::
	Vk.Phd.P -> Vk.QFam.Index -> (forall sd . Vk.Dvc.D sd -> IO a) -> IO a
createLgDvc pd qfi = Vk.Dvc.create pd info nil
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
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qfi,
		Vk.Dvc.queueCreateInfoQueuePriorities = [1.0] }

createCmdPl :: Vk.QFam.Index ->
	Vk.Dvc.D sd -> (forall sc . Vk.CmdPl.C sc -> IO a) -> IO a
createCmdPl qfi dv = Vk.CmdPl.create dv info nil
	where info = Vk.CmdPl.CreateInfo {
		Vk.CmdPl.createInfoNext = TMaybe.N,
		Vk.CmdPl.createInfoFlags = zeroBits,
		Vk.CmdPl.createInfoQueueFamilyIndex = qfi }

body :: forall sd sc img . Vk.ObjB.IsImage img => Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Q.Q -> Vk.CmdPl.C sc -> img -> Vk.Filter -> Int32 -> Int32 -> IO img
body pd dv gq cp img flt n i = resultBffr @img pd dv w h \rb ->
	prepareImg @(Vk.ObjB.ImageFormat img) pd dv w h \imgd ->
	prepareImg @DrawFormat pd dv w h \imgd' ->
	prepareImg @DrawFormat pd dv (w + 2) (h + 2) \imgs' ->
	Vk.ImgVw.create @_ @DrawFormat dv (imageViewCreateInfo imgd' Vk.Img.AspectColorBit) nil \imgvwd' ->
	Vk.ImgVw.create @_ @DrawFormat dv (imageViewCreateInfo imgs' Vk.Img.AspectColorBit) nil \imgvws' ->
	prepareImg pd dv w h \imgs ->
	createBffrImg @img pd dv Vk.Bffr.UsageTransferSrcBit w h
		\(b :: Vk.Bffr.Binded sm sb nm '[o]) bm ->
	Vk.Mm.write @nm @o @0 dv bm zeroBits [img] >>

	createCmpPpl
		@'[Word32]
		@('Vk.PshCnst.Range '[ 'Vk.T.ShaderStageComputeBit] '[Word32])
		dv (tbd :** HPList.Nil) expandWidthShader \wdsl wpl wppl ->
	createDscPl dv \wdp ->
	createDscSt' dv wdp imgvws' wdsl \wds ->

	createCmpPpl
		@'[Word32]
		@('Vk.PshCnst.Range '[ 'Vk.T.ShaderStageComputeBit] '[Word32])
		dv (tbd :** HPList.Nil) expandHeightShader \hdsl hpl hppl ->
	createDscPl dv \hdp ->
	createDscSt' dv hdp imgvws' hdsl \hds ->

	createCmpPpl
		@'[Word32, Word32, Word32]
		@('Vk.PshCnst.Range '[ 'Vk.T.ShaderStageComputeBit] '[Word32, Word32, Word32])
		dv (tbd :** tbd :** HPList.Nil) cubicShader \cdsl cpl cppl ->
	createDscPl dv \dp ->
	createDscSt dv dp imgvws' imgvwd' cdsl \ds ->

	runCmds dv gq cp \cb -> do
	tr cb imgs Vk.Img.LayoutUndefined Vk.Img.LayoutTransferDstOptimal
	copyBffrToImg cb b imgs
	tr cb imgs
		Vk.Img.LayoutTransferDstOptimal Vk.Img.LayoutTransferSrcOptimal
	tr cb imgs' Vk.Img.LayoutUndefined Vk.Img.LayoutTransferDstOptimal
	copyImgToImg' cb imgs imgs' w h flt
--	copyImgToImg cb imgs imgd w h flt n i
	tr cb imgs' Vk.Img.LayoutTransferDstOptimal Vk.Img.LayoutGeneral
	tr cb imgd' Vk.Img.LayoutUndefined Vk.Img.LayoutGeneral

	Vk.Cmd.bindPipelineCompute cb Vk.Ppl.BindPointCompute wppl \ccb -> do
		Vk.Cmd.pushConstantsCompute @'[ 'Vk.T.ShaderStageComputeBit]
			ccb wpl ((w :: Word32) :* HPList.Nil)
		Vk.Cmd.bindDescriptorSetsCompute ccb wpl (HPList.Singleton $ U2 wds) def
		Vk.Cmd.dispatch ccb 1 ((h + 2) `div'` 16) 1

	Vk.Cmd.bindPipelineCompute cb Vk.Ppl.BindPointCompute hppl \ccb -> do
		Vk.Cmd.pushConstantsCompute @'[ 'Vk.T.ShaderStageComputeBit]
			ccb hpl ((h :: Word32) :* HPList.Nil)
		Vk.Cmd.bindDescriptorSetsCompute ccb hpl (HPList.Singleton $ U2 hds) def
		Vk.Cmd.dispatch ccb ((w + 2) `div` 16) 1 1

	Vk.Cmd.bindPipelineCompute cb Vk.Ppl.BindPointCompute cppl \ccb -> do
		Vk.Cmd.pushConstantsCompute @'[ 'Vk.T.ShaderStageComputeBit]
			ccb cpl ((fromIntegral n :: Word32) :* (ix :: Word32) :* (iy :: Word32) :* HPList.Nil)
		Vk.Cmd.bindDescriptorSetsCompute ccb cpl (HPList.Singleton $ U2 ds) def
		Vk.Cmd.dispatch ccb (w `div'` 16) (h `div'` 16) 1

	tr cb imgd' Vk.Img.LayoutGeneral Vk.Img.LayoutTransferSrcOptimal
	tr cb imgd Vk.Img.LayoutUndefined Vk.Img.LayoutTransferDstOptimal
	copyImgToImg cb imgd' imgd w h flt 1 0
	tr cb imgd
		Vk.Img.LayoutTransferDstOptimal Vk.Img.LayoutTransferSrcOptimal
	copyImgToBffr cb imgd rb
	where
	w, h :: Integral n => n
	w = fromIntegral $ Vk.ObjB.imageWidth img
	h = fromIntegral $ Vk.ObjB.imageHeight img
	tr = transitionImgLyt
	ix, iy :: Word32
	ix = fromIntegral $ i `mod` n
	iy = fromIntegral $ i `div` n

div' :: Integral n => n -> n -> n
a `div'` b = case a `divMod` b of (d, 0) -> d; (d, _) -> d + 1

-- BUFFER

bffrInfo :: Vk.Obj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Bffr.CreateInfo 'Nothing '[o]
bffrInfo ln us = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N,
	Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths = HPList.Singleton ln,
	Vk.Bffr.createInfoUsage = us,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

findMmType ::
	Vk.Phd.P -> Vk.Mm.TypeBits -> Vk.Mm.PropertyFlags -> IO Vk.Mm.TypeIndex
findMmType pd tbs prs =
	fromMaybe (error msg) . suit <$> Vk.Phd.getMemoryProperties pd
	where
	msg = "failed to find suitable memory type!"
	suit p = fst <$> L.find ((&&)
		<$> (`Vk.Mm.elemTypeIndex` tbs) . fst
		<*> checkBits prs . Vk.Mm.mTypePropertyFlags . snd)
			(Vk.Phd.memoryPropertiesMemoryTypes p)

createBffr :: forall sd bnm o a . Vk.Obj.SizeAlignment o =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Obj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb bnm '[o] ->
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg bnm '[o])] -> IO a) -> IO a
createBffr pd dv ln us prs f = Vk.Bffr.create dv binfo nil \b -> do
	rqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMmType pd (Vk.Mm.requirementsMemoryTypeBits rqs) prs
	Vk.Mm.allocateBind dv (HPList.Singleton . U2 $ Vk.Mm.Buffer b)
		(ainfo mt) nil
		$ f . \(HPList.Singleton (U2 (Vk.Mm.BufferBinded bd))) -> bd
	where
	binfo = bffrInfo ln us
	ainfo mt = Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mt }

createBffrImg :: forall img sd bnm nm a . Vk.ObjB.IsImage img =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Bffr.UsageFlags ->
	Vk.Dvc.Size -> Vk.Dvc.Size -> (forall sm sb .
		Vk.Bffr.Binded sm sb bnm '[Vk.ObjNA.Image img nm] ->
		Vk.Mm.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg bnm '[Vk.ObjNA.Image img nm] )] ->
		IO a) -> IO a
createBffrImg pd dv us w h = createBffr pd dv (Vk.Obj.LengthImage w w h 1 1) us
	(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit)

prepareImg :: forall fmt sd nm a . Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Word32 -> Word32 ->
	(forall si sm . Vk.Img.Binded sm si nm fmt -> IO a) -> IO a
prepareImg pd dv w h f = Vk.Img.create @'Nothing dv iinfo nil \i -> do
	rqs <- Vk.Img.getMemoryRequirements dv i
	mt <- findMmType pd (Vk.Mm.requirementsMemoryTypeBits rqs) zeroBits
	Vk.Mm.allocateBind dv (HPList.Singleton . U2 $ Vk.Mm.Image i) (minfo mt)
		nil \(HPList.Singleton (U2 (Vk.Mm.ImageBinded bd))) _ -> f bd
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
		Vk.Img.createInfoSamples = Vk.Sample.Count1Bit,
		Vk.Img.createInfoTiling = Vk.Img.TilingOptimal,
		Vk.Img.createInfoUsage =
			Vk.Img.UsageSampledBit .|.
			Vk.Img.UsageStorageBit .|.
			Vk.Img.UsageTransferSrcBit .|.
			Vk.Img.UsageTransferDstBit,
		Vk.Img.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Img.createInfoQueueFamilyIndices = [],
		Vk.Img.createInfoInitialLayout = Vk.Img.LayoutUndefined }
	minfo mt = Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mt }

-- COMMANDS

runCmds :: forall sd sc a . Vk.Dvc.D sd ->
	Vk.Q.Q -> Vk.CmdPl.C sc -> (forall scb . Vk.CBffr.C scb -> IO a) -> IO a
runCmds dv gq cp cmds =
	Vk.CBffr.allocateCs dv cbinfo \(cb :*. HPList.Nil) ->
	Vk.CBffr.begin @_ @'Nothing cb binfo (cmds cb) <* do
	Vk.Q.submit gq (HPList.Singleton . U4 $ sinfo cb) Nothing
	Vk.Q.waitIdle gq
	where
	cbinfo :: Vk.CBffr.AllocateInfo 'Nothing sc '[ '()]
	cbinfo = Vk.CBffr.AllocateInfo {
		Vk.CBffr.allocateInfoNext = TMaybe.N,
		Vk.CBffr.allocateInfoCommandPool = cp,
		Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }
	binfo = Vk.CBffr.BeginInfo {
		Vk.CBffr.beginInfoNext = TMaybe.N,
		Vk.CBffr.beginInfoFlags = Vk.CBffr.UsageOneTimeSubmitBit,
		Vk.CBffr.beginInfoInheritanceInfo = Nothing }
	sinfo cb = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Nil }

copyBffrToImg :: forall scb smb sbb bnm img imgnm smi si inm .
	Storable (Vk.ObjB.ImagePixel img) => Vk.CBffr.C scb ->
	Vk.Bffr.Binded smb sbb bnm '[Vk.ObjNA.Image img imgnm] ->
	Vk.Img.Binded smi si inm (Vk.ObjB.ImageFormat img) -> IO ()
copyBffrToImg cb b@(bffrImgExtent -> (w, h)) i =
	Vk.Cmd.copyBufferToImage @1 @img @'[imgnm] cb b i
		Vk.Img.LayoutTransferDstOptimal
		$ HPList.Singleton Vk.Bffr.ImageCopy {
			Vk.Bffr.imageCopyImageSubresource = colorLayer0,
			Vk.Bffr.imageCopyImageOffset = Vk.Offset3d 0 0 0,
			Vk.Bffr.imageCopyImageExtent = Vk.Extent3d w h 1 }

colorLayer0 :: Vk.Img.SubresourceLayers
colorLayer0 = Vk.Img.SubresourceLayers {
	Vk.Img.subresourceLayersAspectMask = Vk.Img.AspectColorBit,
	Vk.Img.subresourceLayersMipLevel = 0,
	Vk.Img.subresourceLayersBaseArrayLayer = 0,
	Vk.Img.subresourceLayersLayerCount = 1 }

bffrImgExtent :: forall sm sb bnm img nm .
	Vk.Bffr.Binded sm sb bnm '[Vk.ObjNA.Image img nm] -> (Word32, Word32)
bffrImgExtent (Vk.Bffr.lengthBinded -> ln) = (w, h)
	where Vk.Obj.LengthImage _ (fromIntegral -> w) (fromIntegral -> h) _ _ =
		Vk.Obj.lengthOf @(Vk.ObjNA.Image img nm) ln

transitionImgLyt :: Vk.CBffr.C scb ->
	Vk.Img.Binded sm si nm fmt -> Vk.Img.Layout -> Vk.Img.Layout -> IO ()
transitionImgLyt cb i ol nl =
	Vk.Cmd.pipelineBarrier cb
		Vk.Ppl.StageTopOfPipeBit Vk.Ppl.StageTransferBit zeroBits
		HPList.Nil HPList.Nil . HPList.Singleton $ U5 brrr
	where
	brrr = Vk.Img.MemoryBarrier {
		Vk.Img.memoryBarrierNext = TMaybe.N,
		Vk.Img.memoryBarrierOldLayout = ol,
		Vk.Img.memoryBarrierNewLayout = nl,
		Vk.Img.memoryBarrierSrcQueueFamilyIndex = Vk.QFam.Ignored,
		Vk.Img.memoryBarrierDstQueueFamilyIndex = Vk.QFam.Ignored,
		Vk.Img.memoryBarrierImage = i,
		Vk.Img.memoryBarrierSubresourceRange = srr,
		Vk.Img.memoryBarrierSrcAccessMask = zeroBits,
		Vk.Img.memoryBarrierDstAccessMask = case nl of
			Vk.Img.LayoutTransferSrcOptimal ->
				Vk.AccessTransferReadBit
			Vk.Img.LayoutTransferDstOptimal ->
				Vk.AccessTransferWriteBit
			Vk.Img.LayoutGeneral ->
				Vk.AccessTransferReadBit
			_ -> error "unsupported layout transition!" }
	srr = Vk.Img.SubresourceRange {
		Vk.Img.subresourceRangeAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.subresourceRangeBaseMipLevel = 0,
		Vk.Img.subresourceRangeLevelCount = 1,
		Vk.Img.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.subresourceRangeLayerCount = 1 }

copyImgToImg :: Vk.CBffr.C scb ->
	Vk.Img.Binded sms sis nms fmts -> Vk.Img.Binded smd sid nmd fmtd ->
	Int32 -> Int32 -> Vk.Filter -> Int32 -> Int32 -> IO ()
copyImgToImg cb si di w h flt n i = Vk.Cmd.blitImage cb
	si Vk.Img.LayoutTransferSrcOptimal
	di Vk.Img.LayoutTransferDstOptimal [blt] flt
	where
	blt = Vk.Img.Blit {
		Vk.Img.blitSrcSubresource = colorLayer0,
		Vk.Img.blitSrcOffsetFrom = Vk.Offset3d l t 0,
		Vk.Img.blitSrcOffsetTo = Vk.Offset3d r b 1,
		Vk.Img.blitDstSubresource = colorLayer0,
		Vk.Img.blitDstOffsetFrom = Vk.Offset3d 0 0 0,
		Vk.Img.blitDstOffsetTo = Vk.Offset3d w h 1 }
	(l, r, t, b) = (
		w * (i `mod` n) `div` n, w * (i `mod` n + 1) `div` n,
		h * (i `div` n) `div` n, h * (i `div` n + 1) `div` n )

copyImgToImg' :: Vk.CBffr.C scb ->
	Vk.Img.Binded sms sis nms fmts -> Vk.Img.Binded smd sid nmd fmtd ->
	Int32 -> Int32 -> Vk.Filter -> IO ()
copyImgToImg' cb si di w h flt = Vk.Cmd.blitImage cb
	si Vk.Img.LayoutTransferSrcOptimal
	di Vk.Img.LayoutTransferDstOptimal [blt] flt
	where
	blt = Vk.Img.Blit {
		Vk.Img.blitSrcSubresource = colorLayer0,
		Vk.Img.blitSrcOffsetFrom = Vk.Offset3d 0 0 0,
		Vk.Img.blitSrcOffsetTo = Vk.Offset3d w h 1,
		Vk.Img.blitDstSubresource = colorLayer0,
		Vk.Img.blitDstOffsetFrom = Vk.Offset3d 1 1 0,
		Vk.Img.blitDstOffsetTo = Vk.Offset3d (w + 1) (h + 1) 1 }

copyImgToBffr :: forall scb img smi si inm smb sbb bnm imgnm .
	Storable (Vk.ObjB.ImagePixel img) => Vk.CBffr.C scb ->
	Vk.Img.Binded smi si inm (Vk.ObjB.ImageFormat img) ->
	Vk.Bffr.Binded smb sbb bnm '[Vk.ObjNA.Image img imgnm] -> IO ()
copyImgToBffr cb i b@(bffrImgExtent -> (w, h)) =
	Vk.Cmd.copyImageToBuffer @1 @img @'[imgnm] cb i
		Vk.Img.LayoutTransferSrcOptimal b
		$ HPList.Singleton Vk.Bffr.ImageCopy {
			Vk.Bffr.imageCopyImageSubresource = colorLayer0,
			Vk.Bffr.imageCopyImageOffset = Vk.Offset3d 0 0 0,
			Vk.Bffr.imageCopyImageExtent = Vk.Extent3d w h 1 }

-- RESULT BUFFER

resultBffr :: Vk.ObjB.IsImage img =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Dvc.Size -> Vk.Dvc.Size -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[Vk.ObjNA.Image img nmi] -> IO a) ->
	IO img
resultBffr pd dv w h f = head
	<$> createBffrImg pd dv Vk.Bffr.UsageTransferDstBit w h
		\(b :: Vk.Bffr.Binded sm sb nm '[o]) m ->
	f b >> Vk.Mm.read @nm @o @0 dv m zeroBits

createCmpPpl :: forall (pctps :: [Type]) (pcrng :: Vk.PshCnst.Range) sd bds a . (
	Vk.PshCnst.RangeListToMiddle pctps '[pcrng],
	Vk.DscStLyt.BindingListToMiddle bds
	) =>
	Vk.Dvc.D sd -> HPList.PL Vk.DscStLyt.Binding bds -> SpirV.S GlslComputeShader ->
	(forall sds scppl spl .
		Vk.DscStLyt.D sds bds ->
		Vk.PplLyt.P spl '[ '(sds, bds)] pctps ->
		Vk.Ppl.Cp.C scppl '(spl, '[ '( sds, bds)], pctps) ->
		IO a) -> IO a
createCmpPpl d tbds shdr f = createPplLyt @pctps @pcrng
		d tbds \dsl pl ->
	Vk.Ppl.Cp.createCs d Nothing (HPList.Singleton . U4 $ info pl) nil
		\(HPList.Singleton p) -> f dsl pl p
	where
	info pl = Vk.Ppl.Cp.CreateInfo {
		Vk.Ppl.Cp.createInfoNext = TMaybe.N,
		Vk.Ppl.Cp.createInfoFlags = zeroBits,
		Vk.Ppl.Cp.createInfoStage = U5 shdrst,
		Vk.Ppl.Cp.createInfoLayout = U3 pl,
		Vk.Ppl.Cp.createInfoBasePipelineHandleOrIndex = Nothing }
	shdrst :: Vk.Ppl.ShdrSt.CreateInfo
		'Nothing 'Nothing 'GlslComputeShader 'Nothing '[]
	shdrst = Vk.Ppl.ShdrSt.CreateInfo {
		Vk.Ppl.ShdrSt.createInfoNext = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlags = zeroBits,
		Vk.Ppl.ShdrSt.createInfoStage = Vk.ShaderStageComputeBit,
		Vk.Ppl.ShdrSt.createInfoModule =
			(shdrMdInfo shdr, nil),
		Vk.Ppl.ShdrSt.createInfoName = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfo = Nothing }

type DscStLytArg = '[SrcImg, DstImg]
type DstImg = 'Vk.DscStLyt.Image '[ '("destination_image", 'Vk.T.FormatR16g16b16a16Sfloat)]
type SrcImg = 'Vk.DscStLyt.Image '[ '("source_image", 'Vk.T.FormatR16g16b16a16Sfloat)]

shdrMdInfo :: SpirV.S sknd -> Vk.ShdrMd.CreateInfo 'Nothing sknd
shdrMdInfo cd = Vk.ShdrMd.CreateInfo {
	Vk.ShdrMd.createInfoNext = TMaybe.N,
	Vk.ShdrMd.createInfoFlags = zeroBits, Vk.ShdrMd.createInfoCode = cd }

createPplLyt :: forall (pctps :: [Type]) (pcrng :: Vk.PshCnst.Range) sd a bds . (
	Vk.DscStLyt.BindingListToMiddle bds,
	Vk.PshCnst.RangeListToMiddle pctps '[pcrng]
	) =>
	Vk.Dvc.D sd -> HPList.PL Vk.DscStLyt.Binding bds ->
	(forall sl sdsl .
	Vk.DscStLyt.D sdsl bds ->
	Vk.PplLyt.P sl '[ '(sdsl, bds)] pctps -> IO a) -> IO a
createPplLyt dv tbds f = createDscStLyt dv tbds \dsl ->
	Vk.PplLyt.create @_ @_ @_ dv (info dsl) nil $ f dsl
	where
	info :: Vk.DscStLyt.D sdsl bds ->
		Vk.PplLyt.CreateInfo 'Nothing '[ '(sdsl, bds)] ('Vk.PshCnst.Layout
--			'[Word32, Word32, Word32] '[ 'Vk.PshCnst.Range '[ 'Vk.T.ShaderStageComputeBit] '[Word32, Word32, Word32]])
			pctps '[pcrng])
	info dsl = Vk.PplLyt.CreateInfo {
		Vk.PplLyt.createInfoNext = TMaybe.N,
		Vk.PplLyt.createInfoFlags = zeroBits,
		Vk.PplLyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }

createDscStLyt :: Vk.DscStLyt.BindingListToMiddle arg =>
	Vk.Dvc.D sd -> HPList.PL Vk.DscStLyt.Binding arg ->
	(forall (s :: Type) . Vk.DscStLyt.D s arg -> IO a) -> IO a
createDscStLyt dv tbds = Vk.DscStLyt.create dv info nil
	where
	info = Vk.DscStLyt.CreateInfo {
		Vk.DscStLyt.createInfoNext = TMaybe.N,
		Vk.DscStLyt.createInfoFlags = zeroBits,
		Vk.DscStLyt.createInfoBindings = tbds }

tbd :: Vk.DscStLyt.Binding ('Vk.DscStLyt.Image iargs)
tbd = Vk.DscStLyt.BindingImage {
	Vk.DscStLyt.bindingImageDescriptorType = Vk.Dsc.TypeStorageImage,
	Vk.DscStLyt.bindingImageStageFlags = Vk.ShaderStageComputeBit }

createDscPl :: Vk.Dvc.D sd -> (forall sp . Vk.DscPl.P sp -> IO a) -> IO a
createDscPl dv = Vk.DscPl.create dv info nil
	where
	info = Vk.DscPl.CreateInfo {
		Vk.DscPl.createInfoNext = TMaybe.N,
		Vk.DscPl.createInfoFlags = Vk.DscPl.CreateFreeDescriptorSetBit,
		Vk.DscPl.createInfoMaxSets = 10,
		Vk.DscPl.createInfoPoolSizes = [sz] }
	sz = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeStorageImage,
		Vk.DscPl.sizeDescriptorCount = 2 }

createDscSt' ::
	Vk.Dvc.D sd -> Vk.DscPl.P sp ->
	Vk.ImgVw.I "source_image" 'Vk.T.FormatR16g16b16a16Sfloat sivs ->
	Vk.DscStLyt.D sdsl '[SrcImg] ->
	(forall sds . Vk.DscSt.D sds
		'(sdsl, '[SrcImg]) -> IO a) -> IO a
createDscSt' dv dp svw dl a =
	Vk.DscSt.allocateDs dv info \(HPList.Singleton ds) -> (>> a ds)
	$ Vk.DscSt.updateDs dv (
		U5 (dscWriteTxImg ds svw) :** HPList.Nil ) HPList.Nil
	where info = Vk.DscSt.AllocateInfo {
		Vk.DscSt.allocateInfoNext = TMaybe.N,
		Vk.DscSt.allocateInfoDescriptorPool = dp,
		Vk.DscSt.allocateInfoSetLayouts = HPList.Singleton $ U2 dl }

createDscSt ::
	Vk.Dvc.D sd -> Vk.DscPl.P sp ->
	Vk.ImgVw.I "source_image" 'Vk.T.FormatR16g16b16a16Sfloat sivs ->
	Vk.ImgVw.I "destination_image" 'Vk.T.FormatR16g16b16a16Sfloat sivd ->
	Vk.DscStLyt.D sdsl '[SrcImg, DstImg] ->
	(forall sds . Vk.DscSt.D sds
		'(sdsl, '[SrcImg, DstImg]) -> IO a) -> IO a
createDscSt dv dp svw dvw dl a =
	Vk.DscSt.allocateDs dv info \(HPList.Singleton ds) -> (>> a ds)
	$ Vk.DscSt.updateDs dv (
		U5 (dscWriteTxImg ds svw) :**
		U5 (dscWriteTxImg ds dvw) :** HPList.Nil ) HPList.Nil
	where info = Vk.DscSt.AllocateInfo {
		Vk.DscSt.allocateInfoNext = TMaybe.N,
		Vk.DscSt.allocateInfoDescriptorPool = dp,
		Vk.DscSt.allocateInfoSetLayouts = HPList.Singleton $ U2 dl }

dscWriteTxImg :: Vk.DscSt.D sds slbts -> Vk.ImgVw.I nm fmt si ->
	Vk.DscSt.Write 'Nothing sds slbts
		('Vk.DscSt.WriteSourcesArgImage '[ '(ss, nm, fmt, si) ]) 0
dscWriteTxImg ds v = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N, Vk.DscSt.writeDstSet = ds,
	Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeStorageImage,
	Vk.DscSt.writeSources = Vk.DscSt.ImageInfos . HPList.Singleton
		$ U4 Vk.Dsc.ImageInfo {
			Vk.Dsc.imageInfoImageLayout = Vk.Img.LayoutGeneral,
			Vk.Dsc.imageInfoImageView = v,
			Vk.Dsc.imageInfoSampler = Vk.Smplr.Null } }

imageViewCreateInfo ::
	Vk.Img.Binded sm si nm ifmt -> Vk.Img.AspectFlags ->
	Vk.ImgVw.CreateInfo 'Nothing sm si nm ifmt ivfmt
imageViewCreateInfo image aspectFlags = Vk.ImgVw.CreateInfo {
	Vk.ImgVw.createInfoNext = TMaybe.N,
	Vk.ImgVw.createInfoFlags = zeroBits,
	Vk.ImgVw.createInfoImage = image,
	Vk.ImgVw.createInfoViewType = Vk.ImgVw.Type2d,
	Vk.ImgVw.createInfoComponents = def,
	Vk.ImgVw.createInfoSubresourceRange = Vk.Img.SubresourceRange {
		Vk.Img.subresourceRangeAspectMask = aspectFlags,
		Vk.Img.subresourceRangeBaseMipLevel = 0,
		Vk.Img.subresourceRangeLevelCount = Vk.remainingMipLevels,
		Vk.Img.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.subresourceRangeLayerCount = Vk.remainingArrayLayers } }

type DrawFormat = Vk.T.FormatR16g16b16a16Sfloat

expandWidthShader :: SpirV.S GlslComputeShader
expandWidthShader = [glslComputeShader|

#version 460

layout (local_size_x = 16, local_size_y = 16) in;

layout(rgba16f,set = 0, binding = 0) uniform image2D simg;
layout(push_constant) uniform P { uint w; } p;

void
main()
{
	ivec2 texelCoord = ivec2(gl_GlobalInvocationID.xy);
	if (texelCoord.x == 0) {
		vec4 c, c_;
		vec4 c1 = imageLoad(simg, ivec2(1, texelCoord.y));
		vec4 c2 = imageLoad(simg, ivec2(2, texelCoord.y));
		vec4 c1_ = imageLoad(simg, ivec2(p.w, texelCoord.y));
		vec4 c2_ = imageLoad(simg, ivec2(p.w - 1, texelCoord.y));
		c = 2 * c1 - c2;
		c_ = 2 * c1_ - c2_;
		imageStore(simg, texelCoord, c);
		imageStore(simg, ivec2(p.w + 1, texelCoord.y), c_);
		}
}

|]

expandHeightShader :: SpirV.S GlslComputeShader
expandHeightShader = [glslComputeShader|

#version 460

layout (local_size_x = 16, local_size_y = 16) in;

layout(rgba16f,set = 0, binding = 0) uniform image2D simg;
layout(push_constant) uniform P { uint h; } p;

void
main()
{
	ivec2 texelCoord = ivec2(gl_GlobalInvocationID.xy);
	if (texelCoord.y == 0) {
		vec4 c, c_;
		vec4 c1 = imageLoad(simg, ivec2(texelCoord.x, 1));
		vec4 c2 = imageLoad(simg, ivec2(texelCoord.y, 2));
		vec4 c1_ = imageLoad(simg, ivec2(texelCoord.x, p.h));
		vec4 c2_ = imageLoad(simg, ivec2(texelCoord.y, p.h - 1));
		c = 2 * c1 - c2;
		c_ = 2 * c1_ - c2_;
		imageStore(simg, texelCoord, c);
		imageStore(simg, ivec2(texelCoord.x, p.h + 1), c_);
		}
}

|]

cubicShader :: SpirV.S GlslComputeShader
cubicShader = [glslComputeShader|

#version 460

layout (local_size_x = 16, local_size_y = 16) in;

layout(rgba16f,set = 0, binding = 0) uniform image2D simg;
layout(rgba16f,set = 0, binding = 1) uniform image2D dimg;

layout(push_constant) uniform P { uint n; uint ix; uint iy; } p;

float
formula01(float x)
{
	return (3 * pow(x, 3) - 5 * pow(x,  2) + 2) / 2;
}

float
formula12(float x)
{
	return (- pow(x, 3) + 5 * pow(x, 2) - 8 * x + 4) / 2;
}

float
formula_n01(float x)
{
	return 1 - x;
//	if (x < 0.5) return 1; else return 0;
}

float
formula_n12(float x)
{
	return 0;
}

float[4]
coefficients(float x)
{
	float co[4];
	float d = fract(x);
	co[0] = formula12(d + 1); co[1] = formula01(d);
	co[2] = formula01(1 - d); co[3] = formula12(2 - d);
//	co[0] = formula_n12(d + 1); co[1] = formula_n01(d);
//	co[2] = formula_n01(1 - d); co[3] = formula_n12(2 - d);
	return co;
}

vec4[4][4]
points(ivec2 p)
{
	vec4 c16[4][4];

	for (int y = 0; y < 4; y++)
		for (int x = 0; x < 4; x++)
			c16[y][x] = imageLoad(simg, ivec2(x + p.x, y + p.y));
	/*
	for (int y = p.y - 1; y < p.y + 3; y++)
		for (int x = p.x - 1; x < p.x + 3; x++)
			c16[y][x] = imageLoad(simg, ivec2(x, y));
			*/
//	c16[1][1] = vec4(0.0, 0.0, 1.0, 1.0);
	return c16;
}

void
main()
{
	ivec2 texelCoord = ivec2(gl_GlobalInvocationID.xy);
	ivec2 size = imageSize(dimg);

	float n, ix, iy;
	n = float(p.n);
	ix = float(p.ix);
	iy = float(p.iy);

	vec2 pos = vec2(
		float(size.x - 1) * ix / n + float(texelCoord.x) / n,
		float(size.y - 1) * iy / n + float(texelCoord.y) / n);

	float cox[4] = coefficients(pos.x);
	float coy[4] = coefficients(pos.y);

//	cox[0] = 0; cox[1] = 1; cox[2] = 0; cox[3] = 0;
//	coy[0] = 0; coy[1] = 1; coy[2] = 0; coy[3] = 0;

	vec4 c16[4][4] = points(ivec2(floor(pos.x), floor(pos.y)));

//	c16[1][1] = vec4(0.0, 1.0, 0.0, 1.0);

	vec4 c = vec4(0.0, 0.0, 0.0, 0.0);

//	for (int y = int(floor(pos.y)) - 1; y < int(floor(pos.y)) + 3; y++)
//		for (int x = int(floor(pos.x)) - 1; x < int(floor(pos.x)) + 3; x++)
	for (int y = 0; y < 4; y++)
		for (int x = 0; x < 4; x++)
			c += cox[x] * coy[y] * c16[y][x];

//	c = cox[1] * coy[1] * c16[1][1];

//	c = vec4(0.0, 1.0, 0.0, 1.0);

	if (texelCoord.x < size.x && texelCoord.y < size.y)
		imageStore(dimg, texelCoord, c);

/*
	if (texelCoord.x < size.x && texelCoord.y < size.y) {
		vec4 color = vec4(0.0, 0.0, 0.0, 1.0);

		if (1 == 1) {
			color = imageLoad(simg, ivec2(
				size.x * 13 / 25 + texelCoord.x / 25,
				size.y * 15 / 25 + texelCoord.y / 25)); }
//			color = imageLoad(simg, texelCoord); }
//			color.x = float(texelCoord.x) / (size.x);
//			color.y = float(texelCoord.y) / (size.y); }
		imageStore(dimg, texelCoord, c); }
		*/
}

|]
