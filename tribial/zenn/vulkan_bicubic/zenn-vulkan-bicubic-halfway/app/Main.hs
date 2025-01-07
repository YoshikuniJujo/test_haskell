{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
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

getFilter :: String -> Maybe Filter
getFilter = \case
	"nearest" -> Just Nearest; "linear" -> Just Linear; _ -> Nothing

newtype Filter = Filter Word32 deriving (Show, Storable)
pattern Nearest, Linear :: Filter
pattern Nearest = Filter 0; pattern Linear = Filter 1

realMain :: ImageRgba8 -> Filter -> Int32 -> Int32 -> IO ImageRgba8
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
	Vk.Q.Q -> Vk.CmdPl.C sc -> img -> Filter -> Int32 -> Int32 -> IO img
body pd dv gq cp img flt n i = resultBffr @img pd dv w h \rb ->
	prepareImg @(Vk.ObjB.ImageFormat img) pd dv w h \imgd ->
	prepareImg @ShaderFormat pd dv w h \imgd' ->
	prepareImg @ShaderFormat pd dv w h \imgs' ->
	Vk.ImgVw.create @_ @ShaderFormat dv (imgVwInfo imgd') nil \imgvwd' ->
	Vk.ImgVw.create @_ @ShaderFormat dv (imgVwInfo imgs') nil \imgvws' ->
	prepareImg pd dv w h \imgs ->
	createBffrImg @img pd dv Vk.Bffr.UsageTransferSrcBit w h
		\(b :: Vk.Bffr.Binded sm sb nm '[o]) bm ->
	Vk.Mm.write @nm @o @0 dv bm zeroBits [img] >>

	createCmpPpl @PshCnsts
		@('Vk.PshCnst.Range '[ 'Vk.T.ShaderStageComputeBit] PshCnsts)
		dv (strImgBinding :** strImgBinding :** HPList.Nil)
		glslComputeShaderMain \dsl pl ppl ->
	createDscPl dv \dp -> createDscSt dv dp imgvws' imgvwd' dsl \ds ->

	runCmds dv gq cp \cb -> do
	tr cb imgs Vk.Img.LayoutUndefined Vk.Img.LayoutTransferDstOptimal
	copyBffrToImg cb b imgs
	tr cb imgs
		Vk.Img.LayoutTransferDstOptimal Vk.Img.LayoutTransferSrcOptimal
	tr cb imgs' Vk.Img.LayoutUndefined Vk.Img.LayoutTransferDstOptimal
	copyImgToImg cb imgs imgs' w h Vk.FilterNearest 1 0
	tr cb imgs' Vk.Img.LayoutTransferDstOptimal Vk.Img.LayoutGeneral
	tr cb imgd' Vk.Img.LayoutUndefined Vk.Img.LayoutGeneral

	Vk.Cmd.bindPipelineCompute cb Vk.Ppl.BindPointCompute ppl \ccb -> do
		Vk.Cmd.bindDescriptorSetsCompute ccb pl (HPList.Singleton $ U2 ds) def
		Vk.Cmd.pushConstantsCompute @'[ 'Vk.T.ShaderStageComputeBit]
			ccb pl (flt :* (fromIntegral n :: Word32) :* ix :* iy :* HPList.Nil)
		Vk.Cmd.dispatch ccb (w `div'` 16) (h `div'` 16) 1

	tr cb imgd' Vk.Img.LayoutGeneral Vk.Img.LayoutTransferSrcOptimal
	tr cb imgd Vk.Img.LayoutUndefined Vk.Img.LayoutTransferDstOptimal
	copyImgToImg cb imgd' imgd w h Vk.FilterNearest 1 0
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
	a `div'` b = case a `divMod` b of (d, 0) -> d; (d, _) -> d + 1

type ShaderFormat = Vk.T.FormatR16g16b16a16Sfloat

type PshCnsts = '[Filter, Word32, Word32, Word32]

imgVwInfo :: Vk.Img.Binded sm si nm ifmt ->
	Vk.ImgVw.CreateInfo 'Nothing sm si nm ifmt ivfmt
imgVwInfo i = Vk.ImgVw.CreateInfo {
	Vk.ImgVw.createInfoNext = TMaybe.N,
	Vk.ImgVw.createInfoFlags = zeroBits,
	Vk.ImgVw.createInfoImage = i,
	Vk.ImgVw.createInfoViewType = Vk.ImgVw.Type2d,
	Vk.ImgVw.createInfoComponents = def,
	Vk.ImgVw.createInfoSubresourceRange = Vk.Img.SubresourceRange {
		Vk.Img.subresourceRangeAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.subresourceRangeBaseMipLevel = 0,
		Vk.Img.subresourceRangeLevelCount = Vk.remainingMipLevels,
		Vk.Img.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.subresourceRangeLayerCount = Vk.remainingArrayLayers } }

strImgBinding :: Vk.DscStLyt.Binding ('Vk.DscStLyt.Image iargs)
strImgBinding = Vk.DscStLyt.BindingImage {
	Vk.DscStLyt.bindingImageDescriptorType = Vk.Dsc.TypeStorageImage,
	Vk.DscStLyt.bindingImageStageFlags = Vk.ShaderStageComputeBit }

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
			Vk.Img.UsageSampledBit .|. Vk.Img.UsageStorageBit .|.
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
			Vk.Img.LayoutGeneral -> Vk.AccessTransferReadBit
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

createCmpPpl :: forall pctps pcrng sd bds a . (
	Vk.PshCnst.RangeListToMiddle pctps '[pcrng],
	Vk.DscStLyt.BindingListToMiddle bds ) =>
	Vk.Dvc.D sd -> HPList.PL Vk.DscStLyt.Binding bds ->
	SpirV.S GlslComputeShader -> (forall sds scppl spl .
		Vk.DscStLyt.D sds bds ->
		Vk.PplLyt.P spl '[ '(sds, bds)] pctps ->
		Vk.Ppl.Cp.C scppl '(spl, '[ '( sds, bds)], pctps) -> IO a) ->
	IO a
createCmpPpl d bds shdr f =
	createPplLyt @pctps @pcrng d bds \dsl pl ->
	Vk.Ppl.Cp.createCs d Nothing (HPList.Singleton . U4 $ info pl) nil
		\(HPList.Singleton p) -> f dsl pl p
	where
	info :: Vk.PplLyt.P sl sbtss pcw -> Vk.Ppl.Cp.CreateInfo 'Nothing
		'( 'Nothing, 'Nothing, 'GlslComputeShader, 'Nothing, '[])
		'(sl, sbtss, pcw) bpha
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
		Vk.Ppl.ShdrSt.createInfoModule = (shdrmd, nil),
		Vk.Ppl.ShdrSt.createInfoName = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfo = HPList.Nil }
	shdrmd = Vk.ShdrMd.CreateInfo {
		Vk.ShdrMd.createInfoNext = TMaybe.N,
		Vk.ShdrMd.createInfoFlags = zeroBits,
		Vk.ShdrMd.createInfoCode = shdr }

createPplLyt :: forall (pctps :: [Type]) (pcrng :: Vk.PshCnst.Range) sd a bds . (
	Vk.DscStLyt.BindingListToMiddle bds,
	Vk.PshCnst.RangeListToMiddle pctps '[pcrng] ) =>
	Vk.Dvc.D sd -> HPList.PL Vk.DscStLyt.Binding bds -> (forall sl sdsl .
		Vk.DscStLyt.D sdsl bds ->
		Vk.PplLyt.P sl '[ '(sdsl, bds)] pctps -> IO a) -> IO a
createPplLyt dv bds f = createDscStLyt dv bds \dsl ->
	Vk.PplLyt.create dv (info dsl) nil $ f dsl
	where
	info :: Vk.DscStLyt.D sdsl bds ->
		Vk.PplLyt.CreateInfo 'Nothing
			'[ '(sdsl, bds)] ('Vk.PshCnst.Layout pctps '[pcrng])
	info dsl = Vk.PplLyt.CreateInfo {
		Vk.PplLyt.createInfoNext = TMaybe.N,
		Vk.PplLyt.createInfoFlags = zeroBits,
		Vk.PplLyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }

createDscStLyt :: Vk.DscStLyt.BindingListToMiddle arg =>
	Vk.Dvc.D sd -> HPList.PL Vk.DscStLyt.Binding arg ->
	(forall (s :: Type) . Vk.DscStLyt.D s arg -> IO a) -> IO a
createDscStLyt dv bds = Vk.DscStLyt.create dv info nil
	where info = Vk.DscStLyt.CreateInfo {
		Vk.DscStLyt.createInfoNext = TMaybe.N,
		Vk.DscStLyt.createInfoFlags = zeroBits,
		Vk.DscStLyt.createInfoBindings = bds }

createDscPl :: Vk.Dvc.D sd -> (forall sp . Vk.DscPl.P sp -> IO a) -> IO a
createDscPl dv = Vk.DscPl.create dv info nil
	where
	info = Vk.DscPl.CreateInfo {
		Vk.DscPl.createInfoNext = TMaybe.N,
		Vk.DscPl.createInfoFlags = Vk.DscPl.CreateFreeDescriptorSetBit,
		Vk.DscPl.createInfoMaxSets = 1,
		Vk.DscPl.createInfoPoolSizes = [sz] }
	sz = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeStorageImage,
		Vk.DscPl.sizeDescriptorCount = 2 }

createDscSt ::
	Vk.Dvc.D sd -> Vk.DscPl.P sp ->
	Vk.ImgVw.I "source_image" ShaderFormat sivs ->
	Vk.ImgVw.I "destination_image" ShaderFormat sivd ->
	Vk.DscStLyt.D sdsl '[SrcImg, DstImg] ->
	(forall sds . Vk.DscSt.D sds '(sdsl, '[SrcImg, DstImg]) -> IO a) -> IO a
createDscSt dv dp svw dvw dl a =
	Vk.DscSt.allocateDs dv info \(HPList.Singleton ds) -> (>> a ds)
	$ Vk.DscSt.updateDs dv
		(U5 (dscWrite ds svw) :** U5 (dscWrite ds dvw) :** HPList.Nil)
		HPList.Nil
	where info = Vk.DscSt.AllocateInfo {
		Vk.DscSt.allocateInfoNext = TMaybe.N,
		Vk.DscSt.allocateInfoDescriptorPool = dp,
		Vk.DscSt.allocateInfoSetLayouts = HPList.Singleton $ U2 dl }

type SrcImg = 'Vk.DscStLyt.Image '[ '("source_image", ShaderFormat)]
type DstImg = 'Vk.DscStLyt.Image '[ '("destination_image", ShaderFormat)]

dscWrite :: Vk.DscSt.D sds slbts -> Vk.ImgVw.I nm fmt si ->
	Vk.DscSt.Write 'Nothing sds slbts
		('Vk.DscSt.WriteSourcesArgImage '[ '(ss, nm, fmt, si) ]) 0
dscWrite ds v = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N, Vk.DscSt.writeDstSet = ds,
	Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeStorageImage,
	Vk.DscSt.writeSources =
		Vk.DscSt.ImageInfos . HPList.Singleton $ U4 Vk.Dsc.ImageInfo {
			Vk.Dsc.imageInfoImageLayout = Vk.Img.LayoutGeneral,
			Vk.Dsc.imageInfoImageView = v,
			Vk.Dsc.imageInfoSampler = Vk.Smplr.Null } }

[glslComputeShader|

#version 460

#define Nearest 0
#define Linear 1

layout (local_size_x = 16, local_size_y = 16) in;

layout(rgba16f,set = 0, binding = 0) uniform image2D simg;
layout(rgba16f,set = 0, binding = 1) uniform image2D dimg;

layout(push_constant) uniform P { uint fltr; uint n; uint ix; uint iy; } p;

float
formula_n(float x)
{
	if (x < 0.5) return 1; else return 0;
}

float
formula_l(float x)
{
	return 1 - x;
}

float[2]
coefficients(float x)
{
	float co[2];
	float d = fract(x);
	switch (p.fltr) {
	case Nearest:
		co[0] = formula_n(d); co[1] = formula_n(1 - d);
		break;
	case Linear:
		co[0] = formula_l(d); co[1] = formula_l(1 - d);
		break;
	}
	return co;
}

vec4[2][2]
points(ivec2 p)
{
	vec4 c[2][2];

	for (int y = 0; y < 2; y++)
		for (int x = 0; x < 2; x++)
			c[y][x] = imageLoad(simg, ivec2(p.x + x, p.y + y));
	return c;
}

void
main()
{
	ivec2 size = imageSize(dimg);
	ivec2 coord = ivec2(gl_GlobalInvocationID.xy);

	float n, ix, iy;
	n = float(p.n); ix = float(p.ix); iy = float(p.iy);

	vec2 pos = vec2(
		float(size.x - 1) * ix / n + float(coord.x) / n,
		float(size.y - 1) * iy / n + float(coord.y) / n);

	float cox[2] = coefficients(pos.x);
	float coy[2] = coefficients(pos.y);

	vec4 c4[2][2] = points(ivec2(floor(pos.x), floor(pos.y)));

	vec4 c = vec4(0.0, 0.0, 0.0, 0.0);
	for (int y = 0; y < 2; y++)
		for (int x = 0; x < 2; x++)
			c += cox[x] * coy[y] * c4[y][x];

	if (coord.x < size.x && coord.y < size.y) imageStore(dimg, coord, c);
}

|]
