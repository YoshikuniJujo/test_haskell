{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Control.Monad
import Control.Monad.Fix
import Control.Concurrent.STM
import Control.Exception
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.Ord.ToolsYj
import Data.Bits
import Data.Bits.ToolsYj
import Data.Default
import Data.Maybe
import Data.Maybe.ToolsYj
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.List.ToolsYj
import Data.HeteroParList (pattern (:**), pattern (:*), pattern (:*.))
import Data.HeteroParList qualified as HPList
import Data.HeteroParList.Constrained (pattern (:^*))
import Data.HeteroParList.Constrained qualified as HPListC
import Data.Array
import Data.Bool
import Data.Bool.ToolsYj
import Data.Word
import Data.Int
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Text.Read
import System.Environment
import Codec.Picture

import Language.SpirV qualified as SpirV
import Language.SpirV.ShaderKind
import Language.SpirV.Shaderc qualified as Shaderc

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Exception qualified as Vk
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

import Gpu.Vulkan.Semaphore qualified as Vk.Smph

import Gpu.Vulkan.Khr.Swapchain qualified as Vk.Swpch
import Gpu.Vulkan.Khr.Surface qualified as Vk.Sfc
import Gpu.Vulkan.Khr.Surface.PhysicalDevice qualified as Vk.Sfc.Phd
import Gpu.Vulkan.Khr.Surface.Glfw.Window qualified as Vk.Sfc.Glfw.Win

import Graphics.UI.GlfwG qualified as GlfwG
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win
import Graphics.UI.GlfwG.Key qualified as GlfwG.K

import Paths_zenn_vulkan_bicubic_swapchain_v0

---------------------------------------------------------------------------
--
-- * DATA TYPE IMAGE RGBA8
-- * MAIN
-- * KEY EVENTS
-- * BUFFER AND IMAGE
-- * COMMAND BUFFER
-- * COMMANDS
-- * PIPELINE AND DESCRIPTOR SET
-- * SWAP CHAIN
-- * TOOLS
--
---------------------------------------------------------------------------

-- DATA TYPE IMAGE RGBA8

newtype ImageRgba8 = ImageRgba8 (Image PixelRGBA8)
newtype PixelRgba8 = PixelRgba8 PixelRGBA8 deriving Show

instance Vk.ObjB.IsImage ImageRgba8 where
	type ImagePixel ImageRgba8 = PixelRgba8
	type ImageFormat ImageRgba8 = 'Vk.T.FormatR8g8b8a8Srgb
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
	[ifp, ofp, getFilter -> Just flt, readMaybe -> Just a,
		readMaybe -> Just n, readMaybe -> Just i] -> do
		img <- either error convertRGBA8 <$> readImage ifp
		ImageRgba8 img' <- realMain (ImageRgba8 img) flt a n i
		writePng ofp img'
	_ -> error "Invalid command line arguments"

getFilter :: String -> Maybe Filter
getFilter = \case
	"nearest" -> Just Nearest; "linear" -> Just Linear;
	"cubic" -> Just Cubic; _ -> Nothing

newtype Filter = Filter Word32 deriving (Show, Storable)
pattern Nearest, Linear, Cubic :: Filter
pattern Nearest = Filter 0; pattern Linear = Filter 1; pattern Cubic = Filter 2

realMain :: ImageRgba8 -> Filter -> Float -> Int32 -> Int32 -> IO ImageRgba8
realMain img flt a n i = GlfwG.init error $
	createIst \ist -> pickPhd ist >>= \(pd, qfi) ->
	createLgDvc pd qfi \dv -> Vk.Dvc.getQueue dv qfi 0 >>= \gq ->
	createCmdPl qfi dv \cp -> body ist pd dv gq cp img flt a n i

createIst :: (forall si . Vk.Ist.I si -> IO a) -> IO a
createIst a = do
	es <- (Vk.Ist.ExtensionName <$>) <$> GlfwG.getRequiredInstanceExtensions
	Vk.Ist.create (info es) nil a
	where
	info :: [Vk.Ist.ExtensionName] -> Vk.Ist.CreateInfo 'Nothing 'Nothing
	info es = def {
		Vk.Ist.createInfoApplicationInfo = Just ainfo,
		Vk.Ist.createInfoEnabledLayerNames = vldLayers,
		Vk.Ist.createInfoEnabledExtensionNames = es }
	ainfo = Vk.ApplicationInfo {
		Vk.applicationInfoNext = TMaybe.N,
		Vk.applicationInfoApplicationName = "BICUBIC INTERPOLATION",
		Vk.applicationInfoApplicationVersion =
			Vk.makeApiVersion 0 1 0 0,
		Vk.applicationInfoEngineName = "No Engine",
		Vk.applicationInfoEngineVersion = Vk.makeApiVersion 0 1 0 0,
		Vk.applicationInfoApiVersion = Vk.apiVersion_1_3 }

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
createLgDvc pd qfi = Vk.Dvc.create pd info nil where
	info = Vk.Dvc.CreateInfo {
		Vk.Dvc.createInfoNext =
			TMaybe.J (Vk.Phd.vulkan13FeaturesZero TMaybe.N) {
				Vk.Phd.vulkan13FeaturesSynchronization2 = True
				},
		Vk.Dvc.createInfoFlags = zeroBits,
		Vk.Dvc.createInfoQueueCreateInfos = HPList.Singleton qinfo,
		Vk.Dvc.createInfoEnabledLayerNames = vldLayers,
		Vk.Dvc.createInfoEnabledExtensionNames =
			[Vk.Swpch.extensionName],
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
		Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
		Vk.CmdPl.createInfoQueueFamilyIndex = qfi }

type ShaderFormat = Vk.T.FormatR16g16b16a16Sfloat

body :: forall si sd scp img . Vk.ObjB.IsImage img =>
	Vk.Ist.I si -> Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q ->
	Vk.CmdPl.C scp -> img -> Filter -> Float -> Int32 -> Int32 -> IO img
body ist pd dv gq cp img f0 a0 (fromIntegral -> n0) i =
	resultBffr @img pd dv w h \rb ->
	prepareImg @(Vk.ObjB.ImageFormat img) pd dv trsd w h \imgd ->
	prepareImg @ShaderFormat pd dv sts w h \imgd' ->
	prepareImg @ShaderFormat pd dv std (w + 2) (h + 2) \imgs' ->
	Vk.ImgVw.create @_ @ShaderFormat dv (imgVwInfo imgd') nil \imgvwd' ->
	Vk.ImgVw.create @_ @ShaderFormat dv (imgVwInfo imgs') nil \imgvws' ->
	prepareImg pd dv trsd w h \imgs ->
	createBffrImg @img pd dv Vk.Bffr.UsageTransferSrcBit w h
		\(b :: Vk.Bffr.Binded sm sb nm '[o]) bm ->
	Vk.Mm.write @nm @o @0 dv bm zeroBits [img] >>

	compileShader "shader/expandWidth.comp" >>= \exws ->
	createCmpPpl @'[] @'[]
		dv (HPList.Singleton strgImgBinding) exws \wdsl wpl wppl ->
	createDscPl dv \wdp -> createDscStSrc dv wdp imgvws' wdsl \wds ->

	compileShader "shader/expandHeight.comp" >>= \exhs ->
	createCmpPpl @'[] @'[]
		dv (HPList.Singleton strgImgBinding) exhs \hdsl hpl hppl ->
	createDscPl dv \hdp -> createDscStSrc dv hdp imgvws' hdsl \hds ->

	compileShader "shader/interpolate.comp" >>= \shdr ->
	createCmpPpl @PshCnsts
		@'[ 'Vk.PshCnst.Range '[ 'Vk.T.ShaderStageComputeBit] PshCnsts]
		dv (strgImgBinding :** strgImgBinding :** HPList.Nil) shdr
		\dsl pl ppl ->
	createDscPl dv \dp -> createDscSt dv dp imgvws' imgvwd' dsl \ds ->

	allocateCmdBffr dv cp \cb -> do
	runCmds gq cb HPList.Nil HPList.Nil do
		tr cb imgs
			Vk.Img.LayoutUndefined Vk.Img.LayoutTransferDstOptimal
		copyBffrToImg cb b imgs
		tr cb imgs
			Vk.Img.LayoutTransferDstOptimal
			Vk.Img.LayoutTransferSrcOptimal
		tr cb imgs'
			Vk.Img.LayoutUndefined Vk.Img.LayoutTransferDstOptimal
		copyImgToImg cb imgs imgs' w h 1 1
		tr cb imgs' Vk.Img.LayoutTransferDstOptimal Vk.Img.LayoutGeneral
		Vk.Cmd.bindPipelineCompute
				cb Vk.Ppl.BindPointCompute wppl \ccb -> do
			Vk.Cmd.bindDescriptorSetsCompute
				ccb wpl (HPList.Singleton $ U2 wds) def
			Vk.Cmd.dispatch ccb 1 ((h + 2) `div'` 16) 1
		Vk.Cmd.bindPipelineCompute
				cb Vk.Ppl.BindPointCompute hppl \ccb -> do
			Vk.Cmd.bindDescriptorSetsCompute
				ccb hpl (HPList.Singleton $ U2 hds) def
			Vk.Cmd.dispatch ccb ((w + 2) `div'` 16) 1 1

	withWindow w h \win -> Vk.Sfc.Glfw.Win.create ist win nil \sf ->
		createSwpch win sf pd dv \sc ->
		Vk.Smph.create @'Nothing dv def nil \scs ->
		Vk.Smph.create @'Nothing dv def nil \drs -> do
		let	wi = smphInfo scs Vk.Ppl.Stage2ColorAttachmentOutputBit
			si = smphInfo drs Vk.Ppl.Stage2AllGraphicsBit
		cky <- atomically newTChan
		GlfwG.Win.setKeyCallback win . Just $ kCllbck cky
		scis <- Vk.Swpch.getImages dv sc
		($ (f0, a0, n0, x0, y0)) . unc5 $ fix \act f a n ix iy -> do
			ii <- Vk.Swpch.acquireNextImage
				dv sc Nothing (Just scs) Nothing
			draw gq cb wi si ppl pl ds w h imgd' scis ii f a n ix iy
			catchAndSerialize $ Vk.Swpch.queuePresent
				@'Nothing gq $ pinfo sc ii drs
			Vk.Q.waitIdle gq
			GlfwG.waitEvents
			wsc <- GlfwG.Win.shouldClose win
			mk <- atomically $ tryReadTChan cky
			case (wsc, uncurry (procKey w h f a n ix iy) <$> mk) of
				(True, _) -> end n ix iy
				(_, Nothing) -> act f a n ix iy
				(_, Just Nothing) -> end n ix iy
				(_, Just (Just args@(_, a', _, _, _))) ->
					when (a /= a') (bar a') >> unc5 act args

	runCmds gq cb HPList.Nil HPList.Nil do
		tr cb imgd
			Vk.Img.LayoutUndefined Vk.Img.LayoutTransferDstOptimal
		copyImgToImg cb imgd' imgd w h 0 0
		tr cb imgd
			Vk.Img.LayoutTransferDstOptimal
			Vk.Img.LayoutTransferSrcOptimal
		copyImgToBffr cb imgd rb
	where
	trsd = Vk.Img.UsageTransferSrcBit .|. Vk.Img.UsageTransferDstBit
	sts = Vk.Img.UsageStorageBit .|. Vk.Img.UsageTransferSrcBit
	std = Vk.Img.UsageStorageBit .|. Vk.Img.UsageTransferDstBit
	w, h :: Integral n => n
	w = fromIntegral $ Vk.ObjB.imageWidth img
	h = fromIntegral $ Vk.ObjB.imageHeight img
	tr = transitionImgLyt
	x0, y0 :: Word32
	x0 = fromIntegral i `mod` n0
	y0 = fromIntegral i `div` n0
	pinfo sc ii drs = Vk.Swpch.PresentInfo {
		Vk.Swpch.presentInfoNext = TMaybe.N,
		Vk.Swpch.presentInfoWaitSemaphores = HPList.Singleton drs,
		Vk.Swpch.presentInfoSwapchainImageIndices = HPList.Singleton
			$ Vk.Swpch.SwapchainImageIndex sc ii }
	bar a' = do putStrLn ""; putStrLn (barString a'); print a'
	end n ix iy = print (n, n * iy + ix)

resultBffr :: Vk.ObjB.IsImage img =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Dvc.Size -> Vk.Dvc.Size -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[Vk.ObjNA.Image img nmi] -> IO a) ->
	IO img
resultBffr pd dv w h f = head <$>
	createBffrImg pd dv Vk.Bffr.UsageTransferDstBit w h
		\(b :: Vk.Bffr.Binded sm sb nm '[o]) m ->
	f b >> Vk.Mm.read @nm @o @0 dv m zeroBits

imgVwInfo :: Vk.Img.Binded sm si nm ifmt ->
	Vk.ImgVw.CreateInfo 'Nothing sm si nm ifmt vfmt
imgVwInfo i = Vk.ImgVw.CreateInfo {
	Vk.ImgVw.createInfoNext = TMaybe.N, Vk.ImgVw.createInfoFlags = zeroBits,
	Vk.ImgVw.createInfoImage = i,
	Vk.ImgVw.createInfoViewType = Vk.ImgVw.Type2d,
	Vk.ImgVw.createInfoComponents = def,
	Vk.ImgVw.createInfoSubresourceRange = Vk.Img.SubresourceRange {
		Vk.Img.subresourceRangeAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.subresourceRangeBaseMipLevel = 0,
		Vk.Img.subresourceRangeLevelCount = Vk.remainingMipLevels,
		Vk.Img.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.subresourceRangeLayerCount = Vk.remainingArrayLayers } }

type PshCnsts = '[Filter, Float, Word32, Word32, Word32]

strgImgBinding :: Vk.DscStLyt.Binding ('Vk.DscStLyt.Image iargs)
strgImgBinding = Vk.DscStLyt.BindingImage {
	Vk.DscStLyt.bindingImageDescriptorType = Vk.Dsc.TypeStorageImage,
	Vk.DscStLyt.bindingImageStageFlags = Vk.ShaderStageComputeBit }

withWindow :: Int -> Int -> (forall s . GlfwG.Win.W s -> IO a) -> IO a
withWindow w h a = do
	GlfwG.Win.hint `mapM_` [
		GlfwG.Win.WindowHint'ClientAPI GlfwG.Win.ClientAPI'NoAPI,
		GlfwG.Win.WindowHint'Resizable False ]
	GlfwG.Win.create w h "Bicubic Interpolation" Nothing Nothing \win -> do
		(ww, hw) <- GlfwG.Win.getSize win
		(wf, hf) <- GlfwG.Win.getFramebufferSize win
		GlfwG.Win.setSize win (w * ww `div` wf) (h * hw `div` hf)
		waitFramebufferSize win (== (w, h)) >> a win

waitFramebufferSize :: GlfwG.Win.W sw -> ((Int, Int) -> Bool) -> IO ()
waitFramebufferSize win p = GlfwG.Win.getFramebufferSize win >>= \sz ->
	when (not $ p sz) $ fix \go -> (`when` go) . not . p =<<
		GlfwG.waitEvents *> GlfwG.Win.getFramebufferSize win

draw :: (
	Vk.Smph.SubmitInfoListToMiddle wss,
	Vk.Smph.SubmitInfoListToMiddle sss ) => Vk.Q.Q -> Vk.CBffr.C scb ->
	HPList.PL (U2 Vk.Smph.SubmitInfo) wss ->
	HPList.PL (U2 Vk.Smph.SubmitInfo) sss ->
	Vk.Ppl.Cp.C scp '(sl, '[ '(sdsl, '[SrcImg, DstImg])], PshCnsts) ->
	Vk.PplLyt.P sl '[ '(sdsl, '[SrcImg, DstImg])] PshCnsts ->
	Vk.DscSt.D sds '(sdsl, '[SrcImg, DstImg]) ->
	(forall n . Integral n => n) -> (forall n . Integral n => n) ->
	Vk.Img.Binded smd sid nmd fmtd -> [Vk.Img.Binded sm si inm fmt] ->
	Word32 -> Filter -> Float -> Word32 -> Word32 -> Word32 -> IO ()
draw gq cb wi si ppl pl ds w h im scis ii flt a n ix iy = runCmds gq cb wi si do
	tr cb im Vk.Img.LayoutUndefined Vk.Img.LayoutGeneral
	Vk.Cmd.bindPipelineCompute cb Vk.Ppl.BindPointCompute ppl \ccb -> do
		Vk.Cmd.bindDescriptorSetsCompute
			ccb pl (HPList.Singleton $ U2 ds) def
		Vk.Cmd.pushConstantsCompute @'[ 'Vk.T.ShaderStageComputeBit]
			ccb pl (flt :* a :* n :* ix :* iy :* HPList.Nil)
		Vk.Cmd.dispatch ccb (w `div'` 16) (h `div'` 16) 1
	tr cb im Vk.Img.LayoutGeneral Vk.Img.LayoutTransferSrcOptimal
	tr cb sci Vk.Img.LayoutUndefined Vk.Img.LayoutTransferDstOptimal
	copyImgToImg cb im sci w h 0 0
	tr cb sci Vk.Img.LayoutTransferDstOptimal Vk.Img.LayoutPresentSrcKhr
	where tr = transitionImgLyt; sci = scis !! fromIntegral ii

catchAndSerialize :: IO () -> IO ()
catchAndSerialize =
	(`catch` \(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

barString :: Float -> String
barString a = "-1 " ++ take x ('|' : repeat '*') ++ "*" ++
	replicate y ' ' ++ "| 0\n" ++
	"   |" ++ replicate z ' ' ++ "|" ++ replicate w ' ' ++ "|" ++
	replicate v ' ' ++ "|"
	where
	ln :: Num n => n; ln = 70
	x = tr a; y = ln - x
	z = tr (- 0.75) - 1; w = tr (- 0.5) - z - 2; v = tr (- 0.25) - w - z - 3
	tr = round . (ln +) . (ln *)

-- KEY EVENTS

kCllbck :: TChan (K, PR) -> GlfwG.Win.KeyCallback sw
kCllbck cky _ ky _ ks _ = atomically case (keyToK ky, keyStateToPR ks) of
	(Just k, Just pr) -> writeTChan cky (k, pr); _ -> pure ()

data K = Q | N | Semicolon | H | J | K | L | U | I | M | Comma | D | F
	deriving Show

keyToK :: GlfwG.K.Key -> Maybe K
keyToK = \case
	GlfwG.K.Key'Q -> Just Q
	GlfwG.K.Key'N -> Just N; GlfwG.K.Key'Semicolon -> Just Semicolon
	GlfwG.K.Key'H -> Just H; GlfwG.K.Key'J -> Just J
	GlfwG.K.Key'K -> Just K; GlfwG.K.Key'L -> Just L
	GlfwG.K.Key'U -> Just U; GlfwG.K.Key'I -> Just I
	GlfwG.K.Key'M -> Just M; GlfwG.K.Key'Comma -> Just Comma
	GlfwG.K.Key'D -> Just D; GlfwG.K.Key'F -> Just F; _ -> Nothing

data PR = Pr | Rp deriving Show

keyStateToPR :: GlfwG.K.KeyState -> Maybe PR
keyStateToPR = \case
	GlfwG.K.KeyState'Pressed -> Just Pr
	GlfwG.K.KeyState'Repeating -> Just Rp; _ -> Nothing

procKey :: Word32 -> Word32 -> Filter -> Float -> Word32 -> Word32 -> Word32 ->
	K -> PR -> Maybe (Filter, Float, Word32, Word32, Word32)
procKey _ _ _ _ _ _ _ Q _ = Nothing
procKey _ _ _ a n ix iy N _ = Just (Nearest, a, n, ix, iy)
procKey _ _ _ a n ix iy Semicolon _ = Just (Linear, a, n, ix, iy)
procKey _ _ _ _ n ix iy M _ = Just (Cubic, - 0.75, n, ix, iy)
procKey _ _ _ _ n ix iy Comma _ = Just (Cubic, - 0.5, n, ix, iy)
procKey _ _ _ (clamp (- 1) (- 0.25) . subtract 0.01 -> a') n ix iy U Pr =
	Just (Cubic, a', n, ix, iy)
procKey _ _ _ a@(clamp (- 1) (- 0.25) . subtract 0.01 -> a') n ix iy U Rp =
	Just (Cubic, bool a' (- 0.75) (a' < - 0.75 && - 0.75 <= a), n, ix, iy)
procKey _ _ _ (clamp (- 1) (- 0.25) . (+ 0.01) -> a') n ix iy I Pr =
	Just (Cubic, a', n, ix, iy)
procKey _ _ _ a@(clamp (- 1) (- 0.25) . (+ 0.01) -> a') n ix iy I Rp =
	Just (Cubic, bool a' (- 0.5) (a <= - 0.5 && - 0.5 < a'), n, ix, iy)
procKey _ _ f a n ix iy H _ = Just (f, a, n, ix `sub'` 1, iy)
procKey _ _ f a n ix iy J _ = Just (f, a, n, ix, clamp 0 (n - 1) $ iy + 1)
procKey _ _ f a n ix iy K _ = Just (f, a, n, ix, iy `sub'` 1)
procKey _ _ f a n ix iy L _ = Just (f, a, n, clamp 0 (n - 1) $ ix + 1, iy)
procKey w h f a (clamp 1 (max w h) . (`sub'` 1)  -> n) ix iy D _ =
	Just (f, a, n, clamp 0 (n - 1) ix, clamp 0 (n - 1) iy)
procKey w h f a (clamp 1 (max w h) . (+ 1) -> n) ix iy F _ =
	Just (f, a, n, ix, iy)

-- BUFFER AND IMAGE

createBffrImg :: forall img sd bnm nm a . Vk.ObjB.IsImage img =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Bffr.UsageFlags ->
	Vk.Dvc.Size -> Vk.Dvc.Size -> (forall sm sb .
		Vk.Bffr.Binded sm sb bnm '[Vk.ObjNA.Image img nm] ->
		Vk.Mm.M sm '[ '(
			sb, 'Vk.Mm.BufferArg bnm '[Vk.ObjNA.Image img nm] )] ->
		IO a) -> IO a
createBffrImg pd dv us w h = createBffr pd dv (Vk.Obj.LengthImage w w h 1 1) us
	(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit)

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

bffrInfo :: Vk.Obj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Bffr.CreateInfo 'Nothing '[o]
bffrInfo ln us = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N, Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths = HPList.Singleton ln,
	Vk.Bffr.createInfoUsage = us,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

prepareImg :: forall fmt sd nm a . Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Img.UsageFlags -> Word32 -> Word32 ->
	(forall sm si . Vk.Img.Binded sm si nm fmt -> IO a) -> IO a
prepareImg pd dv us w h f = Vk.Img.create @'Nothing dv iinfo nil \i -> do
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
		Vk.Img.createInfoUsage = us,
		Vk.Img.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Img.createInfoQueueFamilyIndices = [],
		Vk.Img.createInfoInitialLayout = Vk.Img.LayoutUndefined }
	minfo mt = Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mt }

-- COMMAND BUFFER

allocateCmdBffr :: forall sd scp a .
	Vk.Dvc.D sd -> Vk.CmdPl.C scp ->
	(forall scb . Vk.CBffr.C scb -> IO a) -> IO a
allocateCmdBffr dv cp f = Vk.CBffr.allocateCs dv info \(b :*. HPList.Nil) -> f b
	where
	info :: Vk.CBffr.AllocateInfo 'Nothing scp '[ '()]
	info = Vk.CBffr.AllocateInfo {
		Vk.CBffr.allocateInfoNext = TMaybe.N,
		Vk.CBffr.allocateInfoCommandPool = cp,
		Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }

runCmds :: forall scb wss sss a . (
	Vk.Smph.SubmitInfoListToMiddle wss,
	Vk.Smph.SubmitInfoListToMiddle sss ) =>
	Vk.Q.Q -> Vk.CBffr.C scb ->
	HPList.PL (U2 Vk.Smph.SubmitInfo) wss ->
	HPList.PL (U2 Vk.Smph.SubmitInfo) sss -> IO a -> IO a
runCmds gq cb wss sss cmds =
	Vk.CBffr.begin @_ @'Nothing cb binfo cmds <* do
	Vk.Q.submit2 gq (HPList.Singleton . U4 $ submitInfo cb wss sss) Nothing
	Vk.Q.waitIdle gq
	where binfo = Vk.CBffr.BeginInfo {
		Vk.CBffr.beginInfoNext = TMaybe.N,
		Vk.CBffr.beginInfoFlags = Vk.CBffr.UsageOneTimeSubmitBit,
		Vk.CBffr.beginInfoInheritanceInfo = Nothing }

submitInfo :: Vk.CBffr.C scb ->
	HPList.PL (U2 Vk.Smph.SubmitInfo) wsas ->
	HPList.PL (U2 Vk.Smph.SubmitInfo) ssas ->
	Vk.SubmitInfo2 'Nothing wsas '[ '( 'Nothing, scb)] ssas
submitInfo cb wsis ssis = Vk.SubmitInfo2 {
	Vk.submitInfo2Next = TMaybe.N, Vk.submitInfo2Flags = zeroBits,
	Vk.submitInfo2WaitSemaphoreInfos = wsis,
	Vk.submitInfo2CommandBufferInfos = HPList.Singleton $ U2 cbi,
	Vk.submitInfo2SignalSemaphoreInfos = ssis }
	where cbi = Vk.CBffr.SubmitInfo {
		Vk.CBffr.submitInfoNext = TMaybe.N,
		Vk.CBffr.submitInfoCommandBuffer = cb,
		Vk.CBffr.submitInfoDeviceMask = def }

smphInfo ::
	Vk.Smph.S ss -> Vk.Ppl.StageFlags2 ->
	HPList.PL (U2 Vk.Smph.SubmitInfo) '[ '( 'Nothing, ss)]
smphInfo smph sm = HPList.Singleton $ U2 Vk.Smph.SubmitInfo {
	Vk.Smph.submitInfoNext = TMaybe.N,
	Vk.Smph.submitInfoSemaphore = smph, Vk.Smph.submitInfoValue = 0,
	Vk.Smph.submitInfoStageMask = sm, Vk.Smph.submitInfoDeviceIndex = 0 }

-- COMMANDS

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
transitionImgLyt cb i ol nl = Vk.Cmd.pipelineBarrier2 cb dinfo
	where
	dinfo = Vk.DependencyInfo {
		Vk.dependencyInfoNext = TMaybe.N,
		Vk.dependencyInfoDependencyFlags = zeroBits,
		Vk.dependencyInfoMemoryBarriers = HPList.Nil,
		Vk.dependencyInfoBufferMemoryBarriers = HPList.Nil,
		Vk.dependencyInfoImageMemoryBarriers =
			HPList.Singleton $ U5 ibrrr }
	ibrrr = Vk.Img.MemoryBarrier2 {
		Vk.Img.memoryBarrier2Next = TMaybe.N,
		Vk.Img.memoryBarrier2SrcStageMask = Vk.Ppl.Stage2AllCommandsBit,
		Vk.Img.memoryBarrier2SrcAccessMask = Vk.Access2MemoryWriteBit,
		Vk.Img.memoryBarrier2DstStageMask = Vk.Ppl.Stage2AllCommandsBit,
		Vk.Img.memoryBarrier2DstAccessMask =
			Vk.Access2MemoryWriteBit .|. Vk.Access2MemoryReadBit,
		Vk.Img.memoryBarrier2OldLayout = ol,
		Vk.Img.memoryBarrier2NewLayout = nl,
		Vk.Img.memoryBarrier2SrcQueueFamilyIndex = Vk.QFam.Ignored,
		Vk.Img.memoryBarrier2DstQueueFamilyIndex = Vk.QFam.Ignored,
		Vk.Img.memoryBarrier2Image = i,
		Vk.Img.memoryBarrier2SubresourceRange = isr }
	isr = Vk.Img.SubresourceRange {
		Vk.Img.subresourceRangeAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.subresourceRangeBaseMipLevel = 0,
		Vk.Img.subresourceRangeLevelCount = Vk.remainingMipLevels,
		Vk.Img.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.subresourceRangeLayerCount = Vk.remainingArrayLayers }

copyImgToImg :: Vk.CBffr.C scb ->
	Vk.Img.Binded sms sis nms fmts -> Vk.Img.Binded smd sid nmd fmtd ->
	Int32 -> Int32 -> Int32 -> Int32 -> IO ()
copyImgToImg cb si di w h dl dt = Vk.Cmd.blitImage2 cb info
	where
	info = Vk.BlitImageInfo2 {
		Vk.blitImageInfo2Next = TMaybe.N,
		Vk.blitImageInfo2SrcImage = si,
		Vk.blitImageInfo2SrcImageLayout =
			Vk.Img.LayoutTransferSrcOptimal,
		Vk.blitImageInfo2DstImage = di,
		Vk.blitImageInfo2DstImageLayout =
			Vk.Img.LayoutTransferDstOptimal,
		Vk.blitImageInfo2Regions = blt,
		Vk.blitImageInfo2Filter = Vk.FilterNearest }
	blt = HPList.Singleton Vk.Img.Blit2 {
		Vk.Img.blit2Next = TMaybe.N,
		Vk.Img.blit2SrcSubresource = colorLayer0,
		Vk.Img.blit2SrcOffsetFrom = Vk.Offset3d 0 0 0,
		Vk.Img.blit2SrcOffsetTo = Vk.Offset3d w h 1,
		Vk.Img.blit2DstSubresource = colorLayer0,
		Vk.Img.blit2DstOffsetFrom = Vk.Offset3d dl dt 0,
		Vk.Img.blit2DstOffsetTo = Vk.Offset3d (w + dl) (h + dt) 1 }

copyImgToBffr :: forall scb img smi si inm smb sbb bnm imgnm .
	Storable (Vk.ObjB.ImagePixel img) => Vk.CBffr.C scb ->
	Vk.Img.Binded smi si inm (Vk.ObjB.ImageFormat img) ->
	Vk.Bffr.Binded smb sbb bnm '[Vk.ObjNA.Image img imgnm] -> IO ()
copyImgToBffr cb i b@(bffrImgExtent -> (w, h)) =
	Vk.Cmd.copyImageToBuffer
		@1 @img @'[imgnm] cb i Vk.Img.LayoutTransferSrcOptimal b
		$ HPList.Singleton Vk.Bffr.ImageCopy {
			Vk.Bffr.imageCopyImageSubresource = colorLayer0,
			Vk.Bffr.imageCopyImageOffset = Vk.Offset3d 0 0 0,
			Vk.Bffr.imageCopyImageExtent = Vk.Extent3d w h 1 }

-- PIPELINE AND DESCRIPTOR SET

createCmpPpl :: forall pcts pcrng sd bds a . (
	Vk.PshCnst.RangeListToMiddle pcts pcrng,
	Vk.DscStLyt.BindingListToMiddle bds ) =>
	Vk.Dvc.D sd -> HPList.PL Vk.DscStLyt.Binding bds ->
	SpirV.S GlslComputeShader -> (forall sds scppl spl .
		Vk.DscStLyt.D sds bds -> Vk.PplLyt.P spl '[ '(sds, bds)] pcts ->
		Vk.Ppl.Cp.C scppl '(spl, '[ '(sds, bds)], pcts) -> IO a) -> IO a
createCmpPpl d bds shdr f =
	createPplLyt @pcts @pcrng d bds \dsl pl ->
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
		Vk.Ppl.ShdrSt.createInfoModule = (shdrmd, nil),
		Vk.Ppl.ShdrSt.createInfoName = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfo = HPList.Nil }
	shdrmd = Vk.ShdrMd.CreateInfo {
		Vk.ShdrMd.createInfoNext = TMaybe.N,
		Vk.ShdrMd.createInfoFlags = zeroBits,
		Vk.ShdrMd.createInfoCode = shdr }

createPplLyt :: forall pcts pcrng sd a bds . (
	Vk.DscStLyt.BindingListToMiddle bds,
	Vk.PshCnst.RangeListToMiddle pcts pcrng ) =>
	Vk.Dvc.D sd -> HPList.PL Vk.DscStLyt.Binding bds -> (forall sl sdsl .
		Vk.DscStLyt.D sdsl bds ->
		Vk.PplLyt.P sl '[ '(sdsl, bds)] pcts -> IO a) -> IO a
createPplLyt dv bds f = createDscStLyt dv bds \dsl ->
	Vk.PplLyt.create dv (info dsl) nil $ f dsl
	where
	info :: Vk.DscStLyt.D sdsl bds -> Vk.PplLyt.CreateInfo 'Nothing
		'[ '(sdsl, bds)] ('Vk.PshCnst.Layout pcts pcrng)
	info dsl = Vk.PplLyt.CreateInfo {
		Vk.PplLyt.createInfoNext = TMaybe.N,
		Vk.PplLyt.createInfoFlags = zeroBits,
		Vk.PplLyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }

createDscStLyt :: Vk.DscStLyt.BindingListToMiddle bts =>
	Vk.Dvc.D sd -> HPList.PL Vk.DscStLyt.Binding bts ->
	(forall sdsl . Vk.DscStLyt.D sdsl bts -> IO a) -> IO a
createDscStLyt dv bds = Vk.DscStLyt.create dv info nil
	where info = Vk.DscStLyt.CreateInfo {
		Vk.DscStLyt.createInfoNext = TMaybe.N,
		Vk.DscStLyt.createInfoFlags = zeroBits,
		Vk.DscStLyt.createInfoBindings = bds }

createDscPl :: Vk.Dvc.D sd -> (forall sdp . Vk.DscPl.P sdp -> IO a) -> IO a
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
	Vk.Dvc.D sd -> Vk.DscPl.P sdp ->
	Vk.ImgVw.I SrcImgNm ShaderFormat sivs ->
	Vk.ImgVw.I DstImgNm ShaderFormat sivd ->
	Vk.DscStLyt.D sdsl '[SrcImg, DstImg] ->
	(forall sds . Vk.DscSt.D sds '(sdsl, '[SrcImg, DstImg]) -> IO a) -> IO a
createDscSt dv dp vs vd dl a =
	Vk.DscSt.allocateDs dv info \(HPList.Singleton ds) ->
	(>> a ds) $ Vk.DscSt.updateDs
		dv (U5 (dscWrite ds vs) :** U5 (dscWrite ds vd) :** HPList.Nil)
		HPList.Nil
	where info = Vk.DscSt.AllocateInfo {
		Vk.DscSt.allocateInfoNext = TMaybe.N,
		Vk.DscSt.allocateInfoDescriptorPool = dp,
		Vk.DscSt.allocateInfoSetLayouts = HPList.Singleton $ U2 dl }

createDscStSrc ::
	Vk.Dvc.D sd -> Vk.DscPl.P sp ->
	Vk.ImgVw.I SrcImgNm ShaderFormat sivs ->
	Vk.DscStLyt.D sdsl '[SrcImg] ->
	(forall sds . Vk.DscSt.D sds '(sdsl, '[SrcImg]) -> IO a) -> IO a
createDscStSrc dv dp vs dl a =
	Vk.DscSt.allocateDs dv info \(HPList.Singleton ds) ->
	(>> a ds) $ Vk.DscSt.updateDs
		dv (HPList.Singleton . U5 $ dscWrite ds vs) HPList.Nil
	where info = Vk.DscSt.AllocateInfo {
		Vk.DscSt.allocateInfoNext = TMaybe.N,
		Vk.DscSt.allocateInfoDescriptorPool = dp,
		Vk.DscSt.allocateInfoSetLayouts = HPList.Singleton $ U2 dl }

type SrcImg = 'Vk.DscStLyt.Image '[ '(SrcImgNm, ShaderFormat)]
type DstImg = 'Vk.DscStLyt.Image '[ '(DstImgNm, ShaderFormat)]
type SrcImgNm = "source_image"; type DstImgNm = "destination_image"

dscWrite :: Vk.DscSt.D sds slbts -> Vk.ImgVw.I nm fmt si ->
	Vk.DscSt.Write 'Nothing sds slbts
		('Vk.DscSt.WriteSourcesArgImage '[ '(ss, nm, fmt, si)]) 0
dscWrite ds v = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N, Vk.DscSt.writeDstSet = ds,
	Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeStorageImage,
	Vk.DscSt.writeSources =
		Vk.DscSt.ImageInfos . HPList.Singleton $ U4 Vk.Dsc.ImageInfo {
			Vk.Dsc.imageInfoImageLayout = Vk.Img.LayoutGeneral,
			Vk.Dsc.imageInfoImageView = v,
			Vk.Dsc.imageInfoSampler = Vk.Smplr.Null } }

compileShader :: FilePath -> IO (SpirV.S GlslComputeShader)
compileShader fp = do
	cd <- BS.readFile =<< getDataFileName fp
	Shaderc.compile @() cd (BSC.pack fp) "main" def

-- SWAP CHAIN

createSwpch :: GlfwG.Win.W sw -> Vk.Sfc.S ssfc -> Vk.Phd.P -> Vk.Dvc.D sd ->
	(forall ss scfmt .
		Vk.T.FormatToValue scfmt => Vk.Swpch.S scfmt ss -> IO a) -> IO a
createSwpch win sfc pd dv f = querySwpchSupport pd sfc \ss -> do
	ex <- swpchExtent win $ capabilities ss
	let	cps = capabilities ss
		pm = findDefault Vk.Sfc.PresentModeFifo
			(== Vk.Sfc.PresentModeMailbox) $ presentModes ss
	chooseSwpSfcFmt (formats ss) \(Vk.Sfc.Format sc :: Vk.Sfc.Format fmt) ->
		Vk.Swpch.create @_ @fmt dv (swpchInfo sfc cps sc pm ex) nil f

querySwpchSupport :: Vk.Phd.P -> Vk.Sfc.S ss -> (forall fmts .
	Show (HPListC.PL Vk.T.FormatToValue Vk.Sfc.Format fmts) =>
	SwpchSupportDetails fmts -> IO a) -> IO a
querySwpchSupport pd sfc f = Vk.Sfc.Phd.getFormats pd sfc \fmts ->
	f =<< SwpchSupportDetails
		<$> Vk.Sfc.Phd.getCapabilities pd sfc
		<*> ((, fmts) <$> Vk.Sfc.Phd.getFormatsFiltered pd sfc)
		<*> Vk.Sfc.Phd.getPresentModes pd sfc

data SwpchSupportDetails fmts = SwpchSupportDetails {
	capabilities :: Vk.Sfc.Capabilities,
	formats :: (
		[Vk.Sfc.Format Vk.T.FormatB8g8r8a8Srgb],
		HPListC.PL Vk.T.FormatToValue Vk.Sfc.Format fmts ),
	presentModes :: [Vk.Sfc.PresentMode] }

deriving instance
	Show (HPListC.PL Vk.T.FormatToValue Vk.Sfc.Format fmts) =>
	Show (SwpchSupportDetails fmts)

swpchExtent :: GlfwG.Win.W sw -> Vk.Sfc.Capabilities -> IO Vk.Extent2d
swpchExtent win cps
	| Vk.extent2dWidth cur /= maxBound = pure cur
	| otherwise = (<$> GlfwG.Win.getFramebufferSize win)
		\(fromIntegral -> w, fromIntegral -> h) ->
		Vk.Extent2d
			(clamp (Vk.extent2dWidth n) (Vk.extent2dWidth x) w)
			(clamp (Vk.extent2dHeight n) (Vk.extent2dHeight x) h)
	where
	cur = Vk.Sfc.capabilitiesCurrentExtent cps
	n = Vk.Sfc.capabilitiesMinImageExtent cps
	x = Vk.Sfc.capabilitiesMaxImageExtent cps

chooseSwpSfcFmt :: (
	[Vk.Sfc.Format Vk.T.FormatB8g8r8a8Srgb],
	HPListC.PL Vk.T.FormatToValue Vk.Sfc.Format fmts ) ->
	(forall fmt . Vk.T.FormatToValue fmt => Vk.Sfc.Format fmt -> a) -> a
chooseSwpSfcFmt (fmts, (fmt0 :^* _)) f = maybe (f fmt0) f $ (`L.find` fmts)
	$ (== Vk.Sfc.ColorSpaceSrgbNonlinear) . Vk.Sfc.formatColorSpace
chooseSwpSfcFmt (_, HPListC.Nil) _ = error "no available swap surface formats"

swpchInfo :: forall fmt ss .
	Vk.Sfc.S ss -> Vk.Sfc.Capabilities -> Vk.Sfc.ColorSpace ->
	Vk.Sfc.PresentMode -> Vk.Extent2d ->
	Vk.Swpch.CreateInfo 'Nothing ss fmt
swpchInfo sfc cps cs pm ex = Vk.Swpch.CreateInfo {
	Vk.Swpch.createInfoNext = TMaybe.N, Vk.Swpch.createInfoFlags = zeroBits,
	Vk.Swpch.createInfoSurface = sfc,
	Vk.Swpch.createInfoMinImageCount = imgc,
	Vk.Swpch.createInfoImageColorSpace = cs,
	Vk.Swpch.createInfoImageExtent = ex,
	Vk.Swpch.createInfoImageArrayLayers = 1,
	Vk.Swpch.createInfoImageUsage = Vk.Img.UsageTransferDstBit,
	Vk.Swpch.createInfoImageSharingMode = Vk.SharingModeExclusive,
	Vk.Swpch.createInfoQueueFamilyIndices = [],
	Vk.Swpch.createInfoPreTransform =
		Vk.Sfc.capabilitiesCurrentTransform cps,
	Vk.Swpch.createInfoCompositeAlpha = Vk.Sfc.CompositeAlphaOpaqueBit,
	Vk.Swpch.createInfoPresentMode = pm,
	Vk.Swpch.createInfoClipped = True,
	Vk.Swpch.createInfoOldSwapchain = Nothing }
	where
	imgc = clamp 0 imgcx (Vk.Sfc.capabilitiesMinImageCount cps + 1)
	imgcx = fromMaybe maxBound
		. onlyIf (> 0) $ Vk.Sfc.capabilitiesMaxImageCount cps

-- TOOLS

unc5 :: (a -> b -> c -> d -> e -> r) -> (a, b, c, d, e) -> r
unc5 f (x, y, z, w, v) = f x y z w v

sub' :: (Ord n, Num n) => n -> n -> n
x `sub'` y | x >= y = x - y | otherwise = 0

div' :: Integral n => n -> n -> n
x `div'` y = case x `divMod` y of (d, 0) -> d; (d, _) -> d + 1
