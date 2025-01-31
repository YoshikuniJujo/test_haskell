{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Concurrent
import Control.Exception
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.Ord.ToolsYj
import Data.Bits
import Data.Bits.ToolsYj
import Data.Default
import Data.Tuple.ToolsYj
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
import Data.IORef
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

import Graphics.UI.GlfwG qualified as GlfwG
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win
import Graphics.UI.GlfwG.Key qualified as GlfwG.Key

import Gpu.Vulkan.Khr.Surface qualified as Vk.Khr.Sfc
import Gpu.Vulkan.Khr.Surface.PhysicalDevice qualified as Vk.Khr.Sfc.Phd
import Gpu.Vulkan.Khr.Surface.Glfw.Window qualified as Vk.Khr.Sfc.Glfw.Win
import Gpu.Vulkan.Khr.Swapchain qualified as Vk.Khr.Swpch
import Gpu.Vulkan.Fence qualified as Vk.Fnc
import Gpu.Vulkan.Semaphore qualified as Vk.Smph

import Control.Concurrent.STM

import Paths_try_vulkan_graphics

-- DATA TYPE IMAGE RGBA8

newtype ImageRgba8 = ImageRgba8 (Image PixelRGBA8)
newtype PixelRgba8 = PixelRgba8 PixelRGBA8 deriving Show

instance Vk.ObjB.IsImage ImageRgba8 where
	type ImagePixel ImageRgba8 = PixelRgba8
--	type ImageFormat ImageRgba8 = 'Vk.T.FormatR8g8b8a8Unorm
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
		Vk.Ist.createInfoEnabledLayerNames = vldLayers,
		Vk.Ist.createInfoEnabledExtensionNames = es }

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
		Vk.Dvc.createInfoEnabledExtensionNames = [Vk.Khr.Swpch.extensionName],
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

type ShaderFormat = Vk.T.FormatR16g16b16a16Sfloat

body :: forall si sd sc img . Vk.ObjB.IsImage img => Vk.Ist.I si -> Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Q.Q -> Vk.CmdPl.C sc -> img -> Filter -> Float -> Int32 -> Int32 -> IO img
body ist pd dv gq cp img flt0 a0 n i = resultBffr @img pd dv w h \rb ->
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
		dv (HPList.Singleton strImgBinding) exws \wdsl wpl wppl ->
	createDscPl dv \wdp -> createDscStSrc dv wdp imgvws' wdsl \wds ->

	compileShader "shader/expandHeight.comp" >>= \exhs ->
	createCmpPpl @'[] @'[]
		dv (HPList.Singleton strImgBinding) exhs \hdsl hpl hppl ->
	createDscPl dv \hdp -> createDscStSrc dv hdp imgvws' hdsl \hds ->

	compileShader "shader/interpolate.comp" >>= \shdr ->
	createCmpPpl @PshCnsts
		@'[ 'Vk.PshCnst.Range '[ 'Vk.T.ShaderStageComputeBit] PshCnsts]
		dv (strImgBinding :** strImgBinding :** HPList.Nil) shdr
		\dsl pl ppl ->
	createDscPl dv \dp -> createDscSt dv dp imgvws' imgvwd' dsl \ds -> do

	runCmds dv gq cp HPList.Nil HPList.Nil \cb -> do
		tr cb imgs Vk.Img.LayoutUndefined Vk.Img.LayoutTransferDstOptimal
		copyBffrToImg cb b imgs
		tr cb imgs
			Vk.Img.LayoutTransferDstOptimal Vk.Img.LayoutTransferSrcOptimal

		tr cb imgs' Vk.Img.LayoutUndefined Vk.Img.LayoutTransferDstOptimal
		copyImgToImg' cb imgs imgs' w h
		tr cb imgs' Vk.Img.LayoutTransferDstOptimal Vk.Img.LayoutGeneral

		Vk.Cmd.bindPipelineCompute cb Vk.Ppl.BindPointCompute wppl \ccb -> do
			Vk.Cmd.bindDescriptorSetsCompute
				ccb wpl (HPList.Singleton $ U2 wds) def
			Vk.Cmd.dispatch ccb 1 ((h + 2) `div'` 16) 1

		Vk.Cmd.bindPipelineCompute cb Vk.Ppl.BindPointCompute hppl \ccb -> do
			Vk.Cmd.bindDescriptorSetsCompute
				ccb hpl (HPList.Singleton $ U2 hds) def
			Vk.Cmd.dispatch ccb ((w + 2) `div'` 16) 1 1

	q <- newIORef False
	fi <- atomically newTChan
	aa <- atomically $ newTVar a0
	ai <- atomically newTChan
	lft <- atomically newTChan
	dwn <- atomically newTChan
	hm <- atomically newTChan
	withWindow w h \win ->
		Vk.Smph.create @'Nothing dv def nil \scs ->
		Vk.Smph.create @'Nothing dv def nil \rs ->
		Vk.Fnc.create @'Nothing dv def {
			Vk.Fnc.createInfoFlags = Vk.Fnc.CreateSignaledBit } nil \rfs ->
		Vk.Khr.Sfc.Glfw.Win.create ist win nil \sfc ->
		createSwpch win sfc pd dv \sc ex ->
		Vk.Khr.Swpch.getImages dv sc >>= \scis -> do
			GlfwG.Win.setKeyCallback win $ Just \_ k _ ks _ -> case (k, ks) of
				(GlfwG.Key.Key'Q, GlfwG.Key.KeyState'Pressed) -> writeIORef q True
				(GlfwG.Key.Key'N, GlfwG.Key.KeyState'Pressed) ->
					atomically $ writeTChan fi Nearest
				(GlfwG.Key.Key'Semicolon, GlfwG.Key.KeyState'Pressed) ->
					atomically $ writeTChan fi Linear
				(GlfwG.Key.Key'U, GlfwG.Key.KeyState'Pressed) -> atomically do
--					modifyTVar aa (sub (- 1) 0.01)
					a <- readTVar aa
					let	a' = a - 0.01
						a'' = if a' > - 1 then a' else a
					writeTChan fi Cubic
					when (a /= a'') do
						writeTVar aa a''
						writeTChan ai a''
				(GlfwG.Key.Key'U, GlfwG.Key.KeyState'Repeating) -> atomically do
--					modifyTVar aa (sub (- 1) 0.01)
					a <- readTVar aa
					let	a' = a - 0.01
						a'' = if a' <= - 1 || a' < - 0.75 && - 0.75 <= a
							then a else a'
					writeTChan fi Cubic
					when (a /= a'') do
						writeTVar aa a''
						writeTChan ai a''
				(GlfwG.Key.Key'I, GlfwG.Key.KeyState'Pressed) -> atomically do
--					modifyTVar aa (add (- 0.25) 0.01)
					a <- readTVar aa
					let	a' = a + 0.01
						a'' = if a' < - 0.25 then a' else a
					writeTChan fi Cubic
					when (a /= a'') do
						writeTVar aa a''
						writeTChan ai a''
				(GlfwG.Key.Key'I, GlfwG.Key.KeyState'Repeating) -> atomically do
--					modifyTVar aa (add (- 0.25) 0.01)
					a <- readTVar aa
					let	a' = a + 0.01
						a'' = if a' >= - 0.25 || a <= - 0.5 && - 0.5 < a'
							then a else a'
					writeTChan fi Cubic
					when (a /= a'') do
						writeTVar aa a''
						writeTChan ai a''
				(GlfwG.Key.Key'M, GlfwG.Key.KeyState'Pressed) -> atomically do
					writeTChan fi Cubic
					a <- readTVar aa
					when (a /= (- 0.75)) do
						writeTVar aa (- 0.75)
						writeTChan ai (- 0.75)
				(GlfwG.Key.Key'Comma, GlfwG.Key.KeyState'Pressed) -> atomically do
					writeTChan fi Cubic
					a <- readTVar aa
					when (a /= (- 0.5)) do
						writeTVar aa (- 0.5)
						writeTChan ai (- 0.5)
				(GlfwG.Key.Key'H, GlfwG.Key.KeyState'Pressed) -> atomically do
					writeTChan lft (- 1)
				(GlfwG.Key.Key'J, GlfwG.Key.KeyState'Pressed) -> atomically do
					writeTChan dwn 1
				(GlfwG.Key.Key'K, GlfwG.Key.KeyState'Pressed) -> atomically do
					writeTChan dwn (- 1)
				(GlfwG.Key.Key'L, GlfwG.Key.KeyState'Pressed) -> atomically do
					writeTChan lft 1
				(GlfwG.Key.Key'Space, GlfwG.Key.KeyState'Pressed) -> atomically do
					writeTChan hm ()
				_ -> pure ()
			($ iy0) . ($ ix0) . ($ a0) . ($ flt0) $ fix \act flt a ix iy -> do
				ii <- Vk.Khr.Swpch.acquireNextImageResult
					[Vk.Success, Vk.SuboptimalKhr] dv sc Nothing (Just scs) Nothing
				runCmds dv gq cp
					(HPList.Singleton $ Vk.SemaphorePipelineStageFlags scs Vk.Ppl.StageColorAttachmentOutputBit)
					(HPList.Singleton rs) \cb -> do
					tr cb imgd' Vk.Img.LayoutUndefined Vk.Img.LayoutGeneral
					Vk.Cmd.bindPipelineCompute cb Vk.Ppl.BindPointCompute ppl \ccb -> do
						Vk.Cmd.bindDescriptorSetsCompute
							ccb pl (HPList.Singleton $ U2 ds) def
						Vk.Cmd.pushConstantsCompute @'[ 'Vk.T.ShaderStageComputeBit]
							ccb pl (flt :* a :* n' :* ix :* iy :* HPList.Nil)
						Vk.Cmd.dispatch ccb (w `div'` 16) (h `div'` 16) 1
					tr cb imgd' Vk.Img.LayoutGeneral Vk.Img.LayoutTransferSrcOptimal
					tr cb (scis !! fromIntegral ii) Vk.Img.LayoutUndefined Vk.Img.LayoutTransferDstOptimal
					copyImgToImg cb imgd' (scis !! fromIntegral ii) w h Vk.FilterNearest 1 0
					tr cb (scis !! fromIntegral ii) Vk.Img.LayoutTransferDstOptimal Vk.Img.LayoutPresentSrcKhr
				catchAndSerialize (Vk.Khr.Swpch.queuePresent @'Nothing gq $ pinfo sc ii rs)
				GlfwG.waitEvents
				wsc <- GlfwG.Win.shouldClose win
				qp <- readIORef q
				Vk.Q.waitIdle gq
				mflt <- atomically $ tryReadTChan fi
				ma <- atomically $ tryReadTChan ai
				let	flt' = fromMaybe flt mflt
					a' = fromMaybe a ma
				maybe (pure ()) print ma
				maybe (pure ()) (putStrLn . bar) ma
				l <- atomically $ fromMaybe 0 <$> tryReadTChan lft
				d <- atomically $ fromMaybe 0 <$> tryReadTChan dwn
				h <- atomically $ maybe False (const True) <$> tryReadTChan hm
				if (wsc || qp) then pure () else
					act flt' a' (bool (ix + l) ix0 h) (bool (iy + d) iy0 h)

	runCmds dv gq cp HPList.Nil HPList.Nil \cb -> do
		tr cb imgd Vk.Img.LayoutUndefined Vk.Img.LayoutTransferDstOptimal
		copyImgToImg cb imgd' imgd w h Vk.FilterNearest 1 0
		tr cb imgd
			Vk.Img.LayoutTransferDstOptimal Vk.Img.LayoutTransferSrcOptimal
		copyImgToBffr cb imgd rb
	where
	trsd = Vk.Img.UsageTransferSrcBit .|. Vk.Img.UsageTransferDstBit
	sts = Vk.Img.UsageStorageBit .|. Vk.Img.UsageTransferSrcBit
	std = Vk.Img.UsageStorageBit .|. Vk.Img.UsageTransferDstBit
	w, h :: Integral n => n
	w = fromIntegral $ Vk.ObjB.imageWidth img
	h = fromIntegral $ Vk.ObjB.imageHeight img
	tr = transitionImgLyt
	n', ix0, iy0 :: Word32
	n' = fromIntegral n
	ix0 = fromIntegral $ i `mod` n
	iy0 = fromIntegral $ i `div` n
	x `div'` y = case x `divMod` y of (d, 0) -> d; (d, _) -> d + 1
	pinfo :: forall scfmt ccs s . Vk.Khr.Swpch.S scfmt ccs -> Word32 -> Vk.Smph.S s -> Vk.Khr.Swpch.PresentInfo 'Nothing '[s] scfmt '[ccs]
	pinfo sc ii rs = Vk.Khr.Swpch.PresentInfo {
		Vk.Khr.Swpch.presentInfoNext = TMaybe.N,
		Vk.Khr.Swpch.presentInfoWaitSemaphores = HPList.Singleton rs,
		Vk.Khr.Swpch.presentInfoSwapchainImageIndices =
			HPList.Singleton $ Vk.Khr.Swpch.SwapchainImageIndex sc ii }

sub, add :: (Ord n, Num n) => n -> n -> n -> n
sub mn d x | x > mn = x - d | otherwise = x
add mx d x | x < mx = x + d | otherwise = x

bar :: Float -> String
bar a = "-1 |" ++ replicate x '*' ++ replicate y ' ' ++ "| 0\n" ++
	"   |" ++ replicate z ' ' ++ "|" ++ replicate w ' ' ++ "|" ++ replicate v ' ' ++ "|"
	where
	x = round $ 71 + 70 * a
	y = 72 - x
	z = round $ 71 + 70 * (- 0.75)
	w = round (71 + 70 * (- 0.5)) - z - 2
	v = round (71 + 70 * (- 0.25)) - w - z - 4

type PshCnsts = '[Filter, Float, Word32, Word32, Word32]

strImgBinding :: Vk.DscStLyt.Binding ('Vk.DscStLyt.Image iargs)
strImgBinding = Vk.DscStLyt.BindingImage {
	Vk.DscStLyt.bindingImageDescriptorType = Vk.Dsc.TypeStorageImage,
	Vk.DscStLyt.bindingImageStageFlags = Vk.ShaderStageComputeBit }

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

withWindow :: Int -> Int -> (forall s . GlfwG.Win.W s -> IO a) -> IO a
withWindow w h a = do
	GlfwG.Win.hint noApi
	GlfwG.Win.hint notResizable
	GlfwG.Win.create (w + 2) (h + 2) "Bicubic Interpolation" Nothing Nothing a
	where
	noApi = GlfwG.Win.WindowHint'ClientAPI GlfwG.Win.ClientAPI'NoAPI
	notResizable = GlfwG.Win.WindowHint'Resizable False

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
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Img.UsageFlags -> Word32 -> Word32 ->
	(forall si sm . Vk.Img.Binded sm si nm fmt -> IO a) -> IO a
prepareImg pd dv usg w h f = Vk.Img.create @'Nothing dv iinfo nil \i -> do
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
		Vk.Img.createInfoUsage = usg,
		Vk.Img.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Img.createInfoQueueFamilyIndices = [],
		Vk.Img.createInfoInitialLayout = Vk.Img.LayoutUndefined }
	minfo mt = Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mt }

-- COMMANDS

runCmds :: forall sd sc wss sss a . Vk.Dvc.D sd ->
	Vk.Q.Q -> Vk.CmdPl.C sc -> HPList.PL Vk.SemaphorePipelineStageFlags wss -> HPList.PL Vk.Smph.S sss ->
	(forall scb . Vk.CBffr.C scb -> IO a) -> IO a
runCmds dv gq cp wss sss cmds =
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
	sinfo :: Vk.CBffr.C scb -> Vk.SubmitInfo 'Nothing wss '[scb] sss
	sinfo cb = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
--		Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
		Vk.submitInfoWaitSemaphoreDstStageMasks = wss,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
--		Vk.submitInfoSignalSemaphores = HPList.Nil }
		Vk.submitInfoSignalSemaphores = sss }

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
	Vk.Cmd.pipelineBarrier cb srcst dstst zeroBits
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
		Vk.Img.memoryBarrierSrcAccessMask = srcam,
		Vk.Img.memoryBarrierDstAccessMask = dstam }
	srr = Vk.Img.SubresourceRange {
		Vk.Img.subresourceRangeAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.subresourceRangeBaseMipLevel = 0,
		Vk.Img.subresourceRangeLevelCount = 1,
		Vk.Img.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.subresourceRangeLayerCount = 1 }
	(srcst, dstst, srcam, dstam) = case (ol, nl) of
		(Vk.Img.LayoutUndefined, Vk.Img.LayoutTransferDstOptimal) -> (
			Vk.Ppl.StageTopOfPipeBit, Vk.Ppl.StageTransferBit,
			zeroBits, Vk.AccessTransferWriteBit )
		(Vk.Img.LayoutUndefined, Vk.Img.LayoutGeneral) -> (
			Vk.Ppl.StageTopOfPipeBit, Vk.Ppl.StageComputeShaderBit,
			zeroBits, Vk.AccessShaderWriteBit )
		(Vk.Img.LayoutTransferDstOptimal,
			Vk.Img.LayoutTransferSrcOptimal) -> (
			Vk.Ppl.StageTransferBit, Vk.Ppl.StageTransferBit,
			Vk.AccessTransferWriteBit, Vk.AccessTransferReadBit )
		(Vk.Img.LayoutTransferDstOptimal, Vk.Img.LayoutGeneral) -> (
			Vk.Ppl.StageTransferBit, Vk.Ppl.StageComputeShaderBit,
			Vk.AccessTransferWriteBit, Vk.AccessShaderReadBit )
		(Vk.Img.LayoutGeneral, Vk.Img.LayoutTransferSrcOptimal) -> (
			Vk.Ppl.StageComputeShaderBit, Vk.Ppl.StageTransferBit,
			Vk.AccessShaderWriteBit, Vk.AccessTransferReadBit )
		(Vk.Img.LayoutTransferDstOptimal, Vk.Img.LayoutPresentSrcKhr) -> (
			Vk.Ppl.StageComputeShaderBit, Vk.Ppl.StageTransferBit,
			Vk.AccessShaderWriteBit, Vk.AccessTransferReadBit )
		_ -> error "unsupported layout transition!"

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

createDscStLyt :: Vk.DscStLyt.BindingListToMiddle bts =>
	Vk.Dvc.D sd -> HPList.PL Vk.DscStLyt.Binding bts ->
	(forall sdsl . Vk.DscStLyt.D sdsl bts -> IO a) -> IO a
createDscStLyt dv bds = Vk.DscStLyt.create dv info nil
	where info = Vk.DscStLyt.CreateInfo {
		Vk.DscStLyt.createInfoNext = TMaybe.N,
		Vk.DscStLyt.createInfoFlags = zeroBits,
		Vk.DscStLyt.createInfoBindings = bds }

createPplLyt :: forall pctps pcrng sd a bds . (
	Vk.DscStLyt.BindingListToMiddle bds,
	Vk.PshCnst.RangeListToMiddle pctps pcrng ) =>
	Vk.Dvc.D sd -> HPList.PL Vk.DscStLyt.Binding bds -> (forall sl sdsl .
		Vk.DscStLyt.D sdsl bds ->
		Vk.PplLyt.P sl '[ '(sdsl, bds)] pctps -> IO a) -> IO a
createPplLyt dv bds f = createDscStLyt dv bds \dsl ->
	Vk.PplLyt.create dv (info dsl) nil $ f dsl
	where
	info :: Vk.DscStLyt.D sdsl bds ->
		Vk.PplLyt.CreateInfo 'Nothing
			'[ '(sdsl, bds)] ('Vk.PshCnst.Layout pctps pcrng)
	info dsl = Vk.PplLyt.CreateInfo {
		Vk.PplLyt.createInfoNext = TMaybe.N,
		Vk.PplLyt.createInfoFlags = zeroBits,
		Vk.PplLyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }

createCmpPpl :: forall pctps pcrng sd bds a . (
	Vk.PshCnst.RangeListToMiddle pctps pcrng,
	Vk.DscStLyt.BindingListToMiddle bds ) =>
	Vk.Dvc.D sd -> HPList.PL Vk.DscStLyt.Binding bds ->
	SpirV.S GlslComputeShader -> (forall sds scppl spl .
		Vk.DscStLyt.D sds bds ->
		Vk.PplLyt.P spl '[ '(sds, bds)] pctps ->
		Vk.Ppl.Cp.C scppl '(spl, '[ '(sds, bds)], pctps) -> IO a) ->
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
	Vk.ImgVw.I "source_image" ShaderFormat sivs ->
	Vk.ImgVw.I "destination_image" ShaderFormat sivd ->
	Vk.DscStLyt.D sdsl '[SrcImg, DstImg] ->
	(forall sds . Vk.DscSt.D sds '(sdsl, '[SrcImg, DstImg]) -> IO a) -> IO a
createDscSt dv dp svw dvw dl a =
	Vk.DscSt.allocateDs dv info \(HPList.Singleton ds) -> (>> a ds)
		$ Vk.DscSt.updateDs dv
			(U5 (dscWrite ds svw) :** U5 (dscWrite ds dvw) :**
				HPList.Nil)
			HPList.Nil
	where info = Vk.DscSt.AllocateInfo {
		Vk.DscSt.allocateInfoNext = TMaybe.N,
		Vk.DscSt.allocateInfoDescriptorPool = dp,
		Vk.DscSt.allocateInfoSetLayouts = HPList.Singleton $ U2 dl }

type SrcImg = 'Vk.DscStLyt.Image '[ '("source_image", ShaderFormat)]
type DstImg = 'Vk.DscStLyt.Image '[ '("destination_image", ShaderFormat)]

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

copyImgToImg' :: Vk.CBffr.C scb -> Vk.Img.Binded sms sis nms fmts ->
	Vk.Img.Binded smd sid nmd fmtd -> Int32 -> Int32 -> IO ()
copyImgToImg' cb si di w h = Vk.Cmd.blitImage cb
	si Vk.Img.LayoutTransferSrcOptimal
	di Vk.Img.LayoutTransferDstOptimal [blt] Vk.FilterNearest
	where blt = Vk.Img.Blit {
		Vk.Img.blitSrcSubresource = colorLayer0,
		Vk.Img.blitSrcOffsetFrom = Vk.Offset3d 0 0 0,
		Vk.Img.blitSrcOffsetTo = Vk.Offset3d w h 1,
		Vk.Img.blitDstSubresource = colorLayer0,
		Vk.Img.blitDstOffsetFrom = Vk.Offset3d 1 1 0,
		Vk.Img.blitDstOffsetTo = Vk.Offset3d (w + 1) (h + 1) 1 }

createDscStSrc ::
	Vk.Dvc.D sd -> Vk.DscPl.P sp ->
	Vk.ImgVw.I "source_image" ShaderFormat sivs ->
	Vk.DscStLyt.D sdsl '[SrcImg] ->
	(forall sds . Vk.DscSt.D sds '(sdsl, '[SrcImg]) -> IO a) -> IO a
createDscStSrc dv dp svw dl a =
	Vk.DscSt.allocateDs dv info \(HPList.Singleton ds) -> (>> a ds) $
	Vk.DscSt.updateDs
		dv (HPList.Singleton . U5 $ dscWrite ds svw) HPList.Nil
	where info = Vk.DscSt.AllocateInfo {
		Vk.DscSt.allocateInfoNext = TMaybe.N,
		Vk.DscSt.allocateInfoDescriptorPool = dp,
		Vk.DscSt.allocateInfoSetLayouts = HPList.Singleton $ U2 dl }

-- SWAP CHAIN

data SwpchSupportDetails fmts = SwpchSupportDetails {
	capabilities :: Vk.Khr.Sfc.Capabilities,
	formats :: (
		[Vk.Khr.Sfc.Format Vk.T.FormatB8g8r8a8Srgb],
		HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts ),
	presentModes :: [Vk.Khr.Sfc.PresentMode] }

deriving instance
	Show (HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts) =>
	Show (SwpchSupportDetails fmts)

querySwpchSupport :: Vk.Phd.P -> Vk.Khr.Sfc.S ss -> (forall fmts .
	Show (HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts) =>
	SwpchSupportDetails fmts -> IO a) -> IO a
querySwpchSupport pd sfc f = Vk.Khr.Sfc.Phd.getFormats pd sfc \fmts ->
	f =<< SwpchSupportDetails
		<$> Vk.Khr.Sfc.Phd.getCapabilities pd sfc
		<*> ((, fmts) <$> Vk.Khr.Sfc.Phd.getFormatsFiltered pd sfc)
		<*> Vk.Khr.Sfc.Phd.getPresentModes pd sfc

createSwpch :: GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	Vk.Dvc.D sd -> (forall ss scfmt . Vk.T.FormatToValue scfmt =>
		Vk.Khr.Swpch.S scfmt ss -> Vk.Extent2d -> IO a) -> IO a
createSwpch win sfc pd dv f = querySwpchSupport pd sfc \ss -> do
	ex <- swapExtent win $ capabilities ss
	let	cps = capabilities ss
		pm = findDefault Vk.Khr.Sfc.PresentModeFifo
			(== Vk.Khr.Sfc.PresentModeMailbox) $ presentModes ss
	chooseSwpSfcFmt (formats ss)
		\(Vk.Khr.Sfc.Format sc :: Vk.Khr.Sfc.Format fmt) ->
		Vk.Khr.Swpch.create @_ @fmt dv
			(swpchInfo sfc cps sc pm ex) nil (`f` ex)

swpchInfo :: forall fmt ss .
	Vk.Khr.Sfc.S ss -> Vk.Khr.Sfc.Capabilities ->
	Vk.Khr.Sfc.ColorSpace -> Vk.Khr.Sfc.PresentMode -> Vk.Extent2d ->
	Vk.Khr.Swpch.CreateInfo 'Nothing ss fmt
swpchInfo sfc cps cs pm ex = Vk.Khr.Swpch.CreateInfo {
	Vk.Khr.Swpch.createInfoNext = TMaybe.N,
	Vk.Khr.Swpch.createInfoFlags = zeroBits,
	Vk.Khr.Swpch.createInfoSurface = sfc,
	Vk.Khr.Swpch.createInfoMinImageCount = imgc,
	Vk.Khr.Swpch.createInfoImageColorSpace = cs,
	Vk.Khr.Swpch.createInfoImageExtent = ex,
	Vk.Khr.Swpch.createInfoImageArrayLayers = 1,
	Vk.Khr.Swpch.createInfoImageUsage = Vk.Img.UsageTransferDstBit,
	Vk.Khr.Swpch.createInfoImageSharingMode = ism,
	Vk.Khr.Swpch.createInfoQueueFamilyIndices = qfis,
	Vk.Khr.Swpch.createInfoPreTransform =
		Vk.Khr.Sfc.capabilitiesCurrentTransform cps,
	Vk.Khr.Swpch.createInfoCompositeAlpha =
		Vk.Khr.Sfc.CompositeAlphaOpaqueBit,
	Vk.Khr.Swpch.createInfoPresentMode = pm,
	Vk.Khr.Swpch.createInfoClipped = True,
	Vk.Khr.Swpch.createInfoOldSwapchain = Nothing }
	where
	imgc = clamp 0 imgcx (Vk.Khr.Sfc.capabilitiesMinImageCount cps + 1)
	imgcx = fromMaybe maxBound
		. onlyIf (> 0) $ Vk.Khr.Sfc.capabilitiesMaxImageCount cps
	(ism, qfis) = (Vk.SharingModeExclusive, [])

swapExtent :: GlfwG.Win.W sw -> Vk.Khr.Sfc.Capabilities -> IO Vk.Extent2d
swapExtent win cps
	| Vk.extent2dWidth cur /= maxBound = pure cur
	| otherwise = (<$> GlfwG.Win.getFramebufferSize win)
		\(fromIntegral -> w, fromIntegral -> h) ->
		Vk.Extent2d
			(clamp (Vk.extent2dWidth n) (Vk.extent2dWidth x) w)
			(clamp (Vk.extent2dHeight n) (Vk.extent2dHeight x) h)
	where
	cur = Vk.Khr.Sfc.capabilitiesCurrentExtent cps
	n = Vk.Khr.Sfc.capabilitiesMinImageExtent cps
	x = Vk.Khr.Sfc.capabilitiesMaxImageExtent cps

chooseSwpSfcFmt :: (
	[Vk.Khr.Sfc.Format Vk.T.FormatB8g8r8a8Srgb],
	HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts ) ->
	(forall fmt . Vk.T.FormatToValue fmt => Vk.Khr.Sfc.Format fmt -> a) -> a
chooseSwpSfcFmt (fmts, (fmt0 :^* _)) f = maybe (f fmt0) f $ (`L.find` fmts)
	$ (== Vk.Khr.Sfc.ColorSpaceSrgbNonlinear) . Vk.Khr.Sfc.formatColorSpace
chooseSwpSfcFmt (_, HPListC.Nil) _ = error "no available swap surface formats"

catchAndSerialize :: IO () -> IO ()
catchAndSerialize =
	(`catch` \(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)
