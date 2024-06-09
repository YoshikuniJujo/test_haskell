{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Concurrent

import GHC.Generics
import GHC.TypeNats
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Control.Arrow hiding (loop)
import Control.Monad
import Control.Monad.Fix
import Control.Exception
import Data.Kind
import Data.Proxy
import Gpu.Vulkan.Object qualified as Obj
import Data.Default
import Data.Bits
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)

import Data.HeteroParList qualified as HPList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Bool
import Data.Maybe
import Data.List qualified as L
import Data.IORef
import Data.List.Length
import Data.Word
import Data.Color

import qualified Data.List.NonEmpty as NE
import qualified Graphics.UI.GLFW as Glfw hiding (createWindowSurface)
import qualified Gpu.Vulkan.Cglm as Cglm
import qualified Foreign.Storable.Generic

import qualified Language.SpirV as SpirV
import Language.SpirV.ShaderKind
import Language.SpirV.Shaderc.TH


import qualified Gpu.Vulkan as Vk
import qualified Gpu.Vulkan.Exception as Vk
import qualified Gpu.Vulkan.Instance as Vk.Ist
import qualified Gpu.Vulkan.Khr.Surface as Vk.Khr
import qualified Gpu.Vulkan.Khr.Swapchain as Vk.Khr
import qualified Gpu.Vulkan.PhysicalDevice as Vk.Phd

import qualified Gpu.Vulkan.Device as Vk.Dvc
import qualified Gpu.Vulkan.Device as Vk.Dvc.M
import qualified Gpu.Vulkan.Khr.Surface as Vk.Khr.Sfc
import qualified Gpu.Vulkan.Khr.Surface.PhysicalDevice as Vk.Khr.Sfc.Phd
import qualified Gpu.Vulkan.Khr.Swapchain as Vk.Khr.Swpch
import qualified Gpu.Vulkan.Image as Vk.Img
import qualified Gpu.Vulkan.Image as Vk.Img.M
import qualified Gpu.Vulkan.ImageView as Vk.ImgVw
import qualified Gpu.Vulkan.Component as Vk.Component
import qualified Gpu.Vulkan.ShaderModule as Vk.ShaderModule
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShdrSt
import qualified Gpu.Vulkan.Pipeline.InputAssemblyState as Vk.Ppl.InpAsmbSt
import qualified Gpu.Vulkan.Pipeline.ViewportState as Vk.Ppl.ViewportSt
import qualified Gpu.Vulkan.Pipeline.RasterizationState as Vk.Ppl.RstSt
import qualified Gpu.Vulkan.Pipeline.MultisampleState as Vk.Ppl.MltSmplSt
import qualified Gpu.Vulkan.Sample as Vk.Sample
import qualified Gpu.Vulkan.Pipeline.ColorBlendAttachment as Vk.Ppl.ClrBlndAtt
import qualified Gpu.Vulkan.ColorComponent as Vk.ClrCmp
import qualified Gpu.Vulkan.Pipeline.ColorBlendState as Vk.Ppl.ClrBlndSt
import qualified Gpu.Vulkan.PipelineLayout as Vk.Ppl.Layout
import qualified Gpu.Vulkan.Attachment as Vk.Att
import qualified Gpu.Vulkan.Subpass as Vk.Subpass
import qualified "try-gpu-vulkan" Gpu.Vulkan.Pipeline as Vk.Ppl
import qualified Gpu.Vulkan.RenderPass as Vk.RndrPss
import qualified Gpu.Vulkan.RenderPass as Vk.RndrPss.M
import qualified Gpu.Vulkan.Pipeline.Graphics as Vk.Ppl.Graphics
import qualified Gpu.Vulkan.Framebuffer as Vk.Frmbffr
import qualified Gpu.Vulkan.CommandPool as Vk.CmdPl
import qualified Gpu.Vulkan.CommandBuffer as Vk.CBffr
import qualified Gpu.Vulkan.Semaphore as Vk.Semaphore
import qualified Gpu.Vulkan.Fence as Vk.Fence
import qualified Gpu.Vulkan.VertexInput as Vk.VtxInp
import qualified Gpu.Vulkan.Buffer as Vk.Bffr
import qualified Gpu.Vulkan.Memory as Vk.Mm.M
import qualified Gpu.Vulkan.Queue as Vk.Q
import qualified Gpu.Vulkan.Memory as Vk.Mm
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscSetLyt
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPl
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSet

import Gpu.Vulkan.TypeEnum qualified as Vk.T

import Gpu.Vulkan.Pipeline.VertexInputState qualified as Vk.Ppl.VtxInpSt

import Graphics.SimplePolygon.DebugMessenger qualified as DbgMsngr
import Graphics.SimplePolygon.Instance qualified as Ist
import Graphics.SimplePolygon.Window qualified as Win
import Graphics.SimplePolygon.Surface qualified as Sfc
import Graphics.SimplePolygon.PhysicalDevice (QFamIndices(..))
import Graphics.SimplePolygon.PhysicalDevice qualified as PhDvc

import Data.Ord.ToolsYj
import Data.Bits.ToolsYj
import Data.List.ToolsYj
import Data.Bool.ToolsYj
import Data.IORef.ToolsYj

import Debug
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win
import Graphics.UI.GlfwG.Window.Type qualified as GlfwG.Win
import Data.HeteroParList.Constrained (pattern (:^*))
import Data.HeteroParList.Constrained qualified as HPListC

import Data.Sequences
import Data.MonoTraversable
import Foreign.Storable.Generic qualified as GStorable

main :: IO ()
main = createControllerEvent >>= \ce -> forkIO (doWhile_ $ controller ce) >>
	Win.create windowSize windowName \(Win.W w fr) ->
	Ist.create debug \ist ->
	bool id (DbgMsngr.setup ist) debug (body w ist fr ce)

createControllerEvent :: IO ControllerEvent
createControllerEvent = ControllerEvent
	<$> newIORef False <*> newIORef False <*> newIORef 0 <*> newIORef 0

controller :: ControllerEvent -> IO Bool
controller ev = do
	threadDelay 10000
	fn <- readIORef $ controllerEventFinished ev
	if fn then pure False else do
		Just (Glfw.GamepadState gb ga) <-
			Glfw.getGamepadState Glfw.Joystick'1
		modifyIORef (controllerEventLeftX ev)
			(+ ga Glfw.GamepadAxis'LeftX)
		modifyIORef (controllerEventLeftY ev)
			(+ ga Glfw.GamepadAxis'LeftY)
		when (gb Glfw.GamepadButton'A ==
				Glfw.GamepadButtonState'Pressed)
			$ writeIORef (controllerEventButtonAEver ev) True
		pure True

data ControllerEvent = ControllerEvent {
	controllerEventFinished :: IORef Bool,
	controllerEventButtonAEver :: IORef Bool,
	controllerEventLeftX :: IORef Float,
	controllerEventLeftY :: IORef Float }

windowName :: String
windowName = "Triangle"

windowSize :: (Int, Int)
windowSize = (width, height) where width = 800; height = 600

type FramebufferResized = IORef Bool

vldLayers :: [Vk.LayerName]
vldLayers = [Vk.layerKhronosValidation]

body :: GlfwG.Win.W sw -> Vk.Ist.I si -> FramebufferResized -> ControllerEvent -> IO ()
body w ist g cev =
	Sfc.create ist w \sfc ->
	pickPhd ist sfc >>= \(pd, qfis) ->
	createLgDvc pd qfis \d gq pq ->
	createCmdPl qfis d \cp ->
	createSwpch w sfc pd qfis d \(sc :: Vk.Khr.Swpch.S scifmt ss) ex ->
	Vk.Khr.Swpch.getImages d sc >>= \scis ->
	createImageViewsNew d scis \scvs ->
	createRenderPassNew @scifmt d \rp ->
	unfrmBffrOstAlgn pd \(_ :: Proxy alu) ->
	createPipelineLayout' d \dscslyt ppllyt ->
	createGraphicsPipeline' d ex rp ppllyt \gpl ->
	createFramebuffers d ex rp scvs \fbs ->
	createVtxBffr pd d gq cp vtcs \vb ->
	createIdxBffr pd d gq cp idcs \ib ->
	createUniformBuffer pd d \ub ubm ->
	createDscPl d \dscp ->
	createDescriptorSet d dscp ub dscslyt \ubds ->
	Vk.CBffr.allocate
		@_ @('[ '()]) d (cmdBffrInfo cp) \(cb :*. HPList.Nil) ->
	createSyncObjects d \sos ->
	mainLoop g w sfc pd qfis d gq pq sc ex scvs rp ppllyt gpl fbs vb ib ubm ubds cb sos cev

pickPhd :: Vk.Ist.I si ->
	Vk.Khr.Sfc.S ss -> IO (Vk.Phd.P, PhDvc.QFamIndices)
pickPhd ist sfc = PhDvc.enumerate ist sfc >>= \case 
	[] -> error "failed to find a sutable GPU!"
	PhDvc.P phd qfi : _ -> pure (phd, qfi)

dvcExtensions :: [Vk.Phd.ExtensionName]
dvcExtensions = [Vk.Khr.Swpch.extensionName]

createLgDvc :: Vk.Phd.P -> PhDvc.QFamIndices -> (forall sd .
		Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q -> IO a) -> IO a
createLgDvc phdvc qfis f =
	let	uniqueQueueFamilies =
			L.nub [PhDvc.grFam qfis, PhDvc.prFam qfis]
		queueCreateInfos qf = Vk.Dvc.QueueCreateInfo {
			Vk.Dvc.queueCreateInfoNext = TMaybe.N,
			Vk.Dvc.queueCreateInfoFlags = def,
			Vk.Dvc.queueCreateInfoQueueFamilyIndex = qf,
			Vk.Dvc.queueCreateInfoQueuePriorities = [1] } in
	mkHeteroParList queueCreateInfos uniqueQueueFamilies \qs -> do
		let	createInfo = Vk.Dvc.M.CreateInfo {
				Vk.Dvc.M.createInfoNext = TMaybe.N,
				Vk.Dvc.M.createInfoFlags = def,
				Vk.Dvc.M.createInfoQueueCreateInfos = qs,
--					queueCreateInfos <$> uniqueQueueFamilies,
				Vk.Dvc.M.createInfoEnabledLayerNames =
					bool [] vldLayers debug,
				Vk.Dvc.M.createInfoEnabledExtensionNames =
					dvcExtensions,
				Vk.Dvc.M.createInfoEnabledFeatures = Just def }
		Vk.Dvc.create phdvc createInfo nil \dvc -> do
			gq <- Vk.Dvc.getQueue dvc (PhDvc.grFam qfis) 0
			pq <- Vk.Dvc.getQueue dvc (PhDvc.prFam qfis) 0
			f dvc gq pq

mkHeteroParList :: WithPoked (TMaybe.M s) => (a -> t s) -> [a] ->
	(forall ss . HPList.ToListWithCM' WithPoked TMaybe.M ss => HPList.PL t ss -> b) -> b
mkHeteroParList _k [] f = f HPList.Nil
mkHeteroParList k (x : xs) f = mkHeteroParList k xs \xs' -> f (k x :** xs')

createSwpch :: GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QFamIndices -> Vk.Dvc.D sd -> (forall ss scfmt .
		Vk.T.FormatToValue scfmt =>
		Vk.Khr.Swpch.S scfmt ss -> Vk.Extent2d -> IO a) -> IO a
createSwpch w sfc pd qfis dv f = querySwpchSupport pd sfc \ss -> do
	ex <- swapExtent w $ capabilities ss
	let	cps = capabilities ss
		pm = findDefault Vk.Khr.PresentModeFifo
			(== Vk.Khr.PresentModeMailbox) $ presentModes ss
	chooseSwpSfcFmt (formats ss)
		\(Vk.Khr.Sfc.Format sc :: Vk.Khr.Sfc.Format fmt) ->
		Vk.Khr.Swpch.create @_ @fmt dv
			(swpchInfo sfc qfis cps sc pm ex) nil (`f` ex)

querySwpchSupport :: Vk.Phd.P -> Vk.Khr.Sfc.S ss -> (forall fmts .
	Show (HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts) =>
	SwpchSupportDetails fmts -> IO a) -> IO a
querySwpchSupport pd sfc f = Vk.Khr.Sfc.Phd.getFormats pd sfc \fmts ->
	f =<< SwpchSupportDetails
		<$> Vk.Khr.Sfc.Phd.getCapabilities pd sfc
		<*> ((, fmts) <$> Vk.Khr.Sfc.Phd.getFormatsFiltered pd sfc)
		<*> Vk.Khr.Sfc.Phd.getPresentModes pd sfc

data SwpchSupportDetails fmts = SwpchSupportDetails {
	capabilities :: Vk.Khr.Sfc.Capabilities,
	formats :: (
		[Vk.Khr.Sfc.Format Vk.T.FormatB8g8r8a8Srgb],
		HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts ),
	presentModes :: [Vk.Khr.PresentMode] }

deriving instance
	Show (HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts) =>
	Show (SwpchSupportDetails fmts)

chooseSwpSfcFmt :: (
	[Vk.Khr.Sfc.Format Vk.T.FormatB8g8r8a8Srgb],
	HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts ) ->
	(forall fmt . Vk.T.FormatToValue fmt => Vk.Khr.Sfc.Format fmt -> a) -> a
chooseSwpSfcFmt (fmts, (fmt0 :^* _)) f = maybe (f fmt0) f $ (`L.find` fmts)
	$ (== Vk.Khr.ColorSpaceSrgbNonlinear) . Vk.Khr.Sfc.formatColorSpace
chooseSwpSfcFmt (_, HPListC.Nil) _ = error "no available swap surface formats"

recreateSwpch :: forall sw ssfc sd fmt ssc . Vk.T.FormatToValue fmt =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QFamIndices -> Vk.Dvc.D sd -> Vk.Khr.Swpch.S fmt ssc -> IO Vk.Extent2d
recreateSwpch win sfc pd qfis0 dvc sc = do
	ss <- querySwpchSupportFmt @fmt pd sfc
	ex <- swapExtent win $ capabilitiesFmt ss
	let	cps = capabilitiesFmt ss
		Vk.Khr.Sfc.Format cs = fromMaybe
			(error "no available swap surface formats")
			. listToMaybe $ formatsFmt ss
		pm = findDefault Vk.Khr.PresentModeFifo
			(== Vk.Khr.PresentModeMailbox) $ presentModesFmt ss
	ex <$ Vk.Khr.Swpch.unsafeRecreate dvc
		(swpchInfo @fmt sfc qfis0 cps cs pm ex) nil sc

querySwpchSupportFmt :: Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Khr.Sfc.S ss -> IO (SwpchSupportDetailsFmt fmt)
querySwpchSupportFmt dvc sfc = SwpchSupportDetailsFmt
	<$> Vk.Khr.Sfc.Phd.getCapabilities dvc sfc
	<*> Vk.Khr.Sfc.Phd.getFormatsFiltered dvc sfc
	<*> Vk.Khr.Sfc.Phd.getPresentModes dvc sfc

data SwpchSupportDetailsFmt fmt = SwpchSupportDetailsFmt {
	capabilitiesFmt :: Vk.Khr.Sfc.Capabilities,
	formatsFmt :: [Vk.Khr.Sfc.Format fmt],
	presentModesFmt :: [Vk.Khr.PresentMode] } deriving Show

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

swpchInfo :: forall fmt ss .
	Vk.Khr.Sfc.S ss -> QFamIndices -> Vk.Khr.Sfc.Capabilities ->
	Vk.Khr.ColorSpace -> Vk.Khr.PresentMode -> Vk.Extent2d ->
	Vk.Khr.Swpch.CreateInfo 'Nothing ss fmt
swpchInfo sfc qfis0 cps cs pm ex = Vk.Khr.Swpch.CreateInfo {
	Vk.Khr.Swpch.createInfoNext = TMaybe.N,
	Vk.Khr.Swpch.createInfoFlags = zeroBits,
	Vk.Khr.Swpch.createInfoSurface = sfc,
	Vk.Khr.Swpch.createInfoMinImageCount = imgc,
	Vk.Khr.Swpch.createInfoImageColorSpace = cs,
	Vk.Khr.Swpch.createInfoImageExtent = ex,
	Vk.Khr.Swpch.createInfoImageArrayLayers = 1,
	Vk.Khr.Swpch.createInfoImageUsage = Vk.Img.UsageColorAttachmentBit,
	Vk.Khr.Swpch.createInfoImageSharingMode = ism,
	Vk.Khr.Swpch.createInfoQueueFamilyIndices = qfis,
	Vk.Khr.Swpch.createInfoPreTransform =
		Vk.Khr.Sfc.capabilitiesCurrentTransform cps,
	Vk.Khr.Swpch.createInfoCompositeAlpha = Vk.Khr.CompositeAlphaOpaqueBit,
	Vk.Khr.Swpch.createInfoPresentMode = pm,
	Vk.Khr.Swpch.createInfoClipped = True,
	Vk.Khr.Swpch.createInfoOldSwapchain = Nothing }
	where
	imgc = clamp 0 imgcx (Vk.Khr.Sfc.capabilitiesMinImageCount cps + 1)
	imgcx = fromMaybe maxBound
		. onlyIf (> 0) $ Vk.Khr.Sfc.capabilitiesMaxImageCount cps
	(ism, qfis) = bool
		(Vk.SharingModeConcurrent, [grFam qfis0, prFam qfis0])
		(Vk.SharingModeExclusive, []) (grFam qfis0 == prFam qfis0)

createImageViewsNew :: Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> [Vk.Img.Binded ss ss nm fmt] ->
	(forall si . HPList.PL (Vk.ImgVw.I nm fmt) si -> IO a) -> IO a
createImageViewsNew _dvc [] f = f HPList.Nil
createImageViewsNew dvc (sci : scis) f =
	createImageView dvc sci \sciv ->
	createImageViewsNew dvc scis \scivs -> f $ sciv :** scivs

recreateImageViewsNew :: Vk.T.FormatToValue scfmt => Vk.Dvc.D sd ->
	[Vk.Img.Binded ss ss nm scfmt] -> HPList.PL (Vk.ImgVw.I nm scfmt) sis -> IO ()
recreateImageViewsNew _dvc [] HPList.Nil = pure ()
recreateImageViewsNew dvc (sci : scis) (iv :** ivs) =
	Vk.ImgVw.unsafeRecreate dvc (mkImageViewCreateInfoNew sci) nil iv >>
	recreateImageViewsNew dvc scis ivs
recreateImageViewsNew _ _ _ =
	error "number of Vk.Img.M.I and Vk.ImgView.M.I should be same"

createImageView :: forall ivfmt sd si sm nm ifmt a .
	Vk.T.FormatToValue ivfmt =>
	Vk.Dvc.D sd -> Vk.Img.Binded sm si nm ifmt ->
	(forall siv . Vk.ImgVw.I nm ivfmt siv -> IO a) -> IO a
createImageView dvc timg f =
	Vk.ImgVw.create dvc (mkImageViewCreateInfoNew timg) nil f

mkImageViewCreateInfoNew ::
	Vk.Img.Binded sm si nm ifmt ->
	Vk.ImgVw.CreateInfo 'Nothing sm si nm ifmt ivfmt
mkImageViewCreateInfoNew sci = Vk.ImgVw.CreateInfo {
	Vk.ImgVw.createInfoNext = TMaybe.N,
	Vk.ImgVw.createInfoFlags = Vk.ImgVw.CreateFlagsZero,
	Vk.ImgVw.createInfoImage = sci,
	Vk.ImgVw.createInfoViewType = Vk.ImgVw.Type2d,
	Vk.ImgVw.createInfoComponents = components,
	Vk.ImgVw.createInfoSubresourceRange = subresourceRange }
	where
	components = Vk.Component.Mapping {
		Vk.Component.mappingR = def, Vk.Component.mappingG = def,
		Vk.Component.mappingB = def, Vk.Component.mappingA = def }
	subresourceRange = Vk.Img.M.SubresourceRange {
		Vk.Img.M.subresourceRangeAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.M.subresourceRangeBaseMipLevel = 0,
		Vk.Img.M.subresourceRangeLevelCount = 1,
		Vk.Img.M.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.M.subresourceRangeLayerCount = 1 }

createRenderPassNew ::
	forall (scifmt :: Vk.T.Format) sd a . Vk.T.FormatToValue scifmt =>
	Vk.Dvc.D sd -> (forall sr . Vk.RndrPss.R sr -> IO a) -> IO a
createRenderPassNew dvc f = do
	let	colorAttachment :: Vk.Att.Description scifmt
		colorAttachment = Vk.Att.Description {
			Vk.Att.descriptionFlags = zeroBits,
			Vk.Att.descriptionSamples = Vk.Sample.Count1Bit,
			Vk.Att.descriptionLoadOp = Vk.Att.LoadOpClear,
			Vk.Att.descriptionStoreOp = Vk.Att.StoreOpStore,
			Vk.Att.descriptionStencilLoadOp = Vk.Att.LoadOpDontCare,
			Vk.Att.descriptionStencilStoreOp =
				Vk.Att.StoreOpDontCare,
			Vk.Att.descriptionInitialLayout =
				Vk.Img.LayoutUndefined,
			Vk.Att.descriptionFinalLayout =
				Vk.Img.LayoutPresentSrcKhr }
		colorAttachmentRef = Vk.Att.Reference {
			Vk.Att.referenceAttachment = 0,
			Vk.Att.referenceLayout =
				Vk.Img.LayoutColorAttachmentOptimal }
		subpass = Vk.Subpass.Description {
			Vk.Subpass.descriptionFlags = zeroBits,
			Vk.Subpass.descriptionPipelineBindPoint =
				Vk.Ppl.BindPointGraphics,
			Vk.Subpass.descriptionInputAttachments = [],
			Vk.Subpass.descriptionColorAndResolveAttachments =
				Left [colorAttachmentRef],
			Vk.Subpass.descriptionDepthStencilAttachment = Nothing,
			Vk.Subpass.descriptionPreserveAttachments = [] }
		dependency = Vk.Subpass.Dependency {
			Vk.Subpass.dependencySrcSubpass = Vk.Subpass.SExternal,
			Vk.Subpass.dependencyDstSubpass = 0,
			Vk.Subpass.dependencySrcStageMask =
				Vk.Ppl.StageColorAttachmentOutputBit .|.
				Vk.Ppl.StageEarlyFragmentTestsBit,
			Vk.Subpass.dependencySrcAccessMask = zeroBits,
			Vk.Subpass.dependencyDstStageMask =
				Vk.Ppl.StageColorAttachmentOutputBit .|.
				Vk.Ppl.StageEarlyFragmentTestsBit,
			Vk.Subpass.dependencyDstAccessMask =
				Vk.AccessColorAttachmentWriteBit .|.
				Vk.AccessDepthStencilAttachmentWriteBit,
			Vk.Subpass.dependencyDependencyFlags = zeroBits }
		renderPassInfo = Vk.RndrPss.M.CreateInfo {
			Vk.RndrPss.M.createInfoNext = TMaybe.N,
			Vk.RndrPss.M.createInfoFlags = zeroBits,
			Vk.RndrPss.M.createInfoAttachments = colorAttachment :** HPList.Nil,
			Vk.RndrPss.M.createInfoSubpasses = [subpass],
			Vk.RndrPss.M.createInfoDependencies = [dependency] }
	Vk.RndrPss.create @'Nothing @'[scifmt] dvc renderPassInfo nil \rp -> f rp

type AtomUbo s = '(s, '[ 'Vk.DscSetLyt.Buffer '[Obj.Atom 256 UniformBufferObject 'Nothing]])

createDescriptorSetLayout :: Vk.Dvc.D sd -> (forall s .
	Vk.DscSetLyt.D s '[ 'Vk.DscSetLyt.Buffer '[Obj.Atom 256 UniformBufferObject 'Nothing]]
	-> IO a) -> IO a
createDescriptorSetLayout dvc = Vk.DscSetLyt.create dvc layoutInfo nil
	where
	layoutInfo :: Vk.DscSetLyt.CreateInfo 'Nothing
		'[ 'Vk.DscSetLyt.Buffer '[Obj.Atom 256 UniformBufferObject 'Nothing] ]
	layoutInfo = Vk.DscSetLyt.CreateInfo {
		Vk.DscSetLyt.createInfoNext = TMaybe.N,
		Vk.DscSetLyt.createInfoFlags = zeroBits,
		Vk.DscSetLyt.createInfoBindings = uboLayoutBinding :** HPList.Nil }
	uboLayoutBinding :: Vk.DscSetLyt.Binding
		('Vk.DscSetLyt.Buffer '[Obj.Atom 256 UniformBufferObject 'Nothing])
	uboLayoutBinding = Vk.DscSetLyt.BindingBuffer {
		Vk.DscSetLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBuffer,
		Vk.DscSetLyt.bindingBufferStageFlags = Vk.ShaderStageVertexBit }

unfrmBffrOstAlgn ::
	Vk.Phd.P -> (forall a . KnownNat a => Proxy a -> IO b) -> IO b
unfrmBffrOstAlgn pd f = (\(SomeNat p) -> f p) . someNatVal . fromIntegral
	. Vk.Phd.limitsMinUniformBufferOffsetAlignment . Vk.Phd.propertiesLimits
	=<< Vk.Phd.getProperties pd

createPipelineLayout' ::
	Vk.Dvc.D sd -> (forall sdsl sl .
		Vk.DscSetLyt.D sdsl
			'[ 'Vk.DscSetLyt.Buffer '[Obj.Atom 256 UniformBufferObject 'Nothing]] ->
		Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] -> IO b) -> IO b
createPipelineLayout' dvc f =
	createDescriptorSetLayout dvc \dsl ->
	let	pipelineLayoutInfo = Vk.Ppl.Layout.CreateInfo {
			Vk.Ppl.Layout.createInfoNext = TMaybe.N,
			Vk.Ppl.Layout.createInfoFlags = zeroBits,
			Vk.Ppl.Layout.createInfoSetLayouts =
				HPList.Singleton $ U2 dsl } in
	Vk.Ppl.Layout.create @'Nothing @_ @_ @'[] dvc pipelineLayoutInfo nil $ f dsl

createGraphicsPipeline' :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	(forall sg . Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Cglm.Vec3)]
		'(sl, '[AtomUbo sdsl], '[]) -> IO a) -> IO a
createGraphicsPipeline' dvc sce rp ppllyt f =
	Vk.Ppl.Graphics.createGs dvc Nothing (U14 pplInfo :** HPList.Nil)
			nil \(U3 gpl :** HPList.Nil) -> f gpl
	where pplInfo = mkGraphicsPipelineCreateInfo' sce rp ppllyt

recreateGraphicsPipeline' :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Cglm.Vec3)]
		'(sl, '[AtomUbo sdsl], '[]) -> IO ()
recreateGraphicsPipeline' dvc sce rp ppllyt gpls = Vk.Ppl.Graphics.unsafeRecreateGs
	dvc Nothing (U14 pplInfo :** HPList.Nil) nil (U3 gpls :** HPList.Nil)
	where pplInfo = mkGraphicsPipelineCreateInfo' sce rp ppllyt

mkGraphicsPipelineCreateInfo' ::
	Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.CreateInfo 'Nothing '[
			'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
			'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
		'(	'Nothing, '[ '(WVertex, 'Vk.VtxInp.RateVertex)],
			'[ '(0, Pos), '(1, Cglm.Vec3)] )
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing '(sl, '[AtomUbo sdsl], '[]) sr '(sb, vs', ts', foo)
mkGraphicsPipelineCreateInfo' sce rp ppllyt = Vk.Ppl.Graphics.CreateInfo {
	Vk.Ppl.Graphics.createInfoNext = TMaybe.N,
	Vk.Ppl.Graphics.createInfoFlags = Vk.Ppl.CreateFlagsZero,
	Vk.Ppl.Graphics.createInfoStages = shaderStages,
	Vk.Ppl.Graphics.createInfoVertexInputState = Just $ U3 def,
	Vk.Ppl.Graphics.createInfoInputAssemblyState = Just inputAssembly,
	Vk.Ppl.Graphics.createInfoViewportState = Just $ mkViewportState sce,
	Vk.Ppl.Graphics.createInfoRasterizationState = Just rasterizer,
	Vk.Ppl.Graphics.createInfoMultisampleState = Just multisampling,
	Vk.Ppl.Graphics.createInfoDepthStencilState = Nothing,
	Vk.Ppl.Graphics.createInfoColorBlendState = Just colorBlending,
	Vk.Ppl.Graphics.createInfoDynamicState = Nothing,
	Vk.Ppl.Graphics.createInfoLayout = U3 ppllyt,
	Vk.Ppl.Graphics.createInfoRenderPass = rp,
	Vk.Ppl.Graphics.createInfoSubpass = 0,
	Vk.Ppl.Graphics.createInfoBasePipelineHandle = Nothing,
	Vk.Ppl.Graphics.createInfoBasePipelineIndex = - 1,
	Vk.Ppl.Graphics.createInfoTessellationState = Nothing }

shaderStages :: HPList.PL (U5 Vk.Ppl.ShdrSt.CreateInfo) '[
	'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
	'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
shaderStages = U5 vertShaderStageInfo :** U5 fragShaderStageInfo :** HPList.Nil
	where
	vertShaderStageInfo = Vk.Ppl.ShdrSt.CreateInfo {
		Vk.Ppl.ShdrSt.createInfoNext = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlags = def,
		Vk.Ppl.ShdrSt.createInfoStage = Vk.ShaderStageVertexBit,
		Vk.Ppl.ShdrSt.createInfoModule = (
			shaderModuleCreateInfo glslVertexShaderMain, nil ),
		Vk.Ppl.ShdrSt.createInfoName = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfo = Nothing }
	fragShaderStageInfo = Vk.Ppl.ShdrSt.CreateInfo {
		Vk.Ppl.ShdrSt.createInfoNext = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlags = def,
		Vk.Ppl.ShdrSt.createInfoStage = Vk.ShaderStageFragmentBit,
		Vk.Ppl.ShdrSt.createInfoModule = (
			shaderModuleCreateInfo glslFragmentShaderMain, nil ),
		Vk.Ppl.ShdrSt.createInfoName = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfo = Nothing }

inputAssembly :: Vk.Ppl.InpAsmbSt.CreateInfo 'Nothing
inputAssembly = Vk.Ppl.InpAsmbSt.CreateInfo {
	Vk.Ppl.InpAsmbSt.createInfoNext = TMaybe.N,
	Vk.Ppl.InpAsmbSt.createInfoFlags = zeroBits,
	Vk.Ppl.InpAsmbSt.createInfoTopology = Vk.PrimitiveTopologyTriangleList,
	Vk.Ppl.InpAsmbSt.createInfoPrimitiveRestartEnable = False }

mkViewportState :: Vk.Extent2d -> Vk.Ppl.ViewportSt.CreateInfo 'Nothing
mkViewportState sce = Vk.Ppl.ViewportSt.CreateInfo {
	Vk.Ppl.ViewportSt.createInfoNext = TMaybe.N,
	Vk.Ppl.ViewportSt.createInfoFlags = zeroBits,
	Vk.Ppl.ViewportSt.createInfoViewports = [viewport],
	Vk.Ppl.ViewportSt.createInfoScissors = [scissor] }
	where
	viewport = Vk.Viewport {
		Vk.viewportX = 0, Vk.viewportY = 0,
		Vk.viewportWidth = fromIntegral $ Vk.extent2dWidth sce,
		Vk.viewportHeight = fromIntegral $ Vk.extent2dHeight sce,
		Vk.viewportMinDepth = 0, Vk.viewportMaxDepth = 1 }
	scissor = Vk.Rect2d {
		Vk.rect2dOffset = Vk.Offset2d 0 0, Vk.rect2dExtent = sce }

rasterizer :: Vk.Ppl.RstSt.CreateInfo 'Nothing
rasterizer = Vk.Ppl.RstSt.CreateInfo {
	Vk.Ppl.RstSt.createInfoNext = TMaybe.N,
	Vk.Ppl.RstSt.createInfoFlags = zeroBits,
	Vk.Ppl.RstSt.createInfoDepthClampEnable = False,
	Vk.Ppl.RstSt.createInfoRasterizerDiscardEnable = False,
	Vk.Ppl.RstSt.createInfoPolygonMode = Vk.PolygonModeFill,
	Vk.Ppl.RstSt.createInfoLineWidth = 1,
	Vk.Ppl.RstSt.createInfoCullMode = Vk.CullModeBackBit,
	Vk.Ppl.RstSt.createInfoFrontFace = Vk.FrontFaceCounterClockwise,
	Vk.Ppl.RstSt.createInfoDepthBiasEnable = False,
	Vk.Ppl.RstSt.createInfoDepthBiasConstantFactor = 0,
	Vk.Ppl.RstSt.createInfoDepthBiasClamp = 0,
	Vk.Ppl.RstSt.createInfoDepthBiasSlopeFactor = 0 }

multisampling :: Vk.Ppl.MltSmplSt.CreateInfo 'Nothing
multisampling = Vk.Ppl.MltSmplSt.CreateInfo {
	Vk.Ppl.MltSmplSt.createInfoNext = TMaybe.N,
	Vk.Ppl.MltSmplSt.createInfoFlags = zeroBits,
	Vk.Ppl.MltSmplSt.createInfoSampleShadingEnable = False,
	Vk.Ppl.MltSmplSt.createInfoRasterizationSamplesAndMask =
		Vk.Sample.CountAndMask Vk.Sample.Count1Bit Nothing,
	Vk.Ppl.MltSmplSt.createInfoMinSampleShading = 1,
	Vk.Ppl.MltSmplSt.createInfoAlphaToCoverageEnable = False,
	Vk.Ppl.MltSmplSt.createInfoAlphaToOneEnable = False }

colorBlending :: Vk.Ppl.ClrBlndSt.CreateInfo 'Nothing
colorBlending = Vk.Ppl.ClrBlndSt.CreateInfo {
	Vk.Ppl.ClrBlndSt.createInfoNext = TMaybe.N,
	Vk.Ppl.ClrBlndSt.createInfoFlags = zeroBits,
	Vk.Ppl.ClrBlndSt.createInfoLogicOpEnable = False,
	Vk.Ppl.ClrBlndSt.createInfoLogicOp = Vk.LogicOpCopy,
	Vk.Ppl.ClrBlndSt.createInfoAttachments = [colorBlendAttachment],
	Vk.Ppl.ClrBlndSt.createInfoBlendConstants =
		fromJust $ rgbaDouble 0 0 0 0 }

colorBlendAttachment :: Vk.Ppl.ClrBlndAtt.State
colorBlendAttachment = Vk.Ppl.ClrBlndAtt.State {
	Vk.Ppl.ClrBlndAtt.stateColorWriteMask =
		Vk.ClrCmp.RBit .|. Vk.ClrCmp.GBit .|.
		Vk.ClrCmp.BBit .|. Vk.ClrCmp.ABit,
	Vk.Ppl.ClrBlndAtt.stateBlendEnable = False,
	Vk.Ppl.ClrBlndAtt.stateSrcColorBlendFactor = Vk.BlendFactorOne,
	Vk.Ppl.ClrBlndAtt.stateDstColorBlendFactor = Vk.BlendFactorZero,
	Vk.Ppl.ClrBlndAtt.stateColorBlendOp = Vk.BlendOpAdd,
	Vk.Ppl.ClrBlndAtt.stateSrcAlphaBlendFactor = Vk.BlendFactorOne,
	Vk.Ppl.ClrBlndAtt.stateDstAlphaBlendFactor = Vk.BlendFactorZero,
	Vk.Ppl.ClrBlndAtt.stateAlphaBlendOp = Vk.BlendOpAdd }

createFramebuffers :: Vk.Dvc.D sd -> Vk.Extent2d ->
	Vk.RndrPss.R sr -> HPList.PL (Vk.ImgVw.I nm fmt) sis ->
	(forall sfs . RecreateFramebuffers sis sfs =>
		HPList.PL Vk.Frmbffr.F sfs -> IO a) -> IO a
createFramebuffers _ _ _ HPList.Nil f = f HPList.Nil
createFramebuffers dvc sce rp (iv :** ivs) f =
	Vk.Frmbffr.create dvc (mkFramebufferCreateInfoNew sce rp iv) nil \fb ->
	createFramebuffers dvc sce rp ivs \fbs -> f (fb :** fbs)

class RecreateFramebuffers (sis :: [Type]) (sfs :: [Type]) where
	recreateFramebuffers :: Vk.Dvc.D sd -> Vk.Extent2d ->
		Vk.RndrPss.R sr -> HPList.PL (Vk.ImgVw.I nm fmt) sis ->
		HPList.PL Vk.Frmbffr.F sfs -> IO ()

instance RecreateFramebuffers '[] '[] where
	recreateFramebuffers _dvc _sce _rp HPList.Nil HPList.Nil = pure ()

instance RecreateFramebuffers sis sfs =>
	RecreateFramebuffers (si ': sis) (sf ': sfs) where
	recreateFramebuffers dvc sce rp (sciv :** scivs) (fb :** fbs) =
		Vk.Frmbffr.unsafeRecreate dvc
			(mkFramebufferCreateInfoNew sce rp sciv) nil fb >>
		recreateFramebuffers dvc sce rp scivs fbs

mkFramebufferCreateInfoNew ::
	Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.ImgVw.I nm fmt si ->
	Vk.Frmbffr.CreateInfo 'Nothing sr '[ '(nm, fmt, si)]
mkFramebufferCreateInfoNew sce rp attch = Vk.Frmbffr.CreateInfo {
	Vk.Frmbffr.createInfoNext = TMaybe.N,
	Vk.Frmbffr.createInfoFlags = zeroBits,
	Vk.Frmbffr.createInfoRenderPass = rp,
	Vk.Frmbffr.createInfoAttachments = U3 attch :** HPList.Nil,
	Vk.Frmbffr.createInfoWidth = w, Vk.Frmbffr.createInfoHeight = h,
	Vk.Frmbffr.createInfoLayers = 1 }
	where
	Vk.Extent2d { Vk.extent2dWidth = w, Vk.extent2dHeight = h } = sce

createCmdPl :: PhDvc.QFamIndices -> Vk.Dvc.D sd ->
	(forall sc . Vk.CmdPl.C sc -> IO a) -> IO a
createCmdPl qfis dvc f =
	Vk.CmdPl.create dvc poolInfo nil \cp -> f cp
	where poolInfo = Vk.CmdPl.CreateInfo {
		Vk.CmdPl.createInfoNext = TMaybe.N,
		Vk.CmdPl.createInfoFlags =
			Vk.CmdPl.CreateResetCommandBufferBit,
		Vk.CmdPl.createInfoQueueFamilyIndex = PhDvc.grFam qfis }

createVtxBffr :: (IsSequence lst, Element lst ~ WVertex) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> lst ->
	(forall sm sb al . KnownNat al => Vk.Bffr.Binded sm sb bnm
		'[Obj.List al WVertex lnm] -> IO a) -> IO a
createVtxBffr = createBffrMem Vk.Bffr.UsageVertexBufferBit

createIdxBffr :: (IsSequence lst, Storable' (Element lst)) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> lst ->
	(forall sm sb al . KnownNat al =>
		Vk.Bffr.Binded sm sb nm '[Obj.List al (Element lst) lnm] ->
		IO a) -> IO a
createIdxBffr pd dv gq cp is a = createBffrMem Vk.Bffr.UsageIndexBufferBit
	pd dv gq cp is \ib -> a ib

createBffrMem :: forall sd sc lst nm lnm a .
	(IsSequence lst, Storable' (Element lst)) =>
	Vk.Bffr.UsageFlags -> Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q ->
	Vk.CmdPl.C sc -> lst ->
	(forall sm sb al . KnownNat al => Vk.Bffr.Binded sm sb nm
		'[Obj.List al (Element lst) lnm] -> IO a) -> IO a
createBffrMem us pd dv gq cp xs@(fromIntegral . olength -> ln) f =
	bffrAlgn @(Obj.List 256 (Element lst) lnm) dv (Obj.LengthList ln)
		(Vk.Bffr.UsageTransferDstBit .|. us) \(_ :: Proxy al) ->
	createBffrLst pd dv ln (Vk.Bffr.UsageTransferDstBit .|. us)
		Vk.Mm.PropertyDeviceLocalBit \b _ -> do
		createBffrLst pd dv ln
			Vk.Bffr.UsageTransferSrcBit (
			Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit ) \
			(b' :: Vk.Bffr.Binded sm sb bnm' '[Obj.List al t lnm'])
			bm' -> do
			Vk.Mm.write
				@bnm' @(Obj.List al t lnm') @0 dv bm' zeroBits xs
			copy b' b
		f b
	where
	copy :: forall sm sb bnm sm' sb' bnm' al . KnownNat al =>
		Vk.Bffr.Binded sm sb bnm '[Obj.List al (Element lst) lnm] ->
		Vk.Bffr.Binded sm' sb' bnm' '[Obj.List al (Element lst) lnm] -> IO ()
	copy s d = singleTimeCmds dv gq cp \cb ->
		Vk.Cmd.copyBuffer @'[ '( '[Obj.List al (Element lst) lnm], 0, 0)] cb s d

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

bffrAlgn :: forall o sd a . Obj.SizeAlignment o =>
	Vk.Dvc.D sd -> Obj.Length o -> Vk.Bffr.UsageFlags ->
	(forall al . KnownNat al => Proxy al -> IO a) -> IO a
bffrAlgn dv ln us f = Vk.Bffr.create dv (bffrInfo ln us) nil \b ->
	(\(SomeNat p) -> f p) . someNatVal . fromIntegral =<<
	Vk.Mm.requirementsAlignment <$> Vk.Bffr.getMemoryRequirements dv b

createBffrLst :: forall al sd bnm lnm t a . (KnownNat al, Storable t) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Dvc.Size -> Vk.Bffr.UsageFlags ->
	Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb bnm '[Obj.List al t lnm] ->
		Vk.Mm.M sm
			'[ '(sb, 'Vk.Mm.BufferArg bnm '[Obj.List al t lnm])] ->
		IO a) -> IO a
createBffrLst p dv ln = createBffr p dv $ Obj.LengthList ln

createBffr :: forall sd bnm o a . Obj.SizeAlignment o =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Obj.Length o ->
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

bffrInfo ::
	Obj.Length o -> Vk.Bffr.UsageFlags -> Vk.Bffr.CreateInfo 'Nothing '[o]
bffrInfo ln us = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N, Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths = HPList.Singleton ln,
	Vk.Bffr.createInfoUsage = us,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

createUniformBuffer :: Vk.Phd.P -> Vk.Dvc.D sd -> (forall sm sb .
		Vk.Bffr.Binded sm sb "uniform-buffer" '[Obj.Atom 256 UniformBufferObject 'Nothing]  ->
		UniformBufferMemory sm sb ->
		IO b) -> IO b
createUniformBuffer phdvc dvc = createBufferAtom' phdvc dvc
	Vk.Bffr.UsageUniformBufferBit
	(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit)

type UniformBufferMemory sm sb = Vk.Mm.M sm '[ '(
	sb,
	'Vk.Mm.BufferArg "uniform-buffer" '[Obj.Atom 256 UniformBufferObject 'Nothing]
	)]

createDscPl :: Vk.Dvc.D sd -> (forall sp . Vk.DscPl.P sp -> IO a) -> IO a
createDscPl dv = Vk.DscPl.create dv info nil
	where
	info = Vk.DscPl.CreateInfo {
		Vk.DscPl.createInfoNext = TMaybe.N,
		Vk.DscPl.createInfoFlags = Vk.DscPl.CreateFreeDescriptorSetBit,
		Vk.DscPl.createInfoMaxSets = 1,
		Vk.DscPl.createInfoPoolSizes = [sz] }
	sz = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeUniformBuffer,
		Vk.DscPl.sizeDescriptorCount = 1 }

createDescriptorSet ::
	Vk.Dvc.D sd -> Vk.DscPl.P sp -> Vk.Bffr.Binded sm sb nm '[Obj.Atom 256 UniformBufferObject 'Nothing] ->
	Vk.DscSetLyt.D sdsc '[ 'Vk.DscSetLyt.Buffer '[Obj.Atom 256 UniformBufferObject 'Nothing]] ->
	(forall sds .
		Vk.DscSet.D sds '(sdsc, '[ 'Vk.DscSetLyt.Buffer '[Obj.Atom 256 UniformBufferObject 'Nothing]]) -> IO a) -> IO a
createDescriptorSet dvc dscp ub dscslyt f =
	Vk.DscSet.allocateDs dvc allocInfo \(HPList.Singleton dscs) -> do
	Vk.DscSet.updateDs dvc
		(HPList.Singleton . U5 $ descriptorWrite ub dscs) HPList.Nil
	f dscs
	where
	allocInfo = Vk.DscSet.AllocateInfo {
		Vk.DscSet.allocateInfoNext = TMaybe.N,
		Vk.DscSet.allocateInfoDescriptorPool = dscp,
		Vk.DscSet.allocateInfoSetLayouts =
			HPList.Singleton $ U2 dscslyt }

descriptorWrite ::
	Vk.Bffr.Binded sm sb nm '[Obj.Atom 256 UniformBufferObject 'Nothing] ->
	Vk.DscSet.D sds slbts ->
	Vk.DscSet.Write 'Nothing sds slbts ('Vk.DscSet.WriteSourcesArgBuffer '[ '(
		sm, sb, nm, Obj.Atom 256 UniformBufferObject 'Nothing, 0 )]) 0
descriptorWrite ub dscs = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = dscs,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeUniformBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		HPList.Singleton bufferInfo }
	where bufferInfo = U5 $ Vk.Dsc.BufferInfo ub

createBufferAtom' :: forall sd nm a b . Storable a => Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> (
		forall sm sb .
		Vk.Bffr.Binded sm sb nm '[Obj.Atom 256 a 'Nothing] ->
		Vk.Mm.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg nm '[Obj.Atom 256 a 'Nothing] )] ->
			IO b) -> IO b
createBufferAtom' p dv usg props = createBuffer' p dv Obj.LengthAtom usg props

createBuffer' :: forall sd nm o a . Obj.SizeAlignment o =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Obj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[o] ->
		Vk.Mm.M sm
			'[ '(sb, 'Vk.Mm.BufferArg nm '[o])] ->
		IO a) -> IO a
createBuffer' p dv ln usg props f = Vk.Bffr.create dv binfo nil \b -> do
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMemoryType p (Vk.Mm.M.requirementsMemoryTypeBits reqs) props
	Vk.Mm.allocateBind dv (HPList.Singleton . U2 $ Vk.Mm.Buffer b)
		(allcInfo mt) nil
		$ f . \(HPList.Singleton (U2 (Vk.Mm.BufferBinded bnd))) -> bnd
	where
	binfo :: Vk.Bffr.CreateInfo 'Nothing '[o]
	binfo = Vk.Bffr.CreateInfo {
		Vk.Bffr.createInfoNext = TMaybe.N,
		Vk.Bffr.createInfoFlags = zeroBits,
		Vk.Bffr.createInfoLengths = HPList.Singleton ln,
		Vk.Bffr.createInfoUsage = usg,
		Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Bffr.createInfoQueueFamilyIndices = [] }
	allcInfo :: Vk.Mm.M.TypeIndex -> Vk.Mm.AllocateInfo 'Nothing
	allcInfo mt = Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mt }

findMemoryType :: Vk.Phd.P -> Vk.Mm.M.TypeBits -> Vk.Mm.PropertyFlags ->
	IO Vk.Mm.M.TypeIndex
findMemoryType phdvc flt props =
	fromMaybe (error msg) . suitable <$> Vk.Phd.getMemoryProperties phdvc
	where
	msg = "failed to find suitable memory type!"
	suitable props1 = fst <$> L.find ((&&)
		<$> (`Vk.Mm.M.elemTypeIndex` flt) . fst
		<*> checkBits props . Vk.Mm.M.mTypePropertyFlags . snd) tps
		where tps = Vk.Phd.memoryPropertiesMemoryTypes props1

cmdBffrInfo :: forall n scp .
	Vk.CmdPl.C scp -> Vk.CBffr.AllocateInfo 'Nothing scp n
cmdBffrInfo cp = Vk.CBffr.AllocateInfo {
	Vk.CBffr.allocateInfoNext = TMaybe.N,
	Vk.CBffr.allocateInfoCommandPool = cp,
	Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }

data SyncObjects (ssos :: (Type, Type, Type)) where
	SyncObjects :: {
		_imageAvailableSemaphores :: Vk.Semaphore.S sias,
		_renderFinishedSemaphores :: Vk.Semaphore.S srfs,
		_inFlightFences :: Vk.Fence.F sfs } ->
		SyncObjects '(sias, srfs, sfs)

createSyncObjects ::
	Vk.Dvc.D sd -> (forall sias srfs siff . SyncObjects '(sias, srfs, siff) -> IO a ) -> IO a
createSyncObjects dvc f =
	Vk.Semaphore.create @'Nothing dvc def nil \ias ->
	Vk.Semaphore.create @'Nothing dvc def nil \rfs ->
	Vk.Fence.create @'Nothing dvc fncInfo nil \iff ->
	f $ SyncObjects ias rfs iff
	where
	fncInfo = def { Vk.Fence.createInfoFlags = Vk.Fence.CreateSignaledBit }

recordCommandBuffer :: forall scb sr sf sl sg sm sb nm sm' sb' nm' sdsl sds alv ali .
	(KnownNat alv, KnownNat ali) =>
	Vk.CBffr.C scb ->
	Vk.RndrPss.R sr -> Vk.Frmbffr.F sf -> Vk.Extent2d ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Cglm.Vec3)] '(sl, '[AtomUbo sdsl], '[]) ->
	Vk.Bffr.Binded sm sb nm '[Obj.List alv WVertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[Obj.List ali Word16 ""] ->
	Vk.DscSet.D sds (AtomUbo sdsl) ->
	IO ()
recordCommandBuffer cb rp fb sce ppllyt gpl vb ib ubds =
	Vk.CBffr.begin @'Nothing @'Nothing cb def $
	Vk.Cmd.beginRenderPass cb rpInfo Vk.Subpass.ContentsInline $
	Vk.Cmd.bindPipelineGraphics cb Vk.Ppl.BindPointGraphics gpl \cbb ->
	Vk.Cmd.bindVertexBuffers cbb
		(HPList.Singleton . U5 $ Vk.Bffr.IndexedForList @_ @_ @_ @WVertex @"" vb) >>
	Vk.Cmd.bindIndexBuffer cbb (Vk.Bffr.IndexedForList @_ @_ @_ @Word16 @"" ib) >>
	Vk.Cmd.bindDescriptorSetsGraphics cbb Vk.Ppl.BindPointGraphics ppllyt
		(HPList.Singleton $ U2 ubds)
		(HPList.Singleton (
			HPList.Nil :**
			HPList.Nil )) >>
	Vk.Cmd.drawIndexed cbb (fromIntegral $ length idcs) 1 0 0 0
	where
	rpInfo :: Vk.RndrPss.BeginInfo 'Nothing sr sf
		'[ 'Vk.ClearTypeColor 'Vk.ClearColorTypeFloat32]
	rpInfo = Vk.RndrPss.BeginInfo {
		Vk.RndrPss.beginInfoNext = TMaybe.N,
		Vk.RndrPss.beginInfoRenderPass = rp,
		Vk.RndrPss.beginInfoFramebuffer = fb,
		Vk.RndrPss.beginInfoRenderArea = Vk.Rect2d {
			Vk.rect2dOffset = Vk.Offset2d 0 0,
			Vk.rect2dExtent = sce },
		Vk.RndrPss.beginInfoClearValues = HPList.Singleton
			. Vk.ClearValueColor . fromJust $ rgbaDouble 0 0 0 1 }

mainLoop :: (KnownNat alv, KnownNat ali, RecreateFramebuffers ss sfs, Vk.T.FormatToValue scfmt) =>
	FramebufferResized ->
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> PhDvc.QFamIndices -> Vk.Dvc.D sd ->
	Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d -> HPList.PL (Vk.ImgVw.I nm scfmt) ss ->
	Vk.RndrPss.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] -> Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Cglm.Vec3)]
		'(sl, '[AtomUbo sdsl], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[Obj.List alv WVertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[Obj.List ali Word16 ""] ->
	UniformBufferMemory sm2 sb2 ->
	Vk.DscSet.D sds (AtomUbo sdsl) ->
	Vk.CBffr.C scb ->
	SyncObjects '(sias, srfs, siff) ->
	ControllerEvent -> IO ()
mainLoop g w sfc phdvc qfis dvc gq pq sc ext0 scivs rp ppllyt gpl fbs vb ib ubm ubds cb iasrfsifs cev = do
	($ ext0) $ fix \loop ext -> do
		Glfw.pollEvents
--		tm <- getCurrentTime
		lx <- readIORef $ controllerEventLeftX cev
		ly <- readIORef $ controllerEventLeftY cev
		runLoop w sfc phdvc qfis dvc gq pq
			sc g ext scivs rp ppllyt gpl fbs vb ib ubm ubds cb iasrfsifs
			(lx / 100, ly / 100) cev loop
--			(realToFrac $ tm `diffUTCTime` tm0) cev loop
	Vk.Dvc.waitIdle dvc

runLoop :: (KnownNat alv, KnownNat ali, RecreateFramebuffers sis sfs, Vk.T.FormatToValue scfmt) =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	PhDvc.QFamIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> FramebufferResized -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPss.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Cglm.Vec3)]
		'(sl, '[AtomUbo sdsl], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[Obj.List alv WVertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[Obj.List ali Word16 ""] ->
	UniformBufferMemory sm2 sb2 ->
	Vk.DscSet.D sds (AtomUbo sdsl) ->
	Vk.CBffr.C scb ->
	SyncObjects '(sias, srfs, siff) -> (Float, Float) -> ControllerEvent ->
	(Vk.Extent2d -> IO ()) -> IO ()
runLoop w@(GlfwG.Win.W win) sfc phdvc qfis dvc gq pq sc frszd ext scivs rp ppllyt gpl fbs vb ib ubm ubds cb iasrfsifs tm cev loop = do
	catchAndRecreate w sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs loop
		$ drawFrame dvc gq pq sc ext rp ppllyt gpl fbs vb ib ubm ubds cb iasrfsifs tm
	cls <- Glfw.windowShouldClose win
	ab <- readIORef $ controllerEventButtonAEver cev
	if (cls || ab)
	then writeIORef (controllerEventFinished cev) True
	else checkFlag frszd >>= bool (loop ext)
		(loop =<< recreateSwapChainEtc
			w sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs)

drawFrame :: forall sfs sd ssc sr sl sg sm sb nm sm' sb' nm' sm2 sb2 scb sias srfs siff sdsl scfmt sds alv ali .
	(KnownNat alv, KnownNat ali) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q -> Vk.Khr.Swpch.S scfmt ssc ->
	Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Cglm.Vec3)]
		'(sl, '[AtomUbo sdsl], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[Obj.List alv WVertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[Obj.List ali Word16 ""] ->
	UniformBufferMemory sm2 sb2 ->
	Vk.DscSet.D sds (AtomUbo sdsl) ->
	Vk.CBffr.C scb -> SyncObjects '(sias, srfs, siff) -> (Float, Float) -> IO ()
drawFrame dvc gq pq sc ext rp ppllyt gpl fbs vb ib ubm ubds cb (SyncObjects ias rfs iff) tm = do
	let	siff = HPList.Singleton iff
	Vk.Fence.waitForFs dvc siff True Nothing
	imgIdx <- Vk.Khr.acquireNextImageResult [Vk.Success, Vk.SuboptimalKhr]
		dvc sc maxBound (Just ias) Nothing
	Vk.Fence.resetFs dvc siff
	Vk.CBffr.reset cb def
	HPList.index fbs imgIdx \fb ->
		recordCommandBuffer cb rp fb ext ppllyt gpl vb ib ubds
	updateUniformBuffer dvc ubm ext tm
	let	submitInfo :: Vk.SubmitInfo 'Nothing '[sias] '[scb] '[srfs]
		submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = TMaybe.N,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Singleton
				$ Vk.SemaphorePipelineStageFlags ias
					Vk.Ppl.StageColorAttachmentOutputBit,
			Vk.submitInfoCommandBuffers = HPList.Singleton cb,
			Vk.submitInfoSignalSemaphores = HPList.Singleton rfs }
		presentInfo = Vk.Khr.PresentInfo {
			Vk.Khr.presentInfoNext = TMaybe.N,
			Vk.Khr.presentInfoWaitSemaphores = HPList.Singleton rfs,
			Vk.Khr.presentInfoSwapchainImageIndices = HPList.Singleton
				$ Vk.Khr.SwapchainImageIndex sc imgIdx }
	Vk.Q.submit gq (HPList.Singleton $ U4 submitInfo) $ Just iff
	catchAndSerialize $ Vk.Khr.queuePresent @'Nothing pq presentInfo

updateUniformBuffer :: Vk.Dvc.D sd ->
	UniformBufferMemory sm2 sb2 -> Vk.Extent2d -> (Float, Float) -> IO ()
updateUniformBuffer dvc um sce (tm, tm') = do
	Vk.Mm.write @"uniform-buffer" @(Obj.Atom 256 UniformBufferObject 'Nothing) @0
		dvc um zeroBits ubo
	where ubo = UniformBufferObject {
		uniformBufferObjectModel =
			Cglm.rotate
				Cglm.mat4Identity
				(tm' * Cglm.rad 90)
				(Cglm.Vec3 $ 0 :. 1 :. 0 :. NilL)
			`Cglm.mat4Mul`
			Cglm.rotate
				Cglm.mat4Identity
				(- tm * Cglm.rad 90)
				(Cglm.Vec3 $ 1 :. 0 :. 0 :. NilL),
		uniformBufferObjectView = Cglm.lookat
			(Cglm.Vec3 $ 5 :. 0 :. 0 :. NilL)
			(Cglm.Vec3 $ 0 :. 0 :. 0 :. NilL)
			(Cglm.Vec3 $ 0 :. 0 :. 1 :. NilL),
		uniformBufferObjectProj = Cglm.modifyMat4 1 1 negate
			$ Cglm.perspective
				(Cglm.rad 45)
				(fromIntegral (Vk.extent2dWidth sce) /
					fromIntegral (Vk.extent2dHeight sce))
				0.1 10 }

catchAndSerialize :: IO () -> IO ()
catchAndSerialize =
	(`catch` \(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

catchAndRecreate :: (RecreateFramebuffers sis sfs, Vk.T.FormatToValue scfmt) =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> PhDvc.QFamIndices -> Vk.Dvc.D sd ->
	Vk.Khr.Swpch.S scfmt ssc ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPss.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Cglm.Vec3)]
		'(sl, '[AtomUbo sdsl], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	(Vk.Extent2d -> IO ()) -> IO () -> IO ()
catchAndRecreate w sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs loop act =
	catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just ()
		Vk.SuboptimalKhr -> Just ()
		_ -> Nothing)
	act
	\_ -> loop =<< recreateSwapChainEtc
		w sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs

recreateSwapChainEtc :: (
	RecreateFramebuffers sis sfs, Vk.T.FormatToValue scfmt ) =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> PhDvc.QFamIndices -> Vk.Dvc.D sd ->
	Vk.Khr.Swpch.S scfmt ssc ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPss.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Cglm.Vec3)]
		'(sl, '[AtomUbo sdsl], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs -> IO Vk.Extent2d
recreateSwapChainEtc w@(GlfwG.Win.W win) sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs = do
	waitFramebufferSize win
	Vk.Dvc.waitIdle dvc

	ext <- recreateSwpch w sfc phdvc qfis dvc sc
	ext <$ do
		Vk.Khr.Swpch.getImages dvc sc >>= \imgs ->
			recreateImageViewsNew dvc imgs scivs
		recreateGraphicsPipeline' dvc ext rp ppllyt gpl
		recreateFramebuffers dvc ext rp scivs fbs

waitFramebufferSize :: Glfw.Window -> IO ()
waitFramebufferSize win = Glfw.getFramebufferSize win >>= \sz ->
	when (zero sz) $ fix \loop -> (`when` loop) . zero =<<
		Glfw.waitEvents *> Glfw.getFramebufferSize win
	where zero = uncurry (||) . ((== 0) *** (== 0))

type WVertex = GStorable.W Vertex

data Vertex = Vertex { vertexPos :: Pos, vertexColor :: Cglm.Vec3 }
	deriving (Show, Generic)

newtype Pos = Pos Cglm.Vec3 deriving (Show, Storable, Vk.Ppl.VtxInpSt.Formattable)

instance Storable Vertex where
	sizeOf = Foreign.Storable.Generic.gSizeOf
	alignment = Foreign.Storable.Generic.gAlignment
	peek = Foreign.Storable.Generic.gPeek
	poke = Foreign.Storable.Generic.gPoke

instance Foreign.Storable.Generic.G Vertex where

vtcs :: [WVertex]
vtcs = GStorable.W <$> [
	Vertex (Pos . Cglm.Vec3 $ 0.5 :. 0 :. 0 :. NilL)
		(Cglm.Vec3 $ 1.0 :. 0.0 :. 0.0 :. NilL),
	Vertex (Pos . Cglm.Vec3 $ (- 0.5) :. (- 0.5) :. 0 :. NilL)
		(Cglm.Vec3 $ 1.0 :. 0.0 :. 0.0 :. NilL),
	Vertex (Pos . Cglm.Vec3 $ (- 0.5) :. 0.5 :. 0 :. NilL)
		(Cglm.Vec3 $ 1.0 :. 0.0 :. 0.0 :. NilL),

	Vertex (Pos . Cglm.Vec3 $ (- 0.5) :. (- 0.5) :. 0 :. NilL)
		(Cglm.Vec3 $ 0.0 :. 1.0 :. 0.0 :. NilL),
	Vertex (Pos . Cglm.Vec3 $ (- 0.5) :. 0.5 :. 0 :. NilL)
		(Cglm.Vec3 $ 0.0 :. 1.0 :. 0.0 :. NilL),
	Vertex (Pos . Cglm.Vec3 $ (- 0.5) :. 0 :. 0.5 :. NilL)
		(Cglm.Vec3 $ 0.0 :. 1.0 :. 0.0 :. NilL),

	Vertex (Pos . Cglm.Vec3 $ 0.5 :. 0 :. 0 :. NilL)
		(Cglm.Vec3 $ 0.0 :. 0.0 :. 1.0 :. NilL),
	Vertex (Pos . Cglm.Vec3 $ (- 0.5) :. (- 0.5) :. 0 :. NilL)
		(Cglm.Vec3 $ 0.0 :. 0.0 :. 1.0 :. NilL),
	Vertex (Pos . Cglm.Vec3 $ (- 0.5) :. 0 :. 0.5 :. NilL)
		(Cglm.Vec3 $ 0.0 :. 0.0 :. 1.0 :. NilL),

	Vertex (Pos . Cglm.Vec3 $ 0.5 :. 0 :. 0 :. NilL)
		(Cglm.Vec3 $ 1.0 :. 1.0 :. 0.0 :. NilL),
	Vertex (Pos . Cglm.Vec3 $ (- 0.5) :. 0.5 :. 0 :. NilL)
		(Cglm.Vec3 $ 1.0 :. 1.0 :. 0.0 :. NilL),
	Vertex (Pos . Cglm.Vec3 $ (- 0.5) :. 0 :. 0.5 :. NilL)
		(Cglm.Vec3 $ 1.0 :. 1.0 :. 0.0 :. NilL)

{-
	Vertex (Pos . Cglm.Vec3 $ (- 0.5) :. 0.5 :. 0.5 :. NilL)
		(Cglm.Vec3 $ 1.0 :. 0.0 :. 0.0 :. NilL),

	Vertex (Pos . Cglm.Vec3 $ (- 0.5) :. 0.5 :. 0.5 :. NilL)
		(Cglm.Vec3 $ 0.0 :. 1.0 :. 0.0 :. NilL),
	Vertex (Pos . Cglm.Vec3 $ (- 0.5) :. 0.5 :. (- 0.5) :. NilL)
		(Cglm.Vec3 $ 0.0 :. 1.0 :. 0.0 :. NilL),
	Vertex (Pos . Cglm.Vec3 $ 0.5 :. 0.5 :. (- 0.5) :. NilL)
		(Cglm.Vec3 $ 0.0 :. 1.0 :. 0.0 :. NilL),
	Vertex (Pos . Cglm.Vec3 $ 0.5 :. 0.5 :. 0.5 :. NilL)
		(Cglm.Vec3 $ 0.0 :. 1.0 :. 0.0 :. NilL),

	Vertex (Pos . Cglm.Vec3 $ (- 0.5) :. (- 0.5) :. 0.5 :. NilL)
		(Cglm.Vec3 $ 0.0 :. 0.0 :. 1.0 :. NilL),
	Vertex (Pos . Cglm.Vec3 $ (- 0.5) :. (- 0.5) :. (- 0.5) :. NilL)
		(Cglm.Vec3 $ 0.0 :. 0.0 :. 1.0 :. NilL),
	Vertex (Pos . Cglm.Vec3 $ (- 0.5) :. 0.5 :. (- 0.5) :. NilL)
		(Cglm.Vec3 $ 0.0 :. 0.0 :. 1.0 :. NilL),
	Vertex (Pos . Cglm.Vec3 $ (- 0.5) :. 0.5 :. 0.5 :. NilL)
		(Cglm.Vec3 $ 0.0 :. 0.0 :. 1.0 :. NilL),

	Vertex (Pos . Cglm.Vec3 $ (- 0.5) :. (- 0.5) :. 0.5 :. NilL)
		(Cglm.Vec3 $ 1.0 :. 1.0 :. 0.0 :. NilL),
	Vertex (Pos . Cglm.Vec3 $ (- 0.5) :. (- 0.5) :. (- 0.5) :. NilL)
		(Cglm.Vec3 $ 1.0 :. 1.0 :. 0.0 :. NilL),
	Vertex (Pos . Cglm.Vec3 $ 0.5 :. (- 0.5) :. (- 0.5) :. NilL)
		(Cglm.Vec3 $ 1.0 :. 1.0 :. 0.0 :. NilL),
	Vertex (Pos . Cglm.Vec3 $ 0.5 :. (- 0.5) :. 0.5 :. NilL)
		(Cglm.Vec3 $ 1.0 :. 1.0 :. 0.0 :. NilL),

	Vertex (Pos . Cglm.Vec3 $ (0.5) :. (- 0.5) :. 0.5 :. NilL)
		(Cglm.Vec3 $ 1.0 :. 0.0 :. 1.0 :. NilL),
	Vertex (Pos . Cglm.Vec3 $ (0.5) :. (- 0.5) :. (- 0.5) :. NilL)
		(Cglm.Vec3 $ 1.0 :. 0.0 :. 1.0 :. NilL),
	Vertex (Pos . Cglm.Vec3 $ (0.5) :. 0.5 :. (- 0.5) :. NilL)
		(Cglm.Vec3 $ 1.0 :. 0.0 :. 1.0 :. NilL),
	Vertex (Pos . Cglm.Vec3 $ (0.5) :. 0.5 :. 0.5 :. NilL)
		(Cglm.Vec3 $ 1.0 :. 0.0 :. 1.0 :. NilL),

	Vertex (Pos . Cglm.Vec3 $ (- 0.5) :. (- 0.5) :. (- 0.5) :. NilL)
		(Cglm.Vec3 $ 0.0 :. 1.0 :. 1.0 :. NilL),
	Vertex (Pos . Cglm.Vec3 $ 0.5 :. (- 0.5) :. (- 0.5) :. NilL)
		(Cglm.Vec3 $ 0.0 :. 1.0 :. 1.0 :. NilL),
	Vertex (Pos . Cglm.Vec3 $ 0.5 :. 0.5 :. (- 0.5) :. NilL)
		(Cglm.Vec3 $ 0.0 :. 1.0 :. 1.0 :. NilL),
	Vertex (Pos . Cglm.Vec3 $ (- 0.5) :. 0.5 :. (- 0.5) :. NilL)
		(Cglm.Vec3 $ 0.0 :. 1.0 :. 1.0 :. NilL)
		-}

		]

idcs :: [Word16]
idcs = [
	2, 1, 0, -- , 2, 3, 0,
	3, 4, 5,
	6, 7, 8,
	11, 10, 9
--	4, 5, 6, 6, 7, 4,
--	4, 7, 6, 6, 5, 4,
--	8, 9, 10, 10, 11, 8
--	8, 11, 10, 10, 9, 8,
--	12, 13, 14, 14, 15, 12,
--	16, 17, 18, 18, 19, 16,

--	20, 21, 22, 22, 23, 20,
--	20, 23, 22, 22, 21, 20
	]

data UniformBufferObject = UniformBufferObject {
	uniformBufferObjectModel :: Cglm.Mat4,
	uniformBufferObjectView :: Cglm.Mat4,
	uniformBufferObjectProj :: Cglm.Mat4 }
	deriving (Show, Generic)

instance Storable UniformBufferObject where
	sizeOf = Foreign.Storable.Generic.gSizeOf
	alignment = Foreign.Storable.Generic.gAlignment
	peek = Foreign.Storable.Generic.gPeek
	poke = Foreign.Storable.Generic.gPoke

instance Foreign.Storable.Generic.G UniformBufferObject

shaderModuleCreateInfo :: SpirV.S sknd -> Vk.ShaderModule.CreateInfo 'Nothing sknd
shaderModuleCreateInfo code = Vk.ShaderModule.CreateInfo {
	Vk.ShaderModule.createInfoNext = TMaybe.N,
	Vk.ShaderModule.createInfoFlags = def,
	Vk.ShaderModule.createInfoCode = code }

doWhile_ :: IO Bool -> IO ()
doWhile_ act = act >>= bool (pure ()) (doWhile_ act)

[glslVertexShader|

#version 450

layout(binding = 0) uniform UniformBufferObject {
	mat4 model;
	mat4 view;
	mat4 proj;
} ubo;

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inColor;

layout(location = 0) out vec3 fragColor;

void
main()
{
	gl_Position = ubo.proj * ubo.view * ubo.model * vec4(inPosition, 1.0);
	fragColor = inColor;
}

|]

[glslFragmentShader|

#version 450

layout(location = 0) in vec3 fragColor;

layout(location = 0) out vec4 outColor;

void
main()
{
	outColor = vec4(fragColor, 1.0);
}

|]
