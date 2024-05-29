{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Concurrent

import GHC.Generics
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Control.Arrow hiding (loop)
import Control.Monad
import Control.Monad.Fix
import Control.Exception
import Data.Kind
import Gpu.Vulkan.Object qualified as VObj
import Data.Default
import Data.Bits
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)

import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Proxy
import Data.Bool
import Data.Maybe
import Data.List qualified as L
import Data.IORef
import Data.List.Length
import Data.Word
import Data.Color
import Data.Time

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
import qualified Gpu.Vulkan.PhysicalDevice as Vk.PhDvc

import qualified Gpu.Vulkan.Device as Vk.Dvc
import qualified Gpu.Vulkan.Device as Vk.Dvc.M
import qualified Gpu.Vulkan.Khr.Surface as Vk.Khr.Surface
import qualified Gpu.Vulkan.Khr.Surface as Vk.Khr.Surface.M
import qualified Gpu.Vulkan.Khr.Surface.PhysicalDevice as
	Vk.Khr.Surface.PhysicalDevice
import qualified Gpu.Vulkan.Khr.Swapchain as Vk.Khr.Swapchain
import qualified Gpu.Vulkan.Image as Vk.Image
import qualified Gpu.Vulkan.Image as Vk.Image.M
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
import qualified Gpu.Vulkan.RenderPass as Vk.RndrPass
import qualified Gpu.Vulkan.RenderPass as Vk.RndrPass.M
import qualified Gpu.Vulkan.Pipeline.Graphics as Vk.Ppl.Graphics
import qualified Gpu.Vulkan.Framebuffer as Vk.Frmbffr
import qualified Gpu.Vulkan.CommandPool as Vk.CmdPool
import qualified Gpu.Vulkan.CommandBuffer as Vk.CmdBffr
import qualified Gpu.Vulkan.Semaphore as Vk.Semaphore
import qualified Gpu.Vulkan.Fence as Vk.Fence
import qualified Gpu.Vulkan.VertexInput as Vk.VtxInp
import qualified Gpu.Vulkan.Buffer as Vk.Bffr
import qualified Gpu.Vulkan.Memory as Vk.Mem.M
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.Memory as Vk.Mem
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscSetLyt
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPool
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSet

import Gpu.Vulkan.TypeEnum qualified as Vk.T

import Gpu.Vulkan.Pipeline.VertexInputState qualified as Vk.Ppl.VtxInpSt

import Graphics.SimplePolygon.DebugMessenger qualified as DbgMsngr
import Graphics.SimplePolygon.Instance qualified as Ist
import Graphics.SimplePolygon.Window qualified as Win
import Graphics.SimplePolygon.Surface qualified as Sfc
import Graphics.SimplePolygon.PhysicalDevice qualified as PhDvc

import Data.Ord.ToolsYj
import Data.Bits.ToolsYj
import Data.Bool.ToolsYj
import Data.IORef.ToolsYj

import Debug

import Graphics.UI.GlfwG.Window.Type qualified as GlfwG.Win

main :: IO ()
main = Win.create windowSize windowName \(Win.W w fr) ->
	Ist.create debug \ist ->
	createControllerEvent >>= \cev ->
	forkIO (controller cev) >>
	if debug
		then DbgMsngr.setup ist $ body w ist fr cev
		else body w ist fr cev

controller :: ControllerEvent -> IO ()
controller ev = do
	threadDelay 10000
	fn <- readIORef $ controllerEventFinished ev
	if fn then pure () else do
		Just (Glfw.GamepadState gb ga) <- Glfw.getGamepadState Glfw.Joystick'1
		modifyIORef (controllerEventLeftX ev) (+ ga Glfw.GamepadAxis'LeftX)
		modifyIORef (controllerEventLeftY ev) (+ ga Glfw.GamepadAxis'LeftY)
		when (gb Glfw.GamepadButton'A == Glfw.GamepadButtonState'Pressed)
			$ writeIORef (controllerEventButtonAEver ev) True
		controller ev

createControllerEvent :: IO ControllerEvent
createControllerEvent = do
	fn <- newIORef False
	ae <- newIORef False
	lx <- newIORef 0
	ly <- newIORef 0
	pure ControllerEvent {
		controllerEventFinished = fn,
		controllerEventButtonAEver = ae,
		controllerEventLeftX = lx,
		controllerEventLeftY = ly
		}

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
body (GlfwG.Win.W w) inst g cev =
	Sfc.create inst (GlfwG.Win.W w) \sfc ->
	pickPhysicalDevice inst sfc >>= \(pd, qfis) ->
	createLgDvc pd qfis \d gq pq ->
	createSwapChainNew w sfc pd qfis d
		\(sc :: Vk.Khr.Swapchain.S scifmt ss) ext ->
	Vk.Khr.Swapchain.getImages d sc >>= \imgs ->
	createImageViewsNew d imgs \scivs ->
	createRenderPassNew @scifmt d \rp ->
	createPipelineLayout' d \dscslyt ppllyt ->
	createGraphicsPipeline' d ext rp ppllyt \gpl ->
	createFramebuffers d ext rp scivs \fbs ->
	createCommandPool qfis d \cp ->
	createVertexBuffer pd d gq cp \vb ->
	createIndexBuffer pd d gq cp \ib ->
	createUniformBuffer pd d \ub ubm ->
	createDescriptorPool d \dscp ->
	createDescriptorSet d dscp ub dscslyt \ubds ->
	createCommandBuffer d cp \cb ->
	createSyncObjects d \sos ->
	getCurrentTime >>= \tm ->
	mainLoop g w sfc pd qfis d gq pq sc ext scivs rp ppllyt gpl fbs vb ib ubm ubds cb sos tm cev

pickPhysicalDevice :: Vk.Ist.I si ->
	Vk.Khr.Surface.S ss -> IO (Vk.PhDvc.P, PhDvc.QFamIndices)
pickPhysicalDevice ist sfc = PhDvc.enumerate ist sfc >>= \case 
	[] -> error "failed to find a sutable GPU!"
	PhDvc.P phd qfi : _ -> pure (phd, qfi)

deviceExtensions :: [Vk.PhDvc.ExtensionName]
deviceExtensions = [Vk.Khr.Swapchain.extensionName]

data SwapChainSupportDetails = SwapChainSupportDetails {
	capabilities :: Vk.Khr.Surface.M.Capabilities,
	formats :: [Vk.Khr.Surface.M.FormatOld],
	presentModes :: [Vk.Khr.PresentMode] }

querySwapChainSupport ::
	Vk.PhDvc.P -> Vk.Khr.Surface.S ss -> IO SwapChainSupportDetails
querySwapChainSupport dvc sfc = SwapChainSupportDetails
	<$> Vk.Khr.Surface.PhysicalDevice.getCapabilities dvc sfc
	<*> Vk.Khr.Surface.PhysicalDevice.getFormatsOld dvc sfc
	<*> Vk.Khr.Surface.PhysicalDevice.getPresentModes dvc sfc

createLgDvc :: Vk.PhDvc.P -> PhDvc.QFamIndices -> (forall sd .
		Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q -> IO a) -> IO a
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
					deviceExtensions,
				Vk.Dvc.M.createInfoEnabledFeatures = Just def }
		Vk.Dvc.create phdvc createInfo nil \dvc -> do
			gq <- Vk.Dvc.getQueue dvc (PhDvc.grFam qfis) 0
			pq <- Vk.Dvc.getQueue dvc (PhDvc.prFam qfis) 0
			f dvc gq pq

mkHeteroParList :: WithPoked (TMaybe.M s) => (a -> t s) -> [a] ->
	(forall ss . HeteroParList.ToListWithCM' WithPoked TMaybe.M ss => HeteroParList.PL t ss -> b) -> b
mkHeteroParList _k [] f = f HeteroParList.Nil
mkHeteroParList k (x : xs) f = mkHeteroParList k xs \xs' -> f (k x :** xs')

createSwapChainNew :: Glfw.Window -> Vk.Khr.Surface.S ssfc -> Vk.PhDvc.P ->
	PhDvc.QFamIndices -> Vk.Dvc.D sd ->
	(forall ss scfmt . Vk.T.FormatToValue scfmt =>
		Vk.Khr.Swapchain.S scfmt ss -> Vk.Extent2d -> IO a) ->
	IO a
createSwapChainNew win sfc phdvc qfis dvc f = do
	spp <- querySwapChainSupport phdvc sfc
	ext <- chooseSwapExtent win $ capabilities spp
	let	fmt = Vk.Khr.Surface.M.formatOldFormat
			. chooseSwapSurfaceFormat $ formats spp
	Vk.T.formatToType fmt \(_ :: Proxy fmt) -> do
		let	crInfo = mkSwapchainCreateInfoNew sfc qfis spp ext
		Vk.Khr.Swapchain.create @'Nothing @fmt dvc crInfo nil
			\sc -> f sc ext

mkSwapchainCreateInfoNew :: Vk.Khr.Surface.S ss -> PhDvc.QFamIndices ->
	SwapChainSupportDetails -> Vk.Extent2d ->
	Vk.Khr.Swapchain.CreateInfo 'Nothing ss fmt
mkSwapchainCreateInfoNew sfc qfis0 spp ext =
	Vk.Khr.Swapchain.CreateInfo {
		Vk.Khr.Swapchain.createInfoNext = TMaybe.N,
		Vk.Khr.Swapchain.createInfoFlags = def,
		Vk.Khr.Swapchain.createInfoSurface = sfc,
		Vk.Khr.Swapchain.createInfoMinImageCount = imgc,
		Vk.Khr.Swapchain.createInfoImageColorSpace =
			Vk.Khr.Surface.M.formatOldColorSpace fmt,
		Vk.Khr.Swapchain.createInfoImageExtent = ext,
		Vk.Khr.Swapchain.createInfoImageArrayLayers = 1,
		Vk.Khr.Swapchain.createInfoImageUsage =
			Vk.Image.UsageColorAttachmentBit,
		Vk.Khr.Swapchain.createInfoImageSharingMode = ism,
		Vk.Khr.Swapchain.createInfoQueueFamilyIndices = qfis,
		Vk.Khr.Swapchain.createInfoPreTransform =
			Vk.Khr.Surface.M.capabilitiesCurrentTransform caps,
		Vk.Khr.Swapchain.createInfoCompositeAlpha =
			Vk.Khr.CompositeAlphaOpaqueBit,
		Vk.Khr.Swapchain.createInfoPresentMode = presentMode,
		Vk.Khr.Swapchain.createInfoClipped = True,
		Vk.Khr.Swapchain.createInfoOldSwapchain = Nothing }
	where
	fmt = chooseSwapSurfaceFormat $ formats spp
	presentMode = chooseSwapPresentMode $ presentModes spp
	caps = capabilities spp
	maxImgc = fromMaybe maxBound . onlyIf (> 0)
		$ Vk.Khr.Surface.M.capabilitiesMaxImageCount caps
	imgc = clamp 0 maxImgc
		(Vk.Khr.Surface.M.capabilitiesMinImageCount caps + 1)
	(ism, qfis) = bool
		(Vk.SharingModeConcurrent,
			[PhDvc.grFam qfis0, PhDvc.prFam qfis0])
		(Vk.SharingModeExclusive, [])
		(PhDvc.grFam qfis0 == PhDvc.prFam qfis0)

recreateSwapChain :: Vk.T.FormatToValue scfmt =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc -> Vk.PhDvc.P ->
	PhDvc.QFamIndices -> Vk.Dvc.D sd -> Vk.Khr.Swapchain.S scfmt ssc ->
	IO Vk.Extent2d
recreateSwapChain win sfc phdvc qfis0 dvc sc = do
	spp <- querySwapChainSupport phdvc sfc
	ext <- chooseSwapExtent win $ capabilities spp
	let	crInfo = mkSwapchainCreateInfoNew sfc qfis0 spp ext
	ext <$ Vk.Khr.Swapchain.unsafeRecreate @'Nothing dvc crInfo nil sc

chooseSwapSurfaceFormat  :: [Vk.Khr.Surface.M.FormatOld] -> Vk.Khr.Surface.M.FormatOld
chooseSwapSurfaceFormat = \case
	availableFormats@(af0 : _) -> fromMaybe af0
		$ L.find preferredSwapSurfaceFormat availableFormats
	_ -> error "no available swap surface formats"

preferredSwapSurfaceFormat :: Vk.Khr.Surface.M.FormatOld -> Bool
preferredSwapSurfaceFormat f =
	Vk.Khr.Surface.M.formatOldFormat f == Vk.FormatB8g8r8a8Srgb &&
	Vk.Khr.Surface.M.formatOldColorSpace f == Vk.Khr.ColorSpaceSrgbNonlinear

chooseSwapPresentMode :: [Vk.Khr.PresentMode] -> Vk.Khr.PresentMode
chooseSwapPresentMode =
	fromMaybe Vk.Khr.PresentModeFifo . L.find (== Vk.Khr.PresentModeMailbox)

chooseSwapExtent :: Glfw.Window -> Vk.Khr.Surface.M.Capabilities -> IO Vk.Extent2d
chooseSwapExtent win caps
	| Vk.extent2dWidth curExt /= maxBound = pure curExt
	| otherwise = do
		(fromIntegral -> w, fromIntegral -> h) <-
			Glfw.getFramebufferSize win
		pure $ Vk.Extent2d
			(clamp (Vk.extent2dWidth n) (Vk.extent2dHeight n) w)
			(clamp (Vk.extent2dWidth x) (Vk.extent2dHeight x) h)
	where
	curExt = Vk.Khr.Surface.M.capabilitiesCurrentExtent caps
	n = Vk.Khr.Surface.M.capabilitiesMinImageExtent caps
	x = Vk.Khr.Surface.M.capabilitiesMaxImageExtent caps

createImageViewsNew :: Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> [Vk.Image.Binded ss ss nm fmt] ->
	(forall si . HeteroParList.PL (Vk.ImgVw.I nm fmt) si -> IO a) -> IO a
createImageViewsNew _dvc [] f = f HeteroParList.Nil
createImageViewsNew dvc (sci : scis) f =
	createImageView dvc sci \sciv ->
	createImageViewsNew dvc scis \scivs -> f $ sciv :** scivs

recreateImageViewsNew :: Vk.T.FormatToValue scfmt => Vk.Dvc.D sd ->
	[Vk.Image.Binded ss ss nm scfmt] -> HeteroParList.PL (Vk.ImgVw.I nm scfmt) sis -> IO ()
recreateImageViewsNew _dvc [] HeteroParList.Nil = pure ()
recreateImageViewsNew dvc (sci : scis) (iv :** ivs) =
	Vk.ImgVw.unsafeRecreate dvc (mkImageViewCreateInfoNew sci) nil iv >>
	recreateImageViewsNew dvc scis ivs
recreateImageViewsNew _ _ _ =
	error "number of Vk.Image.M.I and Vk.ImageView.M.I should be same"

createImageView :: forall ivfmt sd si sm nm ifmt a .
	Vk.T.FormatToValue ivfmt =>
	Vk.Dvc.D sd -> Vk.Image.Binded sm si nm ifmt ->
	(forall siv . Vk.ImgVw.I nm ivfmt siv -> IO a) -> IO a
createImageView dvc timg f =
	Vk.ImgVw.create dvc (mkImageViewCreateInfoNew timg) nil f

mkImageViewCreateInfoNew ::
	Vk.Image.Binded sm si nm ifmt ->
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
	subresourceRange = Vk.Image.M.SubresourceRange {
		Vk.Image.M.subresourceRangeAspectMask = Vk.Image.AspectColorBit,
		Vk.Image.M.subresourceRangeBaseMipLevel = 0,
		Vk.Image.M.subresourceRangeLevelCount = 1,
		Vk.Image.M.subresourceRangeBaseArrayLayer = 0,
		Vk.Image.M.subresourceRangeLayerCount = 1 }

createRenderPassNew ::
	forall (scifmt :: Vk.T.Format) sd a . Vk.T.FormatToValue scifmt =>
	Vk.Dvc.D sd -> (forall sr . Vk.RndrPass.R sr -> IO a) -> IO a
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
				Vk.Image.LayoutUndefined,
			Vk.Att.descriptionFinalLayout =
				Vk.Image.LayoutPresentSrcKhr }
		colorAttachmentRef = Vk.Att.Reference {
			Vk.Att.referenceAttachment = 0,
			Vk.Att.referenceLayout =
				Vk.Image.LayoutColorAttachmentOptimal }
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
		renderPassInfo = Vk.RndrPass.M.CreateInfo {
			Vk.RndrPass.M.createInfoNext = TMaybe.N,
			Vk.RndrPass.M.createInfoFlags = zeroBits,
			Vk.RndrPass.M.createInfoAttachments = colorAttachment :** HeteroParList.Nil,
			Vk.RndrPass.M.createInfoSubpasses = [subpass],
			Vk.RndrPass.M.createInfoDependencies = [dependency] }
	Vk.RndrPass.create @'Nothing @'[scifmt] dvc renderPassInfo nil \rp -> f rp

type AtomUbo s = '(s, '[ 'Vk.DscSetLyt.Buffer '[VObj.Atom 256 UniformBufferObject 'Nothing]])

createDescriptorSetLayout :: Vk.Dvc.D sd -> (forall s .
	Vk.DscSetLyt.D s '[ 'Vk.DscSetLyt.Buffer '[VObj.Atom 256 UniformBufferObject 'Nothing]]
	-> IO a) -> IO a
createDescriptorSetLayout dvc = Vk.DscSetLyt.create dvc layoutInfo nil
	where
	layoutInfo :: Vk.DscSetLyt.CreateInfo 'Nothing
		'[ 'Vk.DscSetLyt.Buffer '[VObj.Atom 256 UniformBufferObject 'Nothing] ]
	layoutInfo = Vk.DscSetLyt.CreateInfo {
		Vk.DscSetLyt.createInfoNext = TMaybe.N,
		Vk.DscSetLyt.createInfoFlags = zeroBits,
		Vk.DscSetLyt.createInfoBindings = uboLayoutBinding :** HeteroParList.Nil }
	uboLayoutBinding :: Vk.DscSetLyt.Binding
		('Vk.DscSetLyt.Buffer '[VObj.Atom 256 UniformBufferObject 'Nothing])
	uboLayoutBinding = Vk.DscSetLyt.BindingBuffer {
		Vk.DscSetLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBuffer,
		Vk.DscSetLyt.bindingBufferStageFlags = Vk.ShaderStageVertexBit }

createPipelineLayout' ::
	Vk.Dvc.D sd -> (forall sdsl sl .
		Vk.DscSetLyt.D sdsl
			'[ 'Vk.DscSetLyt.Buffer '[VObj.Atom 256 UniformBufferObject 'Nothing]] ->
		Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] -> IO b) -> IO b
createPipelineLayout' dvc f =
	createDescriptorSetLayout dvc \dsl ->
	let	pipelineLayoutInfo = Vk.Ppl.Layout.CreateInfo {
			Vk.Ppl.Layout.createInfoNext = TMaybe.N,
			Vk.Ppl.Layout.createInfoFlags = zeroBits,
			Vk.Ppl.Layout.createInfoSetLayouts =
				HeteroParList.Singleton $ U2 dsl } in
	Vk.Ppl.Layout.create @'Nothing @_ @_ @'[] dvc pipelineLayoutInfo nil $ f dsl

createGraphicsPipeline' :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	(forall sg . Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Cglm.Vec3)]
		'(sl, '[AtomUbo sdsl], '[]) -> IO a) -> IO a
createGraphicsPipeline' dvc sce rp ppllyt f =
	Vk.Ppl.Graphics.createGs dvc Nothing (U14 pplInfo :** HeteroParList.Nil)
			nil \(U3 gpl :** HeteroParList.Nil) -> f gpl
	where pplInfo = mkGraphicsPipelineCreateInfo' sce rp ppllyt

recreateGraphicsPipeline' :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Cglm.Vec3)]
		'(sl, '[AtomUbo sdsl], '[]) -> IO ()
recreateGraphicsPipeline' dvc sce rp ppllyt gpls = Vk.Ppl.Graphics.unsafeRecreateGs
	dvc Nothing (U14 pplInfo :** HeteroParList.Nil) nil (U3 gpls :** HeteroParList.Nil)
	where pplInfo = mkGraphicsPipelineCreateInfo' sce rp ppllyt

mkGraphicsPipelineCreateInfo' ::
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.CreateInfo 'Nothing '[
			'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
			'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
		'(	'Nothing, '[ '(Vertex, 'Vk.VtxInp.RateVertex)],
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

shaderStages :: HeteroParList.PL (U5 Vk.Ppl.ShdrSt.CreateInfo) '[
	'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
	'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
shaderStages = U5 vertShaderStageInfo :** U5 fragShaderStageInfo :** HeteroParList.Nil
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
	Vk.RndrPass.R sr -> HeteroParList.PL (Vk.ImgVw.I nm fmt) sis ->
	(forall sfs . RecreateFramebuffers sis sfs =>
		HeteroParList.PL Vk.Frmbffr.F sfs -> IO a) -> IO a
createFramebuffers _ _ _ HeteroParList.Nil f = f HeteroParList.Nil
createFramebuffers dvc sce rp (iv :** ivs) f =
	Vk.Frmbffr.create dvc (mkFramebufferCreateInfoNew sce rp iv) nil \fb ->
	createFramebuffers dvc sce rp ivs \fbs -> f (fb :** fbs)

class RecreateFramebuffers (sis :: [Type]) (sfs :: [Type]) where
	recreateFramebuffers :: Vk.Dvc.D sd -> Vk.Extent2d ->
		Vk.RndrPass.R sr -> HeteroParList.PL (Vk.ImgVw.I nm fmt) sis ->
		HeteroParList.PL Vk.Frmbffr.F sfs -> IO ()

instance RecreateFramebuffers '[] '[] where
	recreateFramebuffers _dvc _sce _rp HeteroParList.Nil HeteroParList.Nil = pure ()

instance RecreateFramebuffers sis sfs =>
	RecreateFramebuffers (si ': sis) (sf ': sfs) where
	recreateFramebuffers dvc sce rp (sciv :** scivs) (fb :** fbs) =
		Vk.Frmbffr.unsafeRecreate dvc
			(mkFramebufferCreateInfoNew sce rp sciv) nil fb >>
		recreateFramebuffers dvc sce rp scivs fbs

mkFramebufferCreateInfoNew ::
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.ImgVw.I nm fmt si ->
	Vk.Frmbffr.CreateInfo 'Nothing sr '[ '(nm, fmt, si)]
mkFramebufferCreateInfoNew sce rp attch = Vk.Frmbffr.CreateInfo {
	Vk.Frmbffr.createInfoNext = TMaybe.N,
	Vk.Frmbffr.createInfoFlags = zeroBits,
	Vk.Frmbffr.createInfoRenderPass = rp,
	Vk.Frmbffr.createInfoAttachments = U3 attch :** HeteroParList.Nil,
	Vk.Frmbffr.createInfoWidth = w, Vk.Frmbffr.createInfoHeight = h,
	Vk.Frmbffr.createInfoLayers = 1 }
	where
	Vk.Extent2d { Vk.extent2dWidth = w, Vk.extent2dHeight = h } = sce

createCommandPool :: PhDvc.QFamIndices -> Vk.Dvc.D sd ->
	(forall sc . Vk.CmdPool.C sc -> IO a) -> IO a
createCommandPool qfis dvc f =
	Vk.CmdPool.create dvc poolInfo nil \cp -> f cp
	where poolInfo = Vk.CmdPool.CreateInfo {
		Vk.CmdPool.createInfoNext = TMaybe.N,
		Vk.CmdPool.createInfoFlags =
			Vk.CmdPool.CreateResetCommandBufferBit,
		Vk.CmdPool.createInfoQueueFamilyIndex = PhDvc.grFam qfis }

createVertexBuffer :: Vk.PhDvc.P ->
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] -> IO a ) -> IO a
createVertexBuffer phdvc dvc gq cp f =
	createBufferList' phdvc dvc (fromIntegral $ length vertices)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		Vk.Mem.PropertyDeviceLocalBit \b _ ->
	createBufferList' phdvc dvc (fromIntegral $ length vertices)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mem.PropertyHostVisibleBit .|.
			Vk.Mem.PropertyHostCoherentBit ) \(b' :: Vk.Bffr.Binded sm sb "vertex-buffer" '[VObj.List 256 t ""]) bm' -> do
	Vk.Mem.write @"vertex-buffer" @(VObj.List 256 Vertex "") @0 dvc bm' zeroBits vertices
	copyBuffer dvc gq cp b' b
	f b

createIndexBuffer :: Vk.PhDvc.P ->
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[VObj.List 256 Word16 ""] -> IO a) -> IO a
createIndexBuffer phdvc dvc gq cp f =
	createBufferList' phdvc dvc (fromIntegral $ length indices)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageIndexBufferBit)
		Vk.Mem.PropertyDeviceLocalBit \b _ ->
	createBufferList' phdvc dvc (fromIntegral $ length indices)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mem.PropertyHostVisibleBit .|.
			Vk.Mem.PropertyHostCoherentBit ) \(b' :: Vk.Bffr.Binded sm sb "vertex-buffer" '[VObj.List 256 t ""]) bm' -> do
	Vk.Mem.write @"vertex-buffer" @(VObj.List 256 Word16 "") @0 dvc bm' zeroBits indices
	copyBuffer dvc gq cp b' b
	f b

createUniformBuffer :: Vk.PhDvc.P -> Vk.Dvc.D sd -> (forall sm sb .
		Vk.Bffr.Binded sm sb "uniform-buffer" '[VObj.Atom 256 UniformBufferObject 'Nothing]  ->
		UniformBufferMemory sm sb ->
		IO b) -> IO b
createUniformBuffer phdvc dvc = createBufferAtom' phdvc dvc
	Vk.Bffr.UsageUniformBufferBit
	(Vk.Mem.PropertyHostVisibleBit .|. Vk.Mem.PropertyHostCoherentBit)

type UniformBufferMemory sm sb = Vk.Mem.M sm '[ '(
	sb,
	'Vk.Mem.BufferArg "uniform-buffer" '[VObj.Atom 256 UniformBufferObject 'Nothing]
	)]

createDescriptorPool ::
	Vk.Dvc.D sd -> (forall sp . Vk.DscPool.P sp -> IO a) -> IO a
createDescriptorPool dvc = Vk.DscPool.create dvc poolInfo nil
	where
	poolInfo = Vk.DscPool.CreateInfo {
		Vk.DscPool.createInfoNext = TMaybe.N,
		Vk.DscPool.createInfoFlags =
			Vk.DscPool.CreateFreeDescriptorSetBit,
		Vk.DscPool.createInfoMaxSets = 1,
		Vk.DscPool.createInfoPoolSizes = [poolSize] }
	poolSize = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeUniformBuffer,
		Vk.DscPool.sizeDescriptorCount = 1 }

createDescriptorSet ::
	Vk.Dvc.D sd -> Vk.DscPool.P sp -> Vk.Bffr.Binded sm sb nm '[VObj.Atom 256 UniformBufferObject 'Nothing] ->
	Vk.DscSetLyt.D sdsc '[ 'Vk.DscSetLyt.Buffer '[VObj.Atom 256 UniformBufferObject 'Nothing]] ->
	(forall sds .
		Vk.DscSet.D sds '(sdsc, '[ 'Vk.DscSetLyt.Buffer '[VObj.Atom 256 UniformBufferObject 'Nothing]]) -> IO a) -> IO a
createDescriptorSet dvc dscp ub dscslyt f =
	Vk.DscSet.allocateDs dvc allocInfo \(HeteroParList.Singleton dscs) -> do
	Vk.DscSet.updateDs dvc
		(HeteroParList.Singleton . U5 $ descriptorWrite ub dscs) HeteroParList.Nil
	f dscs
	where
	allocInfo = Vk.DscSet.AllocateInfo {
		Vk.DscSet.allocateInfoNext = TMaybe.N,
		Vk.DscSet.allocateInfoDescriptorPool = dscp,
		Vk.DscSet.allocateInfoSetLayouts =
			HeteroParList.Singleton $ U2 dscslyt }

descriptorWrite ::
	Vk.Bffr.Binded sm sb nm '[VObj.Atom 256 UniformBufferObject 'Nothing] ->
	Vk.DscSet.D sds slbts ->
	Vk.DscSet.Write 'Nothing sds slbts ('Vk.DscSet.WriteSourcesArgBuffer '[ '(
		sm, sb, nm, VObj.Atom 256 UniformBufferObject 'Nothing, 0 )]) 0
descriptorWrite ub dscs = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = dscs,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeUniformBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		HeteroParList.Singleton bufferInfo }
	where bufferInfo = U5 $ Vk.Dsc.BufferInfo ub

createBufferAtom' :: forall sd nm a b . Storable a => Vk.PhDvc.P -> Vk.Dvc.D sd ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> (
		forall sm sb .
		Vk.Bffr.Binded sm sb nm '[VObj.Atom 256 a 'Nothing] ->
		Vk.Mem.M sm '[ '(
			sb,
			'Vk.Mem.BufferArg nm '[VObj.Atom 256 a 'Nothing] )] ->
			IO b) -> IO b
createBufferAtom' p dv usg props = createBuffer' p dv VObj.LengthAtom usg props

createBufferList' :: forall sd nm t a . Storable t =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Dvc.M.Size -> Vk.Bffr.UsageFlags ->
	Vk.Mem.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[VObj.List 256 t ""] ->
		Vk.Mem.M sm '[ '(
			sb,
			'Vk.Mem.BufferArg nm '[VObj.List 256 t ""] ) ] ->
		IO a) ->
	IO a
createBufferList' p dv ln usg props =
	createBuffer' p dv (VObj.LengthList ln) usg props

createBuffer' :: forall sd nm o a . VObj.SizeAlignment o =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> VObj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[o] ->
		Vk.Mem.M sm
			'[ '(sb, 'Vk.Mem.BufferArg nm '[o])] ->
		IO a) -> IO a
createBuffer' p dv ln usg props f = Vk.Bffr.create dv bffrInfo nil \b -> do
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMemoryType p (Vk.Mem.M.requirementsMemoryTypeBits reqs) props
	Vk.Mem.allocateBind dv (HeteroParList.Singleton . U2 $ Vk.Mem.Buffer b)
		(allcInfo mt) nil
		$ f . \(HeteroParList.Singleton (U2 (Vk.Mem.BufferBinded bnd))) -> bnd
	where
	bffrInfo :: Vk.Bffr.CreateInfo 'Nothing '[o]
	bffrInfo = Vk.Bffr.CreateInfo {
		Vk.Bffr.createInfoNext = TMaybe.N,
		Vk.Bffr.createInfoFlags = zeroBits,
		Vk.Bffr.createInfoLengths = HeteroParList.Singleton ln,
		Vk.Bffr.createInfoUsage = usg,
		Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Bffr.createInfoQueueFamilyIndices = [] }
	allcInfo :: Vk.Mem.M.TypeIndex -> Vk.Mem.AllocateInfo 'Nothing
	allcInfo mt = Vk.Mem.AllocateInfo {
		Vk.Mem.allocateInfoNext = TMaybe.N,
		Vk.Mem.allocateInfoMemoryTypeIndex = mt }

findMemoryType :: Vk.PhDvc.P -> Vk.Mem.M.TypeBits -> Vk.Mem.PropertyFlags ->
	IO Vk.Mem.M.TypeIndex
findMemoryType phdvc flt props =
	fromMaybe (error msg) . suitable <$> Vk.PhDvc.getMemoryProperties phdvc
	where
	msg = "failed to find suitable memory type!"
	suitable props1 = fst <$> L.find ((&&)
		<$> (`Vk.Mem.M.elemTypeIndex` flt) . fst
		<*> checkBits props . Vk.Mem.M.mTypePropertyFlags . snd) tps
		where tps = Vk.PhDvc.memoryPropertiesMemoryTypes props1

copyBuffer :: forall sd sc sm sb nm sm' sb' nm' a . Storable' a =>
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 a ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 a ""] -> IO ()
copyBuffer dvc gq cp src dst = do
	Vk.CmdBffr.allocate
		dvc allocInfo \(cb :*. HeteroParList.Nil) -> do
		let	submitInfo = Vk.SubmitInfo {
				Vk.submitInfoNext = TMaybe.N,
				Vk.submitInfoWaitSemaphoreDstStageMasks =
					HeteroParList.Nil,
				Vk.submitInfoCommandBuffers =
					HeteroParList.Singleton cb,
				Vk.submitInfoSignalSemaphores = HeteroParList.Nil }
		Vk.CmdBffr.begin @'Nothing @'Nothing cb beginInfo do
				Vk.Cmd.copyBuffer @'[ '( '[VObj.List 256 a ""], 0, 0)] cb src dst
		Vk.Queue.submit gq (HeteroParList.Singleton $ U4 submitInfo) Nothing
		Vk.Queue.waitIdle gq
	where
	allocInfo :: Vk.CmdBffr.AllocateInfo 'Nothing sc '[ '()]
	allocInfo = Vk.CmdBffr.AllocateInfo {
		Vk.CmdBffr.allocateInfoNext = TMaybe.N,
		Vk.CmdBffr.allocateInfoCommandPool = cp,
		Vk.CmdBffr.allocateInfoLevel = Vk.CmdBffr.LevelPrimary }
	beginInfo = Vk.CmdBffr.BeginInfo {
		Vk.CmdBffr.beginInfoNext = TMaybe.N,
		Vk.CmdBffr.beginInfoFlags = Vk.CmdBffr.UsageOneTimeSubmitBit,
		Vk.CmdBffr.beginInfoInheritanceInfo = Nothing }

createCommandBuffer ::
	forall sd scp a . Vk.Dvc.D sd -> Vk.CmdPool.C scp ->
	(forall scb . Vk.CmdBffr.C scb -> IO a) ->
	IO a
createCommandBuffer dvc cp f = Vk.CmdBffr.allocate dvc allocInfo $ f . \(cb :*. HeteroParList.Nil) -> cb
	where
	allocInfo :: Vk.CmdBffr.AllocateInfo 'Nothing scp '[ '()]
	allocInfo = Vk.CmdBffr.AllocateInfo {
		Vk.CmdBffr.allocateInfoNext = TMaybe.N,
		Vk.CmdBffr.allocateInfoCommandPool = cp,
		Vk.CmdBffr.allocateInfoLevel = Vk.CmdBffr.LevelPrimary }

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

recordCommandBuffer :: forall scb sr sf sl sg sm sb nm sm' sb' nm' sdsl sds .
	Vk.CmdBffr.C scb ->
	Vk.RndrPass.R sr -> Vk.Frmbffr.F sf -> Vk.Extent2d ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Cglm.Vec3)] '(sl, '[AtomUbo sdsl], '[]) ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 Word16 ""] ->
	Vk.DscSet.D sds (AtomUbo sdsl) ->
	IO ()
recordCommandBuffer cb rp fb sce ppllyt gpl vb ib ubds =
	Vk.CmdBffr.begin @'Nothing @'Nothing cb def $
	Vk.Cmd.beginRenderPass cb rpInfo Vk.Subpass.ContentsInline $
	Vk.Cmd.bindPipelineGraphics cb Vk.Ppl.BindPointGraphics gpl \cbb ->
	Vk.Cmd.bindVertexBuffers cbb
		(HeteroParList.Singleton . U5 $ Vk.Bffr.IndexedForList @_ @_ @_ @Vertex @"" vb) >>
	Vk.Cmd.bindIndexBuffer cbb (Vk.Bffr.IndexedForList @_ @_ @_ @Word16 @"" ib) >>
	Vk.Cmd.bindDescriptorSetsGraphics cbb Vk.Ppl.BindPointGraphics ppllyt
		(HeteroParList.Singleton $ U2 ubds)
		(HeteroParList.Singleton (
			HeteroParList.Nil :**
			HeteroParList.Nil )) >>
	Vk.Cmd.drawIndexed cbb (fromIntegral $ length indices) 1 0 0 0
	where
	rpInfo :: Vk.RndrPass.BeginInfo 'Nothing sr sf
		'[ 'Vk.ClearTypeColor 'Vk.ClearColorTypeFloat32]
	rpInfo = Vk.RndrPass.BeginInfo {
		Vk.RndrPass.beginInfoNext = TMaybe.N,
		Vk.RndrPass.beginInfoRenderPass = rp,
		Vk.RndrPass.beginInfoFramebuffer = fb,
		Vk.RndrPass.beginInfoRenderArea = Vk.Rect2d {
			Vk.rect2dOffset = Vk.Offset2d 0 0,
			Vk.rect2dExtent = sce },
		Vk.RndrPass.beginInfoClearValues = HeteroParList.Singleton
			. Vk.ClearValueColor . fromJust $ rgbaDouble 0 0 0 1 }

mainLoop :: (RecreateFramebuffers ss sfs, Vk.T.FormatToValue scfmt) =>
	FramebufferResized ->
	Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhDvc.P -> PhDvc.QFamIndices -> Vk.Dvc.D sd ->
	Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.Khr.Swapchain.S scfmt ssc -> Vk.Extent2d -> HeteroParList.PL (Vk.ImgVw.I nm scfmt) ss ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] -> Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Cglm.Vec3)]
		'(sl, '[AtomUbo sdsl], '[]) ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 Word16 ""] ->
	UniformBufferMemory sm2 sb2 ->
	Vk.DscSet.D sds (AtomUbo sdsl) ->
	Vk.CmdBffr.C scb ->
	SyncObjects '(sias, srfs, siff) -> UTCTime -> ControllerEvent -> IO ()
mainLoop g w sfc phdvc qfis dvc gq pq sc ext0 scivs rp ppllyt gpl fbs vb ib ubm ubds cb iasrfsifs tm0 cev = do
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

runLoop :: (RecreateFramebuffers sis sfs, Vk.T.FormatToValue scfmt) =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc -> Vk.PhDvc.P ->
	PhDvc.QFamIndices -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.Khr.Swapchain.S scfmt ssc -> FramebufferResized -> Vk.Extent2d ->
	HeteroParList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.G sg '[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Cglm.Vec3)]
		'(sl, '[AtomUbo sdsl], '[]) ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 Word16 ""] ->
	UniformBufferMemory sm2 sb2 ->
	Vk.DscSet.D sds (AtomUbo sdsl) ->
	Vk.CmdBffr.C scb ->
	SyncObjects '(sias, srfs, siff) -> (Float, Float) -> ControllerEvent ->
	(Vk.Extent2d -> IO ()) -> IO ()
runLoop win sfc phdvc qfis dvc gq pq sc frszd ext scivs rp ppllyt gpl fbs vb ib ubm ubds cb iasrfsifs tm cev loop = do
	catchAndRecreate win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs loop
		$ drawFrame dvc gq pq sc ext rp ppllyt gpl fbs vb ib ubm ubds cb iasrfsifs tm
	cls <- Glfw.windowShouldClose win
	ab <- readIORef $ controllerEventButtonAEver cev
	if (cls || ab)
	then writeIORef (controllerEventFinished cev) True
	else checkFlag frszd >>= bool (loop ext)
		(loop =<< recreateSwapChainEtc
			win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs)

drawFrame :: forall sfs sd ssc sr sl sg sm sb nm sm' sb' nm' sm2 sb2 scb sias srfs siff sdsl scfmt sds .
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q -> Vk.Khr.Swapchain.S scfmt ssc ->
	Vk.Extent2d -> Vk.RndrPass.R sr ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.G sg '[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Cglm.Vec3)]
		'(sl, '[AtomUbo sdsl], '[]) ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 Word16 ""] ->
	UniformBufferMemory sm2 sb2 ->
	Vk.DscSet.D sds (AtomUbo sdsl) ->
	Vk.CmdBffr.C scb -> SyncObjects '(sias, srfs, siff) -> (Float, Float) -> IO ()
drawFrame dvc gq pq sc ext rp ppllyt gpl fbs vb ib ubm ubds cb (SyncObjects ias rfs iff) tm = do
	let	siff = HeteroParList.Singleton iff
	Vk.Fence.waitForFs dvc siff True Nothing
	imgIdx <- Vk.Khr.acquireNextImageResult [Vk.Success, Vk.SuboptimalKhr]
		dvc sc maxBound (Just ias) Nothing
	Vk.Fence.resetFs dvc siff
	Vk.CmdBffr.reset cb def
	HeteroParList.index fbs imgIdx \fb ->
		recordCommandBuffer cb rp fb ext ppllyt gpl vb ib ubds
	updateUniformBuffer dvc ubm ext tm
	let	submitInfo :: Vk.SubmitInfo 'Nothing '[sias] '[scb] '[srfs]
		submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = TMaybe.N,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HeteroParList.Singleton
				$ Vk.SemaphorePipelineStageFlags ias
					Vk.Ppl.StageColorAttachmentOutputBit,
			Vk.submitInfoCommandBuffers = HeteroParList.Singleton cb,
			Vk.submitInfoSignalSemaphores = HeteroParList.Singleton rfs }
		presentInfo = Vk.Khr.PresentInfo {
			Vk.Khr.presentInfoNext = TMaybe.N,
			Vk.Khr.presentInfoWaitSemaphores = HeteroParList.Singleton rfs,
			Vk.Khr.presentInfoSwapchainImageIndices = HeteroParList.Singleton
				$ Vk.Khr.SwapchainImageIndex sc imgIdx }
	Vk.Queue.submit gq (HeteroParList.Singleton $ U4 submitInfo) $ Just iff
	catchAndSerialize $ Vk.Khr.queuePresent @'Nothing pq presentInfo

updateUniformBuffer :: Vk.Dvc.D sd ->
	UniformBufferMemory sm2 sb2 -> Vk.Extent2d -> (Float, Float) -> IO ()
updateUniformBuffer dvc um sce (tm, tm') = do
	Vk.Mem.write @"uniform-buffer" @(VObj.Atom 256 UniformBufferObject 'Nothing) @0
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
	Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhDvc.P -> PhDvc.QFamIndices -> Vk.Dvc.D sd ->
	Vk.Khr.Swapchain.S scfmt ssc ->
	HeteroParList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Cglm.Vec3)]
		'(sl, '[AtomUbo sdsl], '[]) ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	(Vk.Extent2d -> IO ()) -> IO () -> IO ()
catchAndRecreate win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs loop act =
	catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just ()
		Vk.SuboptimalKhr -> Just ()
		_ -> Nothing)
	act
	\_ -> loop =<< recreateSwapChainEtc
		win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs

recreateSwapChainEtc :: (
	RecreateFramebuffers sis sfs, Vk.T.FormatToValue scfmt ) =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhDvc.P -> PhDvc.QFamIndices -> Vk.Dvc.D sd ->
	Vk.Khr.Swapchain.S scfmt ssc ->
	HeteroParList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Cglm.Vec3)]
		'(sl, '[AtomUbo sdsl], '[]) ->
	HeteroParList.PL Vk.Frmbffr.F sfs -> IO Vk.Extent2d
recreateSwapChainEtc win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs = do
	waitFramebufferSize win
	Vk.Dvc.waitIdle dvc

	ext <- recreateSwapChain win sfc phdvc qfis dvc sc
	ext <$ do
		Vk.Khr.Swapchain.getImages dvc sc >>= \imgs ->
			recreateImageViewsNew dvc imgs scivs
		recreateGraphicsPipeline' dvc ext rp ppllyt gpl
		recreateFramebuffers dvc ext rp scivs fbs

waitFramebufferSize :: Glfw.Window -> IO ()
waitFramebufferSize win = Glfw.getFramebufferSize win >>= \sz ->
	when (zero sz) $ fix \loop -> (`when` loop) . zero =<<
		Glfw.waitEvents *> Glfw.getFramebufferSize win
	where zero = uncurry (||) . ((== 0) *** (== 0))

data Vertex = Vertex { vertexPos :: Pos, vertexColor :: Cglm.Vec3 }
	deriving (Show, Generic)

newtype Pos = Pos Cglm.Vec3 deriving (Show, Storable, Vk.Ppl.VtxInpSt.Formattable)

instance Storable Vertex where
	sizeOf = Foreign.Storable.Generic.gSizeOf
	alignment = Foreign.Storable.Generic.gAlignment
	peek = Foreign.Storable.Generic.gPeek
	poke = Foreign.Storable.Generic.gPoke

instance Foreign.Storable.Generic.G Vertex where

vertices :: [Vertex]
vertices = [
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

indices :: [Word16]
indices = [
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
