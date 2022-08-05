{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import GHC.Generics
import Foreign.Storable
import Foreign.Storable.SizeAlignment
import Control.Arrow hiding (loop)
import Control.Monad
import Control.Monad.Fix
import Control.Exception
import Data.Kind
import Data.Kind.Object
import Data.Default
import Data.Bits
import Data.HeteroList hiding (length)
import Data.Proxy
import Data.Bool
import Data.Maybe
import Data.List hiding (singleton)
import Data.IORef
import Data.List.Length
import Data.Color

import qualified TypeLevel.List as TpLvlLst

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt
import qualified Graphics.UI.GLFW as Glfw hiding (createWindowSurface)
import qualified Glfw as Glfw
import qualified Cglm
import qualified Foreign.Storable.Generic

import ThEnv
import Shaderc
import Shaderc.EnumAuto
import Shaderc.TH

import Gpu.Vulkan.Base

import qualified Gpu.Vulkan as Vk
import qualified Gpu.Vulkan.Middle as Vk.M
import qualified Gpu.Vulkan.Core as Vk.C
import qualified Gpu.Vulkan.Enum as Vk
import qualified Gpu.Vulkan.Exception as Vk
import qualified Gpu.Vulkan.Exception.Enum as Vk
import qualified Gpu.Vulkan.Instance as Vk.Ist
import qualified Gpu.Vulkan.Instance.Type as Vk.Ist
import qualified Gpu.Vulkan.Instance.Middle as Vk.Ist.M
import qualified Gpu.Vulkan.Khr as Vk.Khr
import qualified Gpu.Vulkan.Khr.Enum as Vk.Khr
import qualified Gpu.Vulkan.Ext.DebugUtils as Vk.Ext.DbgUtls
import qualified Gpu.Vulkan.Ext.DebugUtils.Messenger as Vk.Ext.DbgUtls.Msngr
import qualified Gpu.Vulkan.Ext.DebugUtils.Message.Enum as Vk.Ext.DbgUtls.Msg
import qualified Gpu.Vulkan.PhysicalDevice as Vk.PhDvc
import qualified Gpu.Vulkan.QueueFamily as Vk.QueueFamily
import qualified Gpu.Vulkan.QueueFamily.EnumManual as Vk.QueueFamily
import qualified Gpu.Vulkan.Device as Vk.Dvc
import qualified Gpu.Vulkan.Device.Type as Vk.Dvc
import qualified Gpu.Vulkan.Device.Middle as Vk.Dvc.M
import qualified Gpu.Vulkan.Device.Queue as Vk.Dvc.Queue
import qualified Gpu.Vulkan.Khr.Surface as Vk.Khr.Surface
import qualified Gpu.Vulkan.Khr.Surface.Middle as Vk.Khr.Surface.M
import qualified Gpu.Vulkan.Khr.Surface.PhysicalDevice as
	Vk.Khr.Surface.PhysicalDevice
import qualified Gpu.Vulkan.Khr.Swapchain as Vk.Khr.Swapchain
import qualified Gpu.Vulkan.Khr.Swapchain.Middle as Vk.Khr.Swapchain.M
import qualified Gpu.Vulkan.Image.Type as Vk.Image
import qualified Gpu.Vulkan.Image.Enum as Vk.Image
import qualified Gpu.Vulkan.Image.Middle as Vk.Image.M
import qualified Gpu.Vulkan.ImageView as Vk.ImgVw
import qualified Gpu.Vulkan.ImageView.Enum as Vk.ImgVw
import qualified Gpu.Vulkan.Component as Vk.Component
import qualified Gpu.Vulkan.Component.Enum as Vk.Component
import qualified Gpu.Vulkan.ShaderModule as Vk.Shader.Module
import qualified Gpu.Vulkan.ShaderModule.Middle as Vk.Shader.Module.M
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShdrSt
import qualified Gpu.Vulkan.Pipeline.VertexInputState as Vk.Ppl.VertexInputSt
import qualified Gpu.Vulkan.Pipeline.InputAssemblyState as Vk.Ppl.InpAsmbSt
import qualified Gpu.Vulkan.Pipeline.ViewportState as Vk.Ppl.ViewportSt
import qualified Gpu.Vulkan.Pipeline.RasterizationState as Vk.Ppl.RstSt
import qualified Gpu.Vulkan.Pipeline.MultisampleState as Vk.Ppl.MltSmplSt
import qualified Gpu.Vulkan.Sample as Vk.Sample
import qualified Gpu.Vulkan.Sample.Enum as Vk.Sample
import qualified Gpu.Vulkan.Pipeline.ColorBlendAttachment as Vk.Ppl.ClrBlndAtt
import qualified Gpu.Vulkan.ColorComponent.Enum as Vk.ClrCmp
import qualified Gpu.Vulkan.Pipeline.ColorBlendState as Vk.Ppl.ClrBlndSt
import qualified Gpu.Vulkan.Pipeline.Layout as Vk.Ppl.Layout
import qualified Gpu.Vulkan.Attachment as Vk.Att
import qualified Gpu.Vulkan.Attachment.Enum as Vk.Att
import qualified Gpu.Vulkan.Subpass as Vk.Subpass
import qualified Gpu.Vulkan.Subpass.Enum as Vk.Subpass
import qualified Gpu.Vulkan.Pipeline.Enum as Vk.Ppl
import qualified Gpu.Vulkan.RenderPass as Vk.RndrPass
import qualified Gpu.Vulkan.RenderPass.Middle as Vk.RndrPass.M
import qualified Gpu.Vulkan.Pipeline.Graphics.Type as Vk.Ppl.Graphics
import qualified Gpu.Vulkan.Pipeline.Graphics as Vk.Ppl.Graphics
import qualified Gpu.Vulkan.Framebuffer as Vk.Frmbffr
import qualified Gpu.Vulkan.CommandPool as Vk.CmdPool
import qualified Gpu.Vulkan.CommandPool.Enum as Vk.CmdPool
import qualified Gpu.Vulkan.CommandBuffer as Vk.CmdBffr
import qualified Gpu.Vulkan.CommandBuffer.Middle as Vk.CmdBffr.M
import qualified Gpu.Vulkan.CommandBuffer.Enum as Vk.CmdBffr
import qualified Gpu.Vulkan.Semaphore as Vk.Semaphore
import qualified Gpu.Vulkan.Fence as Vk.Fence
import qualified Gpu.Vulkan.Fence.Enum as Vk.Fence
import qualified Gpu.Vulkan.VertexInput as Vk.VtxInp
import qualified Gpu.Vulkan.Buffer as Vk.Bffr
import qualified Gpu.Vulkan.Buffer.Enum as Vk.Bffr
import qualified Gpu.Vulkan.Memory.Middle as Vk.Mem.M
import qualified Gpu.Vulkan.Memory.Enum as Vk.Mem
import qualified Gpu.Vulkan.Device.Memory.Buffer as Vk.Dvc.Mem.Buffer
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.Queue.Enum as Vk.Queue
import qualified Gpu.Vulkan.Memory as Vk.Mem
import qualified Gpu.Vulkan.Command as Vk.Cmd

import Gpu.Vulkan.Pipeline.VertexInputState.BindingStrideList(AddType)

main :: IO ()
main = do
	g <- newFramebufferResized
	(`withWindow` g) \win -> createInstance \inst -> do
		if enableValidationLayers
			then setupDebugMessenger inst $ const $ run win inst g
			else run win inst g

type FramebufferResized = IORef Bool

globalFramebufferResized :: IORef Bool -> IORef Bool
globalFramebufferResized = id

newFramebufferResized :: IO FramebufferResized
newFramebufferResized = newIORef False

windowName :: String
windowName = "Triangle"

windowSize :: (Int, Int)
windowSize = (width, height) where width = 800; height = 600

enableValidationLayers :: Bool
enableValidationLayers = maybe True (const False) $(lookupCompileEnv "NDEBUG")

validationLayers :: [Txt.Text]
validationLayers = [Vk.Khr.validationLayerName]

maxFramesInFlight :: Integral n => n
maxFramesInFlight = 2

withWindow :: (Glfw.Window -> IO a) -> FramebufferResized -> IO a
withWindow f g = initWindow g >>= \w ->
	f w <* (Glfw.destroyWindow w >> Glfw.terminate)

initWindow :: FramebufferResized -> IO Glfw.Window
initWindow g = do
	Just w <- do
		True <- Glfw.init
		Glfw.windowHint $ Glfw.WindowHint'ClientAPI Glfw.ClientAPI'NoAPI
		uncurry Glfw.createWindow windowSize windowName Nothing Nothing
	w <$ Glfw.setFramebufferSizeCallback w
		(Just $ \_ _ _ -> writeIORef (globalFramebufferResized g) True)

createInstance :: (forall si . Vk.Ist.I si -> IO a) -> IO a
createInstance f = do
	when enableValidationLayers $ bool
		(error "validation layers requested, but not available!")
		(pure ())
		=<< null . (validationLayers \\)
				. (Vk.M.layerPropertiesLayerName <$>)
			<$> Vk.Ist.M.enumerateLayerProperties
	extensions <- bool id (Vk.Ext.DbgUtls.extensionName :)
			enableValidationLayers
		<$> ((cstrToText `mapM`) =<< Glfw.getRequiredInstanceExtensions)
	print extensions
	let	appInfo = Vk.M.ApplicationInfo {
			Vk.M.applicationInfoNext = Nothing,
			Vk.M.applicationInfoApplicationName = "Hello Triangle",
			Vk.M.applicationInfoApplicationVersion =
				Vk.M.makeApiVersion 0 1 0 0,
			Vk.M.applicationInfoEngineName = "No Engine",
			Vk.M.applicationInfoEngineVersion =
				Vk.M.makeApiVersion 0 1 0 0,
			Vk.M.applicationInfoApiVersion = Vk.M.apiVersion_1_0 }
		createInfo :: Vk.Ist.M.CreateInfo
			(Vk.Ext.DbgUtls.Msngr.CreateInfo
				() () () () () ()) ()
		createInfo = Vk.Ist.M.CreateInfo {
			Vk.Ist.M.createInfoNext = Just debugMessengerCreateInfo,
			Vk.Ist.M.createInfoFlags = def,
			Vk.Ist.M.createInfoApplicationInfo = Just appInfo,
			Vk.Ist.M.createInfoEnabledLayerNames =
				bool [] validationLayers enableValidationLayers,
			Vk.Ist.M.createInfoEnabledExtensionNames = extensions }
	Vk.Ist.create createInfo nil nil \i -> f i

instanceToMiddle :: Vk.Ist.I si -> Vk.Ist.M.I
instanceToMiddle (Vk.Ist.I inst) = inst

setupDebugMessenger ::
	Vk.Ist.I si ->
	(forall sm . Vk.Ext.DbgUtls.Msngr.M sm -> IO a) -> IO a
setupDebugMessenger ist f = Vk.Ext.DbgUtls.Msngr.create ist
	debugMessengerCreateInfo nil nil \m -> f m

debugMessengerCreateInfo :: Vk.Ext.DbgUtls.Msngr.CreateInfo () () () () () ()
debugMessengerCreateInfo = Vk.Ext.DbgUtls.Msngr.CreateInfo {
	Vk.Ext.DbgUtls.Msngr.createInfoNext = Nothing,
	Vk.Ext.DbgUtls.Msngr.createInfoFlags = def,
	Vk.Ext.DbgUtls.Msngr.createInfoMessageSeverity =
		Vk.Ext.DbgUtls.Msg.SeverityVerboseBit .|.
		Vk.Ext.DbgUtls.Msg.SeverityWarningBit .|.
		Vk.Ext.DbgUtls.Msg.SeverityErrorBit,
	Vk.Ext.DbgUtls.Msngr.createInfoMessageType =
		Vk.Ext.DbgUtls.Msg.TypeGeneralBit .|.
		Vk.Ext.DbgUtls.Msg.TypeValidationBit .|.
		Vk.Ext.DbgUtls.Msg.TypePerformanceBit,
	Vk.Ext.DbgUtls.Msngr.createInfoFnUserCallback = debugCallback,
	Vk.Ext.DbgUtls.Msngr.createInfoUserData = Nothing }

debugCallback :: Vk.Ext.DbgUtls.Msngr.FnCallback () () () () ()
debugCallback _msgSeverity _msgType cbdt _userData = False <$ Txt.putStrLn
	("validation layer: " <> Vk.Ext.DbgUtls.Msngr.callbackDataMessage cbdt)

run :: Glfw.Window -> Vk.Ist.I si -> FramebufferResized -> IO ()
run w inst g =
	createSurface w inst \sfc ->
	pickPhysicalDevice inst sfc >>= \(phdv, qfis) ->
	createLogicalDevice phdv qfis \dv gq pq ->
	createSwapChain w sfc phdv qfis dv \sc scfmt ext ->
	let	scifmt = Vk.Khr.Surface.M.formatFormat scfmt in
	Vk.Khr.Swapchain.getImages dv sc >>= \imgs ->
	createImageViews dv scifmt imgs \scivs ->
	createRenderPass dv scifmt \rp ->
	createPipelineLayout dv \ppllyt ->
	createGraphicsPipeline dv ext rp ppllyt \gpl ->
	createFramebuffers dv ext scivs rp \fbs ->
	createCommandPool qfis dv \cp ->
	createVertexBuffer phdv dv gq cp \vb ->
	createCommandBuffersNew dv cp \cbs ->
	createSyncObjects dv \iasrfsifs ->
	mainLoop g w sfc phdv qfis dv gq pq sc ext scivs rp ppllyt gpl fbs vb cbs iasrfsifs

createSurface :: Glfw.Window -> Vk.Ist.I si ->
	(forall ss . Vk.Khr.Surface.S ss -> IO a) -> IO a
createSurface win ist f = Glfw.createWindowSurface ist win nil nil \sfc -> f sfc

pickPhysicalDevice :: Vk.Ist.I si ->
	Vk.Khr.Surface.S ss -> IO (Vk.PhDvc.P, QueueFamilyIndices)
pickPhysicalDevice ist sfc = do
	dvcs <- Vk.PhDvc.enumerate ist
	when (null dvcs) $ error "failed to find GPUs with Gpu.Vulkan support!"
	findDevice (`isDeviceSuitable` sfc) dvcs >>= \case
		Just pdvc -> pure pdvc
		Nothing -> error "failed to find a suitable GPU!"

findDevice :: Monad m =>
	(Vk.PhDvc.P -> m (Maybe a)) -> [Vk.PhDvc.P] ->
	m (Maybe (Vk.PhDvc.P, a))
findDevice prd = \case
	[] -> pure Nothing
	p : ps -> prd p >>= \case
		Nothing -> findDevice prd ps; Just x -> pure $ Just (p, x)

isDeviceSuitable ::
	Vk.PhDvc.P -> Vk.Khr.Surface.S ss -> IO (Maybe QueueFamilyIndices)
isDeviceSuitable phdvc sfc = do
	_deviceProperties <- Vk.PhDvc.getProperties phdvc
	_deviceFeatures <- Vk.PhDvc.getFeatures phdvc
	indices <- findQueueFamilies phdvc sfc
	extensionSupported <- checkDeviceExtensionSupport phdvc
	if extensionSupported
	then do	swapChainSupport <- querySwapChainSupport phdvc sfc
		let	swapChainAdequate =
				not (null $ formats swapChainSupport) &&
				not (null $ presentModes swapChainSupport)
		pure if swapChainAdequate
			then completeQueueFamilies indices else Nothing
	else pure Nothing

data QueueFamilyIndices = QueueFamilyIndices {
	graphicsFamily :: Vk.QueueFamily.Index,
	presentFamily :: Vk.QueueFamily.Index }

data QueueFamilyIndicesMaybe = QueueFamilyIndicesMaybe {
	graphicsFamilyMaybe :: Maybe Vk.QueueFamily.Index,
	presentFamilyMaybe :: Maybe Vk.QueueFamily.Index }

completeQueueFamilies :: QueueFamilyIndicesMaybe -> Maybe QueueFamilyIndices
completeQueueFamilies = \case
	QueueFamilyIndicesMaybe {
		graphicsFamilyMaybe = Just gf, presentFamilyMaybe = Just pf } ->
		Just QueueFamilyIndices {
			graphicsFamily = gf, presentFamily = pf }
	_ -> Nothing

findQueueFamilies ::
	Vk.PhDvc.P -> Vk.Khr.Surface.S ss -> IO QueueFamilyIndicesMaybe
findQueueFamilies device sfc = do
	queueFamilies <- Vk.PhDvc.getQueueFamilyProperties device
	pfis <- filterM
		(\i -> Vk.Khr.Surface.PhysicalDevice.getSupport device i sfc)
		(fst <$> queueFamilies)
	pure QueueFamilyIndicesMaybe {
		graphicsFamilyMaybe = fst <$> find
			(checkBits Vk.Queue.GraphicsBit
				. Vk.QueueFamily.propertiesQueueFlags . snd)
			queueFamilies,
		presentFamilyMaybe = listToMaybe pfis }

checkBits :: Bits bs => bs -> bs -> Bool
checkBits bs = (== bs) . (.&. bs)

checkDeviceExtensionSupport :: Vk.PhDvc.P -> IO Bool
checkDeviceExtensionSupport dvc =
	null . (deviceExtensions \\) . (Vk.M.extensionPropertiesExtensionName <$>)
		<$> Vk.PhDvc.enumerateExtensionProperties dvc Nothing

deviceExtensions :: [Txt.Text]
deviceExtensions = [Vk.Khr.Swapchain.M.extensionName]

data SwapChainSupportDetails = SwapChainSupportDetails {
	capabilities :: Vk.Khr.Surface.M.Capabilities,
	formats :: [Vk.Khr.Surface.M.Format],
	presentModes :: [Vk.Khr.PresentMode] }

querySwapChainSupport ::
	Vk.PhDvc.P -> Vk.Khr.Surface.S ss -> IO SwapChainSupportDetails
querySwapChainSupport dvc sfc = SwapChainSupportDetails
	<$> Vk.Khr.Surface.PhysicalDevice.getCapabilities dvc sfc
	<*> Vk.Khr.Surface.PhysicalDevice.getFormats dvc sfc
	<*> Vk.Khr.Surface.PhysicalDevice.getPresentModes dvc sfc

createLogicalDevice :: Vk.PhDvc.P -> QueueFamilyIndices -> (forall sd .
		Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q -> IO a) -> IO a
createLogicalDevice phdvc qfis f =
	let	uniqueQueueFamilies =
			nub [graphicsFamily qfis, presentFamily qfis]
		queueCreateInfos qf = Vk.Dvc.Queue.CreateInfo {
			Vk.Dvc.Queue.createInfoNext = Nothing,
			Vk.Dvc.Queue.createInfoFlags = def,
			Vk.Dvc.Queue.createInfoQueueFamilyIndex = qf,
			Vk.Dvc.Queue.createInfoQueuePriorities = [1] }
		createInfo = Vk.Dvc.M.CreateInfo {
			Vk.Dvc.M.createInfoNext = Nothing,
			Vk.Dvc.M.createInfoFlags = def,
			Vk.Dvc.M.createInfoQueueCreateInfos =
				queueCreateInfos <$> uniqueQueueFamilies,
			Vk.Dvc.M.createInfoEnabledLayerNames =
				bool [] validationLayers enableValidationLayers,
			Vk.Dvc.M.createInfoEnabledExtensionNames =
				deviceExtensions,
			Vk.Dvc.M.createInfoEnabledFeatures = Just def } in
	Vk.Dvc.create @() @() phdvc createInfo nil nil \dvc -> do
		gq <- Vk.Dvc.getQueue dvc (graphicsFamily qfis) 0
		pq <- Vk.Dvc.getQueue dvc (presentFamily qfis) 0
		f dvc gq pq

createSwapChain :: Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd -> (
		forall ss . Vk.Khr.Swapchain.S ss -> Vk.Khr.Surface.M.Format ->
		Vk.C.Extent2d -> IO a ) -> IO a
createSwapChain win sfc phdvc qfis dvc f = do
	swapChainSupport <- querySwapChainSupport phdvc sfc
	extent <- chooseSwapExtent win $ capabilities swapChainSupport
	let	(createInfo, surfaceFormat) =
			mkSwapchainCreateInfo sfc qfis swapChainSupport extent
	Vk.Khr.Swapchain.create @() dvc createInfo nil nil \sc ->
		f sc surfaceFormat extent

recreateSwapChain :: Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Khr.Swapchain.S ssc ->
	IO (Vk.Khr.Surface.M.Format, Vk.C.Extent2d)
recreateSwapChain win sfc phdvc qfis0 dvc sc = do
	swapChainSupport <- querySwapChainSupport phdvc sfc
	extent <- chooseSwapExtent win $ capabilities swapChainSupport
	let	(createInfo, surfaceFormat) =
			mkSwapchainCreateInfo sfc qfis0 swapChainSupport extent
	Vk.Khr.Swapchain.recreate @() dvc createInfo nil nil sc
	pure (surfaceFormat, extent)

mkSwapchainCreateInfo :: Vk.Khr.Surface.S ss -> QueueFamilyIndices ->
	SwapChainSupportDetails -> Vk.C.Extent2d ->
	(Vk.Khr.Swapchain.CreateInfo n ss, Vk.Khr.Surface.M.Format)
mkSwapchainCreateInfo sfc qfis0 swapChainSupport extent = (
	Vk.Khr.Swapchain.CreateInfo {
		Vk.Khr.Swapchain.createInfoNext = Nothing,
		Vk.Khr.Swapchain.createInfoFlags = def,
		Vk.Khr.Swapchain.createInfoSurface = sfc,
		Vk.Khr.Swapchain.createInfoMinImageCount = imageCount,
		Vk.Khr.Swapchain.createInfoImageFormat =
			Vk.Khr.Surface.M.formatFormat surfaceFormat,
		Vk.Khr.Swapchain.createInfoImageColorSpace =
			Vk.Khr.Surface.M.formatColorSpace surfaceFormat,
		Vk.Khr.Swapchain.createInfoImageExtent = extent,
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
		Vk.Khr.Swapchain.createInfoOldSwapchain = Nothing },
	surfaceFormat )
	where
	surfaceFormat = chooseSwapSurfaceFormat $ formats swapChainSupport
	presentMode = chooseSwapPresentMode $ presentModes swapChainSupport
	caps = capabilities swapChainSupport
	maxImageCount = fromMaybe maxBound . onlyIf (> 0)
		$ Vk.Khr.Surface.M.capabilitiesMaxImageCount caps
	imageCount = clamp
		(Vk.Khr.Surface.M.capabilitiesMinImageCount caps + 1) 0
		maxImageCount
	(ism, qfis) = if graphicsFamily qfis0 /= presentFamily qfis0
		then (Vk.SharingModeConcurrent,
			[graphicsFamily qfis0, presentFamily qfis0])
		else (Vk.SharingModeExclusive, [])

chooseSwapSurfaceFormat  :: [Vk.Khr.Surface.M.Format] -> Vk.Khr.Surface.M.Format
chooseSwapSurfaceFormat = \case
	availableFormats@(af0 : _) -> fromMaybe af0
		$ find preferredSwapSurfaceFormat availableFormats
	_ -> error "no available swap surface formats"

preferredSwapSurfaceFormat :: Vk.Khr.Surface.M.Format -> Bool
preferredSwapSurfaceFormat f =
	Vk.Khr.Surface.M.formatFormat f == Vk.FormatB8g8r8a8Srgb &&
	Vk.Khr.Surface.M.formatColorSpace f == Vk.Khr.ColorSpaceSrgbNonlinear

chooseSwapPresentMode :: [Vk.Khr.PresentMode] -> Vk.Khr.PresentMode
chooseSwapPresentMode =
	fromMaybe Vk.Khr.PresentModeFifo . find (== Vk.Khr.PresentModeMailbox)

chooseSwapExtent :: Glfw.Window -> Vk.Khr.Surface.M.Capabilities -> IO Vk.C.Extent2d
chooseSwapExtent win caps
	| Vk.C.extent2dWidth curExt /= maxBound = pure curExt
	| otherwise = do
		(fromIntegral -> w, fromIntegral -> h) <-
			Glfw.getFramebufferSize win
		pure $ Vk.C.Extent2d
			(clamp w (Vk.C.extent2dWidth n) (Vk.C.extent2dHeight n))
			(clamp h (Vk.C.extent2dWidth x) (Vk.C.extent2dHeight x))
	where
	curExt = Vk.Khr.Surface.M.capabilitiesCurrentExtent caps
	n = Vk.Khr.Surface.M.capabilitiesMinImageExtent caps
	x = Vk.Khr.Surface.M.capabilitiesMaxImageExtent caps

clamp :: Ord a => a -> a -> a -> a
clamp x mn mx | x < mn = mn | x < mx = x | otherwise = mx

onlyIf :: (a -> Bool) -> a -> Maybe a
onlyIf p x | p x = Just x | otherwise = Nothing

createImageViews :: Vk.Dvc.D sd -> Vk.Format -> [Vk.Image.Binded ss ss] ->
	(forall si . HeteroVarList Vk.ImgVw.I si -> IO a) -> IO a
createImageViews _dvc _scifmt [] f = f HVNil
createImageViews dvc scifmt (img : imgs) f =
	createImageView1 dvc img scifmt \sciv ->
	createImageViews dvc scifmt imgs \scivs -> f $ sciv :...: scivs

createImageView1 :: Vk.Dvc.D sd -> Vk.Image.Binded ss ss -> Vk.Format ->
	(forall siv . Vk.ImgVw.I siv -> IO a) -> IO a
createImageView1 dvc sci scifmt f = do
	let	createInfo = makeImageViewCreateInfo sci scifmt
	Vk.ImgVw.create @() dvc createInfo nil nil \sciv -> f sciv

recreateImageViews :: Vk.Dvc.D sd ->
	Vk.Format -> [Vk.Image.Binded ss ss] ->
	HeteroVarList Vk.ImgVw.I sis -> IO ()
recreateImageViews _dvc _scifmt [] HVNil = pure ()
recreateImageViews dvc scifmt (img : imgs) (iv :...: ivs) =
	recreateImageView1 dvc img scifmt iv >>
	recreateImageViews dvc scifmt imgs ivs
recreateImageViews _ _ _ _ =
	error "number of Vk.Image.M.I and Vk.ImageView.M.I should be same"

recreateImageView1 :: Vk.Dvc.D sd ->
	Vk.Image.Binded ss ss -> Vk.Format -> Vk.ImgVw.I siv -> IO ()
recreateImageView1 dvc sci scifmt iv = do
	let	createInfo = makeImageViewCreateInfo sci scifmt
	Vk.ImgVw.recreate @() dvc createInfo nil nil iv

makeImageViewCreateInfo ::
	Vk.Image.Binded ss ss -> Vk.Format -> Vk.ImgVw.CreateInfo ss ss n
makeImageViewCreateInfo sci scifmt = Vk.ImgVw.CreateInfo {
	Vk.ImgVw.createInfoNext = Nothing,
	Vk.ImgVw.createInfoFlags = Vk.ImgVw.CreateFlagsZero,
	Vk.ImgVw.createInfoImage = sci,
	Vk.ImgVw.createInfoViewType = Vk.ImgVw.Type2d,
	Vk.ImgVw.createInfoFormat = scifmt,
	Vk.ImgVw.createInfoComponents = components,
	Vk.ImgVw.createInfoSubresourceRange = subresourceRange }
	where
	components = Vk.Component.Mapping {
		Vk.Component.mappingR = Vk.Component.SwizzleIdentity,
		Vk.Component.mappingG = Vk.Component.SwizzleIdentity,
		Vk.Component.mappingB = Vk.Component.SwizzleIdentity,
		Vk.Component.mappingA = Vk.Component.SwizzleIdentity }
	subresourceRange = Vk.Image.M.SubresourceRange {
		Vk.Image.M.subresourceRangeAspectMask = Vk.Image.AspectColorBit,
		Vk.Image.M.subresourceRangeBaseMipLevel = 0,
		Vk.Image.M.subresourceRangeLevelCount = 1,
		Vk.Image.M.subresourceRangeBaseArrayLayer = 0,
		Vk.Image.M.subresourceRangeLayerCount = 1 }

createRenderPass :: Vk.Dvc.D sd ->
	Vk.Format -> (forall sr . Vk.RndrPass.R sr -> IO a) -> IO a
createRenderPass dvc scifmt f = do
	let	colorAttachment = Vk.Att.Description {
			Vk.Att.descriptionFlags = zeroBits,
			Vk.Att.descriptionFormat = scifmt,
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
			Vk.Att.referenceAttachment = Vk.Att.A 0,
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
			Vk.Subpass.dependencyDstSubpass = Vk.Subpass.S 0,
			Vk.Subpass.dependencySrcStageMask =
				Vk.Ppl.StageColorAttachmentOutputBit,
			Vk.Subpass.dependencySrcAccessMask = zeroBits,
			Vk.Subpass.dependencyDstStageMask =
				Vk.Ppl.StageColorAttachmentOutputBit,
			Vk.Subpass.dependencyDstAccessMask =
				Vk.AccessColorAttachmentWriteBit,
			Vk.Subpass.dependencyDependencyFlags = zeroBits }
		renderPassInfo = Vk.RndrPass.M.CreateInfo {
			Vk.RndrPass.M.createInfoNext = Nothing,
			Vk.RndrPass.M.createInfoFlags = zeroBits,
			Vk.RndrPass.M.createInfoAttachments = [colorAttachment],
			Vk.RndrPass.M.createInfoSubpasses = [subpass],
			Vk.RndrPass.M.createInfoDependencies = [dependency] }
	Vk.RndrPass.create @() dvc renderPassInfo nil nil \rp -> f rp

createPipelineLayout ::
	Vk.Dvc.D sd -> (forall sl . Vk.Ppl.Layout.LL sl '[] -> IO b) -> IO b
createPipelineLayout dvc f = do
	let	pipelineLayoutInfo = Vk.Ppl.Layout.CreateInfo {
			Vk.Ppl.Layout.createInfoNext = Nothing,
			Vk.Ppl.Layout.createInfoFlags = zeroBits,
			Vk.Ppl.Layout.createInfoSetLayouts = HVNil,
			Vk.Ppl.Layout.createInfoPushConstantRanges = [] }
	Vk.Ppl.Layout.create @() dvc pipelineLayoutInfo nil nil f

createGraphicsPipeline :: Vk.Dvc.D sd ->
	Vk.C.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.LL sl '[] ->
	(forall sg . Vk.Ppl.Graphics.G sg
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] -> IO a) -> IO a
createGraphicsPipeline dvc sce rp ppllyt f =
	Vk.Ppl.Graphics.createGs' dvc Nothing (V14 pplInfo :...: HVNil)
			nil nil \(V2 gpl :...: HVNil) -> f gpl
	where pplInfo = makeGraphicsPipelineCreateInfo sce rp ppllyt

recreateGraphicsPipeline :: Vk.Dvc.D sd ->
	Vk.C.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.LL sl '[] ->
	Vk.Ppl.Graphics.G sg
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] -> IO ()
recreateGraphicsPipeline dvc sce rp ppllyt gpls = Vk.Ppl.Graphics.recreateGs'
	dvc Nothing (V14 pplInfo :...: HVNil) nil nil (V2 gpls :...: HVNil)
	where pplInfo = makeGraphicsPipelineCreateInfo sce rp ppllyt

makeGraphicsPipelineCreateInfo ::
	Vk.C.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.LL sl '[] ->
	Vk.Ppl.Graphics.CreateInfo' () '[
			'((), (), 'GlslVertexShader, (), (), ()),
			'((), (), 'GlslFragmentShader, (), (), ()) ]
		'(	(), '[AddType Vertex 'Vk.VtxInp.RateVertex],
			'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] )
		() () () () () () () () '(sl, '[]) sr '(sb, vs', ts')
makeGraphicsPipelineCreateInfo sce rp ppllyt = Vk.Ppl.Graphics.CreateInfo' {
	Vk.Ppl.Graphics.createInfoNext' = Nothing,
	Vk.Ppl.Graphics.createInfoFlags' = Vk.Ppl.CreateFlagsZero,
	Vk.Ppl.Graphics.createInfoStages' = shaderStages,
	Vk.Ppl.Graphics.createInfoVertexInputState' = Just $ V3 vertexInputInfo,
	Vk.Ppl.Graphics.createInfoInputAssemblyState' = Just inputAssembly,
	Vk.Ppl.Graphics.createInfoViewportState' = Just $ makeViewportState sce,
	Vk.Ppl.Graphics.createInfoRasterizationState' = Just rasterizer,
	Vk.Ppl.Graphics.createInfoMultisampleState' = Just multisampling,
	Vk.Ppl.Graphics.createInfoDepthStencilState' = Nothing,
	Vk.Ppl.Graphics.createInfoColorBlendState' = Just colorBlending,
	Vk.Ppl.Graphics.createInfoDynamicState' = Nothing,
	Vk.Ppl.Graphics.createInfoLayout' = V2 ppllyt,
	Vk.Ppl.Graphics.createInfoRenderPass' = rp,
	Vk.Ppl.Graphics.createInfoSubpass' = 0,
	Vk.Ppl.Graphics.createInfoBasePipelineHandle' = Nothing,
	Vk.Ppl.Graphics.createInfoBasePipelineIndex' = - 1,
	Vk.Ppl.Graphics.createInfoTessellationState' = Nothing }

shaderStages :: HeteroVarList (V6 Vk.Ppl.ShdrSt.CreateInfo) '[
	'((), (), 'GlslVertexShader, (), (), ()),
	'((), (), 'GlslFragmentShader, (), (), ()) ]
shaderStages = V6 vertShaderStageInfo :...: V6 fragShaderStageInfo :...: HVNil
	where
	vertShaderStageInfo = Vk.Ppl.ShdrSt.CreateInfo {
		Vk.Ppl.ShdrSt.createInfoNext = Nothing,
		Vk.Ppl.ShdrSt.createInfoFlags = def,
		Vk.Ppl.ShdrSt.createInfoStage = Vk.ShaderStageVertexBit,
		Vk.Ppl.ShdrSt.createInfoModule = vertShaderModule,
		Vk.Ppl.ShdrSt.createInfoName = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfo = Nothing }
	fragShaderStageInfo = Vk.Ppl.ShdrSt.CreateInfo {
		Vk.Ppl.ShdrSt.createInfoNext = Nothing,
		Vk.Ppl.ShdrSt.createInfoFlags = def,
		Vk.Ppl.ShdrSt.createInfoStage = Vk.ShaderStageFragmentBit,
		Vk.Ppl.ShdrSt.createInfoModule = fragShaderModule,
		Vk.Ppl.ShdrSt.createInfoName = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfo = Nothing }

vertexInputInfo :: Vk.Ppl.VertexInputSt.CreateInfo
	() '[AddType Vertex 'Vk.VtxInp.RateVertex]
	'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
vertexInputInfo = Vk.Ppl.VertexInputSt.CreateInfo {
	Vk.Ppl.VertexInputSt.createInfoNext = Nothing,
	Vk.Ppl.VertexInputSt.createInfoFlags = zeroBits }

inputAssembly :: Vk.Ppl.InpAsmbSt.CreateInfo ()
inputAssembly = Vk.Ppl.InpAsmbSt.CreateInfo {
	Vk.Ppl.InpAsmbSt.createInfoNext = Nothing,
	Vk.Ppl.InpAsmbSt.createInfoFlags = zeroBits,
	Vk.Ppl.InpAsmbSt.createInfoTopology = Vk.PrimitiveTopologyTriangleList,
	Vk.Ppl.InpAsmbSt.createInfoPrimitiveRestartEnable = False }

makeViewportState :: Vk.C.Extent2d -> Vk.Ppl.ViewportSt.CreateInfo n
makeViewportState sce = Vk.Ppl.ViewportSt.CreateInfo {
	Vk.Ppl.ViewportSt.createInfoNext = Nothing,
	Vk.Ppl.ViewportSt.createInfoFlags = zeroBits,
	Vk.Ppl.ViewportSt.createInfoViewports = [viewport],
	Vk.Ppl.ViewportSt.createInfoScissors = [scissor] }
	where
	viewport = Vk.C.Viewport {
		Vk.C.viewportX = 0, Vk.C.viewportY = 0,
		Vk.C.viewportWidth = fromIntegral $ Vk.C.extent2dWidth sce,
		Vk.C.viewportHeight = fromIntegral $ Vk.C.extent2dHeight sce,
		Vk.C.viewportMinDepth = 0, Vk.C.viewportMaxDepth = 1 }
	scissor = Vk.C.Rect2d {
		Vk.C.rect2dOffset = Vk.C.Offset2d 0 0, Vk.C.rect2dExtent = sce }

rasterizer :: Vk.Ppl.RstSt.CreateInfo ()
rasterizer = Vk.Ppl.RstSt.CreateInfo {
	Vk.Ppl.RstSt.createInfoNext = Nothing,
	Vk.Ppl.RstSt.createInfoFlags = zeroBits,
	Vk.Ppl.RstSt.createInfoDepthClampEnable = False,
	Vk.Ppl.RstSt.createInfoRasterizerDiscardEnable = False,
	Vk.Ppl.RstSt.createInfoPolygonMode = Vk.PolygonModeFill,
	Vk.Ppl.RstSt.createInfoLineWidth = 1,
	Vk.Ppl.RstSt.createInfoCullMode = Vk.CullModeBackBit,
	Vk.Ppl.RstSt.createInfoFrontFace = Vk.FrontFaceClockwise,
	Vk.Ppl.RstSt.createInfoDepthBiasEnable = False,
	Vk.Ppl.RstSt.createInfoDepthBiasConstantFactor = 0,
	Vk.Ppl.RstSt.createInfoDepthBiasClamp = 0,
	Vk.Ppl.RstSt.createInfoDepthBiasSlopeFactor = 0 }

multisampling :: Vk.Ppl.MltSmplSt.CreateInfo ()
multisampling = Vk.Ppl.MltSmplSt.CreateInfo {
	Vk.Ppl.MltSmplSt.createInfoNext = Nothing,
	Vk.Ppl.MltSmplSt.createInfoFlags = zeroBits,
	Vk.Ppl.MltSmplSt.createInfoSampleShadingEnable = False,
	Vk.Ppl.MltSmplSt.createInfoRasterizationSamplesAndMask =
		Vk.Sample.CountAndMask Vk.Sample.Count1Bit Nothing,
	Vk.Ppl.MltSmplSt.createInfoMinSampleShading = 1,
	Vk.Ppl.MltSmplSt.createInfoAlphaToCoverageEnable = False,
	Vk.Ppl.MltSmplSt.createInfoAlphaToOneEnable = False }

colorBlending :: Vk.Ppl.ClrBlndSt.CreateInfo ()
colorBlending = Vk.Ppl.ClrBlndSt.CreateInfo {
	Vk.Ppl.ClrBlndSt.createInfoNext = Nothing,
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

createFramebuffers :: Vk.Dvc.D sd ->
	Vk.C.Extent2d -> HeteroVarList Vk.ImgVw.I sis -> Vk.RndrPass.R sr ->
	(forall sfs . RecreateFramebuffers sis sfs =>
		HeteroVarList Vk.Frmbffr.F sfs -> IO a) -> IO a
createFramebuffers _ _ HVNil _ f = f HVNil
createFramebuffers dvc sce (sciv :...: scivs) rp f =
	createFramebuffer1 dvc sce rp sciv \fb ->
	createFramebuffers dvc sce scivs rp \fbs -> f (fb :...: fbs)

createFramebuffer1 :: Vk.Dvc.D sd -> Vk.C.Extent2d -> Vk.RndrPass.R sr ->
	Vk.ImgVw.I si -> (forall sf . Vk.Frmbffr.F sf -> IO a) -> IO a
createFramebuffer1 dvc sce rp attch = Vk.Frmbffr.create @() dvc fbInfo nil nil
	where fbInfo = makeFramebufferCreateInfo sce rp attch

class RecreateFramebuffers (sis :: [Type]) (sfs :: [Type]) where
	recreateFramebuffers :: Vk.Dvc.D sd -> Vk.C.Extent2d ->
		HeteroVarList Vk.ImgVw.I sis -> Vk.RndrPass.R sr ->
		HeteroVarList Vk.Frmbffr.F sfs -> IO ()

instance RecreateFramebuffers '[] '[] where
	recreateFramebuffers _dvc _sce HVNil _rp HVNil = pure ()

instance RecreateFramebuffers sis sfs =>
	RecreateFramebuffers (si ': sis) (sf ': sfs) where
	recreateFramebuffers dvc sce (sciv :...: scivs) rp (fb :...: fbs) = do
		recreateFramebuffer1 dvc sce rp sciv fb
		recreateFramebuffers dvc sce scivs rp fbs

recreateFramebuffer1 :: Vk.Dvc.D sd -> Vk.C.Extent2d -> Vk.RndrPass.R sr ->
	Vk.ImgVw.I si -> Vk.Frmbffr.F sf -> IO ()
recreateFramebuffer1 dvc sce rp attch fb =
	Vk.Frmbffr.recreate @() dvc fbInfo nil nil fb
	where fbInfo = makeFramebufferCreateInfo sce rp attch

makeFramebufferCreateInfo ::
	Vk.C.Extent2d -> Vk.RndrPass.R sr -> Vk.ImgVw.I si ->
	Vk.Frmbffr.CreateInfo () sr '[si]
makeFramebufferCreateInfo sce rp attch = Vk.Frmbffr.CreateInfo {
	Vk.Frmbffr.createInfoNext = Nothing,
	Vk.Frmbffr.createInfoFlags = zeroBits,
	Vk.Frmbffr.createInfoRenderPass = rp,
	Vk.Frmbffr.createInfoAttachments = attch :...: HVNil,
	Vk.Frmbffr.createInfoWidth = w, Vk.Frmbffr.createInfoHeight = h,
	Vk.Frmbffr.createInfoLayers = 1 }
	where
	Vk.C.Extent2d { Vk.C.extent2dWidth = w, Vk.C.extent2dHeight = h } = sce

createCommandPool :: QueueFamilyIndices -> Vk.Dvc.D sd ->
	(forall sc . Vk.CmdPool.C sc -> IO a) -> IO a
createCommandPool qfis dvc f =
	Vk.CmdPool.create @() dvc poolInfo nil nil \cp -> f cp
	where poolInfo = Vk.CmdPool.CreateInfo {
		Vk.CmdPool.createInfoNext = Nothing,
		Vk.CmdPool.createInfoFlags =
			Vk.CmdPool.CreateResetCommandBufferBit,
		Vk.CmdPool.createInfoQueueFamilyIndex = graphicsFamily qfis }

createVertexBuffer :: Vk.PhDvc.P ->
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc -> (forall sm sb .
		Vk.Bffr.Binded sm sb '[ 'List Vertex] -> IO a ) -> IO a
createVertexBuffer phdvc dvc gq cp f =
	createBuffer phdvc dvc (length vertices)
		(	Vk.Bffr.UsageTransferDstBit .|.
			Vk.Bffr.UsageVertexBufferBit)
		Vk.Mem.PropertyDeviceLocalBit \b _ -> do
	createBuffer phdvc dvc (length vertices)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mem.PropertyHostVisibleBit .|.
			Vk.Mem.PropertyHostCoherentBit ) \b' bm' -> do
		Vk.Dvc.Mem.Buffer.write @('List Vertex)
			dvc bm' Vk.Mem.M.MapFlagsZero vertices
		copyBuffer dvc gq cp b' b
	f b

createBuffer :: Vk.PhDvc.P -> Vk.Dvc.D sd -> Int ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> (
		forall sm sb .
		Vk.Bffr.Binded sm sb '[ 'List Vertex] ->
		Vk.Dvc.Mem.Buffer.M sm '[ '[ 'List Vertex ] ] -> IO a ) -> IO a
createBuffer p dv ln usg props f = Vk.Bffr.create dv bffrInfo nil nil \b -> do
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMemoryType p (Vk.Mem.M.requirementsMemoryTypeBits reqs) props
	let	allocInfo = Vk.Dvc.Mem.Buffer.AllocateInfo {
			Vk.Dvc.Mem.Buffer.allocateInfoNext = Nothing,
			Vk.Dvc.Mem.Buffer.allocateInfoMemoryTypeIndex = mt }
	Vk.Bffr.allocateBind @() dv (V2 b :...: HVNil) allocInfo nil nil
		$ f . \(V2 bnd :...: HVNil) -> bnd
	where
	bffrInfo :: Vk.Bffr.CreateInfo () '[ 'List Vertex]
	bffrInfo = Vk.Bffr.CreateInfo {
		Vk.Bffr.createInfoNext = Nothing,
		Vk.Bffr.createInfoFlags = zeroBits,
		Vk.Bffr.createInfoLengths = ObjectLengthList ln :...: HVNil,
		Vk.Bffr.createInfoUsage = usg,
		Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Bffr.createInfoQueueFamilyIndices = [] }

copyBuffer :: forall sd sc sm sb sm' sb' .
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
	Vk.Bffr.Binded sm sb '[ 'List Vertex] ->
	Vk.Bffr.Binded sm' sb' '[ 'List Vertex] -> IO ()
copyBuffer dvc gq cp srcBuffer dstBuffer = do
	let	allocInfo :: Vk.CmdBffr.AllocateInfoNew () sc '[ '[]]
		allocInfo = Vk.CmdBffr.AllocateInfoNew {
			Vk.CmdBffr.allocateInfoNextNew = Nothing,
			Vk.CmdBffr.allocateInfoCommandPoolNew = cp,
			Vk.CmdBffr.allocateInfoLevelNew =
				Vk.CmdBffr.LevelPrimary }
		beginInfo = Vk.CmdBffr.M.BeginInfo {
			Vk.CmdBffr.beginInfoNext = Nothing,
			Vk.CmdBffr.beginInfoFlags =
				Vk.CmdBffr.UsageOneTimeSubmitBit,
			Vk.CmdBffr.beginInfoInheritanceInfo = Nothing }
	Vk.CmdBffr.allocateNew @() dvc allocInfo \(commandBuffer :...: HVNil) -> do
		let	submitInfo = Vk.SubmitInfo {
				Vk.submitInfoNext = Nothing,
				Vk.submitInfoWaitSemaphoreDstStageMasks = HVNil,
				Vk.submitInfoCommandBuffers = [commandBuffer],
				Vk.submitInfoSignalSemaphores = [] }
		Vk.CmdBffr.begin @() @() commandBuffer beginInfo do
			Vk.Cmd.copyBuffer @'[ '[ 'List Vertex]]
				commandBuffer srcBuffer dstBuffer
		Vk.Queue.submit @() gq [submitInfo] Nothing
		Vk.Queue.waitIdle gq

findMemoryType ::
	Vk.PhDvc.P -> Vk.Mem.M.TypeBits ->
	Vk.Mem.PropertyFlags -> IO Vk.Mem.TypeIndex
findMemoryType phdvc typeFilter properties = do
	memProperties <- Vk.PhDvc.getMemoryProperties phdvc
	let	r = suitable' typeFilter properties memProperties
	print r
	pure $ fromMaybe (error "failed to find suitable memory type!") r

suitable :: Vk.Mem.M.TypeBits -> Vk.Mem.PropertyFlags ->
	Vk.PhDvc.MemoryProperties -> Int -> Bool
suitable typeFilter properties memProperties i =
	(typeFilter .&. Vk.Mem.M.TypeBits 1 `shiftL` i /= zeroBits) &&
	(Vk.Mem.M.mTypePropertyFlags
		(snd $ Vk.PhDvc.memoryPropertiesMemoryTypes memProperties !!
			i) .&. properties == properties)

suitable' :: Vk.Mem.M.TypeBits -> Vk.Mem.PropertyFlags ->
	Vk.PhDvc.MemoryProperties -> Maybe Vk.Mem.TypeIndex
suitable' typeFilter properties memProperties = fst
	<$> find ((&&)	<$> (`Vk.Mem.M.elemTypeIndex` typeFilter) . fst 
			<*> checkMemoryProperties properties . snd) memTypes
	where
	memTypes = Vk.PhDvc.memoryPropertiesMemoryTypes memProperties

checkMemoryProperties :: Vk.Mem.PropertyFlags -> Vk.Mem.M.MType -> Bool
checkMemoryProperties properties prop =
	Vk.Mem.M.mTypePropertyFlags prop .&. properties == properties

size :: forall a . SizeAlignmentList a => a -> Size
size _ = fst (wholeSizeAlignment @a)

addTypeToProxy ::
	Proxy vss -> Proxy ('[AddType Vertex 'Vk.VtxInp.RateVertex] ': vss)
addTypeToProxy Proxy = Proxy

makeVss :: Int -> (forall (vss :: [[Type]]) . (TpLvlLst.Length [Type] vss, ListToHeteroVarList vss, VssList vss) => Proxy vss -> a) -> a
makeVss 0 f = f (Proxy @'[])
makeVss n f = makeVss (n - 1) \p -> f $ addTypeToProxy p

class VssList (vss :: [[Type]]) where
	vssListIndex ::
		HeteroVarList (Vk.CmdBffr.C scb) vss -> Int ->
		Vk.CmdBffr.C scb '[AddType Vertex 'Vk.VtxInp.RateVertex]

instance VssList '[] where
	vssListIndex HVNil _ = error "index too large"

instance VssList vss => VssList ('[AddType Vertex 'Vk.VtxInp.RateVertex] ': vss) where
	vssListIndex (cb :...: _) 0 = cb
	vssListIndex (_ :...: cbs) n = vssListIndex cbs (n - 1)

createCommandBuffersNew ::
	forall sd scp a . Vk.Dvc.D sd -> Vk.CmdPool.C scp ->
	(forall scb vss . VssList vss =>
		HeteroVarList (Vk.CmdBffr.C scb) (vss :: [[Type]]) -> IO a) ->
	IO a
createCommandBuffersNew dvc cp f = makeVss maxFramesInFlight \(_p :: Proxy vss1) ->
	Vk.CmdBffr.allocateNew @() @vss1 dvc (allocInfo @vss1) (f @_ @vss1)
	where
	allocInfo :: forall vss . Vk.CmdBffr.AllocateInfoNew () scp vss
	allocInfo = Vk.CmdBffr.AllocateInfoNew {
		Vk.CmdBffr.allocateInfoNextNew = Nothing,
		Vk.CmdBffr.allocateInfoCommandPoolNew = cp,
		Vk.CmdBffr.allocateInfoLevelNew = Vk.CmdBffr.LevelPrimary }

data SyncObjects (ssos :: ([Type], [Type], [Type])) where
	SyncObjects :: {
		imageAvailableSemaphores :: HeteroVarList Vk.Semaphore.S siass,
		renderFinishedSemaphores :: HeteroVarList Vk.Semaphore.S srfss,
		inFlightFences :: HeteroVarList Vk.Fence.F sfss } ->
		SyncObjects '(siass, srfss, sfss)

createSyncObjects :: Vk.Dvc.D sd ->
	(forall siassrfssfs . SyncObjects siassrfssfs -> IO a ) -> IO a
createSyncObjects dvc f =
	heteroVarListReplicateM maxFramesInFlight
		(Vk.Semaphore.create @() dvc def nil nil) \ias ->
	heteroVarListReplicateM maxFramesInFlight
		(Vk.Semaphore.create @() dvc def nil nil) \rfs ->
	heteroVarListReplicateM maxFramesInFlight
		(Vk.Fence.create dvc fncInfo nil nil) \ifs ->
	f $ SyncObjects ias rfs ifs
	where
	fncInfo :: Vk.Fence.CreateInfo ()
	fncInfo = def { Vk.Fence.createInfoFlags = Vk.Fence.CreateSignaledBit }

recordCommandBuffer :: forall scb sr sf sg sm sb .
	Vk.CmdBffr.C scb '[AddType Vertex 'Vk.VtxInp.RateVertex] ->
	Vk.RndrPass.R sr -> Vk.Frmbffr.F sf -> Vk.C.Extent2d ->
	Vk.Ppl.Graphics.G sg
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] ->
	Vk.Bffr.Binded sm sb '[ 'List Vertex] -> IO ()
recordCommandBuffer cb rp fb sce gpl vb =
	Vk.CmdBffr.begin @() @() cb def $
	Vk.Cmd.beginRenderPass cb rpInfo Vk.Subpass.ContentsInline do
	Vk.Cmd.bindPipeline cb Vk.Ppl.BindPointGraphics gpl
	Vk.Cmd.bindVertexBuffers cb
		. singleton . V3 $ Vk.Bffr.IndexedList @_ @_ @Vertex vb
	Vk.Cmd.draw cb 3 1 0 0
	where
	rpInfo :: Vk.RndrPass.BeginInfo () sr sf
		'[ 'Vk.M.ClearTypeColor 'Vk.M.ClearColorTypeFloat32]
	rpInfo = Vk.RndrPass.BeginInfo {
		Vk.RndrPass.beginInfoNext = Nothing,
		Vk.RndrPass.beginInfoRenderPass = rp,
		Vk.RndrPass.beginInfoFramebuffer = fb,
		Vk.RndrPass.beginInfoRenderArea = Vk.C.Rect2d {
			Vk.C.rect2dOffset = Vk.C.Offset2d 0 0,
			Vk.C.rect2dExtent = sce },
		Vk.RndrPass.beginInfoClearValues = singleton
			. Vk.M.ClearValueColor . fromJust $ rgbaDouble 0 0 0 1 }

mainLoop :: (RecreateFramebuffers ss sfs, VssList vss) => FramebufferResized ->
	Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.Khr.Swapchain.S ssc -> Vk.C.Extent2d -> HeteroVarList Vk.ImgVw.I ss ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.LL sl '[] -> Vk.Ppl.Graphics.G sg
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] ->
	HeteroVarList Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb '[ 'List Vertex] ->
	HeteroVarList (Vk.CmdBffr.C scb) vss ->
	SyncObjects siassrfssfs -> IO ()
mainLoop g w sfc phdvc qfis dvc gq pq sc ext0 scivs rp ppllyt gpl fbs vb cbs iasrfsifs = do
	($ cycle [0 .. maxFramesInFlight - 1]) . ($ ext0) $ fix \loop ext (cf : cfs) -> do
		Glfw.pollEvents
		catchAndRecreateSwapChain g w sfc phdvc qfis dvc
				sc ext scivs rp ppllyt gpl fbs (\e -> loop e cfs)
			$ runLoop w sfc phdvc qfis dvc gq pq
				sc g ext scivs rp ppllyt gpl fbs vb cbs iasrfsifs cf (\e -> loop e cfs)
	Vk.Dvc.waitIdle dvc

runLoop :: (RecreateFramebuffers sis sfs, VssList vss) =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc -> Vk.PhDvc.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.Khr.Swapchain.S ssc -> FramebufferResized -> Vk.C.Extent2d ->
	HeteroVarList Vk.ImgVw.I sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.LL sl '[] ->
	Vk.Ppl.Graphics.G sg '[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] ->
	HeteroVarList Vk.Frmbffr.F sfs ->
	 Vk.Bffr.Binded sm sb '[ 'List Vertex] ->
	HeteroVarList (Vk.CmdBffr.C scb) vss ->
	SyncObjects siassrfssfs ->
	Int ->
	(Vk.C.Extent2d -> IO ()) -> IO ()
runLoop win sfc phdvc qfis dvc gq pq sc frszd ext scivs rp ppllyt gpl fbs vb cbs iasrfsifs cf loop = do
	drawFrame win sfc phdvc qfis dvc gq pq sc ext scivs rp ppllyt gpl fbs vb cbs iasrfsifs frszd cf (\e -> loop e)
	bool (loop ext) (pure ()) =<< Glfw.windowShouldClose win

drawFrame :: forall sis sfs ssfc sd ssc sr sl sg sm sb scb ssos vss .
	(RecreateFramebuffers sis sfs, VssList vss) =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.Khr.Swapchain.S ssc -> Vk.C.Extent2d ->
	HeteroVarList Vk.ImgVw.I sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.LL sl '[] ->
	Vk.Ppl.Graphics.G sg '[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] ->
	HeteroVarList Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb '[ 'List Vertex] ->
	HeteroVarList (Vk.CmdBffr.C scb) vss ->
	SyncObjects ssos -> FramebufferResized ->
	Int -> (Vk.C.Extent2d -> IO ()) -> IO ()
drawFrame win sfc phdvc qfis dvc gq pq sc ext scivs rp ppllyt gpl fbs vb cbs
		(SyncObjects iass rfss iffs) frszd cf loop =
	heteroVarListIndex iass cf \(ias :: Vk.Semaphore.S sias) ->
	heteroVarListIndex rfss cf \(rfs :: Vk.Semaphore.S srfs) ->
	heteroVarListIndex iffs cf \(id &&& singleton -> (iff, siff)) -> do
	Vk.Fence.waitForFs dvc siff True maxBound
	Vk.Fence.resetFs dvc siff
	imgIdx <- Vk.Khr.acquireNextImageResult [Vk.Success, Vk.SuboptimalKhr]
		dvc sc uint64Max (Just ias) Nothing
	Vk.CmdBffr.reset cb def
	heteroVarListIndex fbs imgIdx \fb -> recordCommandBuffer cb rp fb ext gpl vb
	let	submitInfo :: Vk.SubmitInfoNew () '[sias]
			'[ '(scb, '[AddType Vertex 'Vk.VtxInp.RateVertex])]
			'[srfs]
		submitInfo = Vk.SubmitInfoNew {
			Vk.submitInfoNextNew = Nothing,
			Vk.submitInfoWaitSemaphoreDstStageMasksNew =
				Vk.SemaphorePipelineStageFlags ias
					Vk.Ppl.StageColorAttachmentOutputBit
					:...: HVNil,
			Vk.submitInfoCommandBuffersNew = V2 cb :...: HVNil,
			Vk.submitInfoSignalSemaphoresNew = rfs :...: HVNil }
	Vk.Queue.submitNewNew gq (V4 submitInfo :...: HVNil) $ Just iff
	let	presentInfo = Vk.Khr.PresentInfo {
			Vk.Khr.presentInfoNext = Nothing,
			Vk.Khr.presentInfoWaitSemaphores = rfs :...: HVNil,
			Vk.Khr.presentInfoSwapchainImageIndices =
				Vk.Khr.SwapchainImageIndex sc imgIdx :...:
				HVNil }
	catchAndRecreateSwapChain frszd win sfc phdvc qfis dvc
			sc ext scivs rp ppllyt gpl fbs loop
		. catchAndSerialize $ Vk.Khr.queuePresent @() pq presentInfo
	where	cb = cbs `vssListIndex` cf

catchAndSerialize :: IO () -> IO ()
catchAndSerialize =
	(`catch` \(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

catchAndRecreateSwapChain :: RecreateFramebuffers sis sfs =>
	FramebufferResized -> Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Khr.Swapchain.S ssc -> Vk.C.Extent2d ->
	HeteroVarList Vk.ImgVw.I sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.LL sl '[] ->
	Vk.Ppl.Graphics.G sg
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] ->
	HeteroVarList Vk.Frmbffr.F sfs ->
	(Vk.C.Extent2d -> IO ()) -> IO () -> IO ()
catchAndRecreateSwapChain g win sfc phdvc qfis dvc sc ext scivs rp ppllyt gpl fbs loop act = catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just ()
		Vk.SuboptimalKhr -> Just ()
		_ -> Nothing)
	act
	(\_ -> do
		fbr <- readIORef $ globalFramebufferResized g
		if fbr
		then do
			writeIORef (globalFramebufferResized g) False
			e <- recreateSwapChainAndOthers win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs
			loop e
		else loop ext)

doWhile_ :: IO Bool -> IO ()
doWhile_ act = (`when` doWhile_ act) =<< act

recreateSwapChainAndOthers :: RecreateFramebuffers sis sfs =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Khr.Swapchain.S ssc -> HeteroVarList Vk.ImgVw.I sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.LL sl '[] ->
	Vk.Ppl.Graphics.G sg
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] ->
	HeteroVarList Vk.Frmbffr.F sfs ->
	IO Vk.C.Extent2d
recreateSwapChainAndOthers win sfc phdvc qfis dvc@(Vk.Dvc.D dvcm)
	sc scivs rp ppllyt gpl fbs = do
	do	(wdth, hght) <- Glfw.getFramebufferSize win
		when (wdth == 0 || hght == 0) $ doWhile_ do
			Glfw.waitEvents
			(wd, hg) <- Glfw.getFramebufferSize win
			pure $ wd == 0 || hg == 0
	Vk.Dvc.M.waitIdle dvcm

	(scfmt, ext) <- recreateSwapChain win sfc phdvc qfis dvc sc
	let	scifmt = Vk.Khr.Surface.M.formatFormat scfmt
	imgs <- Vk.Khr.Swapchain.getImages dvc sc
	recreateImageViews dvc scifmt imgs scivs
	recreateGraphicsPipeline dvc ext rp ppllyt gpl
	recreateFramebuffers dvc ext scivs rp fbs
	pure ext

data Vertex = Vertex {
	vertexPos :: Cglm.Vec2,
	vertexColor :: Cglm.Vec3 }
	deriving (Show, Generic)

instance Storable Vertex where
	sizeOf = Foreign.Storable.Generic.gSizeOf
	alignment = Foreign.Storable.Generic.gAlignment
	peek = Foreign.Storable.Generic.gPeek
	poke = Foreign.Storable.Generic.gPoke

instance SizeAlignmentList Vertex

instance SizeAlignmentListUntil Cglm.Vec2 Vertex
instance SizeAlignmentListUntil Cglm.Vec3 Vertex

instance Vk.Ppl.VertexInputSt.Formattable Cglm.Vec2 where
	formatOf = Vk.FormatR32g32Sfloat

instance Vk.Ppl.VertexInputSt.Formattable Cglm.Vec3 where
	formatOf = Vk.FormatR32g32b32Sfloat

instance Foreign.Storable.Generic.G Vertex where

vertices :: [Vertex]
vertices = [
	Vertex (Cglm.Vec2 $ 0.0 :. (- 0.5) :. NilL)
--		(Cglm.Vec3 $ 1.0 :. 0.0 :. 0.0 :. NilL),
		(Cglm.Vec3 $ 1.0 :. 1.0 :. 1.0 :. NilL),
	Vertex (Cglm.Vec2 $ 0.5 :. 0.5 :. NilL)
		(Cglm.Vec3 $ 0.0 :. 1.0 :. 0.0 :. NilL),
	Vertex (Cglm.Vec2 $ (- 0.5) :. 0.5 :. NilL)
		(Cglm.Vec3 $ 0.0 :. 0.0 :. 1.0 :. NilL) ]

vertShaderModule :: Vk.Shader.Module.M n 'GlslVertexShader () ()
vertShaderModule = makeShaderModule glslVertexShaderMain

fragShaderModule :: Vk.Shader.Module.M n 'GlslFragmentShader () ()
fragShaderModule = makeShaderModule glslFragmentShaderMain

makeShaderModule :: Spv sknd -> Vk.Shader.Module.M n sknd () ()
makeShaderModule code = Vk.Shader.Module.M createInfo nil nil
	where createInfo = Vk.Shader.Module.M.CreateInfo {
		Vk.Shader.Module.M.createInfoNext = Nothing,
		Vk.Shader.Module.M.createInfoFlags = def,
		Vk.Shader.Module.M.createInfoCode = code }

[glslVertexShader|

#version 450

layout(location = 0) in vec2 inPosition;
layout(location = 1) in vec3 inColor;

layout(location = 0) out vec3 fragColor;

void
main()
{
	gl_Position = vec4(inPosition, 0.0, 1.0);
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
