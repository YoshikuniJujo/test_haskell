{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
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
import Data.Word
import Data.Color
import Data.Time

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
import qualified Gpu.Vulkan.Device.Memory.AllocateInfo as Vk.Dvc.Mem.Buffer
import qualified Gpu.Vulkan.Device.Memory.Buffer as Vk.Dvc.Mem.Buffer
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.Queue.Enum as Vk.Queue
import qualified Gpu.Vulkan.Memory as Vk.Mem
import qualified Gpu.Vulkan.Command as Vk.Cmd

import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified Gpu.Vulkan.Descriptor.Enum as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscSetLyt
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Vk.DscSetLyt
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPool
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSet

import qualified Gpu.Vulkan.DescriptorSet.TypeLevel as Vk.DscSet.T

import Gpu.Vulkan.Pipeline.VertexInputState.BindingStrideList(AddType)

import Tools

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
initWindow frszd = do
	Just w <- do
		True <- Glfw.init
		Glfw.windowHint $ Glfw.WindowHint'ClientAPI Glfw.ClientAPI'NoAPI
		uncurry Glfw.createWindow windowSize windowName Nothing Nothing
	w <$ Glfw.setFramebufferSizeCallback
		w (Just $ \_ _ _ -> writeIORef frszd True)

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
	createSwapChain w sfc phdv qfis dv \sc scifmt ext ->
	Vk.Khr.Swapchain.getImages dv sc >>= \imgs ->
	createImageViews dv scifmt imgs \scivs ->
	createRenderPass dv scifmt \rp ->
	createPipelineLayout dv \dscslyt ppllyt ->
	createGraphicsPipeline dv ext rp ppllyt \gpl ->
	createFramebuffers dv ext rp scivs \fbs ->
	createCommandPool qfis dv \cp ->
	createVertexBuffer phdv dv gq cp \vb ->
	createIndexBuffer phdv dv gq cp \ib ->
	createDescriptorPool dv \dscp ->
	createUniformBuffers phdv dv dscslyt maxFramesInFlight \dscslyts ubs ums ->
	createDescriptorSets dv dscp ubs dscslyts >>= \dscss ->
	createCommandBuffers dv cp \cbs ->
	createSyncObjects dv \sos ->
	getCurrentTime >>= \tm ->
	mainLoop g w sfc phdv qfis dv gq pq sc ext scivs rp ppllyt gpl fbs vb ib cbs sos ubs ums dscss tm

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
	is <- findQueueFamilies phdvc sfc
	extensionSupported <- checkDeviceExtensionSupport phdvc
	if extensionSupported
	then (<$> querySwapChainSupport phdvc sfc) \spp ->
		bool (completeQueueFamilies is) Nothing
			$ null (formats spp) || null (presentModes spp)
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

createSwapChain :: Glfw.Window -> Vk.Khr.Surface.S ssfc -> Vk.PhDvc.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> (forall ss .
		Vk.Khr.Swapchain.S ss -> Vk.Format -> Vk.C.Extent2d -> IO a) ->
	IO a
createSwapChain win sfc phdvc qfis dvc f = do
	spp <- querySwapChainSupport phdvc sfc
	ext <- chooseSwapExtent win $ capabilities spp
	let	(crInfo, scifmt) = mkSwapchainCreateInfo sfc qfis spp ext
	Vk.Khr.Swapchain.create @() dvc crInfo nil nil \sc -> f sc scifmt ext

recreateSwapChain :: Glfw.Window -> Vk.Khr.Surface.S ssfc -> Vk.PhDvc.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Khr.Swapchain.S ssc ->
	IO (Vk.Format, Vk.C.Extent2d)
recreateSwapChain win sfc phdvc qfis0 dvc sc = do
	spp <- querySwapChainSupport phdvc sfc
	ext <- chooseSwapExtent win $ capabilities spp
	let	(crInfo, scifmt) = mkSwapchainCreateInfo sfc qfis0 spp ext
	(scifmt, ext) <$ Vk.Khr.Swapchain.recreate @() dvc crInfo nil nil sc

mkSwapchainCreateInfo :: Vk.Khr.Surface.S ss -> QueueFamilyIndices ->
	SwapChainSupportDetails -> Vk.C.Extent2d ->
	(Vk.Khr.Swapchain.CreateInfo n ss, Vk.Format)
mkSwapchainCreateInfo sfc qfis0 spp ext = (
	Vk.Khr.Swapchain.CreateInfo {
		Vk.Khr.Swapchain.createInfoNext = Nothing,
		Vk.Khr.Swapchain.createInfoFlags = def,
		Vk.Khr.Swapchain.createInfoSurface = sfc,
		Vk.Khr.Swapchain.createInfoMinImageCount = imgc,
		Vk.Khr.Swapchain.createInfoImageFormat =
			Vk.Khr.Surface.M.formatFormat fmt,
		Vk.Khr.Swapchain.createInfoImageColorSpace =
			Vk.Khr.Surface.M.formatColorSpace fmt,
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
		Vk.Khr.Swapchain.createInfoOldSwapchain = Nothing }, scifmt )
	where
	fmt = chooseSwapSurfaceFormat $ formats spp
	scifmt = Vk.Khr.Surface.M.formatFormat fmt
	presentMode = chooseSwapPresentMode $ presentModes spp
	caps = capabilities spp
	maxImgc = fromMaybe maxBound . onlyIf (> 0)
		$ Vk.Khr.Surface.M.capabilitiesMaxImageCount caps
	imgc = clamp
		(Vk.Khr.Surface.M.capabilitiesMinImageCount caps + 1) 0 maxImgc
	(ism, qfis) = bool
		(Vk.SharingModeConcurrent,
			[graphicsFamily qfis0, presentFamily qfis0])
		(Vk.SharingModeExclusive, [])
		(graphicsFamily qfis0 == presentFamily qfis0)

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

createImageViews :: Vk.Dvc.D sd -> Vk.Format -> [Vk.Image.Binded ss ss] ->
	(forall si . HeteroVarList Vk.ImgVw.I si -> IO a) -> IO a
createImageViews _dvc _fmt [] f = f HVNil
createImageViews dvc fmt (sci : scis) f =
	Vk.ImgVw.create dvc (mkImageViewCreateInfo fmt sci) nil nil \sciv ->
	createImageViews dvc fmt scis \scivs -> f $ sciv :...: scivs

recreateImageViews :: Vk.Dvc.D sd -> Vk.Format ->
	[Vk.Image.Binded ss ss] -> HeteroVarList Vk.ImgVw.I sis -> IO ()
recreateImageViews _dvc _scifmt [] HVNil = pure ()
recreateImageViews dvc scifmt (sci : scis) (iv :...: ivs) =
	Vk.ImgVw.recreate dvc (mkImageViewCreateInfo scifmt sci) nil nil iv >>
	recreateImageViews dvc scifmt scis ivs
recreateImageViews _ _ _ _ =
	error "number of Vk.Image.M.I and Vk.ImageView.M.I should be same"

mkImageViewCreateInfo ::
	Vk.Format -> Vk.Image.Binded ss ss -> Vk.ImgVw.CreateInfo ss ss ()
mkImageViewCreateInfo scifmt sci = Vk.ImgVw.CreateInfo {
	Vk.ImgVw.createInfoNext = Nothing,
	Vk.ImgVw.createInfoFlags = Vk.ImgVw.CreateFlagsZero,
	Vk.ImgVw.createInfoImage = sci,
	Vk.ImgVw.createInfoViewType = Vk.ImgVw.Type2d,
	Vk.ImgVw.createInfoFormat = scifmt,
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

type AtomUbo s = '(s, '[ 'Vk.DscSetLyt.Buffer '[ 'Atom UniformBufferObject]])

createPipelineLayout :: Vk.Dvc.D sd -> (forall sl sdsc .
		Vk.DscSetLyt.L sdsc '[ 'Vk.DscSetLyt.Buffer '[ 'Atom UniformBufferObject]] ->
		Vk.Ppl.Layout.LL sl '[AtomUbo sdsc] -> IO b) ->
	IO b
createPipelineLayout dvc f =
	createDescriptorSetLayout dvc \descriptorSetLayout ->
	let	pipelineLayoutInfo = Vk.Ppl.Layout.CreateInfo {
			Vk.Ppl.Layout.createInfoNext = Nothing,
			Vk.Ppl.Layout.createInfoFlags = zeroBits,
			Vk.Ppl.Layout.createInfoSetLayouts = Vk.Ppl.Layout.Layout descriptorSetLayout :...: HVNil,
			Vk.Ppl.Layout.createInfoPushConstantRanges = [] } in
	Vk.Ppl.Layout.create @() dvc pipelineLayoutInfo nil nil \ppllyt -> f descriptorSetLayout ppllyt

createDescriptorSetLayout :: Vk.Dvc.D sd -> (forall (s :: Type) .
	Vk.DscSetLyt.L s '[ 'Vk.DscSetLyt.Buffer '[ 'Atom UniformBufferObject]]
	-> IO a) -> IO a
createDescriptorSetLayout dvc = Vk.DscSetLyt.create dvc layoutInfo nil nil
	where
	layoutInfo :: Vk.DscSetLyt.CreateInfo ()
		'[ 'Vk.DscSetLyt.Buffer '[ 'Atom UniformBufferObject] ]
	layoutInfo = Vk.DscSetLyt.CreateInfo {
		Vk.DscSetLyt.createInfoNext = Nothing,
		Vk.DscSetLyt.createInfoFlags = zeroBits,
		Vk.DscSetLyt.createInfoBindings = uboLayoutBinding :...: HVNil }
	uboLayoutBinding :: Vk.DscSetLyt.Binding
		('Vk.DscSetLyt.Buffer '[ 'Atom UniformBufferObject])
	uboLayoutBinding = Vk.DscSetLyt.BindingBuffer {
		Vk.DscSetLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBuffer,
		Vk.DscSetLyt.bindingBufferStageFlags = Vk.ShaderStageVertexBit }

createGraphicsPipeline :: Vk.Dvc.D sd ->
	Vk.C.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.LL sl '[AtomUbo sdsc] ->
	(forall sg . Vk.Ppl.Graphics.G sg
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] -> IO a) -> IO a
createGraphicsPipeline dvc sce rp ppllyt f =
	Vk.Ppl.Graphics.createGs dvc Nothing (V14 pplInfo :...: HVNil)
			nil nil \(V2 gpl :...: HVNil) -> f gpl
	where pplInfo = mkGraphicsPipelineCreateInfo sce rp ppllyt

recreateGraphicsPipeline :: Vk.Dvc.D sd ->
	Vk.C.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.LL sl '[AtomUbo sdsc] ->
	Vk.Ppl.Graphics.G sg
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] -> IO ()
recreateGraphicsPipeline dvc sce rp ppllyt gpls = Vk.Ppl.Graphics.recreateGs
	dvc Nothing (V14 pplInfo :...: HVNil) nil nil (V2 gpls :...: HVNil)
	where pplInfo = mkGraphicsPipelineCreateInfo sce rp ppllyt

mkGraphicsPipelineCreateInfo ::
	Vk.C.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.LL sl '[AtomUbo sdsc] ->
	Vk.Ppl.Graphics.CreateInfo () '[
			'((), (), 'GlslVertexShader, (), (), ()),
			'((), (), 'GlslFragmentShader, (), (), ()) ]
		'(	(), '[AddType Vertex 'Vk.VtxInp.RateVertex],
			'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] )
		() () () () () () () () '(sl, '[AtomUbo sdsc]) sr '(sb, vs', ts')
mkGraphicsPipelineCreateInfo sce rp ppllyt = Vk.Ppl.Graphics.CreateInfo {
	Vk.Ppl.Graphics.createInfoNext = Nothing,
	Vk.Ppl.Graphics.createInfoFlags = Vk.Ppl.CreateFlagsZero,
	Vk.Ppl.Graphics.createInfoStages = shaderStages,
	Vk.Ppl.Graphics.createInfoVertexInputState = Just $ V3 def,
	Vk.Ppl.Graphics.createInfoInputAssemblyState = Just inputAssembly,
	Vk.Ppl.Graphics.createInfoViewportState = Just $ mkViewportState sce,
	Vk.Ppl.Graphics.createInfoRasterizationState = Just rasterizer,
	Vk.Ppl.Graphics.createInfoMultisampleState = Just multisampling,
	Vk.Ppl.Graphics.createInfoDepthStencilState = Nothing,
	Vk.Ppl.Graphics.createInfoColorBlendState = Just colorBlending,
	Vk.Ppl.Graphics.createInfoDynamicState = Nothing,
	Vk.Ppl.Graphics.createInfoLayout = V2 ppllyt,
	Vk.Ppl.Graphics.createInfoRenderPass = rp,
	Vk.Ppl.Graphics.createInfoSubpass = 0,
	Vk.Ppl.Graphics.createInfoBasePipelineHandle = Nothing,
	Vk.Ppl.Graphics.createInfoBasePipelineIndex = - 1,
	Vk.Ppl.Graphics.createInfoTessellationState = Nothing }

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

inputAssembly :: Vk.Ppl.InpAsmbSt.CreateInfo ()
inputAssembly = Vk.Ppl.InpAsmbSt.CreateInfo {
	Vk.Ppl.InpAsmbSt.createInfoNext = Nothing,
	Vk.Ppl.InpAsmbSt.createInfoFlags = zeroBits,
	Vk.Ppl.InpAsmbSt.createInfoTopology = Vk.PrimitiveTopologyTriangleList,
	Vk.Ppl.InpAsmbSt.createInfoPrimitiveRestartEnable = False }

mkViewportState :: Vk.C.Extent2d -> Vk.Ppl.ViewportSt.CreateInfo n
mkViewportState sce = Vk.Ppl.ViewportSt.CreateInfo {
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
	Vk.Ppl.RstSt.createInfoFrontFace = Vk.FrontFaceCounterClockwise,
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

createFramebuffers :: Vk.Dvc.D sd -> Vk.C.Extent2d ->
	Vk.RndrPass.R sr -> HeteroVarList Vk.ImgVw.I sis ->
	(forall sfs . RecreateFramebuffers sis sfs =>
		HeteroVarList Vk.Frmbffr.F sfs -> IO a) -> IO a
createFramebuffers _ _ _ HVNil f = f HVNil
createFramebuffers dvc sce rp (iv :...: ivs) f =
	Vk.Frmbffr.create dvc (mkFramebufferCreateInfo sce rp iv) nil nil \fb ->
	createFramebuffers dvc sce rp ivs \fbs -> f (fb :...: fbs)

class RecreateFramebuffers (sis :: [Type]) (sfs :: [Type]) where
	recreateFramebuffers :: Vk.Dvc.D sd -> Vk.C.Extent2d ->
		Vk.RndrPass.R sr -> HeteroVarList Vk.ImgVw.I sis ->
		HeteroVarList Vk.Frmbffr.F sfs -> IO ()

instance RecreateFramebuffers '[] '[] where
	recreateFramebuffers _dvc _sce _rp HVNil HVNil = pure ()

instance RecreateFramebuffers sis sfs =>
	RecreateFramebuffers (si ': sis) (sf ': sfs) where
	recreateFramebuffers dvc sce rp (sciv :...: scivs) (fb :...: fbs) =
		Vk.Frmbffr.recreate dvc
			(mkFramebufferCreateInfo sce rp sciv) nil nil fb >>
		recreateFramebuffers dvc sce rp scivs fbs

mkFramebufferCreateInfo ::
	Vk.C.Extent2d -> Vk.RndrPass.R sr -> Vk.ImgVw.I si ->
	Vk.Frmbffr.CreateInfo () sr '[si]
mkFramebufferCreateInfo sce rp attch = Vk.Frmbffr.CreateInfo {
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
		Vk.Bffr.Binded sm sb nm '[ 'List Vertex] -> IO a) -> IO a
createVertexBuffer phdvc dvc gq cp f =
	createBufferList phdvc dvc (length vertices)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		Vk.Mem.PropertyDeviceLocalBit \b _ ->
	createBufferList phdvc dvc (length vertices)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mem.PropertyHostVisibleBit .|.
			Vk.Mem.PropertyHostCoherentBit ) \b' bm' -> do
	Vk.Dvc.Mem.Buffer.write @('List Vertex) dvc bm' zeroBits vertices
	copyBuffer dvc gq cp b' b
	f b

createIndexBuffer :: Vk.PhDvc.P ->
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[ 'List Word16] -> IO a) -> IO a
createIndexBuffer phdvc dvc gq cp f =
	createBufferList phdvc dvc (length indices)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageIndexBufferBit)
		Vk.Mem.PropertyDeviceLocalBit \b _ ->
	createBufferList phdvc dvc (length indices)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mem.PropertyHostVisibleBit .|.
			Vk.Mem.PropertyHostCoherentBit ) \b' bm' -> do
	Vk.Dvc.Mem.Buffer.write @('List Word16) dvc bm' zeroBits indices
	copyBuffer dvc gq cp b' b
	f b

createUniformBuffers ::
	Vk.PhDvc.P -> Vk.Dvc.D sd ->
	Vk.DscSetLyt.L sdsc '[ 'Vk.DscSetLyt.Buffer '[ 'Atom UniformBufferObject]] ->
	Int -> (forall slyts smsbs . (
		ListToHeteroVarList slyts,
		Update smsbs slyts,
		DescriptorSetIndex slyts sdsc ) =>
		HeteroVarList Vk.DscSet.Layout slyts ->
		HeteroVarList BindedUbo smsbs ->
		HeteroVarList MemoryUbo (MapFst smsbs) -> IO a) -> IO a
createUniformBuffers _ _ _ 0 f = f HVNil HVNil HVNil
createUniformBuffers ph dvc dscslyt n f =
	createUniformBuffer1 ph dvc \(b :: BindedUbo smsb) m ->
		createUniformBuffers ph dvc dscslyt (n - 1) \ls (bs :: HeteroVarList BindedUbo smsbs) ms -> f
			(Vk.DscSet.Layout dscslyt :...: ls)
			(b :...: bs :: HeteroVarList BindedUbo (smsb ': smsbs))
			(m :...: ms)

type family MapFst abs where
	MapFst '[] = '[]
	MapFst ( '(a, b, c :: k) ': abs) = a ': MapFst abs

data BindedUbo smsb where
	BindedUbo :: Vk.Bffr.Binded sm sb nm '[ 'Atom UniformBufferObject] ->
		BindedUbo '(sm, sb, nm)

newtype MemoryUbo sm =
	MemoryUbo (Vk.Dvc.Mem.Buffer.M sm '[ '[ 'Atom UniformBufferObject]])

createUniformBuffer1 :: Vk.PhDvc.P -> Vk.Dvc.D sd -> (forall sm sb .
		BindedUbo '(sm, sb, nm) -> MemoryUbo sm -> IO b) -> IO b
createUniformBuffer1 phdvc dvc f = createBufferAtom phdvc dvc
		Vk.Bffr.UsageUniformBufferBit
		(	Vk.Mem.PropertyHostVisibleBit .|.
			Vk.Mem.PropertyHostCoherentBit ) \b m ->
	f (BindedUbo b) (MemoryUbo m)

createDescriptorPool ::
	Vk.Dvc.D sd -> (forall sp . Vk.DscPool.P sp -> IO a) -> IO a
createDescriptorPool dvc = Vk.DscPool.create @() dvc poolInfo nil nil
	where
	poolInfo = Vk.DscPool.CreateInfo {
		Vk.DscPool.createInfoNext = Nothing,
		Vk.DscPool.createInfoFlags = zeroBits,
		Vk.DscPool.createInfoMaxSets = maxFramesInFlight,
		Vk.DscPool.createInfoPoolSizes = [poolSize] }
	poolSize = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeUniformBuffer,
		Vk.DscPool.sizeDescriptorCount = maxFramesInFlight }

createDescriptorSets :: (ListToHeteroVarList ss, Update smsbs ss) =>
	Vk.Dvc.D sd -> Vk.DscPool.P sp -> HeteroVarList BindedUbo smsbs ->
	HeteroVarList Vk.DscSet.Layout ss ->
	IO (HeteroVarList (Vk.DscSet.S sd sp) ss)
createDescriptorSets dvc dscp ubs dscslyts = do
	dscss <- Vk.DscSet.allocateSs @() dvc allocInfo
	update dvc ubs dscss
	pure dscss
	where
	allocInfo = Vk.DscSet.AllocateInfo {
		Vk.DscSet.allocateInfoNext = Nothing,
		Vk.DscSet.allocateInfoDescriptorPool = dscp,
		Vk.DscSet.allocateInfoSetLayouts = dscslyts }

descriptorWrite ::
	Vk.Bffr.Binded sm sb nm '[ 'Atom UniformBufferObject] ->
	Vk.DscSet.S sd sp slbts ->
	Vk.DscSet.Write () sd sp slbts (Vk.DscSet.WriteSourcesArgBuffer '[ '(
		sb, sm, nm,
		'[ 'Atom UniformBufferObject], 'Atom UniformBufferObject )])
descriptorWrite ub dscs = Vk.DscSet.Write {
	Vk.DscSet.writeNext = Nothing,
	Vk.DscSet.writeDstSet = dscs,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeUniformBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		Singleton bufferInfo
	}
	where bufferInfo = Vk.Dsc.BufferInfoAtom ub

class Update smsbs slbtss where
	update :: Vk.Dvc.D sd -> HeteroVarList BindedUbo smsbs -> HeteroVarList (Vk.DscSet.S sd sp) slbtss -> IO ()

instance Update '[] '[] where update _ HVNil HVNil = pure ()

instance Update '[t] '[] where update _ HVNil HVNil = pure ()

instance (
	Vk.DscSet.T.BindingAndArrayElem (Vk.DscSet.T.BindingTypesFromLayoutArg dscs) '[ 'Atom UniformBufferObject],
	Update ubs dscss) => Update (ub ': ubs) (dscs ': dscss) where
	update dvc (BindedUbo ub :...: ubs) (dscs :...: dscss) = do
		Vk.DscSet.updateDs @() @() dvc
			(Singleton . Vk.DscSet.Write_ $ descriptorWrite ub dscs) []
		update dvc ubs dscss

{-
	update :: Vk.DscSet.BindingAndArrayElem (Vk.DscSet.BindingTypesFromLayoutArg 
		HeteroVarList BindedUbo smsbs -> HeteroVarList (Vk.DscSet.S sd sp) slbtss -> IO ()
	update HVNil HVNil = pure ()
	update (BindedUbo ub :...: ubs) (dscs :...: dscss) = do
		Vk.DscSet.updateDs @() @() dvc
			(Singleton . Vk.DscSet.Write_ $ descriptorWrite ub dscs) []
		update ubs dscss
		-}

createBufferAtom :: forall sd nm a b . Storable a => Vk.PhDvc.P -> Vk.Dvc.D sd ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> (
		forall sm sb .
		Vk.Bffr.Binded sm sb nm '[ 'Atom a] ->
		Vk.Dvc.Mem.Buffer.M sm '[ '[ 'Atom a]] -> IO b) -> IO b
createBufferAtom p dv usg props = createBuffer p dv ObjectLengthAtom usg props

createBufferList :: forall sd nm a b . Storable a => Vk.PhDvc.P -> Vk.Dvc.D sd ->
	Int -> Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> (
		forall sm sb .
		Vk.Bffr.Binded sm sb nm '[ 'List a] ->
		Vk.Dvc.Mem.Buffer.M sm '[ '[ 'List a]] -> IO b ) -> IO b
createBufferList p dv ln usg props = createBuffer p dv (ObjectLengthList ln) usg props

createBuffer :: forall sd nm o a . Data.Kind.Object.SizeAlignment o =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> ObjectLength o ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[o] -> Vk.Dvc.Mem.Buffer.M sm '[ '[o]] ->
		IO a) -> IO a
createBuffer p dv ln usg props f = Vk.Bffr.create dv bffrInfo nil nil \b -> do
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMemoryType p (Vk.Mem.M.requirementsMemoryTypeBits reqs) props
	Vk.Bffr.allocateBind dv (Singleton $ V3 b) (allcInfo mt) nil nil
		$ f . \(Singleton (V3 bnd)) -> bnd
	where
	bffrInfo :: Vk.Bffr.CreateInfo () '[o]
	bffrInfo = Vk.Bffr.CreateInfo {
		Vk.Bffr.createInfoNext = Nothing,
		Vk.Bffr.createInfoFlags = zeroBits,
		Vk.Bffr.createInfoLengths = singleton ln,
		Vk.Bffr.createInfoUsage = usg,
		Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Bffr.createInfoQueueFamilyIndices = [] }
	allcInfo :: Vk.Mem.TypeIndex -> Vk.Dvc.Mem.Buffer.AllocateInfo ()
	allcInfo mt = Vk.Dvc.Mem.Buffer.AllocateInfo {
		Vk.Dvc.Mem.Buffer.allocateInfoNext = Nothing,
		Vk.Dvc.Mem.Buffer.allocateInfoMemoryTypeIndex = mt }

findMemoryType :: Vk.PhDvc.P -> Vk.Mem.M.TypeBits -> Vk.Mem.PropertyFlags ->
	IO Vk.Mem.TypeIndex
findMemoryType phdvc flt props =
	fromMaybe (error msg) . suitable <$> Vk.PhDvc.getMemoryProperties phdvc
	where
	msg = "failed to find suitable memory type!"
	suitable props1 = fst <$> find ((&&)
		<$> (`Vk.Mem.M.elemTypeIndex` flt) . fst
		<*> checkBits props . Vk.Mem.M.mTypePropertyFlags . snd) tps
		where tps = Vk.PhDvc.memoryPropertiesMemoryTypes props1

copyBuffer :: forall sd sc sm sb nm sm' sb' nm' a . Storable a =>
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
	Vk.Bffr.Binded sm sb nm '[ 'List a] ->
	Vk.Bffr.Binded sm' sb' nm' '[ 'List a] -> IO ()
copyBuffer dvc gq cp src dst = do
	Vk.CmdBffr.allocateNew
		@() dvc allocInfo \(Singleton (cb :: Vk.CmdBffr.C s '[])) -> do
		let	submitInfo :: Vk.SubmitInfoNew () '[] '[ '(s, '[])] '[]
			submitInfo = Vk.SubmitInfoNew {
				Vk.submitInfoNextNew = Nothing,
				Vk.submitInfoWaitSemaphoreDstStageMasksNew = HVNil,
				Vk.submitInfoCommandBuffersNew = Singleton $ V2 cb,
				Vk.submitInfoSignalSemaphoresNew = HVNil }
		Vk.CmdBffr.begin @() @() cb beginInfo do
			Vk.Cmd.copyBuffer @'[ '[ 'List a]] cb src dst
		Vk.Queue.submitNew gq (Singleton $ V4 submitInfo) Nothing
		Vk.Queue.waitIdle gq
	where
	allocInfo :: Vk.CmdBffr.AllocateInfoNew () sc '[ '[]]
	allocInfo = Vk.CmdBffr.AllocateInfoNew {
		Vk.CmdBffr.allocateInfoNextNew = Nothing,
		Vk.CmdBffr.allocateInfoCommandPoolNew = cp,
		Vk.CmdBffr.allocateInfoLevelNew = Vk.CmdBffr.LevelPrimary }
	beginInfo = Vk.CmdBffr.M.BeginInfo {
		Vk.CmdBffr.beginInfoNext = Nothing,
		Vk.CmdBffr.beginInfoFlags = Vk.CmdBffr.UsageOneTimeSubmitBit,
		Vk.CmdBffr.beginInfoInheritanceInfo = Nothing }

createCommandBuffers ::
	forall sd scp a . Vk.Dvc.D sd -> Vk.CmdPool.C scp ->
	(forall scb vss . VssList vss =>
		HeteroVarList (Vk.CmdBffr.C scb) (vss :: [[Type]]) -> IO a) ->
	IO a
createCommandBuffers dvc cp f = mkVss maxFramesInFlight \(_p :: Proxy vss1) ->
	Vk.CmdBffr.allocateNew @() @vss1 dvc (allocInfo @vss1) (f @_ @vss1)
	where
	allocInfo :: forall vss . Vk.CmdBffr.AllocateInfoNew () scp vss
	allocInfo = Vk.CmdBffr.AllocateInfoNew {
		Vk.CmdBffr.allocateInfoNextNew = Nothing,
		Vk.CmdBffr.allocateInfoCommandPoolNew = cp,
		Vk.CmdBffr.allocateInfoLevelNew = Vk.CmdBffr.LevelPrimary }

class VssList (vss :: [[Type]]) where
	vssListIndex ::
		HeteroVarList (Vk.CmdBffr.C scb) vss -> Int ->
		Vk.CmdBffr.C scb '[AddType Vertex 'Vk.VtxInp.RateVertex]

instance VssList '[] where
	vssListIndex HVNil _ = error "index too large"

instance VssList vss =>
	VssList ('[AddType Vertex 'Vk.VtxInp.RateVertex] ': vss) where
	vssListIndex (cb :...: _) 0 = cb
	vssListIndex (_ :...: cbs) n = vssListIndex cbs (n - 1)

mkVss :: Int -> (forall (vss :: [[Type]]) .
	(TpLvlLst.Length [Type] vss, ListToHeteroVarList vss, VssList vss) =>
	Proxy vss -> a) -> a
mkVss 0 f = f (Proxy @'[])
mkVss n f = mkVss (n - 1) \p -> f $ addTypeToProxy p

addTypeToProxy ::
	Proxy vss -> Proxy ('[AddType Vertex 'Vk.VtxInp.RateVertex] ': vss)
addTypeToProxy Proxy = Proxy

data SyncObjects (ssos :: ([Type], [Type], [Type])) where
	SyncObjects :: {
		imageAvailableSemaphores :: HeteroVarList Vk.Semaphore.S siass,
		renderFinishedSemaphores :: HeteroVarList Vk.Semaphore.S srfss,
		inFlightFences :: HeteroVarList Vk.Fence.F sfss } ->
		SyncObjects '(siass, srfss, sfss)

createSyncObjects ::
	Vk.Dvc.D sd -> (forall ssos . SyncObjects ssos -> IO a ) -> IO a
createSyncObjects dvc f =
	heteroVarListReplicateM maxFramesInFlight
		(Vk.Semaphore.create @() dvc def nil nil) \iass ->
	heteroVarListReplicateM maxFramesInFlight
		(Vk.Semaphore.create @() dvc def nil nil) \rfss ->
	heteroVarListReplicateM maxFramesInFlight
		(Vk.Fence.create @() dvc fncInfo nil nil) \iffs ->
	f $ SyncObjects iass rfss iffs
	where
	fncInfo = def { Vk.Fence.createInfoFlags = Vk.Fence.CreateSignaledBit }

recordCommandBuffer :: forall scb sr sl sdsc sf sg sm sb nm sm' sb' nm' sdsc' sp .
	Vk.CmdBffr.C scb '[AddType Vertex 'Vk.VtxInp.RateVertex] ->
	Vk.RndrPass.R sr -> Vk.Frmbffr.F sf -> Vk.C.Extent2d ->
	Vk.Ppl.Layout.LL sl '[AtomUbo sdsc] ->
	Vk.Ppl.Graphics.G sg
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] ->
	Vk.Bffr.Binded sm sb nm '[ 'List Vertex] ->
	Vk.Bffr.Binded sm' sb' nm' '[ 'List Word16] ->
--	Vk.DscSet.S sdsc' sp s ->
	Vk.DscSet.S sdsc' sp (AtomUbo sdsc) ->
	IO ()
recordCommandBuffer cb rp fb sce ppllyt gpl vb ib dscs =
	Vk.CmdBffr.begin @() @() cb def $
	Vk.Cmd.beginRenderPass cb rpInfo Vk.Subpass.ContentsInline do
	Vk.Cmd.bindPipeline cb Vk.Ppl.BindPointGraphics gpl
	Vk.Cmd.bindVertexBuffers cb
		. singleton . V4 $ Vk.Bffr.IndexedList @_ @_ @_ @Vertex vb
	Vk.Cmd.bindIndexBuffer cb $ Vk.Bffr.IndexedList @_ @_ @_ @Word16 ib
	Vk.Cmd.bindDescriptorSets cb Vk.Ppl.BindPointGraphics ppllyt
		(Singleton $ Vk.Cmd.DescriptorSet dscs) []
	Vk.Cmd.drawIndexed cb (fromIntegral $ length indices) 1 0 0 0
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

mainLoop :: (RecreateFramebuffers ss sfs, VssList vss, DescriptorSetIndex slyts sdsc) =>
	FramebufferResized ->
	Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.Khr.Swapchain.S ssc -> Vk.C.Extent2d -> HeteroVarList Vk.ImgVw.I ss ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.LL sl '[AtomUbo sdsc] -> Vk.Ppl.Graphics.G sg
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] ->
	HeteroVarList Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[ 'List Vertex] ->
	Vk.Bffr.Binded sm' sb' nm' '[ 'List Word16] ->
	HeteroVarList (Vk.CmdBffr.C scb) vss ->
	SyncObjects siassrfssfs ->
	HeteroVarList BindedUbo smsbs ->
	HeteroVarList MemoryUbo (MapFst smsbs) ->
	HeteroVarList (Vk.DscSet.S sd sp) slyts ->
	UTCTime ->
	IO ()
mainLoop g w sfc phdvc qfis dvc gq pq sc ext0 scivs rp ppllyt gpl fbs vb ib cbs iasrfsifs ubs ums dscss tm0 = do
	($ cycle [0 .. maxFramesInFlight - 1]) . ($ ext0) $ fix \loop ext (cf : cfs) -> do
		Glfw.pollEvents
		tm <- getCurrentTime
		runLoop w sfc phdvc qfis dvc gq pq
			sc g ext scivs rp ppllyt gpl fbs vb ib cbs iasrfsifs ubs ums dscss
			(realToFrac $ tm `diffUTCTime` tm0)
			cf (`loop` cfs)
	Vk.Dvc.waitIdle dvc

runLoop :: (RecreateFramebuffers sis sfs, VssList vss, DescriptorSetIndex slyts sdsc) =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc -> Vk.PhDvc.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.Khr.Swapchain.S ssc -> FramebufferResized -> Vk.C.Extent2d ->
	HeteroVarList Vk.ImgVw.I sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.LL sl '[AtomUbo sdsc] ->
	Vk.Ppl.Graphics.G sg '[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] ->
	HeteroVarList Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[ 'List Vertex] ->
	Vk.Bffr.Binded sm' sb' nm' '[ 'List Word16] ->
	HeteroVarList (Vk.CmdBffr.C scb) vss ->
	SyncObjects siassrfssfs ->
	HeteroVarList BindedUbo smsbs ->
	HeteroVarList MemoryUbo (MapFst smsbs) ->
	HeteroVarList (Vk.DscSet.S sd sp) slyts ->
	Float ->
	Int ->
	(Vk.C.Extent2d -> IO ()) -> IO ()
runLoop win sfc phdvc qfis dvc gq pq sc frszd ext scivs rp ppllyt gpl fbs vb ib cbs iasrfsifs ubs ums dscss tm cf loop = do
	catchAndRecreate win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs loop
		$ drawFrame dvc gq pq sc ext rp ppllyt gpl fbs vb ib cbs iasrfsifs ubs ums dscss tm cf
	cls <- Glfw.windowShouldClose win
	if cls then (pure ()) else checkFlag frszd >>= bool (loop ext)
		(loop =<< recreateSwapChainEtc
			win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs)

drawFrame :: forall sfs sd ssc sr sl sdsc sg sm sb nm sm' sb' nm' scb ssos vss smsbs sdsc' sp slyts .
	(VssList vss, DescriptorSetIndex slyts sdsc) =>
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q -> Vk.Khr.Swapchain.S ssc ->
	Vk.C.Extent2d -> Vk.RndrPass.R sr ->
	Vk.Ppl.Layout.LL sl '[AtomUbo sdsc] ->
	Vk.Ppl.Graphics.G sg '[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] ->
	HeteroVarList Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[ 'List Vertex] ->
	Vk.Bffr.Binded sm' sb' nm' '[ 'List Word16] ->
	HeteroVarList (Vk.CmdBffr.C scb) vss -> SyncObjects ssos ->
	HeteroVarList BindedUbo smsbs ->
	HeteroVarList MemoryUbo (MapFst smsbs) ->
	HeteroVarList (Vk.DscSet.S sdsc' sp) slyts ->
	Float ->
	Int -> IO ()
drawFrame dvc gq pq sc ext rp ppllyt gpl fbs vb ib cbs (SyncObjects iass rfss iffs) ubs ums dscss tm cf =
	heteroVarListIndex iass cf \(ias :: Vk.Semaphore.S sias) ->
	heteroVarListIndex rfss cf \(rfs :: Vk.Semaphore.S srfs) ->
	heteroVarListIndex iffs cf \(id &&& singleton -> (iff, siff)) ->
	heteroVarListIndex ubs cf \ub -> heteroVarListIndex ums cf \um ->
	descriptorSetIndex dscss cf \dscs -> do
	Vk.Fence.waitForFs dvc siff True maxBound
	imgIdx <- Vk.Khr.acquireNextImageResult [Vk.Success, Vk.SuboptimalKhr]
		dvc sc uint64Max (Just ias) Nothing
	Vk.Fence.resetFs dvc siff
	Vk.CmdBffr.reset cb def
	heteroVarListIndex fbs imgIdx \fb ->
		recordCommandBuffer cb rp fb ext ppllyt gpl vb ib dscs
	updateUniformBuffer dvc um ext tm
	let	submitInfo :: Vk.SubmitInfoNew () '[sias]
			'[ '(scb, '[AddType Vertex 'Vk.VtxInp.RateVertex])]
			'[srfs]
		submitInfo = Vk.SubmitInfoNew {
			Vk.submitInfoNextNew = Nothing,
			Vk.submitInfoWaitSemaphoreDstStageMasksNew = singleton
				$ Vk.SemaphorePipelineStageFlags ias
					Vk.Ppl.StageColorAttachmentOutputBit,
			Vk.submitInfoCommandBuffersNew = singleton $ V2 cb,
			Vk.submitInfoSignalSemaphoresNew = singleton rfs }
		presentInfo = Vk.Khr.PresentInfo {
			Vk.Khr.presentInfoNext = Nothing,
			Vk.Khr.presentInfoWaitSemaphores = singleton rfs,
			Vk.Khr.presentInfoSwapchainImageIndices = singleton
				$ Vk.Khr.SwapchainImageIndex sc imgIdx }
	Vk.Queue.submitNew gq (singleton $ V4 submitInfo) $ Just iff
	catchAndSerialize $ Vk.Khr.queuePresent @() pq presentInfo
	where	cb = cbs `vssListIndex` cf

class DescriptorSetIndex aus s where
	descriptorSetIndex :: HeteroVarList (Vk.DscSet.S sdsc sp) aus -> Int ->
		(Vk.DscSet.S sdsc sp (AtomUbo s) -> a) -> a

instance DescriptorSetIndex '[] s where
	descriptorSetIndex _ _ f = f $ error "index too large"

instance DescriptorSetIndex aus s =>
	DescriptorSetIndex (AtomUbo s ': aus) s where
	descriptorSetIndex (ds :...: _) 0 f = f ds
	descriptorSetIndex (_ :...: dss) n f = descriptorSetIndex dss (n - 1) f

updateUniformBuffer :: Vk.Dvc.D sd -> MemoryUbo sm -> Vk.C.Extent2d -> Float -> IO ()
updateUniformBuffer dvc (MemoryUbo um) sce tm =
	Vk.Dvc.Mem.Buffer.write @('Atom UniformBufferObject) dvc um zeroBits ubo
	where ubo = UniformBufferObject {
		uniformBufferObjectModel = Cglm.glmRotate
			Cglm.glmMat4Identity
			(tm * Cglm.glmRad 90)
			(Cglm.Vec3 $ 0 :. 0 :. 1 :. NilL),
		uniformBufferObjectView = Cglm.glmLookat -- Cglm.glmMat4Identity,
			(Cglm.Vec3 $ 2 :. 2 :. 2 :. NilL)
			(Cglm.Vec3 $ 0 :. 0 :. 0 :. NilL)
			(Cglm.Vec3 $ 0 :. 0 :. 1 :. NilL),
		uniformBufferObjectProj = Cglm.modifyMat4 1 1 negate -- Cglm.glmMat4Identity }
			$ Cglm.glmPerspective
				(Cglm.glmRad 45)
				(fromIntegral (Vk.C.extent2dWidth sce) /
					fromIntegral (Vk.C.extent2dHeight sce))
				0.1 10 }

catchAndSerialize :: IO () -> IO ()
catchAndSerialize =
	(`catch` \(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

catchAndRecreate :: RecreateFramebuffers sis sfs =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Khr.Swapchain.S ssc ->
	HeteroVarList Vk.ImgVw.I sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.LL sl '[AtomUbo sdsc] ->
	Vk.Ppl.Graphics.G sg
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] ->
	HeteroVarList Vk.Frmbffr.F sfs ->
	(Vk.C.Extent2d -> IO ()) -> IO () -> IO ()
catchAndRecreate win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs loop act =
	catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just ()
		Vk.SuboptimalKhr -> Just ()
		_ -> Nothing)
	act
	\_ -> loop =<< recreateSwapChainEtc
		win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs

recreateSwapChainEtc :: RecreateFramebuffers sis sfs =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Khr.Swapchain.S ssc -> HeteroVarList Vk.ImgVw.I sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.LL sl '[AtomUbo sdsc] ->
	Vk.Ppl.Graphics.G sg
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] ->
	HeteroVarList Vk.Frmbffr.F sfs -> IO Vk.C.Extent2d
recreateSwapChainEtc win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs = do
	waitFramebufferSize win
	Vk.Dvc.waitIdle dvc

	(scifmt, ext) <- recreateSwapChain win sfc phdvc qfis dvc sc
	ext <$ do
		Vk.Khr.Swapchain.getImages dvc sc >>= \imgs ->
			recreateImageViews dvc scifmt imgs scivs
		recreateGraphicsPipeline dvc ext rp ppllyt gpl
		recreateFramebuffers dvc ext rp scivs fbs

waitFramebufferSize :: Glfw.Window -> IO ()
waitFramebufferSize win = Glfw.getFramebufferSize win >>= \sz ->
	when (zero sz) $ fix \loop -> (`when` loop) . zero =<<
		Glfw.waitEvents *> Glfw.getFramebufferSize win
	where zero = uncurry (||) . ((== 0) *** (== 0))

data Vertex = Vertex { vertexPos :: Cglm.Vec2, vertexColor :: Cglm.Vec3 }
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
	Vertex (Cglm.Vec2 $ (- 0.5) :. (- 0.5) :. NilL)
		(Cglm.Vec3 $ 1.0 :. 0.0 :. 0.0 :. NilL),
	Vertex (Cglm.Vec2 $ 0.5 :. (- 0.5) :. NilL)
		(Cglm.Vec3 $ 0.0 :. 1.0 :. 0.0 :. NilL),
	Vertex (Cglm.Vec2 $ 0.5 :. 0.5 :. NilL)
		(Cglm.Vec3 $ 0.0 :. 0.0 :. 1.0 :. NilL),
	Vertex (Cglm.Vec2 $ (- 0.5) :. 0.5 :. NilL)
		(Cglm.Vec3 $ 1.0 :. 1.0 :. 1.0 :. NilL) ]

indices :: [Word16]
indices = [0, 1, 2, 2, 3, 0]

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

instance SizeAlignmentList UniformBufferObject
instance Foreign.Storable.Generic.G UniformBufferObject

vertShaderModule :: Vk.Shader.Module.M n 'GlslVertexShader () ()
vertShaderModule = mkShaderModule glslVertexShaderMain

fragShaderModule :: Vk.Shader.Module.M n 'GlslFragmentShader () ()
fragShaderModule = mkShaderModule glslFragmentShaderMain

mkShaderModule :: Spv sknd -> Vk.Shader.Module.M n sknd () ()
mkShaderModule code = Vk.Shader.Module.M createInfo nil nil
	where createInfo = Vk.Shader.Module.M.CreateInfo {
		Vk.Shader.Module.M.createInfoNext = Nothing,
		Vk.Shader.Module.M.createInfoFlags = def,
		Vk.Shader.Module.M.createInfoCode = code }

[glslVertexShader|

#version 450

layout(binding = 0) uniform UniformBufferObject {
	mat4 model;
	mat4 view;
	mat4 proj;
	} ubo;

layout(location = 0) in vec2 inPosition;
layout(location = 1) in vec3 inColor;

layout(location = 0) out vec3 fragColor;

void
main()
{
	gl_Position = ubo.proj * ubo.view * ubo.model * vec4(inPosition, 0.0, 1.0);
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
