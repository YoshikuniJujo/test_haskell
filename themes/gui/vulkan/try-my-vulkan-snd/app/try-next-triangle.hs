{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import GHC.Generics
import GHC.Tuple
import Foreign.Storable
import Foreign.Pointable
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Exception
import Data.Default
import Data.Bits
import Data.Bool
import Data.Maybe
import Data.List
import Data.Word
import Data.IORef
import Data.List.Length
import Data.Color

import Foreign.Storable.SizeAlignment

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

import qualified Gpu.Vulkan.Middle as Vk
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
import qualified Gpu.Vulkan.PhysicalDevice as Vk.PhysicalDevice
import qualified Gpu.Vulkan.QueueFamily as Vk.QueueFamily
import qualified Gpu.Vulkan.QueueFamily.EnumManual as Vk.QueueFamily
import qualified Gpu.Vulkan.Device as Vk.Device
import qualified Gpu.Vulkan.Device.Type as Vk.Device
import qualified Gpu.Vulkan.Device.Middle as Vk.Device.M
import qualified Gpu.Vulkan.Device.Queue as Vk.Device.Queue
import qualified Gpu.Vulkan.Device.Queue.Enum as Vk.Device.Queue
import qualified Gpu.Vulkan.Khr.Surface as Vk.Khr.Surface
import qualified Gpu.Vulkan.Khr.Surface.Middle as Vk.Khr.Surface.M
import qualified Gpu.Vulkan.Khr.Surface.PhysicalDevice as
	Vk.Khr.Surface.PhysicalDevice
import qualified Gpu.Vulkan.Khr.Swapchain as Vk.Khr.Swapchain
import qualified Gpu.Vulkan.Khr.Swapchain.Enum as Vk.Khr.Swapchain
import qualified Gpu.Vulkan.Image.Middle as Vk.Image
import qualified Gpu.Vulkan.Image.Enum as Vk.Image
import qualified Gpu.Vulkan.ImageView.Middle as Vk.ImageView
import qualified Gpu.Vulkan.ImageView.Enum as Vk.ImageView
import qualified Gpu.Vulkan.Component as Vk.Component
import qualified Gpu.Vulkan.Component.Enum as Vk.Component
import qualified Gpu.Vulkan.ShaderModule.Middle as Vk.Shader.Module
import qualified Gpu.Vulkan.Pipeline.ShaderStage.Middle as Vk.Ppl.ShaderStage
import qualified Gpu.Vulkan.Pipeline.ShaderStage.Enum as Vk.Ppl.ShaderStage
import qualified Gpu.Vulkan.Pipeline.VertexInputState as Vk.Ppl.VertexInputSt
import qualified Gpu.Vulkan.Pipeline.VertexInputState.Middle as
	Vk.Ppl.VertexInputSt.M
import qualified Gpu.Vulkan.Pipeline.InputAssemblyState as Vk.Ppl.InpAsmbSt
import qualified Gpu.Vulkan.Pipeline.ViewportState as Vk.Ppl.ViewportSt
import qualified Gpu.Vulkan.Pipeline.RasterizationState as Vk.Ppl.RstSt
import qualified Gpu.Vulkan.Pipeline.MultisampleState as Vk.Ppl.MulSmplSt
import qualified Gpu.Vulkan.Sample as Vk.Sample
import qualified Gpu.Vulkan.Sample.Enum as Vk.Sample
import qualified Gpu.Vulkan.Pipeline.ColorBlendAttachment as Vk.Ppl.ClrBlndAtt
import qualified Gpu.Vulkan.ColorComponent.Enum as Vk.ClrCmp
import qualified Gpu.Vulkan.Pipeline.ColorBlendState as Vk.Ppl.ClrBlndSt
import qualified Gpu.Vulkan.Pipeline.Layout.Middle as Vk.Ppl.Layout
import qualified Gpu.Vulkan.Attachment as Vk.Att
import qualified Gpu.Vulkan.Attachment.Enum as Vk.Att
import qualified Gpu.Vulkan.Subpass as Vk.Subpass
import qualified Gpu.Vulkan.Subpass.Enum as Vk.Subpass
import qualified Gpu.Vulkan.Pipeline.Enum as Vk.Ppl
import qualified Gpu.Vulkan.RenderPass.Middle as Vk.RenderPass
import qualified Gpu.Vulkan.RenderPass.Enum as Vk.RenderPass
import qualified Gpu.Vulkan.Pipeline.Graphics.Middle as Vk.Ppl.Graphics
import qualified Gpu.Vulkan.Framebuffer.Middle as Vk.Framebuffer
import qualified Gpu.Vulkan.Framebuffer.Enum as Vk.Framebuffer
import qualified Gpu.Vulkan.CommandPool.Middle as Vk.CommandPool
import qualified Gpu.Vulkan.CommandPool.Enum as Vk.CommandPool
import qualified Gpu.Vulkan.CommandBuffer.Middle as Vk.CommandBuffer
import qualified Gpu.Vulkan.CommandBuffer.Enum as Vk.CommandBuffer
import qualified Gpu.Vulkan.Semaphore as Vk.Semaphore
import qualified Gpu.Vulkan.Fence as Vk.Fence
import qualified Gpu.Vulkan.Fence.Enum as Vk.Fence
import qualified Gpu.Vulkan.VertexInput as Vk.VertexInput
import qualified Gpu.Vulkan.Buffer.List.Middle as Vk.Buffer.List
import qualified Gpu.Vulkan.Buffer.Enum as Vk.Buffer
import qualified Gpu.Vulkan.Memory.List.Middle as Vk.Memory.List
import qualified Gpu.Vulkan.Memory.Middle as Vk.Memory.M
import qualified Gpu.Vulkan.Memory.Enum as Vk.Memory
import qualified Gpu.Vulkan.Command.List as Vk.Cmd.List
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.Queue.Enum as Vk.Queue
import qualified Gpu.Vulkan.Memory as Vk.Memory
import qualified Gpu.Vulkan.Command.Middle as Vk.Cmd.M

import Gpu.Vulkan.Pipeline.VertexInputState.BindingStrideList(AddType)
import Gpu.Vulkan.Buffer.List.Middle (BList(..))

main :: IO ()
main = (=<< newGlobal) . runReaderT $ withWindow \win -> createInstance \inst ->
	if enableValidationLayers
		then setupDebugMessenger inst $ const $ run win inst
		else run win inst

windowName :: String
windowName = "Triangle"

windowSize :: (Int, Int)
windowSize = (width, height) where width = 800; height = 600

enableValidationLayers :: Bool
enableValidationLayers = maybe True (const False) $(lookupCompileEnv "NDEBUG")

validationLayers :: [Txt.Text]
validationLayers = [Vk.Khr.validationLayerName]

maxFramesInFlight :: Int
maxFramesInFlight = 2

data Global = Global {
	globalDevice :: IORef Vk.Device.M.D,
	globalGraphicsQueue :: IORef Vk.Queue.Q,
	globalPresentQueue :: IORef Vk.Queue.Q,
	globalSwapChain :: IORef Vk.Khr.Swapchain.S,
	globalSwapChainImages :: IORef [Vk.Image.I],
	globalSwapChainImageFormat :: IORef (Maybe Vk.Format),
	globalSwapChainExtent :: IORef Vk.C.Extent2d,
	globalSwapChainImageViews :: IORef [Vk.ImageView.I],
	globalRenderPass :: IORef Vk.RenderPass.R,
	globalPipelineLayout :: IORef Vk.Ppl.Layout.L,
	globalGraphicsPipeline :: IORef (Vk.Ppl.Graphics.G
		(Solo (AddType Vertex 'Vk.VertexInput.RateVertex))
		'[Cglm.Vec2, Cglm.Vec3]),
	globalSwapChainFramebuffers :: IORef [Vk.Framebuffer.F],
	globalCommandPool :: IORef Vk.CommandPool.C,
	globalCommandBuffers :: IORef [Vk.CommandBuffer.C (
		Solo (AddType Vertex 'Vk.VertexInput.RateVertex) )],
	globalImageAvailableSemaphores :: IORef [Vk.Semaphore.S],
	globalRenderFinishedSemaphores :: IORef [Vk.Semaphore.S],
	globalInFlightFences :: IORef [Vk.Fence.F],
	globalCurrentFrame :: IORef Int,
	globalFramebufferResized :: IORef Bool,
	globalVertexBuffer :: IORef (Vk.Buffer.List.B Vertex),
	globalVertexBufferMemory :: IORef (Vk.Device.M.MemoryList Vertex) }

readGlobal :: (Global -> IORef a) -> ReaderT Global IO a
readGlobal ref = lift . readIORef =<< asks ref

writeGlobal :: (Global -> IORef a) -> a -> ReaderT Global IO ()
writeGlobal ref x = lift . (`writeIORef` x) =<< asks ref

newGlobal :: IO Global
newGlobal = do
	dvc <- newIORef $ Vk.Device.M.D NullPtr
	gq <- newIORef $ Vk.Queue.Q NullPtr
	pq <- newIORef $ Vk.Queue.Q NullPtr
	sc <- newIORef $ Vk.Khr.Swapchain.S NullPtr
	scis <- newIORef []
	scif <- newIORef Nothing
	sce <- newIORef $ Vk.C.Extent2d 0 0
	scivs <- newIORef []
	rp <- newIORef $ Vk.RenderPass.R NullPtr
	ppllyt <- newIORef $ Vk.Ppl.Layout.L NullPtr
	grppl <- newIORef Vk.Ppl.Graphics.GNull
	scfbs <- newIORef []
	cp <- newIORef $ Vk.CommandPool.C NullPtr
	cbs <- newIORef []
	iass <- newIORef []
	rfss <- newIORef []
	iffs <- newIORef []
	cf <- newIORef 0
	fbr <- newIORef False
	vb <- newIORef $ Vk.Buffer.List.B 0 NullPtr
	vbm <- newIORef $ Vk.Device.M.MemoryList 0 NullPtr
	pure Global {
		globalDevice = dvc,
		globalGraphicsQueue = gq,
		globalPresentQueue = pq,
		globalSwapChain = sc,
		globalSwapChainImages = scis,
		globalSwapChainImageFormat = scif,
		globalSwapChainExtent = sce,
		globalSwapChainImageViews = scivs,
		globalRenderPass = rp,
		globalPipelineLayout = ppllyt,
		globalGraphicsPipeline = grppl,
		globalSwapChainFramebuffers = scfbs,
		globalCommandPool = cp,
		globalCommandBuffers = cbs,
		globalImageAvailableSemaphores = iass,
		globalRenderFinishedSemaphores = rfss,
		globalInFlightFences = iffs,
		globalCurrentFrame = cf,
		globalFramebufferResized = fbr,
		globalVertexBuffer = vb,
		globalVertexBufferMemory = vbm }

withWindow :: (Glfw.Window -> ReaderT Global IO a) -> ReaderT Global IO a
withWindow f = initWindow >>= \w ->
	f w <* lift (Glfw.destroyWindow w >> Glfw.terminate)

initWindow :: ReaderT Global IO Glfw.Window
initWindow = do
	Just w <- lift do
		True <- Glfw.init
		Glfw.windowHint $ Glfw.WindowHint'ClientAPI Glfw.ClientAPI'NoAPI
		uncurry Glfw.createWindow windowSize windowName Nothing Nothing
	g <- ask
	w <$ lift (Glfw.setFramebufferSizeCallback w
		(Just $ \_ _ _ -> writeIORef (globalFramebufferResized g) True))

createInstance :: (forall si . Vk.Ist.I si -> ReaderT Global IO a) ->
	ReaderT Global IO a
createInstance f = do
	lift . when enableValidationLayers $ bool
		(error "validation layers requested, but not available!")
		(pure ())
		=<< null . (validationLayers \\)
				. (Vk.layerPropertiesLayerName <$>)
			<$> Vk.Ist.M.enumerateLayerProperties
	extensions <- lift $ bool id (Vk.Ext.DbgUtls.extensionName :)
			enableValidationLayers
		<$> ((cstrToText `mapM`) =<< Glfw.getRequiredInstanceExtensions)
	let	appInfo = Vk.ApplicationInfo {
			Vk.applicationInfoNext = Nothing,
			Vk.applicationInfoApplicationName = "Hello Triangle",
			Vk.applicationInfoApplicationVersion =
				Vk.makeApiVersion 0 1 0 0,
			Vk.applicationInfoEngineName = "No Engine",
			Vk.applicationInfoEngineVersion =
				Vk.makeApiVersion 0 1 0 0,
			Vk.applicationInfoApiVersion = Vk.apiVersion_1_0 }
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
	g <- ask
	lift $ Vk.Ist.create createInfo nil nil \i -> f i `runReaderT` g

instanceToMiddle :: Vk.Ist.I si -> Vk.Ist.M.I
instanceToMiddle (Vk.Ist.I inst) = inst

setupDebugMessenger ::
	Vk.Ist.I si ->
	(forall sm . Vk.Ext.DbgUtls.Msngr.M sm -> ReaderT Global IO a) ->
	ReaderT Global IO a
setupDebugMessenger ist f = ask >>= \g -> lift $ Vk.Ext.DbgUtls.Msngr.create ist
	debugMessengerCreateInfo nil nil \m -> f m `runReaderT` g

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

run :: Glfw.Window -> Vk.Ist.I si -> ReaderT Global IO ()
run win inst = createSurface win inst \sfc -> do
	(phdvc, qfis) <- lift $ pickPhysicalDevice inst sfc
	createLogicalDevice phdvc qfis \(Vk.Device.D dvc) gq pq -> do
		writeGlobal globalDevice dvc
		writeGlobal globalGraphicsQueue gq
		writeGlobal globalPresentQueue pq
		initVulkan win sfc phdvc qfis
		mainLoop win sfc phdvc qfis
		cleanup

createSurface :: Glfw.Window -> Vk.Ist.I si ->
	(forall ss . Vk.Khr.Surface.S ss -> ReaderT Global IO a) ->
	ReaderT Global IO a
createSurface win ist f =
	ask >>= \g -> lift $ Glfw.createWindowSurface ist win nil nil \sfc ->
		f sfc `runReaderT` g

pickPhysicalDevice :: Vk.Ist.I si ->
	Vk.Khr.Surface.S ss -> IO (Vk.PhysicalDevice.P, QueueFamilyIndices)
pickPhysicalDevice ist sfc = do
	dvcs <- Vk.PhysicalDevice.enumerate ist
	when (null dvcs) $ error "failed to find GPUs with Gpu.Vulkan support!"
	findDevice (`isDeviceSuitable` sfc) dvcs >>= \case
		Just pdvc -> pure pdvc
		Nothing -> error "failed to find a suitable GPU!"

findDevice :: Monad m =>
	(Vk.PhysicalDevice.P -> m (Maybe a)) -> [Vk.PhysicalDevice.P] ->
	m (Maybe (Vk.PhysicalDevice.P, a))
findDevice prd = \case
	[] -> pure Nothing
	p : ps -> prd p >>= \case
		Nothing -> findDevice prd ps; Just x -> pure $ Just (p, x)

isDeviceSuitable ::
	Vk.PhysicalDevice.P -> Vk.Khr.Surface.S ss -> IO (Maybe QueueFamilyIndices)
isDeviceSuitable phdvc sfc = do
	_deviceProperties <- Vk.PhysicalDevice.getProperties phdvc
	_deviceFeatures <- Vk.PhysicalDevice.getFeatures phdvc
	indices <- findQueueFamilies phdvc sfc
	extensionSupported <- checkDeviceExtensionSupport phdvc
	swapChainSupport <- querySwapChainSupport phdvc sfc
	let	swapChainAdequate =
			not (null $ formats swapChainSupport) &&
			not (null $ presentModes swapChainSupport)
	pure if extensionSupported && swapChainAdequate
		then completeQueueFamilies indices else Nothing

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
	Vk.PhysicalDevice.P -> Vk.Khr.Surface.S ss -> IO QueueFamilyIndicesMaybe
findQueueFamilies device sfc = do
	queueFamilies <- Vk.PhysicalDevice.getQueueFamilyProperties device
	pfis <- filterM
		(\i -> doesPresentSupport device i sfc) (fst <$> queueFamilies)
	pure QueueFamilyIndicesMaybe {
		graphicsFamilyMaybe = fst <$> find
			(checkBits Vk.Queue.GraphicsBit
				. Vk.QueueFamily.propertiesQueueFlags . snd)
			queueFamilies,
		presentFamilyMaybe = listToMaybe pfis }

checkBits :: Bits bs => bs -> bs -> Bool
checkBits bs = (== bs) . (.&. bs)

doesPresentSupport :: Vk.PhysicalDevice.P ->
	Vk.QueueFamily.Index -> Vk.Khr.Surface.S ss -> IO Bool
doesPresentSupport dvc idx sfc =
	Vk.Khr.Surface.PhysicalDevice.getSupport dvc idx sfc

checkDeviceExtensionSupport :: Vk.PhysicalDevice.P -> IO Bool
checkDeviceExtensionSupport dvc = do
	availableExtensions <-
		Vk.PhysicalDevice.enumerateExtensionProperties dvc Nothing
	pure . null $ deviceExtensions \\
		(Vk.extensionPropertiesExtensionName <$> availableExtensions)

deviceExtensions :: [Txt.Text]
deviceExtensions = [Vk.Khr.Swapchain.extensionName]

data SwapChainSupportDetails = SwapChainSupportDetails {
	capabilities :: Vk.Khr.Surface.M.Capabilities,
	formats :: [Vk.Khr.Surface.M.Format],
	presentModes :: [Vk.Khr.PresentMode] }

querySwapChainSupport ::
	Vk.PhysicalDevice.P -> Vk.Khr.Surface.S ss -> IO SwapChainSupportDetails
querySwapChainSupport dvc sfc = SwapChainSupportDetails
	<$> Vk.Khr.Surface.PhysicalDevice.getCapabilities dvc sfc
	<*> Vk.Khr.Surface.PhysicalDevice.getFormats dvc sfc
	<*> Vk.Khr.Surface.PhysicalDevice.getPresentModes dvc sfc

createLogicalDevice :: Vk.PhysicalDevice.P -> QueueFamilyIndices ->
	(forall sd . Vk.Device.D sd -> Vk.Queue.Q -> Vk.Queue.Q -> ReaderT Global IO a) ->
	ReaderT Global IO a
createLogicalDevice phdvc qfis f = do
	let	uniqueQueueFamilies =
			nub [graphicsFamily qfis, presentFamily qfis]
		queueCreateInfos qf = Vk.Device.Queue.CreateInfo {
			Vk.Device.Queue.createInfoNext = Nothing,
			Vk.Device.Queue.createInfoFlags =
				Vk.Device.Queue.CreateFlagsZero,
			Vk.Device.Queue.createInfoQueueFamilyIndex = qf,
			Vk.Device.Queue.createInfoQueuePriorities = [1] }
		deviceFeatures = Vk.PhysicalDevice.featuresZero
		createInfo = Vk.Device.M.CreateInfo {
			Vk.Device.M.createInfoNext = Nothing,
			Vk.Device.M.createInfoFlags = Vk.Device.M.CreateFlagsZero,
			Vk.Device.M.createInfoQueueCreateInfos =
				queueCreateInfos <$> uniqueQueueFamilies,
			Vk.Device.M.createInfoEnabledLayerNames =
				bool [] validationLayers enableValidationLayers,
			Vk.Device.M.createInfoEnabledExtensionNames =
				deviceExtensions,
			Vk.Device.M.createInfoEnabledFeatures =
				Just deviceFeatures }
	g <- ask
	lift $ Vk.Device.create @() @() phdvc createInfo nil nil \dvc -> do
		gq <- Vk.Device.getQueue dvc (graphicsFamily qfis) 0
		pq <- Vk.Device.getQueue dvc (presentFamily qfis) 0
		f dvc gq pq `runReaderT` g

initVulkan :: Glfw.Window -> Vk.Khr.Surface.S ss ->
	Vk.PhysicalDevice.P -> QueueFamilyIndices -> ReaderT Global IO ()
initVulkan win sfc phdvc qfis = do
		createSwapChain win sfc phdvc qfis
		createImageViews
		createRenderPass
		createGraphicsPipeline
		createFramebuffers
		createCommandPool qfis
		createVertexBuffer phdvc
		createCommandBuffers
		createSyncObjects

createSwapChain :: Glfw.Window -> Vk.Khr.Surface.S ss ->
	Vk.PhysicalDevice.P -> QueueFamilyIndices -> ReaderT Global IO ()
createSwapChain win sfc phdvc qfis0 = do
	swapChainSupport <- lift $ querySwapChainSupport phdvc sfc
	let	surfaceFormat =
			chooseSwapSurfaceFormat $ formats swapChainSupport
		presentMode =
			chooseSwapPresentMode $ presentModes swapChainSupport
	extent <- chooseSwapExtent win $ capabilities swapChainSupport
	let	maxImageCount = fromMaybe maxBound . onlyIf (> 0)
			. Vk.Khr.Surface.M.capabilitiesMaxImageCount
			$ capabilities swapChainSupport
		imageCount = clamp
			(Vk.Khr.Surface.M.capabilitiesMinImageCount
				(capabilities swapChainSupport) + 1)
			0 maxImageCount
		(ism, qfis) = if graphicsFamily qfis0 /= presentFamily qfis0
			then (Vk.SharingModeConcurrent,
				[graphicsFamily qfis0, presentFamily qfis0])
			else (Vk.SharingModeExclusive, [])
		createInfo = Vk.Khr.Swapchain.CreateInfo {
			Vk.Khr.Swapchain.createInfoNext = Nothing,
			Vk.Khr.Swapchain.createInfoFlags =
				Vk.Khr.Swapchain.CreateFlagsZero,
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
				Vk.Khr.Surface.M.capabilitiesCurrentTransform
					$ capabilities swapChainSupport,
			Vk.Khr.Swapchain.createInfoCompositeAlpha =
				Vk.Khr.CompositeAlphaOpaqueBit,
			Vk.Khr.Swapchain.createInfoPresentMode = presentMode,
			Vk.Khr.Swapchain.createInfoClipped = True,
			Vk.Khr.Swapchain.createInfoOldSwapchain = Nothing }
	dvc <- readGlobal globalDevice
	sc <- lift $ Vk.Khr.Swapchain.create @() dvc createInfo nil
	writeGlobal globalSwapChain sc
	writeGlobal globalSwapChainImages
		=<< lift (Vk.Khr.Swapchain.getImages dvc sc)
	writeGlobal globalSwapChainImageFormat
		. Just $ Vk.Khr.Surface.M.formatFormat surfaceFormat
	writeGlobal globalSwapChainExtent extent
	lift do	putStrLn "*** CREATE SWAP CHAIN ***"
		print surfaceFormat
		print presentMode
		print extent

onlyIf :: (a -> Bool) -> a -> Maybe a
onlyIf p x | p x = Just x | otherwise = Nothing

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

chooseSwapExtent :: Glfw.Window -> Vk.Khr.Surface.M.Capabilities -> ReaderT Global IO Vk.C.Extent2d
chooseSwapExtent win caps
	| Vk.C.extent2dWidth curExt /= maxBound = pure curExt
	| otherwise = do
		(fromIntegral -> w, fromIntegral -> h) <-
			lift $ Glfw.getFramebufferSize win
		pure $ Vk.C.Extent2d
			(clamp w (Vk.C.extent2dWidth n) (Vk.C.extent2dHeight n))
			(clamp h (Vk.C.extent2dWidth x) (Vk.C.extent2dHeight x))
	where
	curExt = Vk.Khr.Surface.M.capabilitiesCurrentExtent caps
	n = Vk.Khr.Surface.M.capabilitiesMinImageExtent caps
	x = Vk.Khr.Surface.M.capabilitiesMaxImageExtent caps

clamp :: Ord a => a -> a -> a -> a
clamp x mn mx | x < mn = mn | x < mx = x | otherwise = mx

createImageViews :: ReaderT Global IO ()
createImageViews = writeGlobal globalSwapChainImageViews =<<
	(createImageView1 `mapM`) =<< readGlobal globalSwapChainImages

createImageView1 :: Vk.Image.I -> ReaderT Global IO Vk.ImageView.I
createImageView1 sci = do
	scif <- fromJust <$> readGlobal globalSwapChainImageFormat
	let	createInfo = Vk.ImageView.CreateInfo {
			Vk.ImageView.createInfoNext = Nothing,
			Vk.ImageView.createInfoFlags =
				Vk.ImageView.CreateFlagsZero,
			Vk.ImageView.createInfoImage = sci,
			Vk.ImageView.createInfoViewType = Vk.ImageView.Type2d,
			Vk.ImageView.createInfoFormat = scif,
			Vk.ImageView.createInfoComponents = components,
			Vk.ImageView.createInfoSubresourceRange =
				subresourceRange }
		components = Vk.Component.Mapping {
			Vk.Component.mappingR = Vk.Component.SwizzleIdentity,
			Vk.Component.mappingG = Vk.Component.SwizzleIdentity,
			Vk.Component.mappingB = Vk.Component.SwizzleIdentity,
			Vk.Component.mappingA = Vk.Component.SwizzleIdentity }
		subresourceRange = Vk.Image.SubresourceRange {
			Vk.Image.subresourceRangeAspectMask = Vk.Image.AspectColorBit,
			Vk.Image.subresourceRangeBaseMipLevel = 0,
			Vk.Image.subresourceRangeLevelCount = 1,
			Vk.Image.subresourceRangeBaseArrayLayer = 0,
			Vk.Image.subresourceRangeLayerCount = 1 }
	dvc <- readGlobal globalDevice
	lift $ Vk.ImageView.create @() dvc createInfo nil

createRenderPass :: ReaderT Global IO ()
createRenderPass = do
	Just scif <- readGlobal globalSwapChainImageFormat
	let	colorAttachment = Vk.Att.Description {
			Vk.Att.descriptionFlags = Vk.Att.DescriptionFlagsZero,
			Vk.Att.descriptionFormat = scif,
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
			Vk.Subpass.descriptionFlags =
				Vk.Subpass.DescriptionFlagsZero,
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
			Vk.Subpass.dependencySrcAccessMask = Vk.AccessFlagsZero,
			Vk.Subpass.dependencyDstStageMask =
				Vk.Ppl.StageColorAttachmentOutputBit,
			Vk.Subpass.dependencyDstAccessMask =
				Vk.AccessColorAttachmentWriteBit,
			Vk.Subpass.dependencyDependencyFlags =
				Vk.DependencyFlagsZero }
		renderPassInfo = Vk.RenderPass.CreateInfo {
			Vk.RenderPass.createInfoNext = Nothing,
			Vk.RenderPass.createInfoFlags =
				Vk.RenderPass.CreateFlagsZero,
			Vk.RenderPass.createInfoAttachments = [colorAttachment],
			Vk.RenderPass.createInfoSubpasses = [subpass],
			Vk.RenderPass.createInfoDependencies = [dependency] }
	dvc <- readGlobal globalDevice
	writeGlobal globalRenderPass
		=<< lift (Vk.RenderPass.create @() dvc renderPassInfo nil)

createGraphicsPipeline :: ReaderT Global IO ()
createGraphicsPipeline = do
	vertShaderModule <- createShaderModule glslVertexShaderMain
	fragShaderModule <- createShaderModule glslFragmentShaderMain

	let	vertShaderStageInfo = Vk.Ppl.ShaderStage.CreateInfo {
			Vk.Ppl.ShaderStage.createInfoNext = Nothing,
			Vk.Ppl.ShaderStage.createInfoFlags =
				Vk.Ppl.ShaderStage.CreateFlagsZero,
			Vk.Ppl.ShaderStage.createInfoStage =
				Vk.ShaderStageVertexBit,
			Vk.Ppl.ShaderStage.createInfoModule = vertShaderModule,
			Vk.Ppl.ShaderStage.createInfoName = "main",
			Vk.Ppl.ShaderStage.createInfoSpecializationInfo =
				Nothing }
		fragShaderStageInfo = Vk.Ppl.ShaderStage.CreateInfo {
			Vk.Ppl.ShaderStage.createInfoNext = Nothing,
			Vk.Ppl.ShaderStage.createInfoFlags =
				Vk.Ppl.ShaderStage.CreateFlagsZero,
			Vk.Ppl.ShaderStage.createInfoStage =
				Vk.ShaderStageFragmentBit,
			Vk.Ppl.ShaderStage.createInfoModule = fragShaderModule,
			Vk.Ppl.ShaderStage.createInfoName = "main",
			Vk.Ppl.ShaderStage.createInfoSpecializationInfo =
				Nothing }
		shaderStages =
			vertShaderStageInfo `Vk.Ppl.ShaderStage.CreateInfoCons`
			fragShaderStageInfo `Vk.Ppl.ShaderStage.CreateInfoCons`
			Vk.Ppl.ShaderStage.CreateInfoNil
		vertexInputInfo :: Vk.Ppl.VertexInputSt.CreateInfo
			()
			(Solo (AddType Vertex 'Vk.VertexInput.RateVertex))
			'[Cglm.Vec2, Cglm.Vec3]
		vertexInputInfo = Vk.Ppl.VertexInputSt.CreateInfo {
			Vk.Ppl.VertexInputSt.createInfoNext = Nothing,
			Vk.Ppl.VertexInputSt.createInfoFlags =
				Vk.Ppl.VertexInputSt.M.CreateFlagsZero }
		inputAssembly = Vk.Ppl.InpAsmbSt.CreateInfo {
			Vk.Ppl.InpAsmbSt.createInfoNext = Nothing,
			Vk.Ppl.InpAsmbSt.createInfoFlags =
				Vk.Ppl.InpAsmbSt.CreateFlagsZero,
			Vk.Ppl.InpAsmbSt.createInfoTopology =
				Vk.PrimitiveTopologyTriangleList,
			Vk.Ppl.InpAsmbSt.createInfoPrimitiveRestartEnable =
				False }
	sce <- readGlobal globalSwapChainExtent
	let	viewport = Vk.C.Viewport {
			Vk.C.viewportX = 0, Vk.C.viewportY = 0,
			Vk.C.viewportWidth =
				fromIntegral $ Vk.C.extent2dWidth sce,
			Vk.C.viewportHeight =
				fromIntegral $ Vk.C.extent2dHeight sce,
			Vk.C.viewportMinDepth = 0, Vk.C.viewportMaxDepth = 1 }
		scissor = Vk.C.Rect2d {
			Vk.C.rect2dOffset = Vk.C.Offset2d 0 0,
			Vk.C.rect2dExtent = sce }
		viewportState = Vk.Ppl.ViewportSt.CreateInfo {
			Vk.Ppl.ViewportSt.createInfoNext = Nothing,
			Vk.Ppl.ViewportSt.createInfoFlags =
				Vk.Ppl.ViewportSt.CreateFlagsZero,
			Vk.Ppl.ViewportSt.createInfoViewports = [viewport],
			Vk.Ppl.ViewportSt.createInfoScissors = [scissor] }
		rasterizer = Vk.Ppl.RstSt.CreateInfo {
			Vk.Ppl.RstSt.createInfoNext = Nothing,
			Vk.Ppl.RstSt.createInfoFlags =
				Vk.Ppl.RstSt.CreateFlagsZero,
			Vk.Ppl.RstSt.createInfoDepthClampEnable = False,
			Vk.Ppl.RstSt.createInfoRasterizerDiscardEnable = False,
			Vk.Ppl.RstSt.createInfoPolygonMode = Vk.PolygonModeFill,
			Vk.Ppl.RstSt.createInfoLineWidth = 1,
			Vk.Ppl.RstSt.createInfoCullMode = Vk.CullModeBackBit,
			Vk.Ppl.RstSt.createInfoFrontFace =
				Vk.FrontFaceClockwise,
			Vk.Ppl.RstSt.createInfoDepthBiasEnable = False,
			Vk.Ppl.RstSt.createInfoDepthBiasConstantFactor = 0,
			Vk.Ppl.RstSt.createInfoDepthBiasClamp = 0,
			Vk.Ppl.RstSt.createInfoDepthBiasSlopeFactor = 0 }
		multisampling = Vk.Ppl.MulSmplSt.CreateInfo {
			Vk.Ppl.MulSmplSt.createInfoNext = Nothing,
			Vk.Ppl.MulSmplSt.createInfoFlags =
				Vk.Ppl.MulSmplSt.CreateFlagsZero,
			Vk.Ppl.MulSmplSt.createInfoSampleShadingEnable = False,
			Vk.Ppl.MulSmplSt.createInfoRasterizationSamplesAndMask =
				Vk.Sample.CountAndMask
					Vk.Sample.Count1Bit Nothing,
			Vk.Ppl.MulSmplSt.createInfoMinSampleShading = 1,
			Vk.Ppl.MulSmplSt.createInfoAlphaToCoverageEnable =
				False,
			Vk.Ppl.MulSmplSt.createInfoAlphaToOneEnable = False }
		colorBlendAttachment = Vk.Ppl.ClrBlndAtt.State {
			Vk.Ppl.ClrBlndAtt.stateColorWriteMask =
				Vk.ClrCmp.RBit .|. Vk.ClrCmp.GBit .|.
				Vk.ClrCmp.BBit .|. Vk.ClrCmp.ABit,
			Vk.Ppl.ClrBlndAtt.stateBlendEnable = False,
			Vk.Ppl.ClrBlndAtt.stateSrcColorBlendFactor =
				Vk.BlendFactorOne,
			Vk.Ppl.ClrBlndAtt.stateDstColorBlendFactor =
				Vk.BlendFactorZero,
			Vk.Ppl.ClrBlndAtt.stateColorBlendOp = Vk.BlendOpAdd,
			Vk.Ppl.ClrBlndAtt.stateSrcAlphaBlendFactor =
				Vk.BlendFactorOne,
			Vk.Ppl.ClrBlndAtt.stateDstAlphaBlendFactor =
				Vk.BlendFactorZero,
			Vk.Ppl.ClrBlndAtt.stateAlphaBlendOp = Vk.BlendOpAdd }
		colorBlending = Vk.Ppl.ClrBlndSt.CreateInfo {
			Vk.Ppl.ClrBlndSt.createInfoNext = Nothing,
			Vk.Ppl.ClrBlndSt.createInfoFlags =
				Vk.Ppl.ClrBlndSt.CreateFlagsZero,
			Vk.Ppl.ClrBlndSt.createInfoLogicOpEnable = False,
			Vk.Ppl.ClrBlndSt.createInfoLogicOp = Vk.LogicOpCopy,
			Vk.Ppl.ClrBlndSt.createInfoAttachments =
				[colorBlendAttachment],
			Vk.Ppl.ClrBlndSt.createInfoBlendConstants =
				fromJust $ rgbaDouble 0 0 0 0 }

	let	pipelineLayoutInfo = Vk.Ppl.Layout.CreateInfo {
			Vk.Ppl.Layout.createInfoNext = Nothing,
			Vk.Ppl.Layout.createInfoFlags =
				Vk.Ppl.Layout.CreateFlagsZero,
			Vk.Ppl.Layout.createInfoSetLayouts = [],
			Vk.Ppl.Layout.createInfoPushConstantRanges = [] }
	dvc <- readGlobal globalDevice
	writeGlobal globalPipelineLayout
		=<< lift (Vk.Ppl.Layout.create @() dvc pipelineLayoutInfo nil)

	ppllyt <- readGlobal globalPipelineLayout
	rp <- readGlobal globalRenderPass
	let	pipelineInfo :: Vk.Ppl.Graphics.CreateInfo
			() () '[ 'GlslVertexShader, 'GlslFragmentShader]
			'[(), ()] ()
			(Solo (AddType Vertex 'Vk.VertexInput.RateVertex))
			'[Cglm.Vec2, Cglm.Vec3]
			() () () () () () () () () '[]
		pipelineInfo = Vk.Ppl.Graphics.CreateInfo {
			Vk.Ppl.Graphics.createInfoNext = Nothing,
			Vk.Ppl.Graphics.createInfoFlags =
				Vk.Ppl.CreateFlagsZero,
			Vk.Ppl.Graphics.createInfoStages = shaderStages,
			Vk.Ppl.Graphics.createInfoVertexInputState =
				Just vertexInputInfo,
			Vk.Ppl.Graphics.createInfoInputAssemblyState =
				Just inputAssembly,
			Vk.Ppl.Graphics.createInfoViewportState =
				Just viewportState,
			Vk.Ppl.Graphics.createInfoRasterizationState =
				Just rasterizer,
			Vk.Ppl.Graphics.createInfoMultisampleState =
				Just multisampling,
			Vk.Ppl.Graphics.createInfoDepthStencilState = Nothing,
			Vk.Ppl.Graphics.createInfoColorBlendState =
				Just colorBlending,
			Vk.Ppl.Graphics.createInfoDynamicState = Nothing,
			Vk.Ppl.Graphics.createInfoLayout = ppllyt,
			Vk.Ppl.Graphics.createInfoRenderPass = rp,
			Vk.Ppl.Graphics.createInfoSubpass = 0,
			Vk.Ppl.Graphics.createInfoBasePipelineHandle =
				Vk.Ppl.Graphics.GNull,
			Vk.Ppl.Graphics.createInfoBasePipelineIndex = - 1,
			Vk.Ppl.Graphics.createInfoTessellationState = Nothing }

	gpl `Vk.Ppl.Graphics.PCons` Vk.Ppl.Graphics.PNil <- lift
		$ Vk.Ppl.Graphics.create
			dvc Nothing
			(pipelineInfo `Vk.Ppl.Graphics.CreateInfoCons`
				Vk.Ppl.Graphics.CreateInfoNil)
			nil
	writeGlobal globalGraphicsPipeline gpl

	lift do	Vk.Shader.Module.destroy dvc fragShaderModule nil
		Vk.Shader.Module.destroy dvc vertShaderModule nil

createShaderModule :: Spv sknd -> ReaderT Global IO (Vk.Shader.Module.M sknd)
createShaderModule cd = do
	let	createInfo = Vk.Shader.Module.CreateInfo {
			Vk.Shader.Module.createInfoNext = Nothing,
			Vk.Shader.Module.createInfoFlags =
				Vk.Shader.Module.CreateFlagsZero,
			Vk.Shader.Module.createInfoCode = cd }
	dvc <- readGlobal globalDevice
	lift $ Vk.Shader.Module.create @() dvc createInfo nil

createFramebuffers :: ReaderT Global IO ()
createFramebuffers = writeGlobal globalSwapChainFramebuffers
	=<< (createFramebuffer1 `mapM`) =<< readGlobal globalSwapChainImageViews

createFramebuffer1 :: Vk.ImageView.I -> ReaderT Global IO Vk.Framebuffer.F
createFramebuffer1 attachment = do
	rp <- readGlobal globalRenderPass
	Vk.C.Extent2d {
		Vk.C.extent2dWidth = w,
		Vk.C.extent2dHeight = h } <- readGlobal globalSwapChainExtent
	let	framebufferInfo = Vk.Framebuffer.CreateInfo {
			Vk.Framebuffer.createInfoNext = Nothing,
			Vk.Framebuffer.createInfoFlags =
				Vk.Framebuffer.CreateFlagsZero,
			Vk.Framebuffer.createInfoRenderPass = rp,
			Vk.Framebuffer.createInfoAttachments = [attachment],
			Vk.Framebuffer.createInfoWidth = w,
			Vk.Framebuffer.createInfoHeight = h,
			Vk.Framebuffer.createInfoLayers = 1 }
	dvc <- readGlobal globalDevice
	lift $ Vk.Framebuffer.create @() dvc framebufferInfo nil

createCommandPool :: QueueFamilyIndices -> ReaderT Global IO ()
createCommandPool qfis = do
	let	poolInfo = Vk.CommandPool.CreateInfo {
			Vk.CommandPool.createInfoNext = Nothing,
			Vk.CommandPool.createInfoFlags =
				Vk.CommandPool.CreateResetCommandBufferBit,
			Vk.CommandPool.createInfoQueueFamilyIndex =
				graphicsFamily qfis }
	dvc <- readGlobal globalDevice
	writeGlobal globalCommandPool
		=<< lift (Vk.CommandPool.create @() dvc poolInfo nil)

createVertexBuffer :: Vk.PhysicalDevice.P -> ReaderT Global IO ()
createVertexBuffer phdvc = do
	dvc <- readGlobal globalDevice
	(sb, sbm) <- createBuffer phdvc (length vertices)
		Vk.Buffer.UsageTransferSrcBit $
		Vk.Memory.PropertyHostVisibleBit .|.
		Vk.Memory.PropertyHostCoherentBit
	lift $ Vk.Memory.List.writeList dvc sbm Vk.Memory.M.MapFlagsZero vertices
	(vb, vbm) <- createBuffer phdvc (length vertices)
		(Vk.Buffer.UsageTransferDstBit .|.
			Vk.Buffer.UsageVertexBufferBit)
		Vk.Memory.PropertyDeviceLocalBit
	copyBuffer sb vb (length vertices)
	lift do	Vk.Buffer.List.destroy dvc sb nil
		Vk.Memory.List.free dvc sbm nil
	writeGlobal globalVertexBuffer vb
	writeGlobal globalVertexBufferMemory vbm

createBuffer :: Vk.PhysicalDevice.P ->
	Int -> Vk.Buffer.UsageFlags -> Vk.Memory.PropertyFlags ->
	ReaderT Global IO (Vk.Buffer.List.B Vertex, Vk.Device.M.MemoryList Vertex)
createBuffer phdvc ln usage properties = do
	let	bufferInfo = Vk.Buffer.List.CreateInfo {
			Vk.Buffer.List.createInfoNext = Nothing,
			Vk.Buffer.List.createInfoFlags =
				Vk.Buffer.CreateFlagsZero,
			Vk.Buffer.List.createInfoLength = ln,
			Vk.Buffer.List.createInfoUsage = usage,
			Vk.Buffer.List.createInfoSharingMode =
				Vk.SharingModeExclusive,
			Vk.Buffer.List.createInfoQueueFamilyIndices = [] }
	dvc <- readGlobal globalDevice
	b <- lift $ Vk.Buffer.List.create @() dvc bufferInfo nil
	memRequirements <- lift $ Vk.Buffer.List.getMemoryRequirements dvc b
	mti <- findMemoryType phdvc
		(Vk.Memory.M.requirementsMemoryTypeBits memRequirements)
		properties
	let	allocInfo = Vk.Memory.List.AllocateInfo {
			Vk.Memory.List.allocateInfoNext = Nothing,
			Vk.Memory.List.allocateInfoMemoryTypeIndex = Vk.Memory.TypeIndex mti }
	bm <- lift $ Vk.Memory.List.allocate @() dvc b allocInfo nil
	lift $ Vk.Buffer.List.bindMemory dvc b bm
	pure (b, bm)

copyBuffer :: Storable (Foreign.Storable.Generic.Wrap v) =>
	Vk.Buffer.List.B v -> Vk.Buffer.List.B v -> Int -> ReaderT Global IO ()
copyBuffer srcBuffer dstBuffer ln = do
	cp <- readGlobal globalCommandPool
	dvc <- readGlobal globalDevice
	let	allocInfo = Vk.CommandBuffer.AllocateInfo {
			Vk.CommandBuffer.allocateInfoNext = Nothing,
			Vk.CommandBuffer.allocateInfoLevel =
				Vk.CommandBuffer.LevelPrimary,
			Vk.CommandBuffer.allocateInfoCommandPool = cp,
			Vk.CommandBuffer.allocateInfoCommandBufferCount = 1 }
		beginInfo = Vk.CommandBuffer.BeginInfo {
			Vk.CommandBuffer.beginInfoNext = Nothing,
			Vk.CommandBuffer.beginInfoFlags =
				Vk.CommandBuffer.UsageOneTimeSubmitBit,
			Vk.CommandBuffer.beginInfoInheritanceInfo = Nothing }
		copyRegion = Vk.Buffer.List.Copy {
			Vk.Buffer.List.copyLength = ln }
	[commandBuffer] <- lift $ Vk.CommandBuffer.allocate @() dvc allocInfo
	let	submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = Nothing,
			Vk.submitInfoWaitSemaphoreDstStageMasks = [],
			Vk.submitInfoCommandBuffers = [commandBuffer],
			Vk.submitInfoSignalSemaphores = [] }
	gq <- readGlobal globalGraphicsQueue
	lift do	Vk.CommandBuffer.begin @() @() commandBuffer beginInfo
		Vk.Cmd.List.copyBuffer commandBuffer srcBuffer dstBuffer copyRegion
		Vk.CommandBuffer.end commandBuffer
		Vk.Queue.submit' @() gq [submitInfo] Nothing
		Vk.Queue.waitIdle gq
		Vk.CommandBuffer.freeCs dvc cp [commandBuffer]

findMemoryType ::
	Vk.PhysicalDevice.P -> Vk.Memory.M.TypeBits ->
	Vk.Memory.PropertyFlags -> ReaderT Global IO Word32
findMemoryType phdvc typeFilter properties = do
	memProperties <- lift $ Vk.PhysicalDevice.getMemoryProperties phdvc
	let	r = find (suitable typeFilter properties memProperties)
			[0 .. length (
				Vk.PhysicalDevice.memoryPropertiesMemoryTypes
					memProperties) - 1]
	lift $ print r
	pure $ maybe
		(error "failed to find suitable memory type!") fromIntegral r

suitable :: Vk.Memory.M.TypeBits -> Vk.Memory.PropertyFlags ->
	Vk.PhysicalDevice.MemoryProperties -> Int -> Bool
suitable typeFilter properties memProperties i =
	(typeFilter .&. Vk.Memory.M.TypeBits 1 `shiftL` i /= zeroBits) &&
	(Vk.Memory.M.mTypePropertyFlags
		(snd $ Vk.PhysicalDevice.memoryPropertiesMemoryTypes memProperties !!
			i) .&. properties == properties)

size :: forall a . SizeAlignmentList a => a -> Size
size _ = fst (wholeSizeAlignment @a)

createCommandBuffers :: ReaderT Global IO ()
createCommandBuffers = do
	cp <- readGlobal globalCommandPool
	let	allocInfo = Vk.CommandBuffer.AllocateInfo {
			Vk.CommandBuffer.allocateInfoNext = Nothing,
			Vk.CommandBuffer.allocateInfoCommandPool = cp,
			Vk.CommandBuffer.allocateInfoLevel =
				Vk.CommandBuffer.LevelPrimary,
			Vk.CommandBuffer.allocateInfoCommandBufferCount =
				fromIntegral maxFramesInFlight }
	dvc <- readGlobal globalDevice
	writeGlobal globalCommandBuffers
		=<< lift (Vk.CommandBuffer.allocate @() dvc allocInfo)

createSyncObjects :: ReaderT Global IO ()
createSyncObjects = do
	let	semaphoreInfo = Vk.Semaphore.CreateInfo {
			Vk.Semaphore.createInfoNext = Nothing,
			Vk.Semaphore.createInfoFlags =
				Vk.Semaphore.CreateFlagsZero }
		fenceInfo = Vk.Fence.CreateInfo {
			Vk.Fence.createInfoNext = Nothing,
			Vk.Fence.createInfoFlags = Vk.Fence.CreateSignaledBit }
	dvc <- readGlobal globalDevice
	writeGlobal globalImageAvailableSemaphores
		=<< lift (replicateM maxFramesInFlight
			$ Vk.Semaphore.create @() dvc semaphoreInfo nil)
	writeGlobal globalRenderFinishedSemaphores
		=<< lift (replicateM maxFramesInFlight
			$ Vk.Semaphore.create @() dvc semaphoreInfo nil)
	writeGlobal globalInFlightFences
		=<< lift (replicateM maxFramesInFlight
			$ Vk.Fence.create @() dvc fenceInfo nil)

recordCommandBuffer :: Vk.CommandBuffer.C (Solo (AddType Vertex 'Vk.VertexInput.RateVertex)) -> Word32 -> ReaderT Global IO ()
recordCommandBuffer cb imageIndex = do
	let	beginInfo = Vk.CommandBuffer.BeginInfo {
			Vk.CommandBuffer.beginInfoNext = Nothing,
			Vk.CommandBuffer.beginInfoFlags =
				Vk.CommandBuffer.UsageFlagsZero,
			Vk.CommandBuffer.beginInfoInheritanceInfo = Nothing }
	lift $ Vk.CommandBuffer.begin @() @() cb beginInfo
	rp <- readGlobal globalRenderPass
	scfbs <- readGlobal globalSwapChainFramebuffers
	sce <- readGlobal globalSwapChainExtent
	let	renderPassInfo = Vk.RenderPass.BeginInfo {
			Vk.RenderPass.beginInfoNext = Nothing,
			Vk.RenderPass.beginInfoRenderPass = rp,
			Vk.RenderPass.beginInfoFramebuffer =
				scfbs `genericIndex` imageIndex,
			Vk.RenderPass.beginInfoRenderArea = Vk.C.Rect2d {
				Vk.C.rect2dOffset = Vk.C.Offset2d 0 0,
				Vk.C.rect2dExtent = sce },
			Vk.RenderPass.beginInfoClearValues = [
				Vk.ClearValueColor
					. fromJust $ rgbaDouble 0 0 0 1 ] }
	lift $ Vk.Cmd.M.beginRenderPass @()
		@('Vk.ClearTypeColor 'Vk.ClearColorTypeFloat32)
		cb renderPassInfo Vk.Subpass.ContentsInline
	lift . Vk.Cmd.M.bindPipeline cb Vk.Ppl.BindPointGraphics
		=<< readGlobal globalGraphicsPipeline
	vb <- readGlobal globalVertexBuffer
	lift $ Vk.Cmd.List.bindVertexBuffers cb
		((vb, 0) :!: BNil :: BList '[Vertex])
	lift do	Vk.Cmd.M.draw cb 3 1 0 0
		Vk.Cmd.M.endRenderPass cb
		Vk.CommandBuffer.end cb

mainLoop ::
	Glfw.Window -> Vk.Khr.Surface.S ss ->
	Vk.PhysicalDevice.P -> QueueFamilyIndices -> ReaderT Global IO ()
mainLoop win sfc phdvc qfis = do
	fix \loop -> bool (pure ()) loop =<< do
		lift Glfw.pollEvents
		g <- ask
		lift . catchAndRecreateSwapChain g win sfc phdvc qfis
			$ drawFrame win sfc phdvc qfis `runReaderT` g
		not <$> lift (Glfw.windowShouldClose win)
	lift . Vk.Device.M.waitIdle =<< readGlobal globalDevice

drawFrame ::
	Glfw.Window -> Vk.Khr.Surface.S ss ->
	Vk.PhysicalDevice.P -> QueueFamilyIndices -> ReaderT Global IO ()
drawFrame win sfc phdvc qfis = do
	cf <- readGlobal globalCurrentFrame
	dvc <- readGlobal globalDevice
	iff <- (!! cf) <$> readGlobal globalInFlightFences
	lift $ Vk.Fence.waitForFs dvc [iff] True maxBound
	sc <- readGlobal globalSwapChain
	ias <- (!! cf) <$> readGlobal globalImageAvailableSemaphores
	imageIndex <- lift $ Vk.Khr.acquireNextImageResult [Vk.Success, Vk.SuboptimalKhr]
		dvc sc uint64Max (Just ias) Nothing
	lift $ Vk.Fence.resetFs dvc [iff]
	cb <- (!! cf) <$> readGlobal globalCommandBuffers
	lift $ Vk.CommandBuffer.reset cb Vk.CommandBuffer.ResetFlagsZero
	recordCommandBuffer cb imageIndex
	rfs <- (!! cf) <$> readGlobal globalRenderFinishedSemaphores
	let	submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = Nothing,
			Vk.submitInfoWaitSemaphoreDstStageMasks =
				[(ias, Vk.Ppl.StageColorAttachmentOutputBit)],
			Vk.submitInfoCommandBuffers = [cb],
			Vk.submitInfoSignalSemaphores = [rfs] }
	gq <- readGlobal globalGraphicsQueue
	lift . Vk.Queue.submit' @() gq [submitInfo] $ Just iff
	let	presentInfo = Vk.Khr.PresentInfo {
			Vk.Khr.presentInfoNext = Nothing,
			Vk.Khr.presentInfoWaitSemaphores = [rfs],
			Vk.Khr.presentInfoSwapchainImageIndices =
				[(sc, imageIndex)] }
	pq <- readGlobal globalPresentQueue
	g <- ask
	lift . catchAndRecreateSwapChain g win sfc phdvc qfis . catchAndSerialize
		$ Vk.Khr.queuePresent @() pq presentInfo
	writeGlobal globalCurrentFrame $ (cf + 1) `mod` maxFramesInFlight

catchAndSerialize :: IO () -> IO ()
catchAndSerialize =
	(`catch` \(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

catchAndRecreateSwapChain ::
	Global -> Glfw.Window -> Vk.Khr.Surface.S ss ->
	Vk.PhysicalDevice.P -> QueueFamilyIndices -> IO () -> IO ()
catchAndRecreateSwapChain g win sfc phdvc qfis act = catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just ()
		Vk.SuboptimalKhr -> Just ()
		_ -> Nothing)
	act
	(\_ -> do
		fbr <- readIORef $ globalFramebufferResized g
		when fbr do
			writeIORef (globalFramebufferResized g) False
			recreateSwapChain win sfc phdvc qfis `runReaderT` g)

doWhile_ :: IO Bool -> IO ()
doWhile_ act = (`when` doWhile_ act) =<< act

recreateSwapChain ::
	Glfw.Window -> Vk.Khr.Surface.S ss ->
	Vk.PhysicalDevice.P -> QueueFamilyIndices -> ReaderT Global IO ()
recreateSwapChain win sfc phdvc qfis = do
	lift do	(wdth, hght) <- Glfw.getFramebufferSize win
		when (wdth == 0 || hght == 0) $ doWhile_ do
			Glfw.waitEvents
			(wd, hg) <- Glfw.getFramebufferSize win
			pure $ wd == 0 || hg == 0
	dvc <- readGlobal globalDevice
	lift $ Vk.Device.M.waitIdle dvc

	cleanupSwapChain

	createSwapChain win sfc phdvc qfis
	createImageViews
	createRenderPass
	createGraphicsPipeline
	createFramebuffers

cleanupSwapChain :: ReaderT Global IO ()
cleanupSwapChain = do
	dvc <- readGlobal globalDevice
	scfbs <- readGlobal globalSwapChainFramebuffers
	lift $ flip (Vk.Framebuffer.destroy dvc) nil `mapM_` scfbs
	grppl <- readGlobal globalGraphicsPipeline
	lift $ Vk.Ppl.Graphics.destroy dvc grppl nil
	ppllyt <- readGlobal globalPipelineLayout
	lift $ Vk.Ppl.Layout.destroy dvc ppllyt nil
	rp <- readGlobal globalRenderPass
	lift $ Vk.RenderPass.destroy dvc rp nil
	scivs <- readGlobal globalSwapChainImageViews
	lift $ flip (Vk.ImageView.destroy dvc) nil `mapM_` scivs
	lift . (\sc -> Vk.Khr.Swapchain.destroy dvc sc nil)
		=<< readGlobal globalSwapChain

cleanup :: ReaderT Global IO ()
cleanup = do
	cleanupSwapChain
	dvc <- readGlobal globalDevice

	vb <- readGlobal globalVertexBuffer
	lift $ Vk.Buffer.List.destroy dvc vb nil
	vbm <- readGlobal globalVertexBufferMemory
	lift $ Vk.Memory.List.free dvc vbm nil
	lift . (flip (Vk.Semaphore.destroy dvc) nil `mapM_`)
		=<< readGlobal globalImageAvailableSemaphores
	lift . (flip (Vk.Semaphore.destroy dvc) nil `mapM_`)
		=<< readGlobal globalRenderFinishedSemaphores
	lift . (flip (Vk.Fence.destroy dvc) nil `mapM_`)
		=<< readGlobal globalInFlightFences
	lift . flip (Vk.CommandPool.destroy dvc) nil
		=<< readGlobal globalCommandPool

data Vertex = Vertex {
	vertexPos :: Cglm.Vec2,
	vertexColor :: Cglm.Vec3 }
	deriving (Show, Generic)

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
