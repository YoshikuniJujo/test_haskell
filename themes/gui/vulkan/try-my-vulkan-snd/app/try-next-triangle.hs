{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
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
import Data.Kind
import Data.Default
import Data.Bits
import Data.HeteroList hiding (length)
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
import qualified Gpu.Vulkan.Khr.Surface as Vk.Khr.Surface
import qualified Gpu.Vulkan.Khr.Surface.Middle as Vk.Khr.Surface.M
import qualified Gpu.Vulkan.Khr.Surface.PhysicalDevice as
	Vk.Khr.Surface.PhysicalDevice
import qualified Gpu.Vulkan.Khr.Swapchain as Vk.Khr.Swapchain
import qualified Gpu.Vulkan.Khr.Swapchain.Type as Vk.Khr.Swapchain
import qualified Gpu.Vulkan.Khr.Swapchain.Middle as Vk.Khr.Swapchain.M
import qualified Gpu.Vulkan.Image.Type as Vk.Image
import qualified Gpu.Vulkan.Image.Enum as Vk.Image
import qualified Gpu.Vulkan.Image.Middle as Vk.Image.M
import qualified Gpu.Vulkan.ImageView as Vk.ImageView
import qualified Gpu.Vulkan.ImageView.Middle as Vk.ImageView.M
import qualified Gpu.Vulkan.ImageView.Enum as Vk.ImageView
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
import qualified Gpu.Vulkan.RenderPass.Type as Vk.RndrPass
import qualified Gpu.Vulkan.RenderPass.Middle as Vk.RndrPass.M
import qualified Gpu.Vulkan.Pipeline.Graphics.Type as Vk.Ppl.Graphics
import qualified Gpu.Vulkan.Pipeline.Graphics as Vk.Ppl.Graphics
import qualified Gpu.Vulkan.Framebuffer as Vk.Framebuffer
import qualified Gpu.Vulkan.Framebuffer.Type as Vk.Framebuffer
import qualified Gpu.Vulkan.Framebuffer.Middle as Vk.Framebuffer.M
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
	lift $ print extensions
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
run win inst = ask >>= \g ->
	createSurface win inst \sfc ->
	lift (pickPhysicalDevice inst sfc) >>= \(phdvc, qfis) ->
	createLogicalDevice phdvc qfis \dvc gq pq ->
	createSwapChain win sfc phdvc qfis dvc \sc scfmt ext -> do
	let	scifmt = Vk.Khr.Surface.M.formatFormat scfmt
	imgs <- lift $ Vk.Khr.Swapchain.getImages dvc sc
	createImageViews dvc scifmt imgs \scivs ->
		lift $ createRenderPass dvc scifmt \rp ->
		createPipelineLayout dvc g \ppllyt -> do
		createGraphicsPipeline dvc ext rp ppllyt \gpl -> do
			createFramebuffers dvc ext scivs rp \fbs -> do
				initVulkan phdvc qfis dvc gq
				mainLoop win sfc phdvc qfis dvc gq pq sc ext scivs rp ppllyt gpl fbs
				cleanup dvc

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
	Vk.PhysicalDevice.P -> Vk.Khr.Surface.S ss -> IO QueueFamilyIndicesMaybe
findQueueFamilies device sfc = do
	queueFamilies <- Vk.PhysicalDevice.getQueueFamilyProperties device
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

checkDeviceExtensionSupport :: Vk.PhysicalDevice.P -> IO Bool
checkDeviceExtensionSupport dvc =
	null . (deviceExtensions \\) . (Vk.extensionPropertiesExtensionName <$>)
		<$> Vk.PhysicalDevice.enumerateExtensionProperties dvc Nothing

deviceExtensions :: [Txt.Text]
deviceExtensions = [Vk.Khr.Swapchain.M.extensionName]

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

createLogicalDevice :: Vk.PhysicalDevice.P -> QueueFamilyIndices -> (forall sd .
		Vk.Device.D sd -> Vk.Queue.Q -> Vk.Queue.Q ->
			ReaderT Global IO a) -> ReaderT Global IO a
createLogicalDevice phdvc qfis f =
	let	uniqueQueueFamilies =
			nub [graphicsFamily qfis, presentFamily qfis]
		queueCreateInfos qf = Vk.Device.Queue.CreateInfo {
			Vk.Device.Queue.createInfoNext = Nothing,
			Vk.Device.Queue.createInfoFlags = def,
			Vk.Device.Queue.createInfoQueueFamilyIndex = qf,
			Vk.Device.Queue.createInfoQueuePriorities = [1] }
		createInfo = Vk.Device.M.CreateInfo {
			Vk.Device.M.createInfoNext = Nothing,
			Vk.Device.M.createInfoFlags = def,
			Vk.Device.M.createInfoQueueCreateInfos =
				queueCreateInfos <$> uniqueQueueFamilies,
			Vk.Device.M.createInfoEnabledLayerNames =
				bool [] validationLayers enableValidationLayers,
			Vk.Device.M.createInfoEnabledExtensionNames =
				deviceExtensions,
			Vk.Device.M.createInfoEnabledFeatures = Just def } in
	ask >>= \g ->
	lift $ Vk.Device.create @() @() phdvc createInfo nil nil \dvc -> do
		gq <- Vk.Device.getQueue dvc (graphicsFamily qfis) 0
		pq <- Vk.Device.getQueue dvc (presentFamily qfis) 0
		f dvc gq pq `runReaderT` g

createSwapChain :: Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhysicalDevice.P -> QueueFamilyIndices -> Vk.Device.D sd -> (
		forall ss . Vk.Khr.Swapchain.S ss -> Vk.Khr.Surface.M.Format ->
		Vk.C.Extent2d -> ReaderT Global IO a ) -> ReaderT Global IO a
createSwapChain win sfc phdvc qfis dvc f = do
	swapChainSupport <- lift $ querySwapChainSupport phdvc sfc
	extent <- chooseSwapExtent win $ capabilities swapChainSupport
	let	(createInfo, surfaceFormat) =
			mkSwapchainCreateInfo sfc qfis swapChainSupport extent
	g <- ask
	lift $ Vk.Khr.Swapchain.create @() dvc createInfo nil nil \sc ->
		f sc surfaceFormat extent `runReaderT` g

recreateSwapChain :: Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhysicalDevice.P -> QueueFamilyIndices -> Vk.Device.D sd ->
	Vk.Khr.Swapchain.S ssc ->
	ReaderT Global IO (Vk.Khr.Surface.M.Format, Vk.C.Extent2d)
recreateSwapChain win sfc phdvc qfis0 dvc sc = do
	swapChainSupport <- lift $ querySwapChainSupport phdvc sfc
	extent <- chooseSwapExtent win $ capabilities swapChainSupport
	let	(createInfo, surfaceFormat) =
			mkSwapchainCreateInfo sfc qfis0 swapChainSupport extent
	lift $ Vk.Khr.Swapchain.recreate @() dvc createInfo nil nil sc
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

onlyIf :: (a -> Bool) -> a -> Maybe a
onlyIf p x | p x = Just x | otherwise = Nothing

createImageViews :: Vk.Device.D sd -> Vk.Format -> [Vk.Image.Binded ss ss] ->
	(forall si . HeteroVarList Vk.ImageView.I si -> ReaderT Global IO a) ->
	ReaderT Global IO a
createImageViews _dvc _scifmt [] f = f HVNil
createImageViews dvc scifmt (img : imgs) f =
	createImageView1 dvc img scifmt \sciv ->
	createImageViews dvc scifmt imgs \scivs -> f $ sciv :...: scivs

createImageView1 :: Vk.Device.D sd -> Vk.Image.Binded ss ss -> Vk.Format ->
	(forall siv . Vk.ImageView.I siv -> ReaderT Global IO a) ->
	ReaderT Global IO a
createImageView1 dvc sci scifmt f = do
	let	createInfo = makeImageViewCreateInfo sci scifmt
	g <- ask
	lift $ Vk.ImageView.create @() dvc createInfo nil nil \sciv -> f sciv `runReaderT` g

recreateImageViews :: Vk.Device.D sd ->
	Vk.Format -> [Vk.Image.Binded ss ss] ->
	HeteroVarList Vk.ImageView.I sis -> ReaderT Global IO ()
recreateImageViews _dvc _scifmt [] HVNil = pure ()
recreateImageViews dvc scifmt (img : imgs) (iv :...: ivs) =
	recreateImageView1 dvc img scifmt iv >>
	recreateImageViews dvc scifmt imgs ivs
recreateImageViews _ _ _ _ =
	error "number of Vk.Image.M.I and Vk.ImageView.M.I should be same"

recreateImageView1 :: Vk.Device.D sd ->
	Vk.Image.Binded ss ss -> Vk.Format -> Vk.ImageView.I siv -> ReaderT Global IO ()
recreateImageView1 dvc sci scifmt iv = do
	let	createInfo = makeImageViewCreateInfo sci scifmt
	lift $ Vk.ImageView.recreate @() dvc createInfo nil nil iv

makeImageViewCreateInfo ::
	Vk.Image.Binded ss ss -> Vk.Format -> Vk.ImageView.CreateInfo ss ss n
makeImageViewCreateInfo sci scifmt = Vk.ImageView.CreateInfo {
	Vk.ImageView.createInfoNext = Nothing,
	Vk.ImageView.createInfoFlags = Vk.ImageView.CreateFlagsZero,
	Vk.ImageView.createInfoImage = sci,
	Vk.ImageView.createInfoViewType = Vk.ImageView.Type2d,
	Vk.ImageView.createInfoFormat = scifmt,
	Vk.ImageView.createInfoComponents = components,
	Vk.ImageView.createInfoSubresourceRange = subresourceRange }
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

createRenderPass :: Vk.Device.D sd ->
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

createPipelineLayout :: Vk.Device.D sd ->
	r -> (forall sl . Vk.Ppl.Layout.LL sl '[] -> ReaderT r IO b) -> IO b
createPipelineLayout dvc g f = do
	let	pipelineLayoutInfo = Vk.Ppl.Layout.CreateInfo {
			Vk.Ppl.Layout.createInfoNext = Nothing,
			Vk.Ppl.Layout.createInfoFlags = zeroBits,
			Vk.Ppl.Layout.createInfoSetLayouts = HVNil,
			Vk.Ppl.Layout.createInfoPushConstantRanges = [] }
	Vk.Ppl.Layout.create @() dvc pipelineLayoutInfo nil nil \ppllyt ->
		f ppllyt `runReaderT` g

createGraphicsPipeline :: Vk.Device.D sd ->
	Vk.C.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.LL sl '[] ->
	(forall sg . Vk.Ppl.Graphics.G sg
		(Solo (AddType Vertex 'Vk.VertexInput.RateVertex))
		'[Cglm.Vec2, Cglm.Vec3] -> ReaderT Global IO ()) ->
	ReaderT Global IO ()
createGraphicsPipeline dvc sce rp ppllyt f = ask >>= \g ->
	lift $ Vk.Ppl.Graphics.createGs' dvc Nothing (V14 pplInfo :...: HVNil)
			nil nil \(V2 gpl :...: HVNil) -> f gpl `runReaderT` g
	where pplInfo = makeGraphicsPipelineCreateInfo sce rp ppllyt

recreateGraphicsPipeline :: Vk.Device.D sd ->
	Vk.C.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.LL sl '[] ->
	Vk.Ppl.Graphics.G sg
		(Solo (AddType Vertex 'Vk.VertexInput.RateVertex))
		'[Cglm.Vec2, Cglm.Vec3] -> IO ()
recreateGraphicsPipeline dvc sce rp ppllyt gpls = Vk.Ppl.Graphics.recreateGs'
	dvc Nothing (V14 pplInfo :...: HVNil) nil nil (V2 gpls :...: HVNil)
	where pplInfo = makeGraphicsPipelineCreateInfo sce rp ppllyt

makeGraphicsPipelineCreateInfo ::
	Vk.C.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.LL sl '[] ->
	Vk.Ppl.Graphics.CreateInfo' () '[
			'((), (), 'GlslVertexShader, (), (), ()),
			'((), (), 'GlslFragmentShader, (), (), ()) ]
		'(	(), (Solo (AddType Vertex 'Vk.VertexInput.RateVertex)),
			'[Cglm.Vec2, Cglm.Vec3] )
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
	() (Solo (AddType Vertex 'Vk.VertexInput.RateVertex))
	'[Cglm.Vec2, Cglm.Vec3]
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

initVulkan :: Vk.PhysicalDevice.P -> QueueFamilyIndices ->
	Vk.Device.D sd -> Vk.Queue.Q -> ReaderT Global IO ()
initVulkan phdvc qfis dvc gq = do
	createCommandPool qfis dvc
	createVertexBuffer phdvc dvc gq
	createCommandBuffers dvc
	createSyncObjects dvc

createFramebuffers :: Vk.Device.D sd ->
	Vk.C.Extent2d -> HeteroVarList Vk.ImageView.I sis -> Vk.RndrPass.R sr ->
	(forall sfs . RecreateFramebuffers sis sfs =>
		HeteroVarList Vk.Framebuffer.F sfs -> ReaderT Global IO a) ->
	ReaderT Global IO a
createFramebuffers _ _ HVNil _ f = f HVNil
createFramebuffers dvc sce (sciv :...: scivs) rp f = ask >>= \g ->
	lift $ createFramebuffer1 dvc sce rp sciv \fb ->
	(createFramebuffers dvc sce scivs rp \fbs -> f (fb :...: fbs)) `runReaderT` g

createFramebuffer1 :: Vk.Device.D sd -> Vk.C.Extent2d -> Vk.RndrPass.R sr ->
	Vk.ImageView.I si -> (forall sf . Vk.Framebuffer.F sf -> IO a) -> IO a
createFramebuffer1 dvc sce rp attachment =
	Vk.Framebuffer.create @() dvc framebufferInfo nil nil
	where framebufferInfo = makeFramebufferCreateInfo' sce rp attachment

class RecreateFramebuffers (sis :: [Type]) (sfs :: [Type]) where
	recreateFramebuffers :: Vk.Device.D sd -> Vk.C.Extent2d ->
		HeteroVarList Vk.ImageView.I sis -> Vk.RndrPass.R sr ->
		HeteroVarList Vk.Framebuffer.F sfs -> IO ()

instance RecreateFramebuffers '[] '[] where
	recreateFramebuffers _dvc _sce HVNil _rp HVNil = pure ()

instance RecreateFramebuffers sis sfs => RecreateFramebuffers (si ': sis) (sf ': sfs) where
	recreateFramebuffers dvc sce (sciv :...: scivs) rp (fb :...: fbs) = do
		recreateFramebuffer1 dvc sce rp sciv fb
		recreateFramebuffers dvc sce scivs rp fbs

{-
recreateFramebuffers :: SameNumber sis sfs => Vk.Device.D sd -> Vk.C.Extent2d ->
	HeteroVarList Vk.ImageView.I sis -> Vk.RndrPass.R sr ->
	HeteroVarList Vk.Framebuffer.F sfs -> IO ()
recreateFramebuffers _dvc _sce HVNil _rp HVNil = pure ()
recreateFramebuffers dvc sce (sciv :...: scivs) rp (fb :...: fbs) = do
	recreateFramebuffer1 dvc sce rp sciv fb
	recreateFramebuffers dvc sce scivs rp fbs
	-}

recreateFramebuffer1 :: Vk.Device.D sd -> Vk.C.Extent2d -> Vk.RndrPass.R sr ->
	Vk.ImageView.I si -> Vk.Framebuffer.F sf -> IO ()
recreateFramebuffer1 dvc sce rp attachment fb =
	Vk.Framebuffer.recreate @() dvc framebufferInfo nil nil fb
	where framebufferInfo = makeFramebufferCreateInfo' sce rp attachment

makeFramebufferCreateInfo ::
	Vk.C.Extent2d -> Vk.RndrPass.R sr -> Vk.ImageView.M.I ->
	Vk.Framebuffer.M.CreateInfo ()
makeFramebufferCreateInfo
	sce (Vk.RndrPass.R rp) attachment = Vk.Framebuffer.M.CreateInfo {
	Vk.Framebuffer.M.createInfoNext = Nothing,
	Vk.Framebuffer.M.createInfoFlags = zeroBits,
	Vk.Framebuffer.M.createInfoRenderPass = rp,
	Vk.Framebuffer.M.createInfoAttachments = [attachment],
	Vk.Framebuffer.M.createInfoWidth = w,
	Vk.Framebuffer.M.createInfoHeight = h,
	Vk.Framebuffer.M.createInfoLayers = 1 }
	where
	Vk.C.Extent2d { Vk.C.extent2dWidth = w, Vk.C.extent2dHeight = h } = sce

makeFramebufferCreateInfo' ::
	Vk.C.Extent2d -> Vk.RndrPass.R sr -> Vk.ImageView.I si ->
	Vk.Framebuffer.CreateInfo () sr '[si]
makeFramebufferCreateInfo' sce rp attachment = Vk.Framebuffer.CreateInfo {
	Vk.Framebuffer.createInfoNext = Nothing,
	Vk.Framebuffer.createInfoFlags = zeroBits,
	Vk.Framebuffer.createInfoRenderPass = rp,
	Vk.Framebuffer.createInfoAttachments = attachment :...: HVNil,
	Vk.Framebuffer.createInfoWidth = w,
	Vk.Framebuffer.createInfoHeight = h,
	Vk.Framebuffer.createInfoLayers = 1 }
	where
	Vk.C.Extent2d { Vk.C.extent2dWidth = w, Vk.C.extent2dHeight = h } = sce

createCommandPool :: QueueFamilyIndices -> Vk.Device.D sd -> ReaderT Global IO ()
createCommandPool qfis (Vk.Device.D dvc) = do
	let	poolInfo = Vk.CommandPool.CreateInfo {
			Vk.CommandPool.createInfoNext = Nothing,
			Vk.CommandPool.createInfoFlags =
				Vk.CommandPool.CreateResetCommandBufferBit,
			Vk.CommandPool.createInfoQueueFamilyIndex =
				graphicsFamily qfis }
	writeGlobal globalCommandPool
		=<< lift (Vk.CommandPool.create @() dvc poolInfo nil)

createVertexBuffer :: Vk.PhysicalDevice.P -> Vk.Device.D sd -> Vk.Queue.Q ->
	ReaderT Global IO ()
createVertexBuffer phdvc dvc@(Vk.Device.D dvcm) gq = do
	(sb, sbm) <- createBuffer phdvc dvc (length vertices)
		Vk.Buffer.UsageTransferSrcBit $
		Vk.Memory.PropertyHostVisibleBit .|.
		Vk.Memory.PropertyHostCoherentBit
	lift $ Vk.Memory.List.writeList dvcm sbm Vk.Memory.M.MapFlagsZero vertices
	(vb, vbm) <- createBuffer phdvc dvc (length vertices)
		(Vk.Buffer.UsageTransferDstBit .|.
			Vk.Buffer.UsageVertexBufferBit)
		Vk.Memory.PropertyDeviceLocalBit
	copyBuffer dvc gq sb vb (length vertices)
	lift do	Vk.Buffer.List.destroy dvcm sb nil
		Vk.Memory.List.free dvcm sbm nil
	writeGlobal globalVertexBuffer vb
	writeGlobal globalVertexBufferMemory vbm

createBuffer :: Vk.PhysicalDevice.P -> Vk.Device.D sd ->
	Int -> Vk.Buffer.UsageFlags -> Vk.Memory.PropertyFlags ->
	ReaderT Global IO (Vk.Buffer.List.B Vertex, Vk.Device.M.MemoryList Vertex)
createBuffer phdvc (Vk.Device.D dvc) ln usage properties = do
	let	bufferInfo = Vk.Buffer.List.CreateInfo {
			Vk.Buffer.List.createInfoNext = Nothing,
			Vk.Buffer.List.createInfoFlags =
				Vk.Buffer.CreateFlagsZero,
			Vk.Buffer.List.createInfoLength = ln,
			Vk.Buffer.List.createInfoUsage = usage,
			Vk.Buffer.List.createInfoSharingMode =
				Vk.SharingModeExclusive,
			Vk.Buffer.List.createInfoQueueFamilyIndices = [] }
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
	Vk.Device.D sd -> Vk.Queue.Q ->
	Vk.Buffer.List.B v -> Vk.Buffer.List.B v -> Int -> ReaderT Global IO ()
copyBuffer (Vk.Device.D dvc) gq srcBuffer dstBuffer ln = do
	cp <- readGlobal globalCommandPool
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

createCommandBuffers :: Vk.Device.D sd -> ReaderT Global IO ()
createCommandBuffers (Vk.Device.D dvc) = do
	cp <- readGlobal globalCommandPool
	let	allocInfo = Vk.CommandBuffer.AllocateInfo {
			Vk.CommandBuffer.allocateInfoNext = Nothing,
			Vk.CommandBuffer.allocateInfoCommandPool = cp,
			Vk.CommandBuffer.allocateInfoLevel =
				Vk.CommandBuffer.LevelPrimary,
			Vk.CommandBuffer.allocateInfoCommandBufferCount =
				fromIntegral maxFramesInFlight }
	writeGlobal globalCommandBuffers
		=<< lift (Vk.CommandBuffer.allocate @() dvc allocInfo)

createSyncObjects :: Vk.Device.D sd -> ReaderT Global IO ()
createSyncObjects (Vk.Device.D dvc) = do
	let	semaphoreInfo = Vk.Semaphore.CreateInfo {
			Vk.Semaphore.createInfoNext = Nothing,
			Vk.Semaphore.createInfoFlags =
				Vk.Semaphore.CreateFlagsZero }
		fenceInfo = Vk.Fence.CreateInfo {
			Vk.Fence.createInfoNext = Nothing,
			Vk.Fence.createInfoFlags = Vk.Fence.CreateSignaledBit }
	writeGlobal globalImageAvailableSemaphores
		=<< lift (replicateM maxFramesInFlight
			$ Vk.Semaphore.create @() dvc semaphoreInfo nil)
	writeGlobal globalRenderFinishedSemaphores
		=<< lift (replicateM maxFramesInFlight
			$ Vk.Semaphore.create @() dvc semaphoreInfo nil)
	writeGlobal globalInFlightFences
		=<< lift (replicateM maxFramesInFlight
			$ Vk.Fence.create @() dvc fenceInfo nil)

recordCommandBuffer ::
	Vk.CommandBuffer.C (Solo (AddType Vertex 'Vk.VertexInput.RateVertex)) ->
	Vk.C.Extent2d -> Vk.RndrPass.R sr ->
	Vk.Ppl.Graphics.G sg
		(Solo (AddType Vertex 'Vk.VertexInput.RateVertex))
		'[Cglm.Vec2, Cglm.Vec3] ->
	HeteroVarList Vk.Framebuffer.F sfs ->
	Word32 -> ReaderT Global IO ()
recordCommandBuffer cb sce (Vk.RndrPass.R rp) (Vk.Ppl.Graphics.G gpl) fbs imageIndex = do
	let	beginInfo = Vk.CommandBuffer.BeginInfo {
			Vk.CommandBuffer.beginInfoNext = Nothing,
			Vk.CommandBuffer.beginInfoFlags =
				Vk.CommandBuffer.UsageFlagsZero,
			Vk.CommandBuffer.beginInfoInheritanceInfo = Nothing }
	lift $ Vk.CommandBuffer.begin @() @() cb beginInfo
	let	scfbs = heteroVarListToList (\(Vk.Framebuffer.F f) -> f) fbs
		renderPassInfo = Vk.RndrPass.M.BeginInfo {
			Vk.RndrPass.M.beginInfoNext = Nothing,
			Vk.RndrPass.M.beginInfoRenderPass = rp,
			Vk.RndrPass.M.beginInfoFramebuffer =
				scfbs `genericIndex` imageIndex,
			Vk.RndrPass.M.beginInfoRenderArea = Vk.C.Rect2d {
				Vk.C.rect2dOffset = Vk.C.Offset2d 0 0,
				Vk.C.rect2dExtent = sce },
			Vk.RndrPass.M.beginInfoClearValues = [
				Vk.ClearValueColor
					. fromJust $ rgbaDouble 0 0 0 1 ] }
	lift $ Vk.Cmd.M.beginRenderPass @()
		@('Vk.ClearTypeColor 'Vk.ClearColorTypeFloat32)
		cb renderPassInfo Vk.Subpass.ContentsInline
	lift $ Vk.Cmd.M.bindPipeline cb Vk.Ppl.BindPointGraphics gpl
	vb <- readGlobal globalVertexBuffer
	lift $ Vk.Cmd.List.bindVertexBuffers cb
		((vb, 0) :!: BNil :: BList '[Vertex])
	lift do	Vk.Cmd.M.draw cb 3 1 0 0
		Vk.Cmd.M.endRenderPass cb
		Vk.CommandBuffer.end cb

mainLoop :: RecreateFramebuffers ss sfs =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhysicalDevice.P -> QueueFamilyIndices -> Vk.Device.D sd ->
	Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.Khr.Swapchain.S ssc -> Vk.C.Extent2d -> HeteroVarList Vk.ImageView.I ss ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.LL sl '[] -> Vk.Ppl.Graphics.G sg
		(Solo (AddType Vertex 'Vk.VertexInput.RateVertex))
		'[Cglm.Vec2, Cglm.Vec3] ->
	HeteroVarList Vk.Framebuffer.F sfs ->
	ReaderT Global IO ()
mainLoop win sfc phdvc qfis dvc@(Vk.Device.D dvcm) gq pq sc ext0 scivs rp ppllyt gpl fbs = do
	($ ext0) $ fix \loop ext -> do
		lift Glfw.pollEvents
		g <- ask
		lift . catchAndRecreateSwapChain g win sfc phdvc qfis dvc sc ext scivs rp ppllyt gpl fbs (\e -> loop e `runReaderT` g)
			$ runLoop win sfc phdvc qfis dvc gq pq sc g ext scivs rp ppllyt gpl fbs (\e -> loop e `runReaderT` g)
	lift $ Vk.Device.M.waitIdle dvcm

runLoop :: RecreateFramebuffers sis sfs =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc -> Vk.PhysicalDevice.P ->
	QueueFamilyIndices -> Vk.Device.D sd -> Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.Khr.Swapchain.S ssc -> Global -> Vk.C.Extent2d ->
	HeteroVarList Vk.ImageView.I sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.LL sl '[] ->
	Vk.Ppl.Graphics.G sg (Solo (AddType Vertex 'Vk.VertexInput.RateVertex))
		'[Cglm.Vec2, Cglm.Vec3] ->
	HeteroVarList Vk.Framebuffer.F sfs ->
	(Vk.C.Extent2d -> IO ()) ->
	IO ()
runLoop win sfc phdvc qfis dvc gq pq sc g ext scivs rp ppllyt gpl fbs loop = do
	drawFrame win sfc phdvc qfis dvc gq pq sc ext scivs rp ppllyt gpl fbs (\e -> lift $ loop e) `runReaderT` g
	bool (loop ext) (pure ()) =<< Glfw.windowShouldClose win

drawFrame :: RecreateFramebuffers sis sfs =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhysicalDevice.P -> QueueFamilyIndices -> Vk.Device.D sd ->
	Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.Khr.Swapchain.S ssc -> Vk.C.Extent2d ->
	HeteroVarList Vk.ImageView.I sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.LL sl '[] ->
	Vk.Ppl.Graphics.G sg (Solo (AddType Vertex 'Vk.VertexInput.RateVertex))
		'[Cglm.Vec2, Cglm.Vec3] ->
	HeteroVarList Vk.Framebuffer.F sfs ->
	(Vk.C.Extent2d -> ReaderT Global IO ()) -> ReaderT Global IO ()
drawFrame win sfc phdvc qfis dvc@(Vk.Device.D dvcm) gq pq (Vk.Khr.Swapchain.S sc) ext scivs rp ppllyt gpl fbs loop = do
	cf <- readGlobal globalCurrentFrame
	iff <- (!! cf) <$> readGlobal globalInFlightFences
	lift $ Vk.Fence.waitForFs dvcm [iff] True maxBound
	ias <- (!! cf) <$> readGlobal globalImageAvailableSemaphores
	imageIndex <- lift $ Vk.Khr.acquireNextImageResult [Vk.Success, Vk.SuboptimalKhr]
		dvcm sc uint64Max (Just ias) Nothing
	lift $ Vk.Fence.resetFs dvcm [iff]
	cb <- (!! cf) <$> readGlobal globalCommandBuffers
	lift $ Vk.CommandBuffer.reset cb Vk.CommandBuffer.ResetFlagsZero
	recordCommandBuffer cb ext rp gpl fbs imageIndex
	rfs <- (!! cf) <$> readGlobal globalRenderFinishedSemaphores
	let	submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = Nothing,
			Vk.submitInfoWaitSemaphoreDstStageMasks =
				[(ias, Vk.Ppl.StageColorAttachmentOutputBit)],
			Vk.submitInfoCommandBuffers = [cb],
			Vk.submitInfoSignalSemaphores = [rfs] }
	lift . Vk.Queue.submit' @() gq [submitInfo] $ Just iff
	let	presentInfo = Vk.Khr.PresentInfo {
			Vk.Khr.presentInfoNext = Nothing,
			Vk.Khr.presentInfoWaitSemaphores = [rfs],
			Vk.Khr.presentInfoSwapchainImageIndices =
				[(sc, imageIndex)] }
	g <- ask
	lift . catchAndRecreateSwapChain g win sfc phdvc qfis dvc (Vk.Khr.Swapchain.S sc) ext scivs rp ppllyt gpl fbs (\e -> loop e `runReaderT` g) . catchAndSerialize
		$ Vk.Khr.queuePresent @() pq presentInfo
	writeGlobal globalCurrentFrame $ (cf + 1) `mod` maxFramesInFlight

catchAndSerialize :: IO () -> IO ()
catchAndSerialize =
	(`catch` \(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

catchAndRecreateSwapChain :: RecreateFramebuffers sis sfs =>
	Global -> Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhysicalDevice.P -> QueueFamilyIndices -> Vk.Device.D sd ->
	Vk.Khr.Swapchain.S ssc -> Vk.C.Extent2d ->
	HeteroVarList Vk.ImageView.I sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.LL sl '[] ->
	Vk.Ppl.Graphics.G sg
		(Solo (AddType Vertex 'Vk.VertexInput.RateVertex))
		'[Cglm.Vec2, Cglm.Vec3] ->
	HeteroVarList Vk.Framebuffer.F sfs ->
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
			e <- recreateSwapChainAndOthers win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs `runReaderT` g
			loop e
		else loop ext)

doWhile_ :: IO Bool -> IO ()
doWhile_ act = (`when` doWhile_ act) =<< act

recreateSwapChainAndOthers :: RecreateFramebuffers sis sfs =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhysicalDevice.P -> QueueFamilyIndices -> Vk.Device.D sd ->
	Vk.Khr.Swapchain.S ssc -> HeteroVarList Vk.ImageView.I sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.LL sl '[] ->
	Vk.Ppl.Graphics.G sg
		(Solo (AddType Vertex 'Vk.VertexInput.RateVertex))
		'[Cglm.Vec2, Cglm.Vec3] ->
	HeteroVarList Vk.Framebuffer.F sfs ->
	ReaderT Global IO Vk.C.Extent2d
recreateSwapChainAndOthers win sfc phdvc qfis dvc@(Vk.Device.D dvcm)
	sc scivs rp ppllyt gpl fbs = do
	lift do	(wdth, hght) <- Glfw.getFramebufferSize win
		when (wdth == 0 || hght == 0) $ doWhile_ do
			Glfw.waitEvents
			(wd, hg) <- Glfw.getFramebufferSize win
			pure $ wd == 0 || hg == 0
	lift $ Vk.Device.M.waitIdle dvcm

	(scfmt, ext) <- recreateSwapChain win sfc phdvc qfis dvc sc
	let	scifmt = Vk.Khr.Surface.M.formatFormat scfmt
	imgs <- lift $ Vk.Khr.Swapchain.getImages dvc sc
	recreateImageViews dvc scifmt imgs scivs
	lift $ recreateGraphicsPipeline dvc ext rp ppllyt gpl
	lift $ recreateFramebuffers dvc ext scivs rp fbs
	pure ext

cleanup :: Vk.Device.D sd -> ReaderT Global IO ()
cleanup _dvc@(Vk.Device.D dvcm) = do
	vb <- readGlobal globalVertexBuffer
	lift $ Vk.Buffer.List.destroy dvcm vb nil
	vbm <- readGlobal globalVertexBufferMemory
	lift $ Vk.Memory.List.free dvcm vbm nil
	lift . (flip (Vk.Semaphore.destroy dvcm) nil `mapM_`)
		=<< readGlobal globalImageAvailableSemaphores
	lift . (flip (Vk.Semaphore.destroy dvcm) nil `mapM_`)
		=<< readGlobal globalRenderFinishedSemaphores
	lift . (flip (Vk.Fence.destroy dvcm) nil `mapM_`)
		=<< readGlobal globalInFlightFences
	lift . flip (Vk.CommandPool.destroy dvcm) nil
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
