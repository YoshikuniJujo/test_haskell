{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Pointable
import Control.Monad.Fix
import Control.Monad.Reader
import Data.Bits
import Data.Bool
import Data.Maybe
import Data.List
import Data.Word
import Data.IORef
import Data.Color

import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt
import qualified Graphics.UI.GLFW as GlfwB
import qualified Glfw

import ThEnv
import Shaderc
import Shaderc.TH

import Vulkan.Base

import qualified Vulkan as Vk
import qualified Vulkan.Core as Vk.C
import qualified Vulkan.Enum as Vk
import qualified Vulkan.Instance as Vk.Instance
import qualified Vulkan.Instance.Enum as Vk.Instance
import qualified Vulkan.Khr as Vk.Khr
import qualified Vulkan.Khr.Enum as Vk.Khr
import qualified Vulkan.Ext.DebugUtils as Vk.Ext.DebugUtils
import qualified Vulkan.Ext.DebugUtils.Messenger as Vk.Ext.DebugUtils.Messenger
import qualified Vulkan.Ext.DebugUtils.Message.Enum as Vk.Ext.DebugUtils.Message
import qualified Vulkan.PhysicalDevice as Vk.PhysicalDevice
import qualified Vulkan.QueueFamily as Vk.QueueFamily
import qualified Vulkan.Device as Vk.Device
import qualified Vulkan.Device.Queue as Vk.Device.Queue
import qualified Vulkan.Device.Queue.Enum as Vk.Device.Queue
import qualified Vulkan.Khr.Surface as Vk.Khr.Surface
import qualified Vulkan.Khr.Surface.PhysicalDevice as
	Vk.Khr.Surface.PhysicalDevice
import qualified Vulkan.Khr.Swapchain as Vk.Khr.Swapchain
import qualified Vulkan.Khr.Swapchain.Enum as Vk.Khr.Swapchain
import qualified Vulkan.Image as Vk.Image
import qualified Vulkan.Image.Enum as Vk.Image
import qualified Vulkan.ImageView as Vk.ImageView
import qualified Vulkan.ImageView.Enum as Vk.ImageView
import qualified Vulkan.Component as Vk.Component
import qualified Vulkan.Component.Enum as Vk.Component
import qualified Vulkan.Shader.Module as Vk.Shader.Module
import qualified Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShaderStage
import qualified Vulkan.Pipeline.ShaderStage.Enum as Vk.Ppl.ShaderStage
import qualified Vulkan.Shader.Stage.Enum as Vk.Shader.Stage
import qualified Vulkan.Pipeline.VertexInputState as Vk.Ppl.VertexInputSt
import qualified Vulkan.Pipeline.VertexInputState.Middle as
	Vk.Ppl.VertexInputSt.M
import qualified Vulkan.Pipeline.InputAssemblyState as Vk.Ppl.InpAsmbSt
import qualified Vulkan.Pipeline.ViewportState as Vk.Ppl.ViewportSt
import qualified Vulkan.Pipeline.RasterizationState as Vk.Ppl.RstSt
import qualified Vulkan.Pipeline.MultisampleState as Vk.Ppl.MulSmplSt
import qualified Vulkan.Sample as Vk.Sample
import qualified Vulkan.Sample.Enum as Vk.Sample
import qualified Vulkan.Pipeline.ColorBlendAttachment as Vk.Ppl.ClrBlndAtt
import qualified Vulkan.ColorComponent.Enum as Vk.ClrCmp
import qualified Vulkan.Pipeline.ColorBlendState as Vk.Ppl.ClrBlndSt
import qualified Vulkan.Pipeline.Layout as Vk.Ppl.Layout

main :: IO ()
main = runReaderT run =<< newGlobal

width, height :: Int
width = 800; height = 600

enableValidationLayers :: Bool
enableValidationLayers =
	maybe True (const False) $(lookupCompileEnvExp "NDEBUG")

validationLayers :: [Txt.Text]
validationLayers = [Vk.Khr.validationLayerName]

data Global = Global {
	globalWindow :: IORef (Maybe GlfwB.Window),
	globalInstance :: IORef Vk.Instance.I,
	globalDebugMessenger :: IORef Vk.Ext.DebugUtils.Messenger,
	globalPhysicalDevice :: IORef Vk.PhysicalDevice.P,
	globalDevice :: IORef Vk.Device.D,
	globalGraphicsQueue :: IORef Vk.Queue,
	globalSurface :: IORef Vk.Khr.Surface.S,
	globalPresentQueue :: IORef Vk.Queue,
	globalSwapchain :: IORef Vk.Khr.Swapchain.S,
	globalSwapChainImages :: IORef [Vk.Image.I],
	globalSwapChainImageFormat :: IORef (Maybe Vk.Format),
	globalSwapChainExtent :: IORef Vk.C.Extent2d,
	globalSwapChainImageViews :: IORef [Vk.ImageView.I],
	globalPipelineLayout :: IORef Vk.Ppl.Layout.L
	}

readGlobal :: (Global -> IORef a) -> ReaderT Global IO a
readGlobal ref = lift . readIORef =<< asks ref

writeGlobal :: (Global -> IORef a) -> a -> ReaderT Global IO ()
writeGlobal ref x = lift . (`writeIORef` x) =<< asks ref

newGlobal :: IO Global
newGlobal = do
	win <- newIORef Nothing
	ist <- newIORef $ Vk.Instance.I NullPtr
	dmsgr <- newIORef $ Vk.Ext.DebugUtils.Messenger NullPtr
	pdvc <- newIORef $ Vk.PhysicalDevice.P NullPtr
	dvc <- newIORef $ Vk.Device.D NullPtr
	gq <- newIORef $ Vk.Queue NullPtr
	sfc <- newIORef $ Vk.Khr.Surface.S NullPtr
	pq <- newIORef $ Vk.Queue NullPtr
	sc <- newIORef $ Vk.Khr.Swapchain.S NullPtr
	scis <- newIORef []
	scif <- newIORef Nothing
	sce <- newIORef $ Vk.C.Extent2d 0 0
	scivs <- newIORef []
	ppllyt <- newIORef $ Vk.Ppl.Layout.L NullPtr
	pure Global {
		globalWindow = win,
		globalInstance = ist,
		globalDebugMessenger = dmsgr,
		globalPhysicalDevice = pdvc,
		globalDevice = dvc,
		globalGraphicsQueue = gq,
		globalSurface = sfc,
		globalPresentQueue = pq,
		globalSwapchain = sc,
		globalSwapChainImages = scis,
		globalSwapChainImageFormat = scif,
		globalSwapChainExtent = sce,
		globalSwapChainImageViews = scivs,
		globalPipelineLayout = ppllyt }

run :: ReaderT Global IO ()
run = do
	initWindow
	initVulkan
	mainLoop
	cleanup

initWindow :: ReaderT Global IO ()
initWindow = do
	Just w <- lift do
		True <- GlfwB.init
		GlfwB.windowHint
			$ GlfwB.WindowHint'ClientAPI GlfwB.ClientAPI'NoAPI
		GlfwB.windowHint $ GlfwB.WindowHint'Resizable False
		GlfwB.createWindow width height "Vulkan" Nothing Nothing
	writeGlobal globalWindow $ Just w

initVulkan :: ReaderT Global IO ()
initVulkan = do
	createInstance
	when enableValidationLayers setupDebugMessenger
	createSurface
	pickPhysicalDevice
	createLogicalDevice
	createSwapChain
	createImageViews
	createGraphicsPipeline

createInstance :: ReaderT Global IO ()
createInstance = do
	lift . mapM_
		(Txt.putStrLn . ("\t" <>) . Vk.extensionPropertiesExtensionName)
		=<< lift (Vk.Instance.enumerateExtensionProperties Nothing)
	when enableValidationLayers $ bool
		(lift $ error "validation layers requested, but not available!")
		(pure ())
			=<< checkValidationLayerSupport
	let	appInfo = Vk.ApplicationInfo {
			Vk.applicationInfoNext = Nothing,
			Vk.applicationInfoApplicationName = "Hello Triangle",
			Vk.applicationInfoApplicationVersion =
				Vk.makeApiVersion 0 1 0 0,
			Vk.applicationInfoEngineName = "No Engine",
			Vk.applicationInfoEngineVersion =
				Vk.makeApiVersion 0 1 0 0,
			Vk.applicationInfoApiVersion = Vk.apiVersion_1_0 }
	extensions <- getRequiredExtensions
	let	createInfo = Vk.Instance.CreateInfo {
			Vk.Instance.createInfoNext =
				Just populateDebugMessengerCreateInfo,
			Vk.Instance.createInfoFlags =
				Vk.Instance.CreateFlagsZero,
			Vk.Instance.createInfoApplicationInfo = appInfo,
			Vk.Instance.createInfoEnabledLayerNames =
				bool [] validationLayers enableValidationLayers,
			Vk.Instance.createInfoEnabledExtensionNames = extensions
			}
	writeGlobal globalInstance
		=<< lift (Vk.Instance.create @_ @() @() createInfo Nothing)

checkValidationLayerSupport :: ReaderT Global IO Bool
checkValidationLayerSupport = lift do
	availableLayers <- Vk.Instance.enumerateLayerProperties
	print validationLayers
	print availableLayers
	pure . null $ validationLayers \\
		(Vk.layerPropertiesLayerName <$> availableLayers)

getRequiredExtensions :: ReaderT Global IO [Txt.Text]
getRequiredExtensions = lift do
	glfwExtensions <-
		(cstringToText `mapM`) =<< GlfwB.getRequiredInstanceExtensions
	pure $ bool id (Vk.Ext.DebugUtils.extensionName :)
		enableValidationLayers glfwExtensions

setupDebugMessenger :: ReaderT Global IO ()
setupDebugMessenger = do
	ist <- readGlobal globalInstance
	writeGlobal globalDebugMessenger =<< lift (
		Vk.Ext.DebugUtils.Messenger.create ist
			populateDebugMessengerCreateInfo nil )

populateDebugMessengerCreateInfo ::
	Vk.Ext.DebugUtils.Messenger.CreateInfo () () () () () ()
populateDebugMessengerCreateInfo = Vk.Ext.DebugUtils.Messenger.CreateInfo {
	Vk.Ext.DebugUtils.Messenger.createInfoNext = Nothing,
	Vk.Ext.DebugUtils.Messenger.createInfoFlags =
		Vk.Ext.DebugUtils.Messenger.CreateFlagsZero,
	Vk.Ext.DebugUtils.Messenger.createInfoMessageSeverity =
		Vk.Ext.DebugUtils.Message.SeverityVerboseBit .|.
		Vk.Ext.DebugUtils.Message.SeverityWarningBit .|.
		Vk.Ext.DebugUtils.Message.SeverityErrorBit,
	Vk.Ext.DebugUtils.Messenger.createInfoMessageType =
		Vk.Ext.DebugUtils.Message.TypeGeneralBit .|.
		Vk.Ext.DebugUtils.Message.TypeValidationBit .|.
		Vk.Ext.DebugUtils.Message.TypePerformanceBit,
	Vk.Ext.DebugUtils.Messenger.createInfoFnUserCallback = debugCallback,
	Vk.Ext.DebugUtils.Messenger.createInfoUserData = Nothing }

debugCallback :: Vk.Ext.DebugUtils.Messenger.FnCallback () () () () ()
debugCallback _messageSeverity _messageType callbackData _userData = do
	let	message = Vk.Ext.DebugUtils.Messenger.callbackDataMessage
			callbackData
	Txt.putStrLn $ "validation layer: " <> message
	pure False

createSurface :: ReaderT Global IO ()
createSurface = do
	win <- fromJust <$> readGlobal globalWindow
	ist <- readGlobal globalInstance
	writeGlobal globalSurface
		=<< lift (Glfw.createWindowSurface ist win nil)

pickPhysicalDevice :: ReaderT Global IO ()
pickPhysicalDevice = do
	ist <- readGlobal globalInstance
	devices <- lift $ Vk.PhysicalDevice.enumerate ist
	when (null devices) $ error "failed to find GPUs with Vulkan support!"
	suitableDevices <- filterM isDeviceSuitable devices
	case suitableDevices of
		[] -> error "failed to find a suitable GPU!"
		(pdvc : _) -> writeGlobal globalPhysicalDevice pdvc

isDeviceSuitable :: Vk.PhysicalDevice.P -> ReaderT Global IO Bool
isDeviceSuitable pdvc = do
	_deviceProperties <- lift $ Vk.PhysicalDevice.getProperties pdvc
	_deviceFeatures <- lift $ Vk.PhysicalDevice.getFeatures pdvc
	indices <- findQueueFamilies pdvc
	extensionSupported <- lift $ checkDeviceExtensionSupport pdvc
	swapChainSupport <- querySwapChainSupport pdvc
	let	swapChainAdequate =
			not (null $ formats swapChainSupport) &&
			not (null $ presentModes swapChainSupport)
	pure $ isComplete indices && extensionSupported && swapChainAdequate

deviceExtensions :: [Txt.Text]
deviceExtensions = [Vk.Khr.Swapchain.extensionName]

checkDeviceExtensionSupport :: Vk.PhysicalDevice.P -> IO Bool
checkDeviceExtensionSupport dvc = do
	availableExtensions <-
		Vk.PhysicalDevice.enumerateExtensionProperties dvc Nothing
	pure . null $ deviceExtensions \\
		(Vk.extensionPropertiesExtensionName <$> availableExtensions)

findQueueFamilies :: Vk.PhysicalDevice.P -> ReaderT Global IO QueueFamilyIndices
findQueueFamilies device = do
	queueFamilies <- lift
		$ Vk.PhysicalDevice.getQueueFamilyProperties device
	lift $ print queueFamilies
	sfc <- readGlobal globalSurface
	psi <- listToMaybe <$> lift (
		filterM (\i -> isPresentSupport device i sfc)
			[0 .. fromIntegral $ length queueFamilies - 1] )
	pure QueueFamilyIndices {
		graphicsFamily = fst <$> find
			((/= zeroBits)
				. (.&. Vk.QueueGraphicsBit)
				. Vk.QueueFamily.propertiesQueueFlags
				. snd )
			(zip [0 ..] queueFamilies),
		presentFamily = psi }

isPresentSupport :: Vk.PhysicalDevice.P -> Word32 -> Vk.Khr.Surface.S -> IO Bool
isPresentSupport dvc i sfc = Vk.Khr.Surface.PhysicalDevice.getSupport dvc i sfc

data QueueFamilyIndices = QueueFamilyIndices {
	graphicsFamily :: Maybe Word32,
	presentFamily :: Maybe Word32 }

isComplete :: QueueFamilyIndices -> Bool
isComplete QueueFamilyIndices {
	graphicsFamily = gf, presentFamily = pf } = isJust gf && isJust pf

data SwapChainSupportDetails = SwapChainSupportDetails {
	capabilities :: Vk.Khr.Surface.Capabilities,
	formats :: [Vk.Khr.Surface.Format],
	presentModes :: [Vk.Khr.PresentMode] }

querySwapChainSupport ::
	Vk.PhysicalDevice.P -> ReaderT Global IO SwapChainSupportDetails
querySwapChainSupport dvc = readGlobal globalSurface >>= \sfc ->
	lift $ SwapChainSupportDetails
		<$> Vk.Khr.Surface.PhysicalDevice.getCapabilities dvc sfc
		<*> Vk.Khr.Surface.PhysicalDevice.getFormats dvc sfc
		<*> Vk.Khr.Surface.PhysicalDevice.getPresentModes dvc sfc

createLogicalDevice :: ReaderT Global IO ()
createLogicalDevice = do
	indices <- findQueueFamilies =<< readGlobal globalPhysicalDevice
	let	uniqueQueueFamilies = nub [
			fromJust $ graphicsFamily indices,
			fromJust $ presentFamily indices ]
		queueCreateInfos qf = Vk.Device.Queue.CreateInfo {
			Vk.Device.Queue.createInfoNext = Nothing,
			Vk.Device.Queue.createInfoFlags =
				Vk.Device.Queue.CreateFlagsZero,
			Vk.Device.Queue.createInfoQueueFamilyIndex = qf,
			Vk.Device.Queue.createInfoQueuePriorities = [1] }
		deviceFeatures = Vk.PhysicalDevice.featuresZero
		createInfo = Vk.Device.CreateInfo {
			Vk.Device.createInfoNext = Nothing,
			Vk.Device.createInfoFlags = Vk.Device.CreateFlagsZero,
			Vk.Device.createInfoQueueCreateInfos =
				queueCreateInfos <$> uniqueQueueFamilies,
			Vk.Device.createInfoEnabledLayerNames =
				bool [] validationLayers enableValidationLayers,
			Vk.Device.createInfoEnabledExtensionNames =
				deviceExtensions,
			Vk.Device.createInfoEnabledFeatures =
				Just deviceFeatures }
	pdvc <- readGlobal globalPhysicalDevice
	dvc <- lift (Vk.Device.create @() @() pdvc createInfo nil)
	writeGlobal globalDevice dvc
	writeGlobal globalGraphicsQueue =<< lift (
		Vk.Device.getQueue dvc (fromJust $ graphicsFamily indices) 0 )

createSwapChain :: ReaderT Global IO ()
createSwapChain = do
	swapChainSupport <- querySwapChainSupport
		=<< readGlobal globalPhysicalDevice
	let	surfaceFormat =
			chooseSwapSurfaceFormat $ formats swapChainSupport
		presentMode =
			chooseSwapPresentMode $ presentModes swapChainSupport
	extent <- chooseSwapExtent $ capabilities swapChainSupport
	sfc <- readGlobal globalSurface
	indices <- findQueueFamilies =<< readGlobal globalPhysicalDevice
	let	maxImageCount = fromMaybe maxBound . onlyIf (> 0)
			. Vk.Khr.Surface.capabilitiesMaxImageCount
			$ capabilities swapChainSupport
		imageCount = clamp
			(Vk.Khr.Surface.capabilitiesMinImageCount
				(capabilities swapChainSupport) + 1)
			0 maxImageCount
		(ism, qfis) = if graphicsFamily indices /= presentFamily indices
			then (Vk.SharingModeConcurrent, fromJust <$> [
				graphicsFamily indices, presentFamily indices ])
			else (Vk.SharingModeExclusive, [])
		createInfo = Vk.Khr.Swapchain.CreateInfo {
			Vk.Khr.Swapchain.createInfoNext = Nothing,
			Vk.Khr.Swapchain.createInfoFlags =
				Vk.Khr.Swapchain.CreateFlagsZero,
			Vk.Khr.Swapchain.createInfoSurface = sfc,
			Vk.Khr.Swapchain.createInfoMinImageCount = imageCount,
			Vk.Khr.Swapchain.createInfoImageFormat =
				Vk.Khr.Surface.formatFormat surfaceFormat,
			Vk.Khr.Swapchain.createInfoImageColorSpace =
				Vk.Khr.Surface.formatColorSpace surfaceFormat,
			Vk.Khr.Swapchain.createInfoImageExtent = extent,
			Vk.Khr.Swapchain.createInfoImageArrayLayers = 1,
			Vk.Khr.Swapchain.createInfoImageUsage =
				Vk.Image.UsageColorAttachmentBit,
			Vk.Khr.Swapchain.createInfoImageSharingMode = ism,
			Vk.Khr.Swapchain.createInfoQueueFamilyIndices = qfis,
			Vk.Khr.Swapchain.createInfoPreTransform =
				Vk.Khr.Surface.capabilitiesCurrentTransform
					$ capabilities swapChainSupport,
			Vk.Khr.Swapchain.createInfoCompositeAlpha =
				Vk.Khr.CompositeAlphaOpaqueBit,
			Vk.Khr.Swapchain.createInfoPresentMode = presentMode,
			Vk.Khr.Swapchain.createInfoClipped = True,
			Vk.Khr.Swapchain.createInfoOldSwapchain = Nothing }
	dvc <- readGlobal globalDevice
	sc <- lift $ Vk.Khr.Swapchain.create @() dvc createInfo nil
	writeGlobal globalSwapchain sc
	writeGlobal globalSwapChainImages
		=<< lift (Vk.Khr.Swapchain.getImages dvc sc)
	writeGlobal globalSwapChainImageFormat
		. Just $ Vk.Khr.Surface.formatFormat surfaceFormat
	writeGlobal globalSwapChainExtent extent
	lift do	putStrLn "*** CREATE SWAP CHAIN ***"
		print surfaceFormat
		print presentMode
		print extent

onlyIf :: (a -> Bool) -> a -> Maybe a
onlyIf p x | p x = Just x | otherwise = Nothing

chooseSwapSurfaceFormat  :: [Vk.Khr.Surface.Format] -> Vk.Khr.Surface.Format
chooseSwapSurfaceFormat = \case
	availableFormats@(af0 : _) -> fromMaybe af0
		$ find preferredSwapSurfaceFormat availableFormats
	_ -> error "no available swap surface formats"

preferredSwapSurfaceFormat :: Vk.Khr.Surface.Format -> Bool
preferredSwapSurfaceFormat f =
	Vk.Khr.Surface.formatFormat f == Vk.FormatB8g8r8a8Srgb &&
	Vk.Khr.Surface.formatColorSpace f == Vk.Khr.ColorSpaceSrgbNonlinear

chooseSwapPresentMode :: [Vk.Khr.PresentMode] -> Vk.Khr.PresentMode
chooseSwapPresentMode =
	fromMaybe Vk.Khr.PresentModeFifo . find (== Vk.Khr.PresentModeMailbox)

chooseSwapExtent :: Vk.Khr.Surface.Capabilities -> ReaderT Global IO Vk.C.Extent2d
chooseSwapExtent caps
	| Vk.C.extent2dWidth curExt /= maxBound = pure curExt
	| otherwise = do
		(fromIntegral -> w, fromIntegral -> h) <-
			lift . GlfwB.getFramebufferSize
				=<< fromJust <$> readGlobal globalWindow
		pure $ Vk.C.Extent2d
			(clamp w (Vk.C.extent2dWidth n) (Vk.C.extent2dHeight n))
			(clamp h (Vk.C.extent2dWidth x) (Vk.C.extent2dHeight x))
	where
	curExt = Vk.Khr.Surface.capabilitiesCurrentExtent caps
	n = Vk.Khr.Surface.capabilitiesMinImageExtent caps
	x = Vk.Khr.Surface.capabilitiesMaxImageExtent caps

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

createGraphicsPipeline :: ReaderT Global IO ()
createGraphicsPipeline = do
	vertShaderModule <- createShaderModule glslVertexShaderMain
	fragShaderModule <- createShaderModule glslFragmentShaderMain

	let	vertShaderStageInfo = Vk.Ppl.ShaderStage.CreateInfo {
			Vk.Ppl.ShaderStage.createInfoNext = Nothing,
			Vk.Ppl.ShaderStage.createInfoFlags =
				Vk.Ppl.ShaderStage.CreateFlagsZero,
			Vk.Ppl.ShaderStage.createInfoStage =
				Vk.Shader.Stage.VertexBit,
			Vk.Ppl.ShaderStage.createInfoModule = vertShaderModule,
			Vk.Ppl.ShaderStage.createInfoName = "main",
			Vk.Ppl.ShaderStage.createInfoSpecializationInfo =
				Nothing }
		fragShaderStageInfo = Vk.Ppl.ShaderStage.CreateInfo {
			Vk.Ppl.ShaderStage.createInfoNext = Nothing,
			Vk.Ppl.ShaderStage.createInfoFlags =
				Vk.Ppl.ShaderStage.CreateFlagsZero,
			Vk.Ppl.ShaderStage.createInfoStage =
				Vk.Shader.Stage.FragmentBit,
			Vk.Ppl.ShaderStage.createInfoModule = fragShaderModule,
			Vk.Ppl.ShaderStage.createInfoName = "main",
			Vk.Ppl.ShaderStage.createInfoSpecializationInfo =
				Nothing }
		shaderStages =
			vertShaderStageInfo `Vk.Ppl.ShaderStage.CreateInfoCons`
			fragShaderStageInfo `Vk.Ppl.ShaderStage.CreateInfoCons`
			Vk.Ppl.ShaderStage.CreateInfoNil
		vertexInputInfo ::
			Vk.Ppl.VertexInputSt.CreateInfo () () '[]
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

	dvc <- readGlobal globalDevice
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

mainLoop :: ReaderT Global IO ()
mainLoop = do
	w <- fromJust <$> readGlobal globalWindow
	lift $ fix \loop -> bool (pure ()) loop =<< do
		GlfwB.pollEvents
		not <$> GlfwB.windowShouldClose w

cleanup :: ReaderT Global IO ()
cleanup = do
	dvc <- readGlobal globalDevice
	scivs <- readGlobal globalSwapChainImageViews
	lift $ flip (Vk.ImageView.destroy dvc) nil `mapM_` scivs
	lift . (\sc -> Vk.Khr.Swapchain.destroy dvc sc nil)
		=<< readGlobal globalSwapchain
	lift $ Vk.Device.destroy dvc nil
	ist <- readGlobal globalInstance
	dmsgr <- readGlobal globalDebugMessenger
	when enableValidationLayers
		. lift $ Vk.Ext.DebugUtils.Messenger.destroy ist dmsgr nil
	sfc <- readGlobal globalSurface
	lift $ Vk.Khr.Surface.destroy ist sfc nil
	lift $ Vk.Instance.destroy @() ist Nothing
	lift . GlfwB.destroyWindow . fromJust =<< readGlobal globalWindow
	lift $ GlfwB.terminate

[glslVertexShader|

#version 450

layout(location = 0) out vec3 fragColor;

vec2 positions[3] = vec2[](
	vec2(0.0, - 0.5),
	vec2(0.5, 0.5),
	vec2(- 0.5, 0.5) );

vec3 colors[3] = vec3[](
	vec3(1.0, 0.0, 0.0),
	vec3(0.0, 1.0, 0.0),
	vec3(0.0, 0.0, 1.0) );

void
main()
{
	gl_Position = vec4(positions[gl_VertexIndex], 0.0, 1.0);
	fragColor = colors[gl_VertexIndex];
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
