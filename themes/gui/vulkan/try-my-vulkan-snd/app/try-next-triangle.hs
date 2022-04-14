{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
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

import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt
import qualified Graphics.UI.GLFW as GlfwB
import qualified Glfw

import ThEnv

import Vulkan.Base

import qualified Vulkan as Vk
import qualified Vulkan.Core as Vk.C
import qualified Vulkan.Enum as Vk
import qualified Vulkan.AllocationCallbacks as Vk.AllocationCallbacks
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
	globalSwapChainExtent :: IORef Vk.C.Extent2d
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
		globalSwapChainExtent = sce }

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
			populateDebugMessengerCreateInfo
			Vk.AllocationCallbacks.nil )

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
	writeGlobal globalSurface =<< lift (
		Glfw.createWindowSurface ist win Vk.AllocationCallbacks.nil )

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
	dvc <- lift (
		Vk.Device.create @() @()
			pdvc createInfo Vk.AllocationCallbacks.nil )
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
	sc <- lift $ Vk.Khr.Swapchain.create @()
		dvc createInfo Vk.AllocationCallbacks.nil
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

mainLoop :: ReaderT Global IO ()
mainLoop = do
	w <- fromJust <$> readGlobal globalWindow
	lift $ fix \loop -> bool (pure ()) loop =<< do
		GlfwB.pollEvents
		not <$> GlfwB.windowShouldClose w

cleanup :: ReaderT Global IO ()
cleanup = do
	dvc <- readGlobal globalDevice
	lift . (\sc -> Vk.Khr.Swapchain.destroy
			dvc sc Vk.AllocationCallbacks.nil)
		=<< readGlobal globalSwapchain
	lift $ Vk.Device.destroy dvc Vk.AllocationCallbacks.nil
	ist <- readGlobal globalInstance
	dmsgr <- readGlobal globalDebugMessenger
	when enableValidationLayers . lift
		$ Vk.Ext.DebugUtils.Messenger.destroy
			ist dmsgr Vk.AllocationCallbacks.nil
	sfc <- readGlobal globalSurface
	lift $ Vk.Khr.Surface.destroy ist sfc Vk.AllocationCallbacks.nil
	lift $ Vk.Instance.destroy @() ist Nothing
	lift . GlfwB.destroyWindow . fromJust =<< readGlobal globalWindow
	lift $ GlfwB.terminate
