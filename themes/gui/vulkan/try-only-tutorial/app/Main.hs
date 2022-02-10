{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (readFile)

import Foreign.Ptr
import Foreign.Marshal
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.C.String
import Control.Monad.Fix
import Control.Monad.Cont
import Data.Maybe
import Data.List
import Data.IORef
import Data.Bits
import Data.Bool
import Data.Word
import System.IO hiding (readFile)
import System.IO.Unsafe

import Tools
import Vulkan.Base

import qualified Data.ByteString.Char8 as BSC
import qualified Graphics.UI.GLFW as GlfwB

import qualified Vulkan as Vk
import qualified Vulkan.Instance as Vk.Instance
import qualified Vulkan.Enumerate as Vk.Enumerate
import qualified Vulkan.Ext.DebugUtils.Messenger as Vk.Ext.DU.Msngr
import qualified Vulkan.PhysicalDevice as Vk.PhysicalDevice
import qualified Vulkan.Queue.Family as Vk.Queue.Family

import qualified Vulkan.Device.Queue as Vk.Device.Queue
import qualified Vulkan.Device as Vk.Device

import qualified Vulkan.Khr.Surface as Vk.Khr.Sfc
import qualified Vulkan.Khr.Present as Vk.Khr.Present
import qualified Vulkan.Khr.Surface.PhysicalDevice as
	Vk.Khr.Sfc.PhysicalDevice

import qualified Vulkan.Format as Vk.Format
import qualified Vulkan.Khr.ColorSpace as Vk.Khr.ColorSpace
import qualified Vulkan.Khr.Swapchain as Vk.Khr.Sc
import qualified Vulkan.Khr as Vk.Khr

import qualified Vulkan.ImageView as Vk.ImageView
import qualified Vulkan.Image as Vk.Img
import qualified Vulkan.Component as Vk.Component

import qualified Vulkan.Shader.Module as Vk.Shader.Module
import qualified Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShaderStage

import qualified Vulkan.Pipeline.VertexInputState as Vk.Ppl.VISt
import qualified Vulkan.Pipeline.InputAssemblyState as Vk.Ppl.IASt
import qualified Vulkan.PrimitiveTopology as Vk.PrmTplgy
import qualified Vulkan.Pipeline.ViewportState as Vk.Ppl.VPSt
import qualified Vulkan.Pipeline.RasterizationState as Vk.Ppl.RstSt
import qualified Vulkan.Polygon as Vk.Polygon
import qualified Vulkan.Cull as Vk.Cull
import qualified Vulkan.FrontFace as Vk.FrontFace
import qualified Vulkan.Pipeline.MultisampleState as Vk.Ppl.MSSt
import qualified Vulkan.Sample as Vk.Sample
import qualified Vulkan.Pipeline.ColorBlendAttachmentState as Vk.Ppl.CBASt
import qualified Vulkan.Blend as Vk.Blend
import qualified Vulkan.ColorComponent as Vk.CC
import qualified Vulkan.Pipeline.ColorBlendState as Vk.Ppl.CBSt
import qualified Vulkan.Logic as Vk.Logic
import qualified Vulkan.Pipeline.Layout as Vk.Ppl.Lyt

import qualified Vulkan.Attachment as Vk.Attachment

main :: IO ()
main = run

instance_ :: IORef Vk.Instance.Instance
instance_ = unsafePerformIO $ newIORef NullPtr

debugMessenger :: IORef Vk.Ext.DU.Msngr.Messenger
debugMessenger = unsafePerformIO $ newIORef NullPtr

physicalDevice :: IORef Vk.PhysicalDevice.PhysicalDevice
physicalDevice = unsafePerformIO $ newIORef NullHandle

device :: IORef Vk.Device.Device
device = unsafePerformIO $ newIORef NullPtr

graphicsQueue :: IORef Vk.Device.Queue
graphicsQueue = unsafePerformIO $ newIORef NullPtr

surface :: IORef Vk.Khr.Sfc.Surface
surface = unsafePerformIO $ newIORef NullPtr

presentQueue :: IORef Vk.Device.Queue
presentQueue = unsafePerformIO $ newIORef NullPtr

swapChain :: IORef Vk.Khr.Sc.Swapchain
swapChain = unsafePerformIO $ newIORef NullHandle

swapChainImages :: IORef [Vk.Img.Image]
swapChainImages = unsafePerformIO $ newIORef []

swapChainImageFormat :: IORef Vk.Format
swapChainImageFormat = unsafePerformIO $ newIORef 0

swapChainExtent :: IORef Vk.Extent2d
swapChainExtent = unsafePerformIO $ newIORef $ Vk.Extent2d 0 0

swapChainImageViews :: IORef [Vk.ImageView.ImageView]
swapChainImageViews = unsafePerformIO $ newIORef []

pipelineLayout :: IORef Vk.Ppl.Lyt.Layout
pipelineLayout = unsafePerformIO $ newIORef NullPtr

run :: IO ()
run = do
	win <- initWindow
	initVulkan win
	mainLoop win
	cleanup win

width, height :: Int
width = 800; height = 600

initWindow :: IO GlfwB.Window
initWindow = do
	True <- GlfwB.init
	GlfwB.windowHint $ GlfwB.WindowHint'ClientAPI GlfwB.ClientAPI'NoAPI
	GlfwB.windowHint $ GlfwB.WindowHint'Resizable False
	Just w <- GlfwB.createWindow width height "Vulkan" Nothing Nothing
	pure w

initVulkan :: GlfwB.Window -> IO ()
initVulkan win = do
	createInstance
	setupDebugMessenger
	createSurface win
	pickPhysicalDevice
	createLogicalDevice
	createSwapChain
	createImageViews
	createRenderPass
	createGraphicsPipeline

createInstance :: IO ()
createInstance = ($ pure) $ runContT do
	lift do	b <- checkValidationLayerSupport
		when (not b)
			$ error "validation layers requested, but no available!"
	pExtensionCount <- ContT $ alloca
	(fromIntegral -> extensionCount) <- lift do
		_ <- Vk.Enumerate.instanceExtensionProperties
			NullPtr pExtensionCount NullPtr
		peek pExtensionCount
	pExtensions <- ContT $ allocaArray extensionCount
	lift do	_ <- Vk.Enumerate.instanceExtensionProperties
			NullPtr pExtensionCount pExtensions
		extensions <- peekArray extensionCount pExtensions
		putStrLn "availbale extensions:"
		mapM_ BSC.putStrLn $ ("\t" <>) . BSC.takeWhile (/= '\NUL')
				. Vk.Enumerate.extensionPropertiesExtensionName
			<$> extensions
	hello <- lift $ newCString "Hello Triangle"
	noEngine <- lift $ newCString "No Engine"
	Vk.Ext.DU.Msngr.CreateInfo_ fDebugCreateInfo <-
		lift populateDebugMessengerCreateInfo
	pDebugCreateInfo <- ContT $ withForeignPtr fDebugCreateInfo
	let	Vk.ApplicationInfo_ fAppInfo = Vk.ApplicationInfo {
			Vk.applicationInfoSType = (),
			Vk.applicationInfoPNext = NullPtr,
			Vk.applicationInfoPApplicationName = hello,
			Vk.applicationInfoApplicationVersion =
				Vk.makeApiVersion 0 1 0 0,
			Vk.applicationInfoPEngineName = noEngine,
			Vk.applicationInfoEngineVersion =
				Vk.makeApiVersion 0 1 0 0,
			Vk.applicationInfoApiVersion =
				Vk.makeApiVersion 0 1 0 0 }
	pAppInfo <- ContT $ withForeignPtr fAppInfo
	pValidationLayer <- lift $ newCString "VK_LAYER_KHRONOS_validation"
	pValidationLayers <- ContT $ allocaArray 1
	lift $ pokeArray pValidationLayers [pValidationLayer]
	(glfwExtensionCount, pGlfwExtensions) <- getRequiredExtensions
	let	Vk.Instance.CreateInfo_ fCreateInfo = Vk.Instance.CreateInfo {
			Vk.Instance.createInfoSType = (),
			Vk.Instance.createInfoPNext = castPtr pDebugCreateInfo,
			Vk.Instance.createInfoFlags = 0,
			Vk.Instance.createInfoPApplicationInfo = pAppInfo,
			Vk.Instance.createInfoEnabledLayerCount = 1,
			Vk.Instance.createInfoPpEnabledLayerNames = pValidationLayers,
			Vk.Instance.createInfoEnabledExtensionCount =
				glfwExtensionCount,
			Vk.Instance.createInfoPpEnabledExtensionNames =
				pGlfwExtensions }
	pCreateInfo <- ContT $ withForeignPtr fCreateInfo
	pInstance <- ContT alloca
	lift do	result <- Vk.Instance.create pCreateInfo NullPtr pInstance
		when (result /= success) $ error "failed to create instance!"
		writeIORef instance_ =<< peek pInstance

checkValidationLayerSupport :: IO Bool
checkValidationLayerSupport = ($ pure) $ runContT do
	pLayerCount <- ContT alloca
	(fromIntegral -> layerCount) <- lift do
		_ <- Vk.Enumerate.instanceLayerProperties pLayerCount NullPtr
		peek pLayerCount
	pAvailableLayers <- ContT $ allocaArray layerCount
	lift do	_ <- Vk.Enumerate.instanceLayerProperties pLayerCount pAvailableLayers
		layerNames <- (BSC.takeWhile (/= '\NUL')
				. Vk.Enumerate.layerPropertiesLayerName <$>)
			<$> peekArray layerCount pAvailableLayers
		print layerNames
		pure $ "VK_LAYER_KHRONOS_validation" `elem` layerNames

getRequiredExtensions :: ContT r IO (Word32, Ptr CString)
getRequiredExtensions = do
	glfwExtensions <- lift $ GlfwB.getRequiredInstanceExtensions
	extDebugUtilsExtensionName <- ContT $ withCString "VK_EXT_debug_utils"
	let	extensions = extDebugUtilsExtensionName : glfwExtensions
	pExtensions <- cStringListToCStringArray extensions
	let	extensionCount = fromIntegral $ length extensions
	pure (extensionCount, pExtensions)

setupDebugMessenger :: IO ()
setupDebugMessenger = ($ pure) $ runContT do
	Vk.Ext.DU.Msngr.CreateInfo_ fCreateInfo <-
		lift populateDebugMessengerCreateInfo
	pCreateInfo <- ContT $ withForeignPtr fCreateInfo
	pMessenger <- ContT alloca
	lift do	ist <- readIORef instance_
		r <- Vk.Ext.DU.Msngr.create ist pCreateInfo NullPtr pMessenger
		when (r /= success) $ error "failed to set up debug messenger!"
		writeIORef debugMessenger =<< peek pMessenger

populateDebugMessengerCreateInfo :: IO Vk.Ext.DU.Msngr.CreateInfo
populateDebugMessengerCreateInfo = do
	pDebugCallback <- Vk.Ext.DU.Msngr.wrapCallback debugCallback
	pure Vk.Ext.DU.Msngr.CreateInfo {
		Vk.Ext.DU.Msngr.createInfoSType = (),
		Vk.Ext.DU.Msngr.createInfoPNext = NullPtr,
		Vk.Ext.DU.Msngr.createInfoFlags = 0,
		Vk.Ext.DU.Msngr.createInfoMessageSeverity =
			Vk.Ext.DU.Msngr.severityVerboseBit .|.
			Vk.Ext.DU.Msngr.severityWarningBit .|.
			Vk.Ext.DU.Msngr.severityErrorBit,
		Vk.Ext.DU.Msngr.createInfoMessageType =
			Vk.Ext.DU.Msngr.typeGeneralBit .|.
			Vk.Ext.DU.Msngr.typeValidationBit .|.
			Vk.Ext.DU.Msngr.typePerformanceBit,
		Vk.Ext.DU.Msngr.createInfoPfnUserCallback =
			pDebugCallback,
		Vk.Ext.DU.Msngr.createInfoPUserData = NullPtr }

debugCallback :: Vk.Ext.DU.Msngr.FnCallback
debugCallback _messageSeverity _messageType pCallbackData _pUserData = do
	callbackData <-
		Vk.Ext.DU.Msngr.copyCallbackData pCallbackData
	message <- peekCString
		$ Vk.Ext.DU.Msngr.callbackDataPMessage callbackData
	putStrLn $ "validation layer: " ++ message
	pure vkFalse

createSurface :: GlfwB.Window -> IO ()
createSurface win = ($ pure) $ runContT do
	ist <- lift $ readIORef instance_
	psrfc <- ContT alloca
	lift do	r <- GlfwB.createWindowSurface ist win NullPtr psrfc
		when (r /= success) $ error "failed to create window surface!"
		writeIORef surface =<< peek psrfc

pickPhysicalDevice :: IO ()
pickPhysicalDevice = ($ pure) $ runContT do
	ist <- lift $ readIORef instance_
	pDeviceCount <- ContT alloca
	(fromIntegral -> deviceCount) <- lift do
		_ <- Vk.PhysicalDevice.enumerate ist pDeviceCount NullPtr
		peek pDeviceCount
	pDevices <- ContT $ allocaArray deviceCount
	lift do	_ <- Vk.PhysicalDevice.enumerate ist pDeviceCount pDevices
		devices <- peekArray deviceCount pDevices
		findM isDeviceSuitable devices >>= \case
			Just pd -> writeIORef physicalDevice pd
			Nothing -> error "no matched physical devices"

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = pure Nothing
findM p (x : xs) = bool (findM p xs) (pure $ Just x) =<< p x

isDeviceSuitable :: Vk.PhysicalDevice.PhysicalDevice -> IO Bool
isDeviceSuitable dvc = ($ pure) $ runContT do
	pDeviceProperties <- ContT alloca
	lift do	Vk.PhysicalDevice.getProperties dvc pDeviceProperties
		print =<< peek pDeviceProperties
	pDeviceFeatures <- ContT alloca
	lift do	Vk.PhysicalDevice.getFeatures dvc pDeviceFeatures
		print =<< peek pDeviceFeatures
	indices <- lift $ findQueueFamilies dvc
	extensionSupported <- lift $ checkDeviceExtensionSupport dvc
	swapChainAdequate <- lift $ if extensionSupported
		then do	swapChainSupport <- querySwapChainSupport dvc
			pure $ not (null $ swapChainSupportDetailsFormats
					swapChainSupport) &&
				not (null $ swapChainSupportDetailsPresentModes
					swapChainSupport)
		else pure False
	pure $ isComplete indices && extensionSupported && swapChainAdequate

checkDeviceExtensionSupport :: Vk.PhysicalDevice.PhysicalDevice -> IO Bool
checkDeviceExtensionSupport dvc = ($ pure) $ runContT do
	pExtensionCount <- ContT alloca
	(fromIntegral -> extensionCount) <- lift do
		_ <- Vk.PhysicalDevice.enumerateExtensionProperties
			dvc NullPtr pExtensionCount NullPtr
		peek pExtensionCount
	pAvailableExtensions <- ContT $ allocaArray extensionCount
	lift do	_ <- Vk.PhysicalDevice.enumerateExtensionProperties
			dvc NullPtr pExtensionCount pAvailableExtensions
		es <- map (BSC.takeWhile (/= '\NUL') . Vk.Enumerate.extensionPropertiesExtensionName)
			<$> peekArray extensionCount pAvailableExtensions
		print es
		pure $ elem "VK_KHR_swapchain" es

data QueueFamilyIndices = QueueFamilyIndices {
	graphicsFamily :: Maybe Word32,
	presentFamily :: Maybe Word32 }
	deriving Show

isComplete :: QueueFamilyIndices -> Bool
isComplete QueueFamilyIndices {
	graphicsFamily = gf, presentFamily = pf } = isJust gf && isJust pf

findQueueFamilies :: Vk.PhysicalDevice.PhysicalDevice -> IO QueueFamilyIndices
findQueueFamilies dvc = ($ pure) $ runContT do
	let	indices = QueueFamilyIndices {
			graphicsFamily = Nothing,
			presentFamily = Nothing }
	pQueueFamilyCount <- ContT alloca
	(fromIntegral -> queueFamilyCount) <- lift do
		Vk.PhysicalDevice.getQueueFamilyProperties
			dvc pQueueFamilyCount NullPtr
		peek pQueueFamilyCount
	pQueueFamilies <- ContT $ allocaArray queueFamilyCount
	lift do	Vk.PhysicalDevice.getQueueFamilyProperties
			dvc pQueueFamilyCount pQueueFamilies
		props <- peekArray queueFamilyCount pQueueFamilies
		pfi <- getPresentFamilyIndex dvc (fromIntegral $ length props) 0
		pure indices {
			graphicsFamily = fromIntegral
				<$> findIndex checkGraphicsBit props,
			presentFamily = pfi }

getPresentFamilyIndex :: Vk.PhysicalDevice.PhysicalDevice ->
	Word32 -> Word32 -> IO (Maybe Word32)
getPresentFamilyIndex pd n i
	| i >= n = pure Nothing
	| otherwise = ($ pure) $ runContT do
		pPresentSupport <- ContT alloca
		lift do	sfc <- readIORef surface
			_ <- Vk.PhysicalDevice.getSurfaceSupport pd i sfc pPresentSupport
			presentSupport <- peek pPresentSupport
			if presentSupport == vkTrue
			then pure $ Just i
			else getPresentFamilyIndex pd n (i + 1)

checkGraphicsBit :: Vk.Queue.Family.Properties -> Bool
checkGraphicsBit prop =
	Vk.Queue.Family.propertiesQueueFlags prop .&. queueGraphicsBit /= 0

data SwapChainSupportDetails = SwapChainSupportDetails {
	swapChainSupportDetailsCapabilities :: Vk.Khr.Sfc.Capabilities,
	swapChainSupportDetailsFormats :: [Vk.Khr.Sfc.Format],
	swapChainSupportDetailsPresentModes :: [Vk.Khr.Present.Mode] }
	deriving Show

querySwapChainSupport ::
	Vk.PhysicalDevice.PhysicalDevice -> IO SwapChainSupportDetails
querySwapChainSupport dvc = ($ pure) $ runContT do
	sfc <- lift $ readIORef surface
	pCapabilities <- ContT alloca
	cps <- lift do
		_ <- Vk.Khr.Sfc.PhysicalDevice.getCapabilities
			dvc sfc pCapabilities
		peek pCapabilities
	pFormatCount <- ContT alloca
	(fromIntegral -> formatCount) <- lift do
		_ <- Vk.Khr.Sfc.PhysicalDevice.getFormats
			dvc sfc pFormatCount NullPtr
		peek pFormatCount
	pFormats <- ContT $ allocaArray formatCount
	fmts <- lift do
		_ <- Vk.Khr.Sfc.PhysicalDevice.getFormats
			dvc sfc pFormatCount pFormats
		peekArray formatCount pFormats
	pPresentModeCount <- ContT alloca
	(fromIntegral -> presentModeCount) <- lift do
		_ <- Vk.Khr.Sfc.PhysicalDevice.getPresentModes
			dvc sfc pPresentModeCount NullPtr
		peek pPresentModeCount
	pPresentModes <- ContT $ allocaArray presentModeCount
	presentModes <- lift do
		_ <- Vk.Khr.Sfc.PhysicalDevice.getPresentModes
			dvc sfc pPresentModeCount pPresentModes
		peekArray presentModeCount pPresentModes
	lift $ print presentModeCount
	pure SwapChainSupportDetails {
		swapChainSupportDetailsCapabilities = cps,
		swapChainSupportDetailsFormats = fmts,
		swapChainSupportDetailsPresentModes = presentModes
		}

createLogicalDevice :: IO ()
createLogicalDevice = ($ pure) $ runContT do
	pd <- lift $ readIORef physicalDevice
	indices <- lift $ findQueueFamilies pd
	lift $ print indices
	pQueuePriority <- ContT alloca
	lift $ poke pQueuePriority 1
	let	Vk.Device.Queue.CreateInfo_ fQueueCreateInfo =
			Vk.Device.Queue.CreateInfo {
				Vk.Device.Queue.createInfoSType = (),
				Vk.Device.Queue.createInfoPNext = NullPtr,
				Vk.Device.Queue.createInfoFlags = 0,
				Vk.Device.Queue.createInfoQueueFamilyIndex =
					fromJust $ graphicsFamily indices,
				Vk.Device.Queue.createInfoQueueCount = 1,
				Vk.Device.Queue.createInfoPQueuePriorities =
					pQueuePriority }
	pQueueCreateInfo <- ContT $ withForeignPtr fQueueCreateInfo
	Vk.PhysicalDevice.Features_ fDeviceFeatures <-
		lift $ Vk.PhysicalDevice.getCleardFeatures
	pDeviceFeatures <- ContT $ withForeignPtr fDeviceFeatures
	pValidationLayer <- lift $ newCString "VK_LAYER_KHRONOS_validation"
	pValidationLayers <- ContT $ allocaArray 1
	lift $ pokeArray pValidationLayers [pValidationLayer]
	pSwapchainExtention <- lift $ newCString "VK_KHR_swapchain"
	pSwapchainExtentions <- ContT $ allocaArray 1
	lift $ pokeArray pSwapchainExtentions [pSwapchainExtention]
	let	Vk.Device.CreateInfo_ fCreateInfo = Vk.Device.CreateInfo {
			Vk.Device.createInfoSType = (),
			Vk.Device.createInfoPNext = NullPtr,
			Vk.Device.createInfoFlags = 0,
			Vk.Device.createInfoQueueCreateInfoCount = 1,
			Vk.Device.createInfoPQueueCreateInfos =
				pQueueCreateInfo,
			Vk.Device.createInfoPEnabledFeatures = pDeviceFeatures,
			Vk.Device.createInfoEnabledExtensionCount = 1,
			Vk.Device.createInfoPpEnabledExtensionNames =
				pSwapchainExtentions,
			Vk.Device.createInfoEnabledLayerCount = 1,
			Vk.Device.createInfoPpEnabledLayerNames =
				pValidationLayers }
	pCreateInfo <- ContT $ withForeignPtr fCreateInfo
	pDevice <- ContT alloca
	dvc <- lift do
		r <- Vk.Device.create pd pCreateInfo NullPtr pDevice
		when (r /= success) $ error "failed to create logical device!"
		dvc' <- peek pDevice
		writeIORef device dvc'
		pure dvc'
	pGraphicsQueue <- ContT alloca
	pPresentQueue <- ContT alloca
	lift do	Vk.Device.getQueue
			dvc (fromJust $ graphicsFamily indices) 0 pGraphicsQueue
		Vk.Device.getQueue
			dvc (fromJust $ graphicsFamily indices) 0 pPresentQueue
		writeIORef graphicsQueue =<< peek pGraphicsQueue
		writeIORef presentQueue =<< peek pPresentQueue

createSwapChain :: IO ()
createSwapChain = ($ pure) $ runContT do
	dvc <- lift $ readIORef device
	Vk.Khr.Sc.CreateInfo_ fCreateInfo <- lift do
		pd <- readIORef physicalDevice
		swapChainSupport <- querySwapChainSupport pd
		sfc <- readIORef surface
		phd <- readIORef physicalDevice
		indices <- findQueueFamilies phd
		print indices
		let	cap = swapChainSupportDetailsCapabilities
				swapChainSupport
			surfaceFormat = chooseSwapSurfaceFormat
				$ swapChainSupportDetailsFormats
					swapChainSupport
			presentMode = chooseSwapPresentMode
				$ swapChainSupportDetailsPresentModes
					swapChainSupport
			extent = chooseSwapExtent cap
			minImageCount = Vk.Khr.Sfc.capabilitiesMinImageCount cap
			maxImageCount = Vk.Khr.Sfc.capabilitiesMaxImageCount cap
			imageCount = if maxImageCount > 0
				then min (minImageCount + 1) maxImageCount
				else minImageCount + 1
			createInfo' = Vk.Khr.Sc.CreateInfo {
				Vk.Khr.Sc.createInfoSType = (),
				Vk.Khr.Sc.createInfoPNext = NullPtr,
				Vk.Khr.Sc.createInfoFlags = 0,
				Vk.Khr.Sc.createInfoSurface = sfc,
				Vk.Khr.Sc.createInfoMinImageCount = imageCount,
				Vk.Khr.Sc.createInfoImageFormat =
					Vk.Khr.Sfc.formatFormat surfaceFormat,
				Vk.Khr.Sc.createInfoImageColorSpace =
					Vk.Khr.Sfc.formatColorSpace
						surfaceFormat,
				Vk.Khr.Sc.createInfoImageExtent = extent,
				Vk.Khr.Sc.createInfoImageArrayLayers = 1,
				Vk.Khr.Sc.createInfoImageUsage =
					Vk.imageUsageColorAttachmentBit,
				Vk.Khr.Sc.createInfoImageSharingMode =
					Vk.sharingModeExclusive,
				Vk.Khr.Sc.createInfoQueueFamilyIndexCount = 0,
				Vk.Khr.Sc.createInfoPQueueFamilyIndices =
					NullPtr,
				Vk.Khr.Sc.createInfoPreTransform =
					Vk.Khr.Sfc.capabilitiesCurrentTransform
						cap,
				Vk.Khr.Sc.createInfoCompositeAlpha =
					Vk.Khr.compositeAlphaOpaqueBit,
				Vk.Khr.Sc.createInfoPresentMode = presentMode,
				Vk.Khr.Sc.createInfoClipped = vkTrue,
				Vk.Khr.Sc.createInfoOldSwapchain = NullHandle }
		print surfaceFormat
		print presentMode
		print (Vk.Format.b8g8r8a8Srgb, Vk.Khr.ColorSpace.srgbNonlinear)
		print Vk.Khr.Present.modeMailbox
		print extent
		print imageCount
		print maxImageCount
		writeIORef swapChainImageFormat
			$ Vk.Khr.Sfc.formatFormat surfaceFormat
		writeIORef swapChainExtent extent
		pure createInfo'
	pCreateInfo <- ContT $ withForeignPtr fCreateInfo
	pSwapChain <- ContT alloca
	sc <- lift do
		r <- Vk.Khr.Sc.create dvc pCreateInfo NullPtr pSwapChain
		when (r /= success) $ error "failed to create swap chain!"
		sc <- peek pSwapChain
		sc <$ writeIORef swapChain sc
	pImageCount <- ContT alloca
	(fromIntegral -> imageCount) <- lift do
		_ <- Vk.Khr.Sc.getImages dvc sc pImageCount NullPtr
		peek pImageCount
	pSwapChainImages <- ContT $ allocaArray imageCount
	lift do	_ <- Vk.Khr.Sc.getImages dvc sc pImageCount pSwapChainImages
		writeIORef swapChainImages
			=<< peekArray imageCount pSwapChainImages

chooseSwapSurfaceFormat :: [Vk.Khr.Sfc.Format] -> Vk.Khr.Sfc.Format
chooseSwapSurfaceFormat availableFormats = fromMaybe (head availableFormats)
	$ find (\f ->
		Vk.Khr.Sfc.formatFormat f == Vk.Format.b8g8r8a8Srgb &&
		Vk.Khr.Sfc.formatColorSpace f ==
			Vk.Khr.ColorSpace.srgbNonlinear) availableFormats

chooseSwapPresentMode :: [Vk.Khr.Present.Mode] -> Vk.Khr.Present.Mode
chooseSwapPresentMode availablePresentModes =
	if Vk.Khr.Present.modeMailbox `elem` availablePresentModes
		then Vk.Khr.Present.modeMailbox
		else Vk.Khr.Present.modeFifo

chooseSwapExtent :: Vk.Khr.Sfc.Capabilities -> Vk.Extent2d
chooseSwapExtent capabilities =
	if Vk.extent2dWidth ce /= uint32Max
		then ce
		else error "Ah!"
	where ce = Vk.Khr.Sfc.capabilitiesCurrentExtent capabilities

createImageViews :: IO ()
createImageViews = do
	scis <- readIORef swapChainImages
	ivs <- createImageView1 `mapM` scis
	writeIORef swapChainImageViews ivs

createImageView1 :: Vk.Img.Image -> IO Vk.ImageView.ImageView
createImageView1 img = ($ pure) $ runContT do
	fmt <- lift $ readIORef swapChainImageFormat
	let	Vk.ImageView.CreateInfo_ fCreateInfo = Vk.ImageView.CreateInfo {
			Vk.ImageView.createInfoSType = (),
			Vk.ImageView.createInfoPNext = NullPtr,
			Vk.ImageView.createInfoFlags = 0,
			Vk.ImageView.createInfoImage = img,
			Vk.ImageView.createInfoViewType = Vk.ImageView.type2d,
			Vk.ImageView.createInfoFormat = fmt,
			Vk.ImageView.createInfoComponents =
				Vk.Component.Mapping {
					Vk.Component.mappingR =
						Vk.Component.swizzleIdentity,
					Vk.Component.mappingG =
						Vk.Component.swizzleIdentity,
					Vk.Component.mappingB =
						Vk.Component.swizzleIdentity,
					Vk.Component.mappingA =
						Vk.Component.swizzleIdentity },
			Vk.ImageView.createInfoSubresourceRange =
				Vk.Img.SubresourceRange {
					Vk.Img.subresourceRangeAspectMask =
						Vk.Img.aspectColorBit,
					Vk.Img.subresourceRangeBaseMipLevel = 0,
					Vk.Img.subresourceRangeLevelCount = 1,
					Vk.Img.subresourceRangeBaseArrayLayer =
						0,
					Vk.Img.subresourceRangeLayerCount = 1 } }
	dvc <- lift $ readIORef device
	pCreateInfo <- ContT $ withForeignPtr fCreateInfo
	pView <- ContT alloca
	lift do	r <- Vk.ImageView.create dvc pCreateInfo NullPtr pView
		when (r /= success) $ error "failed to create image views!"
		peek pView

createRenderPass :: IO ()
createRenderPass = do
	scif <- readIORef swapChainImageFormat
	let	colorAttachment = Vk.Attachment.Description {
			Vk.Attachment.descriptionFlags = 0,
			Vk.Attachment.descriptionFormat = scif,
			Vk.Attachment.descriptionSamples = Vk.Sample.count1Bit,
			Vk.Attachment.descriptionLoadOp =
				Vk.Attachment.loadOpClear,
			Vk.Attachment.descriptionStoreOp =
				Vk.Attachment.storeOpStore,
			Vk.Attachment.descriptionStencilLoadOp =
				Vk.Attachment.loadOpDontCare,
			Vk.Attachment.descriptionStencilStoreOp =
				Vk.Attachment.storeOpDontCare,
			Vk.Attachment.descriptionInitialLayout =
				Vk.Img.layoutUndefined,
			Vk.Attachment.descriptionFinalLayout =
				Vk.Img.layoutPresentSrcKhr }
	pure ()

createGraphicsPipeline :: IO ()
createGraphicsPipeline = ($ pure) $ runContT do
	dvc <- lift $ readIORef device
	vertShaderCode <- lift $ readFile "shaders/vert.spv"
	fragShaderCode <- lift $ readFile "shaders/frag.spv"
	vertShaderModule <- lift $ createShaderModule vertShaderCode
	fragShaderModule <- lift $ createShaderModule fragShaderCode
	cnm <- lift $ newCString "main"
	sce <- lift $ readIORef swapChainExtent
	let	vertShaderStageInfo = Vk.Ppl.ShaderStage.CreateInfo {
			Vk.Ppl.ShaderStage.createInfoSType = (),
			Vk.Ppl.ShaderStage.createInfoPNext = NullPtr,
			Vk.Ppl.ShaderStage.createInfoFlags = 0,
			Vk.Ppl.ShaderStage.createInfoStage =
				Vk.Ppl.ShaderStage.vertexBit,
			Vk.Ppl.ShaderStage.createInfoModule = vertShaderModule,
			Vk.Ppl.ShaderStage.createInfoPName = cnm,
			Vk.Ppl.ShaderStage.createInfoPSpecializationInfo =
				NullPtr }
		fragShaderStageInfo = Vk.Ppl.ShaderStage.CreateInfo {
			Vk.Ppl.ShaderStage.createInfoSType = (),
			Vk.Ppl.ShaderStage.createInfoPNext = NullPtr,
			Vk.Ppl.ShaderStage.createInfoFlags = 0,
			Vk.Ppl.ShaderStage.createInfoStage =
				Vk.Ppl.ShaderStage.fragmentBit,
			Vk.Ppl.ShaderStage.createInfoModule = fragShaderModule,
			Vk.Ppl.ShaderStage.createInfoPName = cnm,
			Vk.Ppl.ShaderStage.createInfoPSpecializationInfo =
				NullPtr }
		shaderStages = [vertShaderStageInfo, fragShaderStageInfo]
		vertexInputInfo = Vk.Ppl.VISt.CreateInfo {
			Vk.Ppl.VISt.createInfoSType = (),
			Vk.Ppl.VISt.createInfoPNext = NullPtr,
			Vk.Ppl.VISt.createInfoFlags = 0,
			Vk.Ppl.VISt.createInfoVertexBindingDescriptionCount =
				0,
			Vk.Ppl.VISt.createInfoPVertexBindingDescriptions =
				NullPtr,
			Vk.Ppl.VISt.createInfoVertexAttributeDescriptionCount =
				0,
			Vk.Ppl.VISt.createInfoPVertexAttributeDescriptions =
				NullPtr }
		inputAssembly = Vk.Ppl.IASt.CreateInfo {
			Vk.Ppl.IASt.createInfoSType = (),
			Vk.Ppl.IASt.createInfoPNext = NullPtr,
			Vk.Ppl.IASt.createInfoFlags = 0,
			Vk.Ppl.IASt.createInfoTopology = Vk.PrmTplgy.triangleList,
			Vk.Ppl.IASt.createInfoPrimitiveRestartEnable = vkFalse }
		Vk.Viewport_ fViewport = Vk.Viewport {
			Vk.viewportX = 0, Vk.viewportY = 0,
			Vk.viewportWidth = fromIntegral $ Vk.extent2dWidth sce,
			Vk.viewportHeight = fromIntegral $ Vk.extent2dHeight sce,
			Vk.viewportMinDepth = 0, Vk.viewportMaxDepth = 1 }
		Vk.Rect2d_ fScissor = Vk.Rect2d {
			Vk.rect2dOffset = Vk.Offset2d {
				Vk.offset2dX = 0, Vk.offset2dY = 0 },
			Vk.rect2dExtent = sce }
	pViewport <- ContT $ withForeignPtr fViewport
	pScissor <- ContT $ withForeignPtr fScissor
	let	viewportState = Vk.Ppl.VPSt.CreateInfo {
			Vk.Ppl.VPSt.createInfoSType = (),
			Vk.Ppl.VPSt.createInfoPNext = NullPtr,
			Vk.Ppl.VPSt.createInfoFlags = 0,
			Vk.Ppl.VPSt.createInfoViewportCount = 1,
			Vk.Ppl.VPSt.createInfoPViewports = pViewport,
			Vk.Ppl.VPSt.createInfoScissorCount = 1,
			Vk.Ppl.VPSt.createInfoPScissors = pScissor }
		rasterizer = Vk.Ppl.RstSt.CreateInfo {
			Vk.Ppl.RstSt.createInfoSType = (),
			Vk.Ppl.RstSt.createInfoPNext = NullPtr,
			Vk.Ppl.RstSt.createInfoFlags = 0,
			Vk.Ppl.RstSt.createInfoDepthClampEnable = vkFalse,
			Vk.Ppl.RstSt.createInfoRasterizerDiscardEnable =
				vkFalse,
			Vk.Ppl.RstSt.createInfoPolygonMode =
				Vk.Polygon.modeFill,
			Vk.Ppl.RstSt.createInfoCullMode = Vk.Cull.modeBackBit,
			Vk.Ppl.RstSt.createInfoFrontFace =
				Vk.FrontFace.clockwise,
			Vk.Ppl.RstSt.createInfoDepthBiasEnable = vkFalse,
			Vk.Ppl.RstSt.createInfoDepthBiasConstantFactor = 0,
			Vk.Ppl.RstSt.createInfoDepthBiasClamp = 0,
			Vk.Ppl.RstSt.createInfoDepthBiasSlopeFactor = 0,
			Vk.Ppl.RstSt.createInfoLineWidth = 1 }
		multisampling = Vk.Ppl.MSSt.CreateInfo {
			Vk.Ppl.MSSt.createInfoSType = (),
			Vk.Ppl.MSSt.createInfoPNext = NullPtr,
			Vk.Ppl.MSSt.createInfoFlags = 0,
			Vk.Ppl.MSSt.createInfoRasterizationSamples =
				Vk.Sample.count1Bit,
			Vk.Ppl.MSSt.createInfoSampleShadingEnable = vkFalse,
			Vk.Ppl.MSSt.createInfoMinSampleShading = 1,
			Vk.Ppl.MSSt.createInfoPSampleMask = NullPtr,
			Vk.Ppl.MSSt.createInfoAlphaToCoverageEnable = vkFalse,
			Vk.Ppl.MSSt.createInfoAlphaToOneEnable = vkFalse }
		Vk.Ppl.CBASt.State_ fColorBlendAttachment = Vk.Ppl.CBASt.State {
			Vk.Ppl.CBASt.stateBlendEnable = vkFalse,
			Vk.Ppl.CBASt.stateSrcColorBlendFactor =
				Vk.Blend.factorOne,
			Vk.Ppl.CBASt.stateDstColorBlendFactor =
				Vk.Blend.factorZero,
			Vk.Ppl.CBASt.stateColorBlendOp = Vk.Blend.opAdd,
			Vk.Ppl.CBASt.stateSrcAlphaBlendFactor =
				Vk.Blend.factorOne,
			Vk.Ppl.CBASt.stateDstAlphaBlendFactor =
				Vk.Blend.factorZero,
			Vk.Ppl.CBASt.stateAlphaBlendOp = Vk.Blend.opAdd,
			Vk.Ppl.CBASt.stateColorWriteMask =
				Vk.CC.rBit .|. Vk.CC.gBit .|. Vk.CC.bBit .|.
				Vk.CC.aBit }
	pColorBlendAttachment <- ContT $ withForeignPtr fColorBlendAttachment
	let	colorBlending = Vk.Ppl.CBSt.CreateInfo {
			Vk.Ppl.CBSt.createInfoSType = (),
			Vk.Ppl.CBSt.createInfoPNext = NullPtr,
			Vk.Ppl.CBSt.createInfoFlags = 0,
			Vk.Ppl.CBSt.createInfoLogicOpEnable = vkFalse,
			Vk.Ppl.CBSt.createInfoLogicOp = Vk.Logic.opCopy,
			Vk.Ppl.CBSt.createInfoAttachmentCount = 1,
			Vk.Ppl.CBSt.createInfoPAttachments =
				pColorBlendAttachment,
			Vk.Ppl.CBSt.createInfoBlendConstants = [0, 0, 0, 0] }
		Vk.Ppl.Lyt.CreateInfo_ fPipelineLayoutInfo =
			Vk.Ppl.Lyt.CreateInfo {
				Vk.Ppl.Lyt.createInfoSType = (),
				Vk.Ppl.Lyt.createInfoPNext = NullPtr,
				Vk.Ppl.Lyt.createInfoFlags = 0,
				Vk.Ppl.Lyt.createInfoSetLayoutCount = 0,
				Vk.Ppl.Lyt.createInfoPSetLayouts = NullPtr,
				Vk.Ppl.Lyt.createInfoPushConstantRangeCount = 0,
				Vk.Ppl.Lyt.createInfoPPushConstantRanges =
					NullPtr }
	pPipelineLayoutInfo <- ContT $ withForeignPtr fPipelineLayoutInfo
	pPipelineLayout <- ContT alloca
	lift do r <- Vk.Ppl.Lyt.create
			dvc pPipelineLayoutInfo NullPtr pPipelineLayout
		when (r /= success) $ error "failed to creaet pipeline layout!"
		writeIORef pipelineLayout =<< peek pPipelineLayout
		Vk.Shader.Module.destroy dvc fragShaderModule NullPtr
		Vk.Shader.Module.destroy dvc vertShaderModule NullPtr

createShaderModule :: (Ptr Word32, Integer) -> IO Vk.Shader.Module.Module
createShaderModule (p, n) = ($ pure) $ runContT do
	dvc <- lift $ readIORef device
	let	Vk.Shader.Module.CreateInfo_ fCreateInfo =
			Vk.Shader.Module.CreateInfo {
				Vk.Shader.Module.createInfoSType = (),
				Vk.Shader.Module.createInfoPNext = NullPtr,
				Vk.Shader.Module.createInfoFlags = 0,
				Vk.Shader.Module.createInfoCodeSize =
					fromIntegral n,
				Vk.Shader.Module.createInfoPCode = p }
	pCreateInfo <- ContT $ withForeignPtr fCreateInfo
	pShaderModule <- ContT alloca
	lift do	r <- Vk.Shader.Module.create dvc pCreateInfo NullPtr pShaderModule
		when (r /= success) $ error "failed to create shader module!"
		peek pShaderModule

readFile :: FilePath -> IO (Ptr Word32, Integer)
readFile fp = do
	h <- openFile fp ReadMode
	hSeek h SeekFromEnd 0
	fileSize <- hTell h
	putStrLn $ fp ++ ": " ++ show fileSize
	pbuff <- mallocArray ((fromIntegral fileSize - 1) `div` 4 + 1)
	hSeek h AbsoluteSeek 0
	print =<< hGetBuf h pbuff (fromIntegral fileSize)
	pure (pbuff, fileSize)

mainLoop :: GlfwB.Window -> IO ()
mainLoop win = do
	fix \loop -> bool (pure ()) loop =<< do
		GlfwB.pollEvents
		not <$> GlfwB.windowShouldClose win

cleanup :: GlfwB.Window -> IO ()
cleanup win = do
	dvc <- readIORef device
	pl <- readIORef pipelineLayout
	Vk.Ppl.Lyt.destroy dvc pl NullPtr
	ivs <- readIORef swapChainImageViews
	(\iv -> Vk.ImageView.destroy dvc iv NullPtr) `mapM_` ivs
	(\sc -> Vk.Khr.Sc.destroy dvc sc NullPtr) =<< readIORef swapChain
	Vk.Device.destroy dvc NullPtr
	ist <- readIORef instance_
	(\sfc -> Vk.Khr.Sfc.destroy ist sfc NullPtr) =<< readIORef surface
	(\dm -> Vk.Ext.DU.Msngr.destroy ist dm NullPtr)
		=<< readIORef debugMessenger
	Vk.Instance.destroy ist NullPtr
	GlfwB.destroyWindow win
	GlfwB.terminate
