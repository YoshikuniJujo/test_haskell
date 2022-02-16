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

import qualified Vulkan.Pipeline.VertexInputState as Vk.Ppl.VI
import qualified Vulkan.Pipeline.InputAssemblyState as Vk.Ppl.IA
import qualified Vulkan.PrimitiveTopology as Vk.PrmTplgy
import qualified Vulkan.Pipeline.ViewportState as Vk.Ppl.VP
import qualified Vulkan.Pipeline.RasterizationState as Vk.Ppl.RstSt
import qualified Vulkan.Polygon as Vk.Polygon
import qualified Vulkan.Cull as Vk.Cull
import qualified Vulkan.FrontFace as Vk.FrontFace
import qualified Vulkan.Pipeline.MultisampleState as Vk.Ppl.MS
import qualified Vulkan.Sample as Vk.Sample
import qualified Vulkan.Pipeline.ColorBlendAttachmentState as Vk.Ppl.CBASt
import qualified Vulkan.Blend as Vk.Blend
import qualified Vulkan.ColorComponent as Vk.CC
import qualified Vulkan.Pipeline.ColorBlendState as Vk.Ppl.CB
import qualified Vulkan.Logic as Vk.Logic
import qualified Vulkan.Pipeline.Layout as Vk.Ppl.Lyt

import qualified Vulkan.Attachment as Vk.Att
import qualified Vulkan.Subpass as Vk.Subpass
import qualified Vulkan.Pipeline as Vk.Ppl
import qualified Vulkan.RenderPass as Vk.RndrPss

import qualified Vulkan.Framebuffer as Vk.Fb
import qualified Vulkan.CommandPool as Vk.CP
import qualified Vulkan.CommandBuffer as Vk.CB
import qualified Vulkan.Command as Vk.Cmd
import qualified Vulkan.Semaphore as Vk.Smp
import qualified Vulkan.Access as Vk.Access

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

graphicsQueue :: IORef Vk.Queue
graphicsQueue = unsafePerformIO $ newIORef NullPtr

surface :: IORef Vk.Khr.Sfc.Surface
surface = unsafePerformIO $ newIORef NullPtr

presentQueue :: IORef Vk.Queue
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

renderPass :: IORef Vk.RndrPss.RenderPass
renderPass = unsafePerformIO $ newIORef NullPtr

pipelineLayout :: IORef Vk.Ppl.Lyt.Layout
pipelineLayout = unsafePerformIO $ newIORef NullPtr

graphicsPipeline :: IORef Vk.Ppl.Pipeline
graphicsPipeline = unsafePerformIO $ newIORef NullPtr

swapChainFramebuffers :: IORef [Framebuffer]
swapChainFramebuffers = unsafePerformIO $ newIORef []

commandPool :: IORef Vk.CP.CommandPool
commandPool = unsafePerformIO $ newIORef NullPtr

commandBuffers :: IORef [Vk.CommandBuffer]
commandBuffers = unsafePerformIO $ newIORef []

imageAvailableSemaphore, renderFinishedSemaphore :: IORef Vk.Semaphore
(imageAvailableSemaphore, renderFinishedSemaphore) = unsafePerformIO
	$ (,) <$> newIORef NullPtr <*> newIORef NullPtr
{-
imageAvailableSemaphore = unsafePerformIO $ newIORef NullPtr
renderFinishedSemaphore = unsafePerformIO $ newIORef NullPtr
-}

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
	createFramebuffers
	createCommandPool
	createCommandBuffers
	createSemaphores

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
createRenderPass = ($ pure) $ runContT do
	scif <- lift $ readIORef swapChainImageFormat
	let	Vk.Att.Description_ fColorAttachment = Vk.Att.Description {
			Vk.Att.descriptionFlags = 0,
			Vk.Att.descriptionFormat = scif,
			Vk.Att.descriptionSamples = Vk.Sample.count1Bit,
			Vk.Att.descriptionLoadOp = Vk.Att.loadOpClear,
			Vk.Att.descriptionStoreOp = Vk.Att.storeOpStore,
			Vk.Att.descriptionStencilLoadOp = Vk.Att.loadOpDontCare,
			Vk.Att.descriptionStencilStoreOp =
				Vk.Att.storeOpDontCare,
			Vk.Att.descriptionInitialLayout =
				Vk.Img.layoutUndefined,
			Vk.Att.descriptionFinalLayout =
				Vk.Img.layoutPresentSrcKhr }
		Vk.Att.Reference_ fColorAttachmentRef =
			Vk.Att.Reference {
				Vk.Att.referenceAttachment = 0,
				Vk.Att.referenceLayout =
					Vk.Img.layoutColorAttachmentOptimal }
	pColorAttachment <- ContT $ withForeignPtr fColorAttachment
	pColorAttachmentRef <- ContT $ withForeignPtr fColorAttachmentRef
	let	Vk.Subpass.Description_ fSubpass = Vk.Subpass.Description {
			Vk.Subpass.descriptionFlags = 0,
			Vk.Subpass.descriptionPipelineBindPoint =
				Vk.Ppl.bindPointGraphics,
			Vk.Subpass.descriptionInputAttachmentCount = 0,
			Vk.Subpass.descriptionPInputAttachments = NullPtr,
			Vk.Subpass.descriptionColorAttachmentCount = 1,
			Vk.Subpass.descriptionPColorAttachments =
				pColorAttachmentRef,
			Vk.Subpass.descriptionPResolveAttachments = NullPtr,
			Vk.Subpass.descriptionPDepthStencilAttachment = NullPtr,
			Vk.Subpass.descriptionPreserveAttachmentCount = 0,
			Vk.Subpass.descriptionPPreserveAttachments = NullPtr }
	pSubpass <- ContT $ withForeignPtr fSubpass
	let	Vk.Subpass.Dependency_ fDependency = Vk.Subpass.Dependency {
			Vk.Subpass.dependencySrcSubpass = Vk.Subpass.external,
			Vk.Subpass.dependencyDstSubpass = 0,
			Vk.Subpass.dependencySrcStageMask =
				Vk.Ppl.stageColorAttachmentOutputBit,
			Vk.Subpass.dependencySrcAccessMask = 0,
			Vk.Subpass.dependencyDstStageMask =
				Vk.Ppl.stageColorAttachmentOutputBit,
			Vk.Subpass.dependencyDstAccessMask =
				Vk.Access.colorAttachmentWriteBit,
			Vk.Subpass.dependencyDependencyFlags = 0 }
	pDependency <- ContT $ withForeignPtr fDependency
	let	Vk.RndrPss.CreateInfo_ fRenderPassInfo = Vk.RndrPss.CreateInfo {
			Vk.RndrPss.createInfoSType = (),
			Vk.RndrPss.createInfoPNext = NullPtr,
			Vk.RndrPss.createInfoFlags = 0,
			Vk.RndrPss.createInfoAttachmentCount = 1,
			Vk.RndrPss.createInfoPAttachments = pColorAttachment,
			Vk.RndrPss.createInfoSubpassCount = 1,
			Vk.RndrPss.createInfoPSubpasses = pSubpass,
			Vk.RndrPss.createInfoDependencyCount = 1,
			Vk.RndrPss.createInfoPDependencies = pDependency }
	dvc <- lift $ readIORef device
	pRenderPassInfo <- ContT $ withForeignPtr fRenderPassInfo
	pRenderPass <- ContT alloca
	lift do	r <- Vk.RndrPss.create dvc pRenderPassInfo NullPtr pRenderPass
		when (r /= success) $ error "failed to create render pass!"
		writeIORef renderPass =<< peek pRenderPass

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
		shaderStageList = [vertShaderStageInfo, fragShaderStageInfo]
		Vk.Ppl.VI.CreateInfo_ fVertexInputInfo = Vk.Ppl.VI.CreateInfo {
			Vk.Ppl.VI.createInfoSType = (),
			Vk.Ppl.VI.createInfoPNext = NullPtr,
			Vk.Ppl.VI.createInfoFlags = 0,
			Vk.Ppl.VI.createInfoVertexBindingDescriptionCount =
				0,
			Vk.Ppl.VI.createInfoPVertexBindingDescriptions =
				NullPtr,
			Vk.Ppl.VI.createInfoVertexAttributeDescriptionCount =
				0,
			Vk.Ppl.VI.createInfoPVertexAttributeDescriptions =
				NullPtr }
		Vk.Ppl.IA.CreateInfo_ fInputAssembly = Vk.Ppl.IA.CreateInfo {
			Vk.Ppl.IA.createInfoSType = (),
			Vk.Ppl.IA.createInfoPNext = NullPtr,
			Vk.Ppl.IA.createInfoFlags = 0,
			Vk.Ppl.IA.createInfoTopology = Vk.PrmTplgy.triangleList,
			Vk.Ppl.IA.createInfoPrimitiveRestartEnable = vkFalse }
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
	let	Vk.Ppl.VP.CreateInfo_ fViewportState = Vk.Ppl.VP.CreateInfo {
			Vk.Ppl.VP.createInfoSType = (),
			Vk.Ppl.VP.createInfoPNext = NullPtr,
			Vk.Ppl.VP.createInfoFlags = 0,
			Vk.Ppl.VP.createInfoViewportCount = 1,
			Vk.Ppl.VP.createInfoPViewports = pViewport,
			Vk.Ppl.VP.createInfoScissorCount = 1,
			Vk.Ppl.VP.createInfoPScissors = pScissor }
		Vk.Ppl.RstSt.CreateInfo_ fRasterizer = Vk.Ppl.RstSt.CreateInfo {
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
		Vk.Ppl.MS.CreateInfo_ fMultisampling = Vk.Ppl.MS.CreateInfo {
			Vk.Ppl.MS.createInfoSType = (),
			Vk.Ppl.MS.createInfoPNext = NullPtr,
			Vk.Ppl.MS.createInfoFlags = 0,
			Vk.Ppl.MS.createInfoRasterizationSamples =
				Vk.Sample.count1Bit,
			Vk.Ppl.MS.createInfoSampleShadingEnable = vkFalse,
			Vk.Ppl.MS.createInfoMinSampleShading = 1,
			Vk.Ppl.MS.createInfoPSampleMask = NullPtr,
			Vk.Ppl.MS.createInfoAlphaToCoverageEnable = vkFalse,
			Vk.Ppl.MS.createInfoAlphaToOneEnable = vkFalse }
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
	let	Vk.Ppl.CB.CreateInfo_ fColorBlending = Vk.Ppl.CB.CreateInfo {
			Vk.Ppl.CB.createInfoSType = (),
			Vk.Ppl.CB.createInfoPNext = NullPtr,
			Vk.Ppl.CB.createInfoFlags = 0,
			Vk.Ppl.CB.createInfoLogicOpEnable = vkFalse,
			Vk.Ppl.CB.createInfoLogicOp = Vk.Logic.opCopy,
			Vk.Ppl.CB.createInfoAttachmentCount = 1,
			Vk.Ppl.CB.createInfoPAttachments =
				pColorBlendAttachment,
			Vk.Ppl.CB.createInfoBlendConstants = [0, 0, 0, 0] }
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
	shaderStages <- ContT $ allocaArray 2
	lift $ pokeArray shaderStages shaderStageList
	pVertexInputInfo <- ContT $ withForeignPtr fVertexInputInfo
	pInputAssembly <- ContT $ withForeignPtr fInputAssembly
	pViewportState <- ContT $ withForeignPtr fViewportState
	pRasterizer <- ContT $ withForeignPtr fRasterizer
	pMultisampling <- ContT $ withForeignPtr fMultisampling
	pColorBlending <- ContT $ withForeignPtr fColorBlending
	pplLyt <- lift $ readIORef pipelineLayout
	rp <- lift $ readIORef renderPass
	let	Vk.Ppl.CreateInfo_ fPipelineInfo = Vk.Ppl.CreateInfo {
			Vk.Ppl.createInfoSType = (),
			Vk.Ppl.createInfoPNext = NullPtr,
			Vk.Ppl.createInfoFlags = 0,
			Vk.Ppl.createInfoStageCount = 2,
			Vk.Ppl.createInfoPStages = shaderStages,
			Vk.Ppl.createInfoPVertexInputState = pVertexInputInfo,
			Vk.Ppl.createInfoPInputAssemblyState = pInputAssembly,
			Vk.Ppl.createInfoPTessellationState = NullPtr,
			Vk.Ppl.createInfoPViewportState = pViewportState,
			Vk.Ppl.createInfoPRasterizationState = pRasterizer,
			Vk.Ppl.createInfoPMultisampleState = pMultisampling,
			Vk.Ppl.createInfoPDepthStencilState = NullPtr,
			Vk.Ppl.createInfoPColorBlendState = pColorBlending,
			Vk.Ppl.createInfoPDynamicState = NullPtr,
			Vk.Ppl.createInfoLayout = pplLyt,
			Vk.Ppl.createInfoRenderPass = rp,
			Vk.Ppl.createInfoSubpass = 0,
			Vk.Ppl.createInfoBasePipelineHandle = NullHandle,
			Vk.Ppl.createInfoBasePipelineIndex = - 1 }
	pPipelineInfo <- ContT $ withForeignPtr fPipelineInfo
	pGraphicsPipeline <- ContT alloca
	lift do	r <- Vk.Ppl.create
			dvc NullPtr 1 pPipelineInfo NullPtr pGraphicsPipeline
		when (r /= success) $ error "failed to create graphics pipeline!"
		writeIORef graphicsPipeline =<< peek pGraphicsPipeline
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

createFramebuffers :: IO ()
createFramebuffers = do
	scivs <- readIORef swapChainImageViews
	fbs <- createFramebuffer1 `mapM` scivs
	writeIORef swapChainFramebuffers fbs

createFramebuffer1 :: Vk.ImageView.ImageView -> IO Framebuffer
createFramebuffer1 attachment = ($ pure) $ runContT do
	rndrPss <- lift $ readIORef renderPass
	attachments <- ContT $ allocaArray 1
	lift $ pokeArray attachments [attachment]
	sce <- lift $ readIORef swapChainExtent
	let	Vk.Fb.CreateInfo_ fFramebufferInfo = Vk.Fb.CreateInfo {
			Vk.Fb.createInfoSType = (),
			Vk.Fb.createInfoPNext = NullPtr,
			Vk.Fb.createInfoFlags = 0,
			Vk.Fb.createInfoRenderPass = rndrPss,
			Vk.Fb.createInfoAttachmentCount = 1,
			Vk.Fb.createInfoPAttachments = attachments,
			Vk.Fb.createInfoWidth = Vk.extent2dWidth sce,
			Vk.Fb.createInfoHeight = Vk.extent2dHeight sce,
			Vk.Fb.createInfoLayers = 1 }
	dvc <- lift $ readIORef device
	pFramebufferInfo <- ContT $ withForeignPtr fFramebufferInfo
	pfb <- ContT alloca
	lift do	r <- Vk.Fb.create dvc pFramebufferInfo NullPtr pfb
		when (r /= success) $ error "failed to create framebuffer!"
		peek pfb

createCommandPool :: IO ()
createCommandPool = ($ pure) $ runContT do
	lift $ putStrLn "=== CREATE COMMAND POOL ==="
	queueFamilyIndices <- lift do
		pd <- readIORef physicalDevice
		findQueueFamilies pd
	lift $ print queueFamilyIndices
	let	Vk.CP.CreateInfo_ fPoolInfo = Vk.CP.CreateInfo {
			Vk.CP.createInfoSType = (),
			Vk.CP.createInfoPNext = NullPtr,
			Vk.CP.createInfoFlags = 0,
			Vk.CP.createInfoQueueFamilyIndex =
				fromJust $ graphicsFamily queueFamilyIndices }
	dvc <- lift $ readIORef device
	pPoolInfo <- ContT $ withForeignPtr fPoolInfo
	pCommandPool <- ContT alloca
	lift do	r <- Vk.CP.create dvc pPoolInfo NullPtr pCommandPool
		when (r /= success) $ error "failed to create command pool!"
		writeIORef commandPool =<< peek pCommandPool
		putStrLn "=== END ==="

createCommandBuffers :: IO ()
createCommandBuffers = do
	scfbs <- readIORef swapChainFramebuffers
	cbs <- createCommandBuffersGen (length scfbs)
	writeIORef commandBuffers cbs
	uncurry beginCommandBuffer1 `mapM_` zip cbs scfbs

createCommandBuffersGen :: Int -> IO [Vk.CommandBuffer]
createCommandBuffersGen cbc = ($ pure) $ runContT do
	cp <- lift $ readIORef commandPool
	let	Vk.CB.AllocateInfo_ fAllocInfo = Vk.CB.AllocateInfo {
			Vk.CB.allocateInfoSType = (),
			Vk.CB.allocateInfoPNext = NullPtr,
			Vk.CB.allocateInfoCommandPool = cp,
			Vk.CB.allocateInfoLevel = Vk.CB.levelPrimary,
			Vk.CB.allocateInfoCommandBufferCount = fromIntegral cbc }
	dvc <- lift $ readIORef device
	pAllocInfo <- ContT $ withForeignPtr fAllocInfo
	pCommandBuffers <- ContT $ allocaArray cbc
	lift do	r <- Vk.CB.allocate dvc pAllocInfo pCommandBuffers
		when (r /= success) $ error "faied to allocate command buffers!"
		peekArray cbc pCommandBuffers

beginCommandBuffer1 :: Vk.CommandBuffer -> Framebuffer -> IO ()
beginCommandBuffer1 cb fb = ($ pure) $ runContT do
	let	Vk.CB.BeginInfo_ fBeginInfo = Vk.CB.BeginInfo {
			Vk.CB.beginInfoSType = (),
			Vk.CB.beginInfoPNext = NullPtr,
			Vk.CB.beginInfoFlags = 0,
			Vk.CB.beginInfoPInheritanceInfo = NullPtr }
	pBeginInfo <- ContT $ withForeignPtr fBeginInfo
	lift do	r <- Vk.CB.begin cb pBeginInfo
		when (r /= success)
			$ error "failed to begin recording command buffer!"
	rp <- lift $ readIORef renderPass
	sce <- lift $ readIORef swapChainExtent
	pClearColor <- ContT $ allocaArray 4
	lift $ pokeArray pClearColor [0, 0, 0, 1]
	let	Vk.RndrPss.BeginInfo_ fRenderPassInfo = Vk.RndrPss.BeginInfo {
			Vk.RndrPss.beginInfoSType = (),
			Vk.RndrPss.beginInfoPNext = NullPtr,
			Vk.RndrPss.beginInfoRenderPass = rp,
			Vk.RndrPss.beginInfoFramebuffer = fb,
			Vk.RndrPss.beginInfoRenderArea = Vk.Rect2d {
				Vk.rect2dOffset = Vk.Offset2d 0 0,
				Vk.rect2dExtent = sce },
			Vk.RndrPss.beginInfoClearValueCount = 1,
			Vk.RndrPss.beginInfoPClearColorValueFloats =
				pClearColor }
	pRenderPassInfo <- ContT $ withForeignPtr fRenderPassInfo
	lift do	Vk.Cmd.beginRenderPass
			cb pRenderPassInfo Vk.Subpass.contentsInline
		gppl <- readIORef graphicsPipeline
		Vk.Cmd.bindPipeline cb Vk.Ppl.bindPointGraphics gppl
		Vk.Cmd.draw cb 3 1 0 0
		Vk.Cmd.endRenderPass cb
		r <- Vk.CB.end cb
		when (r /= success) $ error "failed to record command buffer!"

createSemaphores :: IO ()
createSemaphores = ($ pure) $ runContT do
	let	Vk.Smp.CreateInfo_ fSemaphoreInfo = Vk.Smp.CreateInfo {
			Vk.Smp.createInfoSType = (),
			Vk.Smp.createInfoPNext = NullPtr,
			Vk.Smp.createInfoFlags = 0 }
	dvc <- lift $ readIORef device
	pSemaphoreInfo <- ContT $ withForeignPtr fSemaphoreInfo
	pImageAvailableSemaphore <- ContT alloca
	pRenderFinishedSemaphore <- ContT alloca
	lift do r <- Vk.Smp.create
			dvc pSemaphoreInfo NullPtr pImageAvailableSemaphore
		r' <- Vk.Smp.create
			dvc pSemaphoreInfo NullPtr pRenderFinishedSemaphore
		when (r /= success || r' /= success)
			$ error "failed to create semaphores!"
		writeIORef imageAvailableSemaphore
			=<< peek pImageAvailableSemaphore
		writeIORef renderFinishedSemaphore
			=<< peek pRenderFinishedSemaphore
		putStr "imageAvailableSemaphore: "
		print =<< peek pImageAvailableSemaphore
		print =<< readIORef imageAvailableSemaphore
		putStr "renderFinishedSemaphore: "
		print =<< peek pRenderFinishedSemaphore
		print =<< readIORef renderFinishedSemaphore

mainLoop :: GlfwB.Window -> IO ()
mainLoop win = do
	fix \loop -> bool (pure ()) loop =<< do
		GlfwB.pollEvents
		drawFrame
		not <$> GlfwB.windowShouldClose win
	r <- Vk.Device.waitIdle =<< readIORef device
	when (r /= success) $ error "wait idle failure"

drawFrame :: IO ()
drawFrame = ($ pure) $ runContT do
	lift $ putStrLn "=== DRAW FRAME ==="
	pImageIndex <- ContT alloca
	dvc <- lift $ readIORef device
	sc <- lift $ readIORef swapChain
	ias <- lift $ readIORef imageAvailableSemaphore
	lift do	r <- Vk.Khr.acquireNextImage
			dvc sc uint64Max ias NullHandle pImageIndex
		when (r /= success) $ error "bad"
	(fromIntegral -> imageIndex) <- lift $ peek pImageIndex
	lift $ print imageIndex
	pWaitSemaphores <- ContT $ allocaArray 1
	lift $ pokeArray pWaitSemaphores . (: [])
		=<< readIORef imageAvailableSemaphore
	pWaitStages <- ContT $ allocaArray 1
	lift $ pokeArray pWaitStages [Vk.Ppl.stageColorAttachmentOutputBit]
	cbs <- lift $ readIORef commandBuffers
	pcb1 <- ContT $ allocaArray 1
	lift $ pokeArray pcb1 [cbs !! imageIndex]
	pSignalSemaphores <- ContT $ allocaArray 1
	lift $ pokeArray pSignalSemaphores . (: [])
		=<< readIORef renderFinishedSemaphore
	let	Vk.SubmitInfo_ fSubmitInfo = Vk.SubmitInfo {
			Vk.submitInfoSType = (),
			Vk.submitInfoPNext = NullPtr,
			Vk.submitInfoWaitSemaphoreCount = 1,
			Vk.submitInfoPWaitSemaphores = pWaitSemaphores,
			Vk.submitInfoPWaitDstStageMask = pWaitStages,
			Vk.submitInfoCommandBufferCount = 1,
			Vk.submitInfoPCommandBuffers = pcb1,
			Vk.submitInfoSignalSemaphoreCount = 1,
			Vk.submitInfoPSignalSemaphores = pSignalSemaphores }
	gq <- lift $ readIORef graphicsQueue
	pSubmitInfo <- ContT $ withForeignPtr fSubmitInfo
	lift do	r <- Vk.queueSubmit gq 1 pSubmitInfo NullHandle
		when (r /= success) $ error "failed to submit draw command buffer!"
	pSwapchains <- ContT $ allocaArray 1
	lift $ pokeArray pSwapchains . (: []) =<< readIORef swapChain
	let	Vk.Khr.PresentInfo_ fPresentInfo = Vk.Khr.PresentInfo {
			Vk.Khr.presentInfoSType = (),
			Vk.Khr.presentInfoPNext = NullPtr,
			Vk.Khr.presentInfoWaitSemaphoreCount = 1,
			Vk.Khr.presentInfoPWaitSemaphores = pSignalSemaphores,
			Vk.Khr.presentInfoSwapchainCount = 1,
			Vk.Khr.presentInfoPSwapchains = pSwapchains,
			Vk.Khr.presentInfoPImageIndices = pImageIndex,
			Vk.Khr.presentInfoPResults = NullPtr }
	pq <- lift $ readIORef presentQueue
	pPresentInfo <- ContT $ withForeignPtr fPresentInfo
	lift do	r <- Vk.Khr.queuePresent pq pPresentInfo
		when (r /= success) $ error "bad"
		r' <- Vk.queueWaitIdle =<< readIORef presentQueue
		when (r' /= success) $ error "bad"
	lift $ putStrLn "=== END ==="

cleanup :: GlfwB.Window -> IO ()
cleanup win = do
	dvc <- readIORef device
	rfs <- readIORef renderFinishedSemaphore
	ias <- readIORef imageAvailableSemaphore
	putStr "renderFinishedSemaphore: "
	print rfs
	Vk.Smp.destroy dvc rfs NullPtr
	putStr "imageAvailableSemaphore: "
	print ias
	Vk.Smp.destroy dvc ias NullPtr
	cp <- readIORef commandPool
	Vk.CP.destroy dvc cp NullPtr
	fbs <- readIORef swapChainFramebuffers
	(\fb -> Vk.Fb.destroy dvc fb NullPtr) `mapM_` fbs
	gppl <- readIORef graphicsPipeline
	Vk.Ppl.destroy dvc gppl NullPtr
	pl <- readIORef pipelineLayout
	Vk.Ppl.Lyt.destroy dvc pl NullPtr
	rp <- readIORef renderPass
	Vk.RndrPss.destroy dvc rp NullPtr
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
