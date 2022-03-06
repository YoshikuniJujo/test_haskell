{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (readFile)

import Foreign.Ptr
import Foreign.Marshal
import Foreign.ForeignPtr hiding (newForeignPtr)
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

import ThEnv
import Vulkan.Base

import qualified Data.Set as Set
import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt
import qualified Graphics.UI.GLFW as GlfwB

import qualified Glfw

import qualified Vulkan as Vk
import qualified Vulkan.Enum as Vk
import qualified Vulkan.AllocationCallbacks as Vk.AC
import qualified Vulkan.Instance as Vk.Ist
import qualified Vulkan.Instance.Enum as Vk.Ist
import qualified Vulkan.Enumerate as Vk.Enumerate
import qualified Vulkan.Ext.DebugUtils.Messenger as Vk.Ext.DU.Msngr
import qualified Vulkan.Ext.DebugUtils.Message.Enum as Vk.Ext.DU.Msg

import qualified Vulkan.PhysicalDevice as Vk.PhysicalDevice
import qualified Vulkan.QueueFamily as Vk.QueueFamily
import qualified Vulkan.Device.Queue as Vk.Device.Queue
import qualified Vulkan.Device.Queue.Enum as Vk.Device.Queue
import qualified Vulkan.Device as Vk.Device

import qualified Vulkan.Khr as Vk.Khr
import qualified Vulkan.Khr.Surface as Vk.Khr.Sfc

import qualified Vulkan.Core as Vk.C
import qualified Vulkan.Ext.DebugUtils as Vk.Ext.DU
import qualified Vulkan.PhysicalDevice.Core as Vk.PhysicalDevice.C
import qualified Vulkan.QueueFamily.Core as Vk.QueueFamily.C

import qualified Vulkan.Device.Core as Vk.Device.C
import qualified Vulkan.Khr.Swapchain as Vk.Khr.Sc

import qualified Vulkan.Khr.Surface.Core as Vk.Khr.Sfc.C
import qualified Vulkan.Khr.Present as Vk.Khr.Present
import qualified Vulkan.Khr.Surface.PhysicalDevice as
	Vk.Khr.Sfc.PhysicalDevice

import qualified Vulkan.Format as Vk.Format
import qualified Vulkan.Khr.ColorSpace as Vk.Khr.ColorSpace
import qualified Vulkan.Khr.Swapchain.Core as Vk.Khr.Sc.C
import qualified Vulkan.Khr.Core as Vk.Khr.C

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

enableValidationLayers :: Bool
enableValidationLayers =
	maybe True (const False) $(lookupCompileEnvExp "NDEBUG")

validationLayers :: [Txt.Text]
validationLayers = [Vk.Khr.validationLayerName]

swapChain :: IORef Vk.Khr.Sc.C.Swapchain
swapChain = unsafePerformIO $ newIORef NullHandle

swapChainImages :: IORef [Vk.Img.Image]
swapChainImages = unsafePerformIO $ newIORef []

swapChainImageFormat :: IORef Vk.C.Format
swapChainImageFormat = unsafePerformIO $ newIORef 0

swapChainExtent :: IORef Vk.C.Extent2d
swapChainExtent = unsafePerformIO $ newIORef $ Vk.C.Extent2d 0 0

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

commandBuffers :: IORef [Vk.C.CommandBuffer]
commandBuffers = unsafePerformIO $ newIORef []

imageAvailableSemaphore, renderFinishedSemaphore :: IORef Vk.C.Semaphore
(imageAvailableSemaphore, renderFinishedSemaphore) = unsafePerformIO
	$ (,) <$> newIORef NullPtr <*> newIORef NullPtr

data Global = Global {
	globalWindow :: GlfwB.Window,
	globalInstance :: IORef Vk.Instance,
	globalDebugMessenger :: IORef Vk.Ext.DU.Messenger,
	globalPhysicalDevice :: IORef Vk.PhysicalDevice,
	globalDevice :: IORef Vk.Device,
	globalGraphicsQueue :: IORef Vk.Queue,
	globalPresentQueue :: IORef Vk.Queue,
	globalSurface :: IORef Vk.Khr.Surface
	}

newGlobal :: GlfwB.Window -> IO Global
newGlobal w = do
	ist <- newIORef $ Vk.Instance NullPtr
	dmsgr <- newIORef $ Vk.Ext.DU.Messenger NullPtr
	pdvc <- newIORef $ Vk.PhysicalDevice NullPtr
	dvc <- newIORef $ Vk.Device NullPtr
	gq <- newIORef $ Vk.Queue NullPtr
	pq <- newIORef $ Vk.Queue NullPtr
	sfc <- newIORef $ Vk.Khr.Surface NullPtr
	pure Global {
		globalWindow = w,
		globalInstance = ist,
		globalDebugMessenger = dmsgr,
		globalPhysicalDevice = pdvc,
		globalDevice = dvc,
		globalGraphicsQueue = gq,
		globalPresentQueue = pq,
		globalSurface = sfc
		}

run :: IO ()
run = do
	w <- initWindow
	g <- newGlobal w
	initVulkan g
	mainLoop g
	cleanup g

width, height :: Int
width = 800; height = 600

initWindow :: IO GlfwB.Window
initWindow = do
	True <- GlfwB.init
	GlfwB.windowHint $ GlfwB.WindowHint'ClientAPI GlfwB.ClientAPI'NoAPI
	GlfwB.windowHint $ GlfwB.WindowHint'Resizable False
	Just w <- GlfwB.createWindow width height "Vulkan" Nothing Nothing
	pure w

initVulkan :: Global -> IO ()
initVulkan g = do
	createInstance g
	when enableValidationLayers $ setupDebugMessenger g
	createSurface g
	pickPhysicalDevice g
	createLogicalDevice g
	createSwapChain g
	createImageViews g
	createRenderPass g
	createGraphicsPipeline g
	createFramebuffers g
	createCommandPool g
	createCommandBuffers g
	createSemaphores g

createInstance :: Global -> IO ()
createInstance Global { globalInstance = rist } = ($ pure) $ runContT do
	lift do	b <- checkValidationLayerSupport
		when (enableValidationLayers && not b)
			$ error "validation layers requested, but no available!"
		putStrLn "available extensions:"
		mapM_ (Txt.putStrLn . ("\t" <>)
				. Vk.extensionPropertiesExtensionName)
			=<< Vk.Enumerate.instanceExtensionProperties Nothing
	pValidationLayer <- lift $ newCString "VK_LAYER_KHRONOS_validation"
	pValidationLayers <- ContT $ allocaArray 1
	lift $ pokeArray pValidationLayers [pValidationLayer]
	extensions <- lift getRequiredExtensions
	let	appInfo = Vk.ApplicationInfo {
			Vk.applicationInfoNext = Nothing,
			Vk.applicationInfoApplicationName = "Hello Triangle",
			Vk.applicationInfoApplicationVersion =
				Vk.makeApiVersion 0 1 0 0,
			Vk.applicationInfoEngineName = "No Engine",
			Vk.applicationInfoEngineVersion =
				Vk.makeApiVersion 0 1 0 0,
			Vk.applicationInfoApiVersion = Vk.apiVersion_1_0 }
		createInfo = Vk.Ist.CreateInfo {
			Vk.Ist.createInfoNext = Just debugMessengerCreateInfo,
			Vk.Ist.createInfoFlags = Vk.Ist.CreateFlagsZero,
			Vk.Ist.createInfoApplicationInfo = appInfo,
			Vk.Ist.createInfoEnabledLayerNames =
				bool [] validationLayers enableValidationLayers,
			Vk.Ist.createInfoEnabledExtensionNames = extensions }
	lift $ writeIORef rist
		=<< Vk.Ist.create @_ @() @() createInfo Nothing

checkValidationLayerSupport :: IO Bool
checkValidationLayerSupport =
	(\lns -> all (`elem` lns) validationLayers)
			. map Vk.layerPropertiesLayerName
		<$> Vk.Enumerate.instanceLayerProperties

getRequiredExtensions :: IO [Txt.Text]
getRequiredExtensions =
	bool id (Vk.Ext.DU.extensionName :) enableValidationLayers
		<$> ((cstringToText `mapM`)
			=<< GlfwB.getRequiredInstanceExtensions)

setupDebugMessenger :: Global -> IO ()
setupDebugMessenger Global {
	globalInstance = rist,
	globalDebugMessenger = rdmsgr } = do
	ist <- readIORef rist
	msgr <- Vk.Ext.DU.Msngr.create ist debugMessengerCreateInfo Vk.AC.nil
	writeIORef rdmsgr msgr

debugMessengerCreateInfo :: Vk.Ext.DU.Msngr.CreateInfo () () () () () ()
debugMessengerCreateInfo = Vk.Ext.DU.Msngr.CreateInfo {
	Vk.Ext.DU.Msngr.createInfoNext = Nothing,
	Vk.Ext.DU.Msngr.createInfoFlags = Vk.Ext.DU.Msngr.CreateFlagsZero,
	Vk.Ext.DU.Msngr.createInfoMessageSeverity =
		Vk.Ext.DU.Msg.SeverityVerboseBit .|.
		Vk.Ext.DU.Msg.SeverityWarningBit .|.
		Vk.Ext.DU.Msg.SeverityErrorBit,
	Vk.Ext.DU.Msngr.createInfoMessageType =
		Vk.Ext.DU.Msg.TypeGeneralBit .|.
		Vk.Ext.DU.Msg.TypeValidationBit .|.
		Vk.Ext.DU.Msg.TypePerformanceBit,
	Vk.Ext.DU.Msngr.createInfoFnUserCallback = debugCallback,
	Vk.Ext.DU.Msngr.createInfoUserData = Nothing }

debugCallback :: Vk.Ext.DU.Msngr.FnCallback () () () () ()
debugCallback _messageSeverity _messageType callbackData _userData = do
	let	message = Vk.Ext.DU.Msngr.callbackDataMessage callbackData
	Txt.putStrLn $ "validation layer: " <> message
	pure False

createSurface :: Global -> IO ()
createSurface Global {
	globalWindow = win, globalInstance = rist, globalSurface = rsfc } = do
	ist <- readIORef rist
	writeIORef rsfc =<< Glfw.createWindowSurface @() ist win Nothing

pickPhysicalDevice :: Global -> IO ()
pickPhysicalDevice g@Global {
	globalInstance = rist, globalPhysicalDevice = rpdvc } = do
	devices <- Vk.PhysicalDevice.enumerate =<< readIORef rist
	findM (isDeviceSuitable g) devices >>= \case
		Just pd -> writeIORef rpdvc pd
		Nothing -> error "no matched physical devices"

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = pure Nothing
findM p (x : xs) = bool (findM p xs) (pure $ Just x) =<< p x

isDeviceSuitable :: Global -> Vk.PhysicalDevice -> IO Bool
isDeviceSuitable g dvc@(Vk.PhysicalDevice cdvc) = do
	putStrLn "*** IS DEVICE SUITABLE ***"
	print =<< Vk.PhysicalDevice.getProperties dvc
	print =<< Vk.PhysicalDevice.getFeatures dvc

	indices <- findQueueFamilies g dvc
	extensionSupported <- checkDeviceExtensionSupport dvc
	swapChainAdequate <- if extensionSupported
		then do	swapChainSupport <- querySwapChainSupport g cdvc
			pure $ not (null $ swapChainSupportDetailsFormats
					swapChainSupport) &&
				not (null $ swapChainSupportDetailsPresentModes
					swapChainSupport)
		else pure False
	pure $ isComplete indices && extensionSupported && swapChainAdequate

deviceExtensions :: [Txt.Text]
deviceExtensions = [Vk.Khr.Sc.extensionName]

checkDeviceExtensionSupport :: Vk.PhysicalDevice -> IO Bool
checkDeviceExtensionSupport dvc@(Vk.PhysicalDevice cdvc) = ($ pure) $ runContT do
	lift $ putStrLn "CHECK DEVICE EXTENSION SUPPORT"
	lift $ putStrLn "foo"
	lift $ print =<<
		Vk.PhysicalDevice.enumerateExtensionProperties dvc Nothing
	lift $ putStrLn "barbarbar"
	pExtensionCount <- ContT alloca
	(fromIntegral -> extensionCount) <- lift do
		_ <- Vk.PhysicalDevice.C.enumerateExtensionProperties
			cdvc NullPtr pExtensionCount NullPtr
		peek pExtensionCount
	pAvailableExtensions <- ContT $ allocaArray extensionCount
	lift do	_ <- Vk.PhysicalDevice.C.enumerateExtensionProperties
			cdvc NullPtr pExtensionCount pAvailableExtensions
		es <- map Vk.C.extensionPropertiesExtensionName
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

findQueueFamilies :: Global -> Vk.PhysicalDevice -> IO QueueFamilyIndices
findQueueFamilies Global { globalSurface = rsfc } dvc = do
	putStrLn "*** FIND QUEUE FAMILIES ***"
	props <- Vk.PhysicalDevice.getQueueFamilyProperties dvc
	print props
	sfc <- readIORef rsfc
	pfi <- getPresentFamilyIndex sfc dvc (fromIntegral $ length props) 0
	pure QueueFamilyIndices {
		graphicsFamily = fromIntegral
			<$> findIndex checkGraphicsBit props,
		presentFamily = pfi }

getPresentFamilyIndex ::
	Vk.Khr.Surface -> Vk.PhysicalDevice -> Word32 -> Word32 -> IO (Maybe Word32)
getPresentFamilyIndex sfc pd n i
	| i >= n = pure Nothing
	| otherwise = do
		presentSupport <- Vk.PhysicalDevice.getSurfaceSupport pd i sfc
		if presentSupport
			then pure $ Just i
			else getPresentFamilyIndex sfc pd n (i + 1)

checkGraphicsBit :: Vk.QueueFamily.Properties -> Bool
checkGraphicsBit prop = let
	Vk.QueueFlagBits b = Vk.QueueFamily.propertiesQueueFlags prop in
	b .&. queueGraphicsBit /= 0

checkGraphicsBitCore :: Vk.QueueFamily.C.Properties -> Bool
checkGraphicsBitCore prop =
	Vk.QueueFamily.C.propertiesQueueFlags prop .&. queueGraphicsBit /= 0

data SwapChainSupportDetails = SwapChainSupportDetails {
	swapChainSupportDetailsCapabilities :: Vk.Khr.Sfc.C.Capabilities,
	swapChainSupportDetailsFormats :: [Vk.Khr.Sfc.C.Format],
	swapChainSupportDetailsPresentModes :: [Vk.Khr.Present.Mode] }
	deriving Show

querySwapChainSupport :: Global ->
	Vk.PhysicalDevice.C.PhysicalDevice -> IO SwapChainSupportDetails
querySwapChainSupport Global {
	globalSurface = rsfc } dvc = ($ pure) $ runContT do
	Vk.Khr.Surface sfc <- lift $ readIORef rsfc
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

createLogicalDevice :: Global -> IO ()
createLogicalDevice g@Global {
	globalPhysicalDevice = rphdvc,
	globalDevice = rdvc,
	globalGraphicsQueue = rgq,
	globalPresentQueue = rpq } = do
	phdvc <- readIORef rphdvc
	indices <- findQueueFamilies g phdvc
	let	uniqueQueueFamilies = Set.fromList [
			fromJust $ graphicsFamily indices,
			fromJust $ presentFamily indices ]
		queueCreateInfo qf = Vk.Device.Queue.CreateInfo {
			Vk.Device.Queue.createInfoNext = Nothing,
			Vk.Device.Queue.createInfoFlags =
				Vk.Device.Queue.CreateFlagsZero,
			Vk.Device.Queue.createInfoQueueFamilyIndex = qf,
			Vk.Device.Queue.createInfoQueuePriorities = [1.0] }
		deviceFeatures = Vk.PhysicalDevice.featuresZero
		createInfo = Vk.Device.CreateInfo {
			Vk.Device.createInfoNext = Nothing,
			Vk.Device.createInfoFlags = Vk.Device.CreateFlagsZero,
			Vk.Device.createInfoQueueCreateInfos =
				((: []) . queueCreateInfo) `foldMap`
					uniqueQueueFamilies,
			Vk.Device.createInfoEnabledLayerNames =
				if enableValidationLayers
					then validationLayers else [],
			Vk.Device.createInfoEnabledExtensionNames =
				["VK_KHR_swapchain"],
			Vk.Device.createInfoEnabledFeatures = deviceFeatures }
	dvc <- Vk.Device.create @() @() @() phdvc createInfo Nothing
	writeIORef rdvc dvc
	gq <- Vk.Device.getQueue dvc (fromJust $ graphicsFamily indices) 0
	pq <- Vk.Device.getQueue dvc (fromJust $ presentFamily indices) 0
	writeIORef rgq gq
	writeIORef rpq pq

createSwapChain :: Global -> IO ()
createSwapChain g@Global {
	globalPhysicalDevice = rpdvc,
	globalDevice = rdvc,
	globalSurface = rsfc
	} = ($ pure) $ runContT do
	Vk.Device dvc <- lift $ readIORef rdvc
	Vk.Khr.Sc.C.CreateInfo_ fCreateInfo <- lift do
		pdvc@(Vk.PhysicalDevice pd) <- readIORef rpdvc
		swapChainSupport <- querySwapChainSupport g pd
		Vk.Khr.Surface sfc <- readIORef rsfc
		indices <- findQueueFamilies g pdvc
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
			minImageCount = Vk.Khr.Sfc.C.capabilitiesMinImageCount cap
			maxImageCount = Vk.Khr.Sfc.C.capabilitiesMaxImageCount cap
			imageCount = if maxImageCount > 0
				then min (minImageCount + 1) maxImageCount
				else minImageCount + 1
			createInfo' = Vk.Khr.Sc.C.CreateInfo {
				Vk.Khr.Sc.C.createInfoSType = (),
				Vk.Khr.Sc.C.createInfoPNext = NullPtr,
				Vk.Khr.Sc.C.createInfoFlags = 0,
				Vk.Khr.Sc.C.createInfoSurface = sfc,
				Vk.Khr.Sc.C.createInfoMinImageCount = imageCount,
				Vk.Khr.Sc.C.createInfoImageFormat =
					Vk.Khr.Sfc.C.formatFormat surfaceFormat,
				Vk.Khr.Sc.C.createInfoImageColorSpace =
					Vk.Khr.Sfc.C.formatColorSpace
						surfaceFormat,
				Vk.Khr.Sc.C.createInfoImageExtent = extent,
				Vk.Khr.Sc.C.createInfoImageArrayLayers = 1,
				Vk.Khr.Sc.C.createInfoImageUsage =
					Vk.C.imageUsageColorAttachmentBit,
				Vk.Khr.Sc.C.createInfoImageSharingMode =
					Vk.C.sharingModeExclusive,
				Vk.Khr.Sc.C.createInfoQueueFamilyIndexCount = 0,
				Vk.Khr.Sc.C.createInfoPQueueFamilyIndices =
					NullPtr,
				Vk.Khr.Sc.C.createInfoPreTransform =
					Vk.Khr.Sfc.C.capabilitiesCurrentTransform
						cap,
				Vk.Khr.Sc.C.createInfoCompositeAlpha =
					Vk.Khr.C.compositeAlphaOpaqueBit,
				Vk.Khr.Sc.C.createInfoPresentMode = presentMode,
				Vk.Khr.Sc.C.createInfoClipped = vkTrue,
				Vk.Khr.Sc.C.createInfoOldSwapchain = NullHandle }
		print surfaceFormat
		print presentMode
		print (Vk.Format.b8g8r8a8Srgb, Vk.Khr.ColorSpace.srgbNonlinear)
		print Vk.Khr.Present.modeMailbox
		print extent
		print imageCount
		print maxImageCount
		writeIORef swapChainImageFormat
			$ Vk.Khr.Sfc.C.formatFormat surfaceFormat
		writeIORef swapChainExtent extent
		pure createInfo'
	pCreateInfo <- ContT $ withForeignPtr fCreateInfo
	pSwapChain <- ContT alloca
	sc <- lift do
		r <- Vk.Khr.Sc.C.create dvc pCreateInfo NullPtr pSwapChain
		when (r /= success) $ error "failed to create swap chain!"
		sc <- peek pSwapChain
		sc <$ writeIORef swapChain sc
	pImageCount <- ContT alloca
	(fromIntegral -> imageCount) <- lift do
		_ <- Vk.Khr.Sc.C.getImages dvc sc pImageCount NullPtr
		peek pImageCount
	pSwapChainImages <- ContT $ allocaArray imageCount
	lift do	_ <- Vk.Khr.Sc.C.getImages dvc sc pImageCount pSwapChainImages
		writeIORef swapChainImages
			=<< peekArray imageCount pSwapChainImages

chooseSwapSurfaceFormat :: [Vk.Khr.Sfc.C.Format] -> Vk.Khr.Sfc.C.Format
chooseSwapSurfaceFormat availableFormats = fromMaybe (head availableFormats)
	$ find (\f ->
		Vk.Khr.Sfc.C.formatFormat f == Vk.Format.b8g8r8a8Srgb &&
		Vk.Khr.Sfc.C.formatColorSpace f ==
			Vk.Khr.ColorSpace.srgbNonlinear) availableFormats

chooseSwapPresentMode :: [Vk.Khr.Present.Mode] -> Vk.Khr.Present.Mode
chooseSwapPresentMode availablePresentModes =
	if Vk.Khr.Present.modeMailbox `elem` availablePresentModes
		then Vk.Khr.Present.modeMailbox
		else Vk.Khr.Present.modeFifo

chooseSwapExtent :: Vk.Khr.Sfc.C.Capabilities -> Vk.C.Extent2d
chooseSwapExtent capabilities =
	if Vk.C.extent2dWidth ce /= uint32Max
		then ce
		else error "Ah!"
	where ce = Vk.Khr.Sfc.C.capabilitiesCurrentExtent capabilities

createImageViews :: Global -> IO ()
createImageViews g = do
	scis <- readIORef swapChainImages
	ivs <- createImageView1 g `mapM` scis
	writeIORef swapChainImageViews ivs

createImageView1 :: Global -> Vk.Img.Image -> IO Vk.ImageView.ImageView
createImageView1 Global { globalDevice = rdvc } img = ($ pure) $ runContT do
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
	Vk.Device dvc <- lift $ readIORef rdvc
	pCreateInfo <- ContT $ withForeignPtr fCreateInfo
	pView <- ContT alloca
	lift do	r <- Vk.ImageView.create dvc pCreateInfo NullPtr pView
		when (r /= success) $ error "failed to create image views!"
		peek pView

createRenderPass :: Global -> IO ()
createRenderPass Global { globalDevice = rdvc } = ($ pure) $ runContT do
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
	Vk.Device dvc <- lift $ readIORef rdvc
	pRenderPassInfo <- ContT $ withForeignPtr fRenderPassInfo
	pRenderPass <- ContT alloca
	lift do	r <- Vk.RndrPss.create dvc pRenderPassInfo NullPtr pRenderPass
		when (r /= success) $ error "failed to create render pass!"
		writeIORef renderPass =<< peek pRenderPass

createGraphicsPipeline :: Global -> IO ()
createGraphicsPipeline g@Global { globalDevice = rdvc } = ($ pure) $ runContT do
	Vk.Device dvc <- lift $ readIORef rdvc
	vertShaderCode <- lift $ readFile "shaders/vert.spv"
	fragShaderCode <- lift $ readFile "shaders/frag.spv"
	vertShaderModule <- lift $ createShaderModule g vertShaderCode
	fragShaderModule <- lift $ createShaderModule g fragShaderCode
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
		Vk.C.Viewport_ fViewport = Vk.C.Viewport {
			Vk.C.viewportX = 0, Vk.C.viewportY = 0,
			Vk.C.viewportWidth = fromIntegral $ Vk.C.extent2dWidth sce,
			Vk.C.viewportHeight = fromIntegral $ Vk.C.extent2dHeight sce,
			Vk.C.viewportMinDepth = 0, Vk.C.viewportMaxDepth = 1 }
		Vk.C.Rect2d_ fScissor = Vk.C.Rect2d {
			Vk.C.rect2dOffset = Vk.C.Offset2d {
				Vk.C.offset2dX = 0, Vk.C.offset2dY = 0 },
			Vk.C.rect2dExtent = sce }
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

createShaderModule :: Global -> (Ptr Word32, Integer) -> IO Vk.Shader.Module.Module
createShaderModule Global { globalDevice = rdvc } (p, n) =
	($ pure) $ runContT do
		Vk.Device dvc <- lift $ readIORef rdvc
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

createFramebuffers :: Global -> IO ()
createFramebuffers g = do
	scivs <- readIORef swapChainImageViews
	fbs <- createFramebuffer1 g `mapM` scivs
	writeIORef swapChainFramebuffers fbs

createFramebuffer1 :: Global -> Vk.ImageView.ImageView -> IO Framebuffer
createFramebuffer1 Global {
	globalDevice = rdvc } attachment = ($ pure) $ runContT do
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
			Vk.Fb.createInfoWidth = Vk.C.extent2dWidth sce,
			Vk.Fb.createInfoHeight = Vk.C.extent2dHeight sce,
			Vk.Fb.createInfoLayers = 1 }
	Vk.Device dvc <- lift $ readIORef rdvc
	pFramebufferInfo <- ContT $ withForeignPtr fFramebufferInfo
	pfb <- ContT alloca
	lift do	r <- Vk.Fb.create dvc pFramebufferInfo NullPtr pfb
		when (r /= success) $ error "failed to create framebuffer!"
		peek pfb

createCommandPool :: Global -> IO ()
createCommandPool g@Global {
	globalPhysicalDevice = rpdvc,
	globalDevice = rdvc } = ($ pure) $ runContT do
	lift $ putStrLn "=== CREATE COMMAND POOL ==="
	queueFamilyIndices <- lift do
		pdvc <- readIORef rpdvc
		findQueueFamilies g pdvc
	lift $ print queueFamilyIndices
	let	Vk.CP.CreateInfo_ fPoolInfo = Vk.CP.CreateInfo {
			Vk.CP.createInfoSType = (),
			Vk.CP.createInfoPNext = NullPtr,
			Vk.CP.createInfoFlags = 0,
			Vk.CP.createInfoQueueFamilyIndex =
				fromJust $ graphicsFamily queueFamilyIndices }
	Vk.Device dvc <- lift $ readIORef rdvc
	pPoolInfo <- ContT $ withForeignPtr fPoolInfo
	pCommandPool <- ContT alloca
	lift do	r <- Vk.CP.create dvc pPoolInfo NullPtr pCommandPool
		when (r /= success) $ error "failed to create command pool!"
		writeIORef commandPool =<< peek pCommandPool
		putStrLn "=== END ==="

createCommandBuffers :: Global -> IO ()
createCommandBuffers g = do
	scfbs <- readIORef swapChainFramebuffers
	cbs <- createCommandBuffersGen g (length scfbs)
	writeIORef commandBuffers cbs
	uncurry beginCommandBuffer1 `mapM_` zip cbs scfbs

createCommandBuffersGen :: Global -> Int -> IO [Vk.C.CommandBuffer]
createCommandBuffersGen Global {
	globalDevice = rdvc } cbc = ($ pure) $ runContT do
	cp <- lift $ readIORef commandPool
	let	Vk.CB.AllocateInfo_ fAllocInfo = Vk.CB.AllocateInfo {
			Vk.CB.allocateInfoSType = (),
			Vk.CB.allocateInfoPNext = NullPtr,
			Vk.CB.allocateInfoCommandPool = cp,
			Vk.CB.allocateInfoLevel = Vk.CB.levelPrimary,
			Vk.CB.allocateInfoCommandBufferCount = fromIntegral cbc }
	Vk.Device dvc <- lift $ readIORef rdvc
	pAllocInfo <- ContT $ withForeignPtr fAllocInfo
	pCommandBuffers <- ContT $ allocaArray cbc
	lift do	r <- Vk.CB.allocate dvc pAllocInfo pCommandBuffers
		when (r /= success) $ error "faied to allocate command buffers!"
		peekArray cbc pCommandBuffers

beginCommandBuffer1 :: Vk.C.CommandBuffer -> Framebuffer -> IO ()
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
			Vk.RndrPss.beginInfoRenderArea = Vk.C.Rect2d {
				Vk.C.rect2dOffset = Vk.C.Offset2d 0 0,
				Vk.C.rect2dExtent = sce },
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

createSemaphores :: Global -> IO ()
createSemaphores Global { globalDevice = rdvc } = ($ pure) $ runContT do
	let	Vk.Smp.CreateInfo_ fSemaphoreInfo = Vk.Smp.CreateInfo {
			Vk.Smp.createInfoSType = (),
			Vk.Smp.createInfoPNext = NullPtr,
			Vk.Smp.createInfoFlags = 0 }
	Vk.Device dvc <- lift $ readIORef rdvc
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

mainLoop :: Global -> IO ()
mainLoop g@Global {
	globalWindow = win,
	globalDevice = rdvc } = do
	fix \loop -> bool (pure ()) loop =<< do
		GlfwB.pollEvents
		drawFrame g
		not <$> GlfwB.windowShouldClose win
	r <- Vk.Device.C.waitIdle . (\(Vk.Device d) -> d) =<< readIORef rdvc
	when (r /= success) $ error "wait idle failure"

drawFrame :: Global -> IO ()
drawFrame Global {
	globalDevice = rdvc,
	globalGraphicsQueue = rgq,
	globalPresentQueue = rpq } = ($ pure) $ runContT do
--	lift $ putStrLn "=== DRAW FRAME ==="
	pImageIndex <- ContT alloca
	Vk.Device dvc <- lift $ readIORef rdvc
	sc <- lift $ readIORef swapChain
	ias <- lift $ readIORef imageAvailableSemaphore
	lift do	r <- Vk.Khr.C.acquireNextImage
			dvc sc uint64Max ias NullHandle pImageIndex
		when (r /= success) $ error "bad"
	(fromIntegral -> imageIndex) <- lift $ peek pImageIndex
--	lift $ print imageIndex
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
	let	Vk.C.SubmitInfo_ fSubmitInfo = Vk.C.SubmitInfo {
			Vk.C.submitInfoSType = (),
			Vk.C.submitInfoPNext = NullPtr,
			Vk.C.submitInfoWaitSemaphoreCount = 1,
			Vk.C.submitInfoPWaitSemaphores = pWaitSemaphores,
			Vk.C.submitInfoPWaitDstStageMask = pWaitStages,
			Vk.C.submitInfoCommandBufferCount = 1,
			Vk.C.submitInfoPCommandBuffers = pcb1,
			Vk.C.submitInfoSignalSemaphoreCount = 1,
			Vk.C.submitInfoPSignalSemaphores = pSignalSemaphores }
	Vk.Queue gq <- lift $ readIORef rgq
	pSubmitInfo <- ContT $ withForeignPtr fSubmitInfo
	lift do	r <- Vk.C.queueSubmit gq 1 pSubmitInfo NullHandle
		when (r /= success) $ error "failed to submit draw command buffer!"
	pSwapchains <- ContT $ allocaArray 1
	lift $ pokeArray pSwapchains . (: []) =<< readIORef swapChain
	let	Vk.Khr.C.PresentInfo_ fPresentInfo = Vk.Khr.C.PresentInfo {
			Vk.Khr.C.presentInfoSType = (),
			Vk.Khr.C.presentInfoPNext = NullPtr,
			Vk.Khr.C.presentInfoWaitSemaphoreCount = 1,
			Vk.Khr.C.presentInfoPWaitSemaphores = pSignalSemaphores,
			Vk.Khr.C.presentInfoSwapchainCount = 1,
			Vk.Khr.C.presentInfoPSwapchains = pSwapchains,
			Vk.Khr.C.presentInfoPImageIndices = pImageIndex,
			Vk.Khr.C.presentInfoPResults = NullPtr }
	Vk.Queue pq <- lift $ readIORef rpq
	pPresentInfo <- ContT $ withForeignPtr fPresentInfo
	lift do	r <- Vk.Khr.C.queuePresent pq pPresentInfo
		when (r /= success) $ error "bad"
		r' <- Vk.C.queueWaitIdle . (\(Vk.Queue q) -> q) =<< readIORef rpq
		when (r' /= success) $ error "bad"
--	lift $ putStrLn "=== END ==="

cleanup :: Global -> IO ()
cleanup Global {
	globalWindow = win,
	globalInstance = rist,
	globalDebugMessenger = rdmsgr,
	globalDevice = rdvc,
	globalSurface = rsfc } = do
	dvc@(Vk.Device cdvc) <- readIORef rdvc
	rfs <- readIORef renderFinishedSemaphore
	ias <- readIORef imageAvailableSemaphore
	putStr "renderFinishedSemaphore: "
	print rfs
	Vk.Smp.destroy cdvc rfs NullPtr
	putStr "imageAvailableSemaphore: "
	print ias
	Vk.Smp.destroy cdvc ias NullPtr
	cp <- readIORef commandPool
	Vk.CP.destroy cdvc cp NullPtr
	fbs <- readIORef swapChainFramebuffers
	(\fb -> Vk.Fb.destroy cdvc fb NullPtr) `mapM_` fbs
	gppl <- readIORef graphicsPipeline
	Vk.Ppl.destroy cdvc gppl NullPtr
	pl <- readIORef pipelineLayout
	Vk.Ppl.Lyt.destroy cdvc pl NullPtr
	rp <- readIORef renderPass
	Vk.RndrPss.destroy cdvc rp NullPtr
	ivs <- readIORef swapChainImageViews
	(\iv -> Vk.ImageView.destroy cdvc iv NullPtr) `mapM_` ivs
	(\sc -> Vk.Khr.Sc.C.destroy cdvc sc NullPtr) =<< readIORef swapChain
	Vk.Device.destroy @() dvc Nothing
	ist <- readIORef rist
	(\sfc -> Vk.Khr.Sfc.destroy @() ist sfc Nothing) =<< readIORef rsfc
	when enableValidationLayers $ readIORef rdmsgr >>= \dm ->
		Vk.Ext.DU.Msngr.destroy ist dm Vk.AC.nil
	Vk.Ist.destroy @() ist Nothing
	GlfwB.destroyWindow win
	GlfwB.terminate
