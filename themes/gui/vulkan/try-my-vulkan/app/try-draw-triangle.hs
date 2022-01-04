{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.String
import Control.Monad
import Control.Monad.Fix
import Data.Foldable
import Data.Bits
import Data.Bool
import Data.Maybe
import Data.List
import Data.Word

import qualified Data.Set as Set

import qualified Graphics.UI.GLFW as GlfwB
import qualified Vulkan as Vk
import qualified Vulkan.Base as Vk
import qualified Vulkan.Format as Vk
import qualified Vulkan.Image as Vk
import qualified Vulkan.Ext as Vk.Ext
import qualified Vulkan.Ext.Internal as Vk.Ext.I

import qualified Vulkan.Instance as Vk
import qualified Vulkan.AllocationCallbacks as Vk
import qualified Vulkan.PhysicalDevice as Vk
import qualified Vulkan.Device as Vk
import qualified Vulkan.Device.Internal as Vk.I
import qualified Vulkan.Khr.Surface as Vk.Khr
import qualified Vulkan.Khr.Swapchain as Vk.Khr
import qualified Vulkan.Khr.Swapchain.Internal as Vk.Khr.I

import qualified Glfw as Glfw

import ThEnv

width, height :: Int
width = 800; height = 600

validationLayers :: [String]
validationLayers = [
	"VK_LAYER_KHRONOS_validation"
	]

deviceExtensions :: [String]
deviceExtensions = [
	Vk.Khr.swapchainExtensionName
	]

enableValidationLayers :: Bool
enableValidationLayers =
	maybe True (const False) $(lookupCompileEnvExp "NDEBUG")

main :: IO ()
main = run

run :: IO ()
run = do
	w <- initWindow
	(ist, dbgMssngr, dv, gq, sfc, sc) <- initVulkan w
	mainLoop w
	cleanup w ist dbgMssngr dv sfc sc

initWindow :: IO GlfwB.Window
initWindow = do
	True <- GlfwB.init
	GlfwB.windowHint $ GlfwB.WindowHint'ClientAPI GlfwB.ClientAPI'NoAPI
	GlfwB.windowHint $ GlfwB.WindowHint'Resizable False
	Just w <- GlfwB.createWindow width height "Vulkan" Nothing Nothing
	pure w

initVulkan :: GlfwB.Window -> IO (
	Vk.Instance, Maybe Vk.Ext.I.DebugUtilsMessenger, Vk.Device, Vk.Queue,
	Vk.Khr.Surface, Vk.Khr.I.Swapchain )
initVulkan w = do
	ist <- createInstance
	dbgMssngr <- if enableValidationLayers
		then Just <$> setupDebugMessenger ist
		else pure Nothing
	sfc <- createSurface ist w
	pd <- pickPhysicalDevice ist sfc
	(dv, gq, pq) <- createLogicalDevice pd sfc
	(sc, scis, scif, sce) <- createSwapChain w pd dv sfc
	pure (ist, dbgMssngr, dv, gq, sfc, sc)

createInstance :: IO Vk.Instance
createInstance = do
	cvls <- checkValidationLayerSupport
	when (enableValidationLayers && not cvls)
		$ error "validation layers requested, but not available!"
	putStrLn "available extensions:"
	mapM_ (putStrLn . ('\t' :) . showExtensionProperties)
		=<< Vk.enumerateInstanceExtensionProperties Nothing
	extensions <- getRequiredExtensions
	print extensions
	let	appInfo = Vk.ApplicationInfo {
			Vk.applicationInfoNext = Nothing :: Maybe Bool,
			Vk.applicationInfoApplicationName = "Hello Triangle",
			Vk.applicationInfoApplicationVersion =
				Vk.makeApiVersion 0 1 0 0,
			Vk.applicationInfoEngineName = "No Engine",
			Vk.applicationInfoEngineVersion = Vk.makeApiVersion 0 1 0 0,
			Vk.applicationInfoApiVersion = Vk.apiVersion1_0 }
		createInfo = Vk.InstanceCreateInfo {
			Vk.instanceCreateInfoNext = if enableValidationLayers
				then Just populateDebugMessengerCreateInfo else Nothing,
			Vk.instanceCreateInfoApplicationInfo = appInfo,
			Vk.instanceCreateInfoFlags = Vk.InstanceCreateFlagsZero,
			Vk.instanceCreateInfoEnabledLayers =
				if enableValidationLayers then validationLayers else [],
			Vk.instanceCreateInfoEnabledExtensions = extensions }
	i <- Vk.createInstance createInfo (Nothing :: Maybe (Vk.AllocationCallbacks ()))
	print i
	pure i

checkValidationLayerSupport :: IO Bool
checkValidationLayerSupport = do
	availableLayers <- Vk.enumerateInstanceLayerProperties
	pure . null $ validationLayers \\
		(Vk.layerPropertiesLayerName <$> availableLayers)

getRequiredExtensions :: IO [String]
getRequiredExtensions = do
	glfwExtensions <- GlfwB.getRequiredInstanceExtensions
	bool id (Vk.Ext.debugUtilsExtensionName :) enableValidationLayers
		<$> peekCString `mapM` glfwExtensions

setupDebugMessenger :: Vk.Instance -> IO Vk.Ext.I.DebugUtilsMessenger
setupDebugMessenger ist = Vk.Ext.createDebugUtilsMessenger
	@() @() @() ist populateDebugMessengerCreateInfo Nothing

populateDebugMessengerCreateInfo :: Vk.Ext.DebugUtilsMessengerCreateInfo () ()
populateDebugMessengerCreateInfo = Vk.Ext.DebugUtilsMessengerCreateInfo {
	Vk.Ext.debugUtilsMessengerCreateInfoNext = Nothing,
	Vk.Ext.debugUtilsMessengerCreateInfoFlags =
		Vk.Ext.I.DebugUtilsMessengerCreateFlagsZero,
	Vk.Ext.debugUtilsMessengerCreateInfoMessageSeverity =
		Vk.Ext.I.DebugUtilsMessageSeverityVerboseBit .|.
		Vk.Ext.I.DebugUtilsMessageSeverityWarningBit .|.
		Vk.Ext.I.DebugUtilsMessageSeverityErrorBit,
	Vk.Ext.debugUtilsMessengerCreateInfoMessageType =
		Vk.Ext.I.DebugUtilsMessageTypeGeneralBit .|.
		Vk.Ext.I.DebugUtilsMessageTypeValidationBit .|.
		Vk.Ext.I.DebugUtilsMessageTypePerformanceBit,
	Vk.Ext.debugUtilsMessengerCreateInfoFnUserCallback = debugCallback,
	Vk.Ext.debugUtilsMessengerCreateInfoUserData = Nothing }

debugCallback :: Vk.Ext.FnDebugUtilsMessengerCallback ()
debugCallback _messageSeverity _messageType callbackData _userData =
	putStrLn $ "validation layer: " ++
		Vk.Ext.debugUtilsMessengerCallbackDataMessage callbackData

createSurface :: Vk.Instance -> GlfwB.Window -> IO Vk.Khr.Surface
createSurface ist w = Glfw.createWindowSurface @() ist w Nothing

pickPhysicalDevice :: Vk.Instance -> Vk.Khr.Surface -> IO Vk.PhysicalDevice
pickPhysicalDevice ist sfc = do
	devices <- Vk.enumeratePhysicalDevices ist
	when (null devices) $ error "failed to find GPUs with Vulkan support!"
	physicalDevice <- head . (++ [Vk.PhysicalDeviceNullHandle])
		<$> filterM (`isDeviceSuitable` sfc) devices
	case physicalDevice of
		Vk.PhysicalDeviceNullHandle ->
			error "failed to find a suitable GPU!"
		_ -> pure physicalDevice

isDeviceSuitable :: Vk.PhysicalDevice -> Vk.Khr.Surface -> IO Bool
isDeviceSuitable device sfc = do
	deviceProperties <- Vk.getPhysicalDeviceProperties device
	print deviceProperties
	(putStrLn `mapM_`) . (convertHead ' ' '\t' <$>) . devideWithComma . show $ Vk.physicalDevicePropertiesLimits deviceProperties
	deviceFeatures <- Vk.getPhysicalDeviceFeatures device
	print deviceFeatures

	print $ Vk.physicalDevicePropertiesDeviceType deviceProperties
	print $ Vk.physicalDeviceFeaturesGeometryShader deviceFeatures

	indices <- findQueueFamilies device sfc

	extensionSupported <- checkDeviceExtensionSupport device

	if extensionSupported
		then do	swapChainSupport <- querySwapChainSupport device sfc
			let	swapChainAdequate =
					not (null $ swapChainSupportDetailsFormats
						swapChainSupport) &&
					not (null $ swapChainSupportDetailsPresentModes
						swapChainSupport)
			pure $ queueFamilyIndicesIsComplete indices && swapChainAdequate
		else pure False

checkDeviceExtensionSupport :: Vk.PhysicalDevice -> IO Bool
checkDeviceExtensionSupport device = do
	putStr "ENUMERATE DEVICE EXTENSION PROPERTIES: "
	availableExtensions <- Vk.enumerateDeviceExtensionProperties device Nothing
	(\x -> putStrLn "PhysicalDeviceProperties: " >> mapM_ (mapM_ putStrLn . devideWithComma . show) x)
		availableExtensions
	putStrLn "ENUMERATE DEVICE EXTENSION PROPERTIES END"
	pure . null $ deviceExtensions \\
		(Vk.extensionPropertiesExtensionName <$> availableExtensions)

data QueueFamilyIndices = QueueFamilyIndices {
	queueFamilyIndicesGraphicsFamily :: Maybe Word32,
	queueFamilyIndicesPresentFamily :: Maybe Word32 }
	deriving Show

queueFamilyIndicesIsComplete :: QueueFamilyIndices -> Bool
queueFamilyIndicesIsComplete is =
	isJust (queueFamilyIndicesGraphicsFamily is) &&
	isJust (queueFamilyIndicesPresentFamily is)

bool32ToBool :: Vk.Bool32 -> Bool
bool32ToBool = \case Vk.False -> False; _ -> True

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM f xs = listToMaybe <$> filterM f xs

findMBool32 :: Monad m => (a -> m Vk.Bool32) -> [a] -> m (Maybe a)
findMBool32 f = findM $ (bool32ToBool <$>) . f

findQueueFamilies :: Vk.PhysicalDevice -> Vk.Khr.Surface -> IO QueueFamilyIndices
findQueueFamilies device sfc = do
	queueFamilies <- Vk.getPhysicalDeviceQueueFamilyProperties device
	pfi <- findMBool32 (\i -> Vk.Khr.getPhysicalDeviceSurfaceSupport device i sfc)
		[0 .. genericLength queueFamilies - 1]
	let	qfi = QueueFamilyIndices {
			queueFamilyIndicesGraphicsFamily = fromIntegral <$> findIndex
				((/= zeroBits)
					. (.&. Vk.QueueGraphicsBit)
					. Vk.queueFamilyPropertiesQueueFlags)
				queueFamilies,
			queueFamilyIndicesPresentFamily = pfi
			}
	print qfi
	pure qfi

devideWithComma :: String -> [String]
devideWithComma = \case
	"" -> [""]
	'[' : cs -> let
		(bd, ']' : rst) = span (/= ']') cs
		str : strs = devideWithComma rst in ('[' : bd ++ "]" ++ str) : strs
	'{' : cs -> let
		(bd, '}' : rst) = span (/= '}') cs
		str : strs = devideWithComma rst in ('{' : bd ++ "}" ++ str) : strs
	',' : cs -> "" : devideWithComma cs
	c : cs -> let str : strs = devideWithComma cs in (c : str) : strs

convertHead :: Eq a => a -> a -> [a] -> [a]
convertHead s d = \case
	[] -> []
	c : cs	| c == s -> d : cs
		| otherwise -> c : cs

data SwapChainSupportDetails = SwapChainSupportDetails {
	swapChainSupportDetailsCapabilities :: Vk.Khr.SurfaceCapabilities,
	swapChainSupportDetailsFormats :: [Vk.Khr.SurfaceFormat],
	swapChainSupportDetailsPresentModes :: [Vk.Khr.PresentMode] }
	deriving Show

querySwapChainSupport :: Vk.PhysicalDevice -> Vk.Khr.Surface -> IO SwapChainSupportDetails
querySwapChainSupport device surface = SwapChainSupportDetails
	<$> Vk.Khr.getPhysicalDeviceSurfaceCapabilities device surface
	<*> Vk.Khr.getPhysicalDeviceSurfaceFormats device surface
	<*> Vk.Khr.getPhysicalDeviceSurfacePresentModes device surface

createLogicalDevice :: Vk.PhysicalDevice -> Vk.Khr.Surface -> IO (Vk.Device, Vk.Queue, Vk.Queue)
createLogicalDevice pd sfc = do
	indices <- findQueueFamilies pd sfc
	let	uniqueQueueFamilies = Set.fromList [
			queueFamilyIndicesGraphicsFamily indices,
			queueFamilyIndicesPresentFamily indices ]
		queueCreateInfos = (<$> toList uniqueQueueFamilies) \i -> Vk.DeviceQueueCreateInfo {
			Vk.deviceQueueCreateInfoQueueFamilyIndex = fromJust i,
			Vk.deviceQueueCreateInfoNext = Nothing,
			Vk.deviceQueueCreateInfoFlags =
				Vk.I.DeviceQueueCreateFlagBitsZero,
			Vk.deviceQueueCreateInfoQueuePriorities = [1.0] }
		deviceFeatures = Vk.PhysicalDeviceFeatures {
			Vk.physicalDeviceFeaturesRobutBufferAccess = Vk.False,
			Vk.physicalDeviceFeaturesFullDrawIndexUint32 = Vk.False,
			Vk.physicalDeviceFeaturesImageCubeArray = Vk.False,
			Vk.physicalDeviceFeaturesIndependentBlend = Vk.False,
			Vk.physicalDeviceFeaturesGeometryShader = Vk.False,
			Vk.physicalDeviceFeaturesTessellationShader = Vk.False,
			Vk.physicalDeviceFeaturesSampleRateShading = Vk.False,
			Vk.physicalDeviceFeaturesDualSrcBlend = Vk.False,
			Vk.physicalDeviceFeaturesLogicOp = Vk.False,
			Vk.physicalDeviceFeaturesMultiDrawIndirect = Vk.False,
			Vk.physicalDeviceFeaturesDrawIndirectFirstInstance = Vk.False,
			Vk.physicalDeviceFeaturesDepthClamp = Vk.False,
			Vk.physicalDeviceFeaturesDepthBiasClamp = Vk.False,
			Vk.physicalDeviceFeaturesFillModeNonSolid = Vk.False,
			Vk.physicalDeviceFeaturesDepthBounds = Vk.False,
			Vk.physicalDeviceFeaturesWideLines = Vk.False,
			Vk.physicalDeviceFeaturesLargePoints = Vk.False,
			Vk.physicalDeviceFeaturesAlphaToOne = Vk.False,
			Vk.physicalDeviceFeaturesMultiViewport = Vk.False,
			Vk.physicalDeviceFeaturesSamplerAnisotropy = Vk.False,
			Vk.physicalDeviceFeaturesTextureCompressionEtc2 =
				Vk.False,
			Vk.physicalDeviceFeaturesTextureCompressionAstcLdr =
				Vk.False,
			Vk.physicalDeviceFeaturesTextureCompressionBc = Vk.False,
			Vk.physicalDeviceFeaturesOcclusionQueryPrecise = Vk.False,
			Vk.physicalDeviceFeaturesPipelineStatisticsQuery =
				Vk.False,
			Vk.physicalDeviceFeaturesVertexPipelineStoresAndAtomics
				= Vk.False,
			Vk.physicalDeviceFeaturesFragmentStoresAndAtomics =
				Vk.False,
			Vk.physicalDeviceFeaturesShaderTessellationAndGeometryPointSize
				= Vk.False,
			Vk.physicalDeviceFeaturesShaderImageGatherExtended = Vk.False,
			Vk.physicalDeviceFeaturesShaderStorageImageExtendedFormats
				= Vk.False,
			Vk.physicalDeviceFeaturesShaderStorageImageMultisample =
				Vk.False,
			Vk.physicalDeviceFeaturesShaderStorageImageReadWithoutFormat
				= Vk.False,
			Vk.physicalDeviceFeaturesShaderStorageImageWriteWithoutFormat
				= Vk.False,
			Vk.physicalDeviceFeaturesShaderUniformBufferArrayDynamicIndexing
				= Vk.False,
			Vk.physicalDeviceFeaturesShaderSampledImageArrayDynamicIndexing
				= Vk.False,
			Vk.physicalDeviceFeaturesShaderStorageBufferArrayDynamicIndexing
				= Vk.False,
			Vk.physicalDeviceFeaturesShaderStorageImageArrayDynamicIndexing
				= Vk.False,
			Vk.physicalDeviceFeaturesShaderClipDistance = Vk.False,
			Vk.physicalDeviceFeaturesShaderCullDistance = Vk.False,
			Vk.physicalDeviceFeaturesShaderFloat64 = Vk.False,
			Vk.physicalDeviceFeaturesShaderInt64 = Vk.False,
			Vk.physicalDeviceFeaturesShaderInt16 = Vk.False,
			Vk.physicalDeviceFeaturesShaderResourceResidency =
				Vk.False,
			Vk.physicalDeviceFeaturesShaderResourceMinLod =
				Vk.False,
			Vk.physicalDeviceFeaturesSparseBinding = Vk.False,
			Vk.physicalDeviceFeaturesSparseResidencyBuffer =
				Vk.False,
			Vk.physicalDeviceFeaturesSparseResidencyImage2D =
				Vk.False,
			Vk.physicalDeviceFeaturesSparseResidencyImage3D =
				Vk.False,
			Vk.physicalDeviceFeaturesSparseResidency2Samples =
				Vk.False,
			Vk.physicalDeviceFeaturesSparseResidency4Samples =
				Vk.False,
			Vk.physicalDeviceFeaturesSparseResidency8Samples =
				Vk.False,
			Vk.physicalDeviceFeaturesSparseResidency16Samples =
				Vk.False,
			Vk.physicalDeviceFeaturesSparseResidencyAliased =
				Vk.False,
			Vk.physicalDeviceFeaturesVariableMultisampleRate =
				Vk.False,
			Vk.physicalDeviceFeaturesInheritedQueries = Vk.False }
		createInfo = Vk.DeviceCreateInfo {
			Vk.deviceCreateInfoNext = Nothing,
			Vk.deviceCreateInfoFlags =
				Vk.I.DeviceCreateFlagBitsZero,
			Vk.deviceCreateInfoQueueCreateInfos = queueCreateInfos,
			Vk.deviceCreateInfoEnabledLayerNames =
				if enableValidationLayers
					then validationLayers else [],
			Vk.deviceCreateInfoEnabledExtensionNames = deviceExtensions,
			Vk.deviceCreateInfoEnabledFeatures = deviceFeatures }
	dv <- Vk.createDevice @() @() @() pd createInfo Nothing
	gq <- Vk.getDeviceQueue
		dv (fromJust $ queueFamilyIndicesGraphicsFamily indices) 0
	pq <- Vk.getDeviceQueue
		dv (fromJust $ queueFamilyIndicesPresentFamily indices) 0
	pure (dv, gq, pq)

createSwapChain ::
	GlfwB.Window -> Vk.PhysicalDevice -> Vk.Device -> Vk.Khr.Surface ->
	IO (Vk.Khr.I.Swapchain, [Vk.Image], Vk.Format, Vk.Extent2D)
createSwapChain win pDevice device surface = do
	swapChainSupport <- querySwapChainSupport pDevice surface
	let	surfaceFormat = chooseSwapSurfaceFormat
			$ swapChainSupportDetailsFormats swapChainSupport
		presentMode = chooseSwapPresentMode
			$ swapChainSupportDetailsPresentModes swapChainSupport
		capabilities =
			swapChainSupportDetailsCapabilities swapChainSupport
		minImageCountPlus1 =
			Vk.Khr.surfaceCapabilitiesMinImageCount capabilities + 1
		maxImageCount =
			Vk.Khr.surfaceCapabilitiesMaxImageCount capabilities
		imageCount = bool minImageCountPlus1 maxImageCount $
			0 < maxImageCount && maxImageCount < minImageCountPlus1
	extent <- chooseSwapExtent win capabilities
	putStrLn "createSwapChain: surfaceFormat"
	print surfaceFormat
	print $ swapChainSupportDetailsPresentModes swapChainSupport
	print presentMode
	indices <- findQueueFamilies pDevice surface
	let	graphicsFamily = fromJust $ queueFamilyIndicesGraphicsFamily indices
		presentFamily = fromJust $ queueFamilyIndicesPresentFamily indices
		queueFamilyIndices = [graphicsFamily, presentFamily]
		(ism, qfis) = if graphicsFamily /= presentFamily
			then (Vk.SharingModeConcurrent, queueFamilyIndices)
			else (Vk.SharingModeExclusive, [])
		createInfo = Vk.Khr.SwapchainCreateInfo {
			Vk.Khr.swapchainCreateInfoNext = Nothing,
			Vk.Khr.swapchainCreateInfoFlags =
				Vk.Khr.I.SwapchainCreateFlagBitsZero,
			Vk.Khr.swapchainCreateInfoSurface = surface,
			Vk.Khr.swapchainCreateInfoMinImageCount = imageCount,
			Vk.Khr.swapchainCreateInfoImageFormat =
				Vk.Khr.surfaceFormatFormat surfaceFormat,
			Vk.Khr.swapchainCreateInfoImageColorSpace =
				Vk.Khr.surfaceFormatColorSpace surfaceFormat,
			Vk.Khr.swapchainCreateInfoImageExtent = extent,
--			Vk.Khr.swapchainCreateInfoImageExtent = Vk.Extent2D 0 0,
			Vk.Khr.swapchainCreateInfoImageArrayLayers = 1,
			Vk.Khr.swapchainCreateInfoImageUsage =
				Vk.ImageUsageColorAttachmentBit,
			Vk.Khr.swapchainCreateInfoImageSharingMode = ism,
			Vk.Khr.swapchainCreateInfoQueueFamilyIndices = qfis,
			Vk.Khr.swapchainCreateInfoPreTransform =
				Vk.Khr.surfaceCapabilitiesCurrentTransform
					capabilities,
			Vk.Khr.swapchainCreateInfoCompositeAlpha =
				Vk.Khr.CompositeAlphaOpaqueBit,
			Vk.Khr.swapchainCreateInfoPresentMode = presentMode,
			Vk.Khr.swapchainCreateInfoClipped = True,
			Vk.Khr.swapchainCreateInfoOldSwapchain = Nothing }
	swapChain <- Vk.Khr.createSwapchain @() @() device createInfo Nothing
	swapChainImages <- Vk.Khr.getSwapchainImages device swapChain
	pure (swapChain, swapChainImages,
		Vk.Khr.surfaceFormatFormat surfaceFormat, extent)

chooseSwapSurfaceFormat :: [Vk.Khr.SurfaceFormat] -> Vk.Khr.SurfaceFormat
chooseSwapSurfaceFormat [] = error "no swap surface format"
chooseSwapSurfaceFormat availableFormats@(af0 : _)  =
	bool af0  af1 $ elem af1 availableFormats
	where af1 = Vk.Khr.SurfaceFormat
		Vk.FormatB8g8r8a8Srgb Vk.Khr.ColorSpaceSrgbNonlinear

chooseSwapPresentMode :: [Vk.Khr.PresentMode] -> Vk.Khr.PresentMode
chooseSwapPresentMode availablePresentModes =
	bool Vk.Khr.PresentModeFifo Vk.Khr.PresentModeMailbox
		$ elem Vk.Khr.PresentModeMailbox availablePresentModes

chooseSwapExtent :: GlfwB.Window -> Vk.Khr.SurfaceCapabilities -> IO Vk.Extent2D
chooseSwapExtent win capabilities = if cw < Vk.uint32Max then pure ce else do
	(gw, gh) <- GlfwB.getFramebufferSize win
	pure $ Vk.Extent2D (clamp mnw mxw cw) (clamp mnh mxh ch)
	where
	ce@(Vk.Extent2D cw ch) =
		Vk.Khr.surfaceCapabilitiesCurrentExtent capabilities
	Vk.Extent2D mnw mnh =
		Vk.Khr.surfaceCapabilitiesMinImageExtent capabilities
	Vk.Extent2D mxw mxh =
		Vk.Khr.surfaceCapabilitiesMaxImageExtent capabilities

clamp :: Ord a => a -> a -> a -> a
clamp mn mx x | x <= mn = mn | mx <= x = mx | otherwise = x

mainLoop :: GlfwB.Window -> IO ()
mainLoop w = do
	fix \loop -> bool (pure ()) loop =<< do
		GlfwB.pollEvents
		not <$> GlfwB.windowShouldClose w

cleanup :: GlfwB.Window -> Vk.Instance -> Maybe Vk.Ext.I.DebugUtilsMessenger ->
	Vk.Device -> Vk.Khr.Surface -> Vk.Khr.I.Swapchain -> IO ()
cleanup w ist mdbgMssngr dv sfc sc = do
	Vk.Khr.destroySwapchain @() dv sc Nothing
	Vk.destroyDevice @() dv Nothing
	maybe (pure ())
		(\dm -> Vk.Ext.destroyDebugUtilsMessenger @() ist dm Nothing)
		mdbgMssngr
	Vk.Khr.destroySurface @() ist sfc Nothing
	Vk.destroyInstance ist (Nothing :: Maybe (Vk.AllocationCallbacks ()))
	GlfwB.destroyWindow w
	GlfwB.terminate

showExtensionProperties :: Vk.ExtensionProperties -> String
showExtensionProperties ep =
	Vk.extensionPropertiesExtensionName ep ++
	" (" ++ show (Vk.extensionPropertiesSpecVersion ep) ++ ")"
