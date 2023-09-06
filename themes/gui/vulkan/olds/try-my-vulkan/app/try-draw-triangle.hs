{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent

import Foreign.C.String
import Control.Monad
import Control.Monad.Fix
import Data.Foldable
import Data.Bits
import Data.Bool
import Data.Maybe
import Data.List
import Data.List.Length
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.Set as Set

import qualified Graphics.UI.GLFW as GlfwB
import qualified Vulkan as Vk
import qualified Vulkan.Base as Vk
import qualified Vulkan.Format as Vk
import qualified Vulkan.Image as Vk
import qualified Vulkan.ImageAspectFlagBits as Vk
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

import qualified Vulkan.Shader as Vk
import qualified Vulkan.Shader.Internal as Vk.I
import qualified Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShaderStage
import qualified Vulkan.Pipeline.ShaderStage.Internal as Vk.I
import qualified Vulkan.ShaderStageFlagBits as Vk

import qualified Vulkan.Pipeline.VertexInputState as Vk.Ppl.VertexInputState
import qualified Vulkan.Pipeline.VertexInputState.Internal as Vk.I

import qualified Vulkan.Pipeline.InputAssemblyState as
	Vk.Pipeline.InputAssemblyState
import qualified Vulkan.Pipeline.InputAssemblyState.Internal as
	Vk.Pipeline.InputAssemblyState.I
import qualified Vulkan.PrimitiveTopology as Vk

import qualified Vulkan.Viewport as Vk
import qualified Vulkan.Pipeline.ViewportState as Vk.Pipeline.ViewportState
import qualified Vulkan.Pipeline.ViewportState.Internal as
	Vk.Pipeline.ViewportState.I
import qualified Vulkan.Pipeline.RasterizationState as Vk.Ppl.RasterizSt
import qualified Vulkan.Pipeline.RasterizationState.Internal as
	Vk.Ppl.RasterizSt.I
import qualified Vulkan.PolygonMode as Vk
import qualified Vulkan.CullModeFlagBits as Vk
import qualified Vulkan.FrontFace as Vk

import qualified Vulkan.Pipeline.MultisampleState as Vk.Ppl.MultisampleSt
import qualified Vulkan.Pipeline.MultisampleState.Internal as
	Vk.Ppl.MultisampleSt.I
import qualified Vulkan.SampleCountFlagBits as Vk

import qualified Vulkan.Pipeline.ColorBlendState as Vk.Ppl.ClrBlendSt
import qualified Vulkan.Pipeline.ColorBlendState.Internal as Vk.Ppl.ClrBlendSt.I
import qualified Vulkan.ColorComponentFlagBits as Vk
import qualified Vulkan.BlendFactor as Vk
import qualified Vulkan.BlendOp as Vk
import qualified Vulkan.LogicOp as Vk

import qualified Vulkan.Pipeline.Layout as Vk.Ppl.Layout
import qualified Vulkan.Pipeline.Layout.Internal as Vk.Ppl.Layout.I
import qualified Vulkan.Pipeline.Layout as Vk (
	PipelineLayout, createPipelineLayout, destroyPipelineLayout )

import qualified Vulkan.RenderPass as Vk.RenderPass
import qualified Vulkan.RenderPass as Vk (SubpassDescription(..))
import qualified Vulkan.RenderPass.Internal as Vk.I
import qualified Vulkan.AttachmentLoadOp as Vk
import qualified Vulkan.AttachmentStoreOp as Vk
import qualified Vulkan.ImageLayout as Vk
import qualified Vulkan.AccessFlagBits as Vk
import qualified Vulkan.DependencyFlagBits as Vk

import qualified Vulkan.PipelineBindPoint as Vk
import qualified Vulkan.SubpassDescriptionFlagBits as Vk
import qualified Vulkan.AttachmentDescriptionFlagBits as Vk
import qualified Vulkan.RenderPassCreateFlagBits as Vk

import qualified Vulkan.Pipeline as Vk.Ppl
import qualified Vulkan.PipelineCreateFlagBits as Vk
import qualified Vulkan.Pipeline.Cache as Vk (pattern PipelineCacheNullHandle)

import qualified Vulkan.Framebuffer as Vk.Framebuffer
import qualified Vulkan.FramebufferCreateFlagBits as Vk

import qualified Vulkan.CommandPool as Vk.CommandPool
import qualified Vulkan.CommandPoolCreateFlagBits as Vk
import qualified Vulkan.CommandBuffer as Vk.CommandBuffer
import qualified Vulkan.CommandBufferLevel as Vk
import qualified Vulkan.CommandBufferUsageFlagBits as Vk
import qualified Vulkan.Clear as Vk.Clear
import qualified Vulkan.Command as Vk.Cmd
import qualified Vulkan.SubpassContents as Vk

import qualified Vulkan.Semaphore as Vk.Semaphore
import qualified Vulkan.Semaphore.Internal as Vk.Semaphore.I
import qualified Vulkan.Fence as Vk.Fence
import qualified Vulkan.Khr as Vk.Khr
import qualified Vulkan.Submit as Vk.Submit
import qualified Vulkan.PipelineStageFlagBits as Vk

import Vulkan.Submit (CommandBufferList(..))
import qualified Vulkan.Khr.Present as Vk.Khr.Present

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
	(ist, dbgMssngr, dv, gq, pq, sfc, sc, ivs, rp, ppl, gpl, scfbs, cp,
		cbs, ias, rfs) <- initVulkan w
	mainLoop w dv gq pq sc cbs ias rfs
	cleanup w ist dbgMssngr dv sfc sc ivs rp ppl gpl scfbs cp ias rfs

initWindow :: IO GlfwB.Window
initWindow = do
	True <- GlfwB.init
	GlfwB.windowHint $ GlfwB.WindowHint'ClientAPI GlfwB.ClientAPI'NoAPI
	GlfwB.windowHint $ GlfwB.WindowHint'Resizable False
	Just w <- GlfwB.createWindow width height "Vulkan" Nothing Nothing
	pure w

initVulkan :: GlfwB.Window -> IO (
	Vk.Instance, Maybe Vk.Ext.I.DebugUtilsMessenger, Vk.Device, Vk.Queue,
	Vk.Queue, Vk.Khr.Surface, Vk.Khr.I.Swapchain, [Vk.ImageView],
	Vk.RenderPass, Vk.PipelineLayout, Vk.Pipeline () '[],
	[Vk.Framebuffer.Framebuffer], Vk.CommandPool.CommandPool,
	[Vk.CommandBuffer.CommandBuffer () '[]],
	Vk.Semaphore.Semaphore, Vk.Semaphore.Semaphore )
initVulkan w = do
	ist <- createInstance
	dbgMssngr <- if enableValidationLayers
		then Just <$> setupDebugMessenger ist
		else pure Nothing
	sfc <- createSurface ist w
	pd <- pickPhysicalDevice ist sfc
	(dv, gq, pq) <- createLogicalDevice pd sfc
	(sc, scis, scif, sce) <- createSwapChain w pd dv sfc
	ivs <- createImageViews dv scif scis
	rp <- createRenderPass dv scif
	(ppl, gpl) <- createGraphicsPipeline dv sce rp
	scfbs <- createFramebuffers dv rp sce ivs
	cp <- createCommandPool pd dv sfc
	cbs <- createCommandBuffers dv sce rp gpl scfbs cp
	(ias, rfs) <- createSemaphores dv
	pure (ist, dbgMssngr, dv, gq, pq, sfc, sc, ivs, rp, ppl, gpl, scfbs, cp,
		cbs, ias, rfs)

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
	availableExtensions <- Vk.enumerateDeviceExtensionProperties device Nothing
	(\x -> putStrLn "PHYSICAL DEVICE PROPERTIES: " >> mapM_ (mapM_ putStrLn . devideWithComma . show) x)
		availableExtensions
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
	IO (Vk.Khr.I.Swapchain, [Vk.Image], Vk.Format, Vk.Extent2d)
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
--			Vk.Khr.swapchainCreateInfoImageExtent = Vk.Extent2d 0 0,
			Vk.Khr.swapchainCreateInfoImageArrayLayers = 1,
			Vk.Khr.swapchainCreateInfoImageUsage =
				Vk.ImageUsageColorAttachmentBit,
			Vk.Khr.swapchainCreateInfoImageSharingMode = ism,
--			Vk.Khr.swapchainCreateInfoImageSharingMode = Vk.SharingModeConcurrent,
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

chooseSwapExtent :: GlfwB.Window -> Vk.Khr.SurfaceCapabilities -> IO Vk.Extent2d
chooseSwapExtent win capabilities = if cw < Vk.uint32Max then pure ce else do
	((fromIntegral -> gw), (fromIntegral -> gh)) <-
		GlfwB.getFramebufferSize win
	pure $ Vk.Extent2d (clamp mnw mxw gw) (clamp mnh mxh gh)
	where
	ce@(Vk.Extent2d cw ch) =
		Vk.Khr.surfaceCapabilitiesCurrentExtent capabilities
	Vk.Extent2d mnw mnh =
		Vk.Khr.surfaceCapabilitiesMinImageExtent capabilities
	Vk.Extent2d mxw mxh =
		Vk.Khr.surfaceCapabilitiesMaxImageExtent capabilities

clamp :: Ord a => a -> a -> a -> a
clamp mn mx x | x <= mn = mn | mx <= x = mx | otherwise = x

createImageViews :: Vk.Device -> Vk.Format -> [Vk.Image] -> IO [Vk.ImageView]
createImageViews dvc scif imgs = createImageView1 dvc scif `mapM` imgs

createImageView1 :: Vk.Device -> Vk.Format -> Vk.Image -> IO Vk.ImageView
createImageView1 dvc scif img = do
	let	createInfo = Vk.ImageViewCreateInfo {
			Vk.imageViewCreateInfoNext = Nothing,
			Vk.imageViewCreateInfoFlags =
				Vk.ImageViewCreateFlagsZero,
			Vk.imageViewCreateInfoImage = img,
			Vk.imageViewCreateInfoViewType = Vk.ImageViewType2d,
			Vk.imageViewCreateInfoFormat = scif,
			Vk.imageViewCreateInfoComponents = Vk.ComponentMapping {
				Vk.componentMappingR = Vk.ComponentSwizzleIdentity,
				Vk.componentMappingG = Vk.ComponentSwizzleIdentity,
				Vk.componentMappingB = Vk.ComponentSwizzleIdentity,
				Vk.componentMappingA = Vk.ComponentSwizzleIdentity },
			Vk.imageViewCreateInfoSubresourceRange = Vk.ImageSubresourceRange {
				Vk.imageSubresourceRangeAspectMask = Vk.ImageAspectColorBit,
				Vk.imageSubresourceRangeBaseMipLevel = 0,
				Vk.imageSubresourceRangeLevelCount = 1,
				Vk.imageSubresourceRangeBaseArrayLayer = 0,
				Vk.imageSubresourceRangeLayerCount = 1 } }
	Vk.createImageView @() @() dvc createInfo Nothing

createRenderPass :: Vk.Device -> Vk.Format -> IO Vk.RenderPass
createRenderPass dvc scif = do
	let	colorAttachment = Vk.I.AttachmentDescription {
			Vk.I.attachmentDescriptionFlags =
				Vk.AttachmentDescriptionFlagsZero,
			Vk.I.attachmentDescriptionFormat = scif,
			Vk.I.attachmentDescriptionSamples = Vk.SampleCount1Bit,
			Vk.I.attachmentDescriptionLoadOp =
				Vk.AttachmentLoadOpClear,
			Vk.I.attachmentDescriptionStoreOp =
				Vk.AttachmentStoreOpStore,
			Vk.I.attachmentDescriptionStencilLoadOp =
				Vk.AttachmentLoadOpDontCare,
			Vk.I.attachmentDescriptionStencilStoreOp =
				Vk.AttachmentStoreOpDontCare,
			Vk.I.attachmentDescriptionInitialLayout =
				Vk.ImageLayoutUndefined,
			Vk.I.attachmentDescriptionFinalLayout =
				Vk.ImageLayoutPresentSrcKhr }
		colorAttachmentRef = Vk.I.AttachmentReference {
			Vk.I.attachmentReferenceAttachment = 0,
			Vk.I.attachmentReferenceLayout =
				Vk.ImageLayoutColorAttachmentOptimal }
		subpass = Vk.SubpassDescription {
			Vk.subpassDescriptionFlags =
				Vk.SubpassDescriptionFlagsZero,
			Vk.subpassDescriptionPipelineBindPoint =
				Vk.PipelineBindPointGraphics,
			Vk.subpassDescriptionColorAndResolveAttachments =
				Left [colorAttachmentRef],
			Vk.subpassDescriptionInputAttachments = [],
			Vk.subpassDescriptionDepthStencilAttachment = Nothing,
			Vk.subpassDescriptionPreserveAttachments = [] }
		dependency = Vk.I.SubpassDependency {
			Vk.I.subpassDependencySrcSubpass = Vk.subpassExternal,
			Vk.I.subpassDependencyDstSubpass = 0,
			Vk.I.subpassDependencySrcStageMask =
				Vk.PipelineStageColorAttachmentOutputBit,
			Vk.I.subpassDependencySrcAccessMask =
				Vk.AccessFlagsZero,
			Vk.I.subpassDependencyDstStageMask =
				Vk.PipelineStageColorAttachmentOutputBit,
			Vk.I.subpassDependencyDstAccessMask =
				Vk.AccessColorAttachmentWriteBit,
			Vk.I.subpassDependencyDependencyFlags =
				Vk.DependencyFlagsZero }
		renderPassInfo = Vk.RenderPass.CreateInfo {
			Vk.RenderPass.createInfoNext = Nothing,
			Vk.RenderPass.createInfoFlags =
				Vk.RenderPassCreateFlagsZero,
			Vk.RenderPass.createInfoAttachments = [colorAttachment],
			Vk.RenderPass.createInfoSubpasses = [subpass],
			Vk.RenderPass.createInfoDependencies = [dependency] }
	Vk.RenderPass.create @() @() dvc renderPassInfo Nothing

createGraphicsPipeline ::
	Vk.Device -> Vk.Extent2d -> Vk.RenderPass ->
	IO (Vk.PipelineLayout, Vk.Pipeline () '[])
createGraphicsPipeline dvc sce rp = do
	vertShaderCode <- BS.readFile "shaders/vert.spv"
	fragShaderCode <- BS.readFile "shaders/frag.spv"
	vertShaderModule <- createShaderModule dvc vertShaderCode
	fragShaderModule <- createShaderModule dvc fragShaderCode
	let	vertShaderStageInfo = Vk.Ppl.ShaderStage.CreateInfo {
			Vk.Ppl.ShaderStage.createInfoNext = Nothing,
			Vk.Ppl.ShaderStage.createInfoFlags =
				Vk.I.PipelineShaderStageCreateFlagBitsZero,
			Vk.Ppl.ShaderStage.createInfoStage =
				Vk.ShaderStageVertexBit,
			Vk.Ppl.ShaderStage.createInfoModule = vertShaderModule,
			Vk.Ppl.ShaderStage.createInfoName = "main",
			Vk.Ppl.ShaderStage.createInfoSpecializationInfo =
				Nothing }
		fragShaderStageInfo = Vk.Ppl.ShaderStage.CreateInfo {
			Vk.Ppl.ShaderStage.createInfoNext = Nothing,
			Vk.Ppl.ShaderStage.createInfoFlags =
				Vk.I.PipelineShaderStageCreateFlagBitsZero,
			Vk.Ppl.ShaderStage.createInfoStage =
				Vk.ShaderStageFragmentBit,
			Vk.Ppl.ShaderStage.createInfoModule = fragShaderModule,
			Vk.Ppl.ShaderStage.createInfoName = "main",
			Vk.Ppl.ShaderStage.createInfoSpecializationInfo =
				Nothing }
		shaderStages = [vertShaderStageInfo, fragShaderStageInfo]
		vertexInputInfo :: Vk.Ppl.VertexInputState.CreateInfo () () '[]
		vertexInputInfo = Vk.Ppl.VertexInputState.CreateInfo {
			Vk.Ppl.VertexInputState.createInfoNext = Nothing,
			Vk.Ppl.VertexInputState.createInfoFlags =
				Vk.I.PipelineVertexInputStateCreateFlagsZero }
		inputAssembly = Vk.Pipeline.InputAssemblyState.CreateInfo {
			Vk.Pipeline.InputAssemblyState.createInfoNext = Nothing,
			Vk.Pipeline.InputAssemblyState.createInfoFlags =
				Vk.Pipeline.InputAssemblyState.I.CreateFlagsZero,
			Vk.Pipeline.InputAssemblyState.createInfoTopology =
				Vk.PrimitiveTopologyTriangleList,
			Vk.Pipeline.InputAssemblyState.createInfoPrimitiveRestartEnable =
				False }
		viewport = Vk.Viewport {
			Vk.viewportX = 0, Vk.viewportY = 0,
			Vk.viewportWidth = fromIntegral $ Vk.extent2dWidth sce,
			Vk.viewportHeight =
				fromIntegral $ Vk.extent2dHeight sce,
			Vk.viewportMinDepth = 0, Vk.viewportMaxDepth = 1 }
		scissor = Vk.Rect2d {
			Vk.rect2dOffset = Vk.Offset2d 0 0,
			Vk.rect2dExtent = sce }
		viewportState = Vk.Pipeline.ViewportState.CreateInfo {
			Vk.Pipeline.ViewportState.createInfoNext = Nothing,
			Vk.Pipeline.ViewportState.createInfoFlags =
				Vk.Pipeline.ViewportState.I.CreateFlagsZero,
			Vk.Pipeline.ViewportState.createInfoViewports =
				[viewport],
			Vk.Pipeline.ViewportState.createInfoScissors =
				[scissor] }
		rasterizer = Vk.Ppl.RasterizSt.CreateInfo {
			Vk.Ppl.RasterizSt.createInfoNext = Nothing,
			Vk.Ppl.RasterizSt.createInfoFlags =
				Vk.Ppl.RasterizSt.I.CreateFlagsZero,
			Vk.Ppl.RasterizSt.createInfoDepthClampEnable = False,
			Vk.Ppl.RasterizSt.createInfoRasterizerDiscardEnable =
				False,
			Vk.Ppl.RasterizSt.createInfoPolygonMode =
				Vk.PolygonModeFill,
			Vk.Ppl.RasterizSt.createInfoLineWidth = 1.0,
			Vk.Ppl.RasterizSt.createInfoCullMode =
				Vk.CullModeBackBit,
			Vk.Ppl.RasterizSt.createInfoFrontFace =
				Vk.FrontFaceClockwise,
			Vk.Ppl.RasterizSt.createInfoDepthBiasEnable = False,
			Vk.Ppl.RasterizSt.createInfoDepthBiasConstantFactor = 0,
			Vk.Ppl.RasterizSt.createInfoDepthBiasClamp = 0,
			Vk.Ppl.RasterizSt.createInfoDepthBiasSlopeFactor = 0 }
		multisampling = Vk.Ppl.MultisampleSt.CreateInfo {
			Vk.Ppl.MultisampleSt.createInfoNext = Nothing,
			Vk.Ppl.MultisampleSt.createInfoFlags =
				Vk.Ppl.MultisampleSt.I.CreateFlagsZero,
			Vk.Ppl.MultisampleSt.createInfoSampleShadingEnable =
				False,
			Vk.Ppl.MultisampleSt.createInfoRasterizationSamples =
				Vk.SampleCount1Bit,
			Vk.Ppl.MultisampleSt.createInfoMinSampleShading = 1,
			Vk.Ppl.MultisampleSt.createInfoSampleMasks = [],
			Vk.Ppl.MultisampleSt.createInfoAlphaToCoverageEnable =
				False,
			Vk.Ppl.MultisampleSt.createInfoAlphaToOneEnable = False
			}
		colorBlendAttachment = Vk.Ppl.ClrBlendSt.I.AttachmentState {
			Vk.Ppl.ClrBlendSt.I.attachmentStateColorWriteMask =
				Vk.ColorComponentRBit .|.
				Vk.ColorComponentGBit .|.
				Vk.ColorComponentBBit .|.
				Vk.ColorComponentABit,
			Vk.Ppl.ClrBlendSt.I.attachmentStateBlendEnable =
				False,
			Vk.Ppl.ClrBlendSt.I.attachmentStateSrcColorBlendFactor =
				Vk.BlendFactorOne,
			Vk.Ppl.ClrBlendSt.I.attachmentStateDstColorBlendFactor =
				Vk.BlendFactorZero,
			Vk.Ppl.ClrBlendSt.I.attachmentStateColorBlendOp =
				Vk.BlendOpAdd,
			Vk.Ppl.ClrBlendSt.I.attachmentStateSrcAlphaBlendFactor =
				Vk.BlendFactorOne,
			Vk.Ppl.ClrBlendSt.I.attachmentStateDstAlphaBlendFactor =
				Vk.BlendFactorZero,
			Vk.Ppl.ClrBlendSt.I.attachmentStateAlphaBlendOp =
				Vk.BlendOpAdd }
		colorBlending = Vk.Ppl.ClrBlendSt.CreateInfo {
			Vk.Ppl.ClrBlendSt.createInfoNext = Nothing,
			Vk.Ppl.ClrBlendSt.createInfoFlags =
				Vk.Ppl.ClrBlendSt.I.CreateFlagBitsZero,
			Vk.Ppl.ClrBlendSt.createInfoLogicOpEnable = False,
			Vk.Ppl.ClrBlendSt.createInfoLogicOp = Vk.LogicOpCopy,
			Vk.Ppl.ClrBlendSt.createInfoAttachments =
				[colorBlendAttachment],
			Vk.Ppl.ClrBlendSt.createInfoBlendConstants =
				0 :. 0 :. 0 :. 0 :. NilL }
		pipelineLayoutInfo = Vk.Ppl.Layout.CreateInfo {
			Vk.Ppl.Layout.createInfoNext = Nothing,
			Vk.Ppl.Layout.createInfoFlags =
				Vk.Ppl.Layout.I.CreateFlagBitsZero,
			Vk.Ppl.Layout.createInfoSetLayouts = [],
			Vk.Ppl.Layout.createInfoPushConstantRanges = [] }
	pll <- Vk.createPipelineLayout @() @() dvc pipelineLayoutInfo Nothing
	let	pipelineInfo = Vk.Ppl.CreateInfo {
			Vk.Ppl.createInfoNext = Nothing,
			Vk.Ppl.createInfoFlags = Vk.PipelineCreateFlagsZero,
			Vk.Ppl.createInfoStages = shaderStages,
			Vk.Ppl.createInfoVertexInputState =
				Just vertexInputInfo,
			Vk.Ppl.createInfoInputAssemblyState =
				Just inputAssembly,
			Vk.Ppl.createInfoTessellationState = Nothing,
			Vk.Ppl.createInfoViewportState = Just viewportState,
			Vk.Ppl.createInfoRasterizationState = rasterizer,
			Vk.Ppl.createInfoMultisampleState = Just multisampling,
			Vk.Ppl.createInfoDepthStencilState = Nothing,
			Vk.Ppl.createInfoColorBlendState = Just colorBlending,
			Vk.Ppl.createInfoDynamicState = Nothing,
			Vk.Ppl.createInfoLayout = pll,
			Vk.Ppl.createInfoRenderPass = rp,
			Vk.Ppl.createInfoSubpass = 0,
			Vk.Ppl.createInfoBasePipelineHandle =
				Vk.PipelineNullHandle,
			Vk.Ppl.createInfoBasePipelineIndex = - 1
			}
	gpl <- Vk.Ppl.createSingle @() @() @() @() @() @() @() @() @() @() @() @()
		dvc Vk.PipelineCacheNullHandle pipelineInfo Nothing
	Vk.destroyShaderModule @() dvc fragShaderModule Nothing
	Vk.destroyShaderModule @() dvc vertShaderModule Nothing
	pure (pll, gpl)

createShaderModule :: Vk.Device -> BS.ByteString -> IO Vk.ShaderModule
createShaderModule dvc code = do
	let	createInfo = Vk.ShaderModuleCreateInfo {
			Vk.shaderModuleCreateInfoNext = Nothing,
			Vk.shaderModuleCreateInfoFlags =
				Vk.I.ShaderModuleCreateFlagsZero,
			Vk.shaderModuleCreateInfoCode = code }
	Vk.createShaderModule @() @() dvc createInfo Nothing

createFramebuffers ::
	Vk.Device -> Vk.RenderPass -> Vk.Extent2d ->
	[Vk.ImageView] -> IO [Vk.Framebuffer.Framebuffer]
createFramebuffers dvc rp sce = mapM $ createFramebuffer1 dvc rp sce

createFramebuffer1 ::
	Vk.Device -> Vk.RenderPass -> Vk.Extent2d ->
	Vk.ImageView -> IO Vk.Framebuffer.Framebuffer
createFramebuffer1 dvc rp sce iv = do
	let	framebufferInfo = Vk.Framebuffer.CreateInfo {
			Vk.Framebuffer.createInfoNext = Nothing,
			Vk.Framebuffer.createInfoFlags =
				Vk.FramebufferCreateFlagsZero,
			Vk.Framebuffer.createInfoRenderPass = rp,
			Vk.Framebuffer.createInfoAttachments = [iv],
			Vk.Framebuffer.createInfoWidth = Vk.extent2dWidth sce,
			Vk.Framebuffer.createInfoHeight = Vk.extent2dHeight sce,
			Vk.Framebuffer.createInfoLayers = 1 }
	Vk.Framebuffer.create @() @() dvc framebufferInfo Nothing

createCommandPool ::
	Vk.PhysicalDevice -> Vk.Device -> Vk.Khr.Surface ->
	IO Vk.CommandPool.CommandPool
createCommandPool pd dvc sfc = do
	queueFamilyIndices <- findQueueFamilies pd sfc
	let	poolInfo = Vk.CommandPool.CreateInfo {
			Vk.CommandPool.createInfoNext = Nothing,
			Vk.CommandPool.createInfoFlags =
				Vk.CommandPoolCreateFlagsZero,
			Vk.CommandPool.createInfoQueueFamilyIndex = fromJust
				$ queueFamilyIndicesGraphicsFamily
					queueFamilyIndices }
	Vk.CommandPool.create @() @() dvc poolInfo Nothing

createCommandBuffers ::
	Vk.Device -> Vk.Extent2d -> Vk.RenderPass -> Vk.Pipeline vs ts ->
	[Vk.Framebuffer.Framebuffer] -> Vk.CommandPool.CommandPool ->
	IO [Vk.CommandBuffer.CommandBuffer vs ts]
createCommandBuffers dvc sce rp gpl scfbs cp = do
	let	allocInfo = Vk.CommandBuffer.AllocateInfo {
			Vk.CommandBuffer.allocateInfoNext = Nothing,
			Vk.CommandBuffer.allocateInfoCommandPool = cp,
			Vk.CommandBuffer.allocateInfoLevel =
				Vk.CommandBufferLevelPrimary,
			Vk.CommandBuffer.allocateInfoCommandBufferCount =
				fromIntegral $ length scfbs }
	commandBuffers <- Vk.CommandBuffer.allocate @() dvc allocInfo
	uncurry (beginCommandBuffer1 sce rp gpl) `mapM_` zip scfbs commandBuffers
	pure commandBuffers

beginCommandBuffer1 :: Vk.Extent2d -> Vk.RenderPass -> Vk.Pipeline vs ts ->
	Vk.Framebuffer.Framebuffer -> Vk.CommandBuffer.CommandBuffer vs ts -> IO ()
beginCommandBuffer1 sce rp gpl fb cb = do
	let	beginInfo = Vk.CommandBuffer.BeginInfo {
			Vk.CommandBuffer.beginInfoNext = Nothing,
			Vk.CommandBuffer.beginInfoFlags =
				Vk.CommandBufferUsageFlagsZero,
			Vk.CommandBuffer.beginInfoInheritanceInfo = Nothing }
	Vk.CommandBuffer.begin @() @() cb beginInfo
	let	renderPassInfo = Vk.RenderPass.BeginInfo {
			Vk.RenderPass.beginInfoNext = Nothing,
			Vk.RenderPass.beginInfoRenderPass =  rp,
			Vk.RenderPass.beginInfoFramebuffer = fb,
			Vk.RenderPass.beginInfoRenderArea = Vk.Rect2d {
				Vk.rect2dOffset = Vk.Offset2d 0 0,
				Vk.rect2dExtent = sce },
			Vk.RenderPass.beginInfoClearValues =
				[clearColorValueFloatWhite] }
	Vk.Cmd.beginRenderPass @() cb renderPassInfo Vk.SubpassContentsInline
	Vk.Cmd.bindPipeline cb Vk.PipelineBindPointGraphics gpl
	Vk.Cmd.draw cb 3 1 0 0
	Vk.Cmd.endRenderPass cb
	Vk.CommandBuffer.end cb

clearColorValueFloatWhite :: Vk.Clear.Value
clearColorValueFloatWhite = Vk.Clear.fromColorValue
	$ Vk.Clear.fromColorValueFloat Vk.Clear.ColorValueFloat {
		Vk.Clear.colorValueFloatRgba = 1 :. 1 :. 0 :. 1 :. NilL }

createSemaphores :: Vk.Device -> IO (Vk.Semaphore.Semaphore, Vk.Semaphore.Semaphore)
createSemaphores dvc = do
	let	semaphoreInfo = Vk.Semaphore.CreateInfo {
			Vk.Semaphore.createInfoNext = Nothing,
			Vk.Semaphore.createInfoFlags =
				Vk.Semaphore.I.CreateFlagsZero }
	ias <- Vk.Semaphore.create @() @() dvc semaphoreInfo Nothing
	rfs <- Vk.Semaphore.create @() @() dvc semaphoreInfo Nothing
	pure (ias, rfs)

mainLoop ::
	GlfwB.Window -> Vk.Device -> Vk.Queue -> Vk.Queue ->
	Vk.Khr.I.Swapchain -> [Vk.CommandBuffer.CommandBuffer () '[]] ->
	Vk.Semaphore.Semaphore -> Vk.Semaphore.Semaphore -> IO ()
mainLoop w dvc gq pq sc cbs ias rfs = do
	fix \loop -> bool (pure ()) loop =<< do
		GlfwB.pollEvents
		drawFrame dvc gq pq sc cbs ias rfs
		threadDelay 1000000
		not <$> GlfwB.windowShouldClose w

drawFrame :: Vk.Device -> Vk.Queue -> Vk.Queue -> Vk.Khr.I.Swapchain ->
	[Vk.CommandBuffer.CommandBuffer () '[]] ->
	Vk.Semaphore.Semaphore -> Vk.Semaphore.Semaphore ->
	IO ()
drawFrame dvc gq pq sc cbs ias rfs = do
	imageIndex <- Vk.Khr.acquireNextImage
		dvc sc Vk.uint64Max ias Vk.Fence.FenceNullHandle
	print imageIndex
	let	submitInfo = Vk.Submit.Info {
			Vk.Submit.infoNext = Nothing,
			Vk.Submit.infoWaitSemaphoreAndDstStageMasks = [
				(ias, Vk.PipelineStageColorAttachmentOutputBit)
				],
			Vk.Submit.infoCommandBuffers =
				(cbs !! fromIntegral imageIndex) :+: CBNil,
			Vk.Submit.infoSignalSemaphores = [rfs] }
	Vk.Submit.queue @() gq [submitInfo] Vk.Fence.FenceNullHandle
	let	presentInfo = Vk.Khr.Present.Info {
			Vk.Khr.Present.infoNext = Nothing,
			Vk.Khr.Present.infoWaitSemaphores = [rfs],
			Vk.Khr.Present.infoSwapchainImageIndices =
				[(sc, imageIndex)] }
	Vk.Khr.Present.queuePresent @() pq presentInfo

cleanup :: GlfwB.Window -> Vk.Instance -> Maybe Vk.Ext.I.DebugUtilsMessenger ->
	Vk.Device -> Vk.Khr.Surface -> Vk.Khr.I.Swapchain -> [Vk.ImageView] ->
	Vk.RenderPass -> Vk.PipelineLayout -> Vk.Pipeline () '[] ->
	[Vk.Framebuffer.Framebuffer] -> Vk.CommandPool.CommandPool ->
	Vk.Semaphore.Semaphore -> Vk.Semaphore.Semaphore ->
	IO ()
cleanup w ist mdbgMssngr dv sfc sc ivs rp ppl gpl scfbs cp ias rfs = do
	Vk.Semaphore.destroy @() dv ias Nothing
	Vk.Semaphore.destroy @() dv rfs Nothing
	Vk.CommandPool.destroy @() dv cp Nothing
	flip (Vk.Framebuffer.destroy @() dv) Nothing `mapM_` scfbs
	Vk.Ppl.destroy @() dv gpl Nothing
	Vk.destroyPipelineLayout @() dv ppl Nothing
	Vk.RenderPass.destroy @() dv rp Nothing
	flip (Vk.destroyImageView @() dv) Nothing `mapM_` ivs
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
