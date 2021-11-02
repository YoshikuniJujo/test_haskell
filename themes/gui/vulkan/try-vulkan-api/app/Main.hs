{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String
import Control.Monad
import Data.Foldable
import Data.Traversable
import Data.Bits
import Data.Maybe
import Data.List
import Data.Bool

import qualified Data.Set as St
import qualified Graphics.Vulkan as Vk
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Ext.VK_EXT_debug_utils as Vk
import qualified Graphics.Vulkan.Ext.VK_KHR_surface as Vk
import qualified Graphics.UI.GLFW as Glfw

import Lib
import ThEnv

validationLayers :: [String]
validationLayers = [
	"VK_LAYER_KHRONOS_validation"
	]

deviceExtensions :: [String]
deviceExtensions = [
	"VK_KHR_swapchain"
	]

withCStringArray :: [String] -> (Ptr CString -> IO a) -> IO a
withCStringArray ss f = do
	css <- newCString `mapM` ss
	withArray css f <* free `mapM_` css

enableValidationLayers :: Bool
enableValidationLayers =
	maybe True (const False) $(lookupCompileEnvExp "NDEBUG")

main :: IO ()
main = do
	print enableValidationLayers
	w <- initWindow
	(i, dm, d, sfc) <- initVulkan w
	mainLoop w
	cleanup w i dm d sfc

initWindow :: IO Glfw.Window
initWindow = do
	True <- Glfw.init
	Glfw.windowHint $ Glfw.WindowHint'ClientAPI Glfw.ClientAPI'NoAPI
	Glfw.windowHint $ Glfw.WindowHint'Resizable False
	Just w <- Glfw.createWindow 800 600 "Vulkan" Nothing Nothing
	pure w

initVulkan :: Glfw.Window -> IO (
	Vk.VkInstance, Ptr Vk.VkDebugUtilsMessengerEXT, Vk.VkDevice,
	Vk.VkSurfaceKHR )
initVulkan w = do
	checkExtensionSupport
	i <- createInstance
	dm <- setupDebugMessenger i
	sfc <- createSurface i w
	pd <- pickPhysicalDevice i sfc
	(d, gq, pq) <- createLogicalDevice pd sfc
	pure (i, dm, d, sfc)

createSurface :: Vk.VkInstance -> Glfw.Window -> IO Vk.VkSurfaceKHR
createSurface i w = alloca \pSurface -> do
	Vk.VK_SUCCESS <- Glfw.createWindowSurface i w nullPtr pSurface
	peek pSurface

createLogicalDevice :: Vk.VkPhysicalDevice -> Vk.VkSurfaceKHR -> IO (Vk.VkDevice, Vk.VkQueue, Vk.VkQueue)
createLogicalDevice pd sfc = alloca \pQueuePriority -> do
	withCStringArray validationLayers \cvls -> withCStringArray deviceExtensions \cdes -> do
		indices <- findQueueFamilies pd sfc
		let	uniqueQueueFamilies = St.fromList [
				graphicsFamily indices, presentFamily indices ]
		putStrLn $ "uniqueQueueFamilies: " ++ show uniqueQueueFamilies
		poke pQueuePriority 1
		queueCreateInfos :: [Vk.VkDeviceQueueCreateInfo] <- for (toList uniqueQueueFamilies) \queueFamily ->
			Vk.newVkData \p -> do
				Vk.clearStorable p
				Vk.writeField @"sType" p Vk.VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
				Vk.writeField @"queueFamilyIndex" p $ fromJust queueFamily
				Vk.writeField @"queueCount" p 1
				Vk.writeField @"pQueuePriorities" p pQueuePriority
		withArray queueCreateInfos \pQueueCreateInfos -> do
			deviceFeatures :: Vk.VkPhysicalDeviceFeatures <- Vk.mallocVkData
			Vk.clearStorable $ Vk.unsafePtr deviceFeatures
			createInfo :: Vk.VkDeviceCreateInfo <- Vk.newVkData \p -> do
				Vk.clearStorable p
				Vk.writeField @"sType" p Vk.VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
				Vk.writeField @"pQueueCreateInfos" p pQueueCreateInfos
				Vk.writeField @"queueCreateInfoCount" p 1
				Vk.writeField @"pEnabledFeatures" p $ Vk.unsafePtr deviceFeatures
				Vk.writeField @"enabledExtensionCount" p . fromIntegral $ length deviceExtensions
				Vk.writeField @"ppEnabledExtensionNames" p cdes
				if enableValidationLayers
				then do	Vk.writeField @"enabledLayerCount" p . fromIntegral $ length validationLayers
					Vk.writeField @"ppEnabledLayerNames" p cvls
				else Vk.writeField @"enabledLayerCount" p 0
			Vk.touchVkData deviceFeatures
			pVkDevice <- malloc
			Vk.VK_SUCCESS <- Vk.vkCreateDevice pd (Vk.unsafePtr createInfo) nullPtr pVkDevice
			Vk.touchVkData createInfo
			pGraphicsQueue <- malloc
			device <- peek pVkDevice
			Vk.vkGetDeviceQueue device (fromJust $ graphicsFamily indices) 0 pGraphicsQueue
			pPresentQueue <- malloc
			Vk.vkGetDeviceQueue device (fromJust $ presentFamily indices) 0 pPresentQueue
			graphicsQueue <- peek pGraphicsQueue
			presentQueue <- peek pPresentQueue
			free pVkDevice
			free pGraphicsQueue
			free pPresentQueue
			pure (device, graphicsQueue, presentQueue)

pickPhysicalDevice :: Vk.VkInstance -> Vk.VkSurfaceKHR -> IO Vk.VkPhysicalDevice
pickPhysicalDevice i sfc = alloca \pn -> do
	Vk.VK_SUCCESS <- Vk.vkEnumeratePhysicalDevices i pn nullPtr
	n <- peek pn
	putStrLn $ "PHYSICAL DEVICE NUMBER: " ++ show n
	when (n == 0) $ error "failed to find GPUs with Vulkan support!"
	pDevices <- mallocArray $ fromIntegral n
	Vk.VK_SUCCESS <- Vk.vkEnumeratePhysicalDevices i pn pDevices
	ds <- peekArray (fromIntegral n) pDevices
	ds' <- (`filterM` ds) \d -> isDeviceSuitable d sfc
	when (null ds') $ error "failed to find a suitable GPU!"
	pure $ head ds'

data QueueFamilyIndices = QueueFamilyIndices {
	graphicsFamily :: Maybe Vk.Word32,
	presentFamily :: Maybe Vk.Word32,
	isComplete :: QueueFamilyIndices -> Bool
	}

queueFamilyIndices0 :: QueueFamilyIndices
queueFamilyIndices0 = QueueFamilyIndices Nothing Nothing \q ->
	isJust (graphicsFamily q) && isJust (presentFamily q)

isDeviceSuitable :: Vk.VkPhysicalDevice -> Vk.VkSurfaceKHR -> IO Bool
isDeviceSuitable d sfc = alloca \pdp -> alloca \pdf -> do
	Vk.vkGetPhysicalDeviceProperties d pdp
	print =<< peek pdp
	print =<< Vk.readField @"deviceName" pdp
	let	os = Vk.fieldOffset @"deviceName" @Vk.VkPhysicalDeviceProperties
		ln = Vk.fieldArrayLength @"deviceName" @Vk.VkPhysicalDeviceProperties
	putStrLn . ('\t' :) =<< peekCStringLen (pdp `plusPtr` os, ln)
	print =<< Vk.readField @"deviceType" pdp
	Vk.vkGetPhysicalDeviceFeatures d pdf
	print =<< peek pdf
	print =<< Vk.readField @"geometryShader" pdf

	indices <- findQueueFamilies d sfc

	extensionsSupported <- checkDeviceExtensionSupport d
	swapChainAdequate <- if not extensionsSupported then pure False else do
		swapChainSupport <- querySwapChainSupport d sfc
		pure $ not (null $ formats swapChainSupport) && not (null $ presentModes swapChainSupport)
	pure $ isComplete indices indices && extensionsSupported && swapChainAdequate

data SwapChainSupportDetails = SwapChainSupportDetails {
	capabilities :: Vk.VkSurfaceCapabilitiesKHR,
	formats :: [Vk.VkSurfaceFormatKHR],
	presentModes :: [Vk.VkPresentModeKHR] } deriving Show

querySwapChainSupport :: Vk.VkPhysicalDevice -> Vk.VkSurfaceKHR -> IO SwapChainSupportDetails
querySwapChainSupport d sfc = do
	cpbs <- alloca \pcpbs -> do
		Vk.VK_SUCCESS <- Vk.vkGetPhysicalDeviceSurfaceCapabilitiesKHR d sfc pcpbs
		peek pcpbs
	putStrLn $ "dertails.capabilities: " ++ show cpbs
	fmts <- alloca \pn -> do
		Vk.VK_SUCCESS <- Vk.vkGetPhysicalDeviceSurfaceFormatsKHR d sfc pn nullPtr
		n <- peek pn
		if n == 0 then pure []
		else allocaArray (fromIntegral n) \pfmts -> do
			Vk.VK_SUCCESS <- Vk.vkGetPhysicalDeviceSurfaceFormatsKHR d sfc pn pfmts
			peekArray (fromIntegral n) pfmts
	print fmts
	pms <- alloca \pn -> do
		Vk.VK_SUCCESS <- Vk.vkGetPhysicalDeviceSurfacePresentModesKHR d sfc pn nullPtr
		n <- fromIntegral <$> peek pn
		if n == 0 then pure []
		else allocaArray n \ppms -> do
			Vk.VK_SUCCESS <- Vk.vkGetPhysicalDeviceSurfacePresentModesKHR d sfc pn ppms
			peekArray n ppms
	print pms
	pure $ SwapChainSupportDetails cpbs fmts pms

checkDeviceExtensionSupport :: Vk.VkPhysicalDevice -> IO Bool
checkDeviceExtensionSupport d = alloca \pn ->  do
	Vk.VK_SUCCESS <- Vk.vkEnumerateDeviceExtensionProperties d nullPtr pn nullPtr
	n <- peek pn
	putStrLn $ "EXTENSION PROPERTY NUMBER: " ++ show n
	allocaArray (fromIntegral n) \pAvailableExtensions -> do
		Vk.VK_SUCCESS <- Vk.vkEnumerateDeviceExtensionProperties d
			nullPtr pn pAvailableExtensions
		putStrLn "EXTENSION PROPERTIES: "
		ens <- mapM ((takeWhile (/= '\NUL') <$>) . peekExtensionName)
			=<< peekArray (fromIntegral n) pAvailableExtensions
		(putStrLn . ('\t' :)) `mapM_` ens
		pure . null $ deviceExtensions \\ ens

findQueueFamilies :: Vk.VkPhysicalDevice -> Vk.VkSurfaceKHR -> IO QueueFamilyIndices
findQueueFamilies d sfc = alloca \pn -> do
	Vk.vkGetPhysicalDeviceQueueFamilyProperties d pn nullPtr
	n <- peek pn
	pqfps <- mallocArray $ fromIntegral n
	Vk.vkGetPhysicalDeviceQueueFamilyProperties d pn pqfps
	qfps <- peekArray (fromIntegral n) pqfps
	print qfps
	flags <- for qfps \qfp -> Vk.readField @"queueFlags" (Vk.unsafePtr qfp)
	print flags
	flags' <- for [0 .. n - 1] \i -> alloca \pb -> do
		Vk.VK_SUCCESS <- Vk.vkGetPhysicalDeviceSurfaceSupportKHR d i sfc pb
		peek pb
	print flags'
	pure queueFamilyIndices0 {
		graphicsFamily = fromIntegral
			<$> findIndex ((/= zeroBits) . (.&. Vk.VK_QUEUE_GRAPHICS_BIT)) flags,
		presentFamily = fromIntegral <$> findIndex (== Vk.VK_TRUE) flags'
		}

createInstance :: IO Vk.VkInstance
createInstance = do
	when enableValidationLayers do
		b <- checkValidationLayerSupport
		when (not b) $ error
			"validation layers requested, but not available!"
	withCString "Hello Triangle" \cstr_ht ->
		withCString "No Engine" \cstr_ne ->
			getRequiredExtensions \es -> withArrayLen es \n es' ->
				withCStringArray validationLayers \cvls -> do
					appInfo :: Vk.VkApplicationInfo <- Vk.newVkData \p -> do
						Vk.writeField @"sType" p Vk.VK_STRUCTURE_TYPE_APPLICATION_INFO
						Vk.writeField @"pApplicationName" p cstr_ht
						Vk.writeField @"applicationVersion" p $ Vk._VK_MAKE_VERSION 1 0 0
						Vk.writeField @"pEngineName" p cstr_ne
						Vk.writeField @"engineVersion" p $ Vk._VK_MAKE_VERSION 1 0 0
						Vk.writeField @"apiVersion" p Vk.VK_API_VERSION_1_0
					createInfo :: Vk.VkInstanceCreateInfo <- Vk.newVkData \p -> do
						Vk.writeField @"sType" p Vk.VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
						Vk.writeField @"pApplicationInfo" p $ Vk.unsafePtr appInfo
						Vk.writeField @"enabledExtensionCount" p $ fromIntegral n
						Vk.writeField @"ppEnabledExtensionNames" p es'
						if enableValidationLayers
						then do	Vk.writeField @"enabledLayerCount" p
								. fromIntegral $ length validationLayers
							Vk.writeField @"ppEnabledLayerNames" p cvls
							debugCreateInfo <- populateDebugMessengerCreateInfo
							Vk.writeField @"pNext" p . castPtr $ Vk.unsafePtr debugCreateInfo
							Vk.touchVkData debugCreateInfo
						else do	Vk.writeField @"enabledLayerCount" p 0
							Vk.writeField @"pNext" p nullPtr
					i <- malloc
					Vk.VK_SUCCESS <- Vk.vkCreateInstance (Vk.unsafePtr createInfo) nullPtr i
					Vk.touchVkData createInfo
					Vk.touchVkData appInfo
					peek i

getRequiredExtensions :: ([CString] -> IO a) -> IO a
getRequiredExtensions f = do
	extensions <- Glfw.getRequiredInstanceExtensions
	if enableValidationLayers
	then withCString "VK_EXT_debug_utils" \cs -> f (extensions ++ [cs])
	else f extensions

checkExtensionSupport :: IO ()
checkExtensionSupport = alloca \pn -> do
	Vk.VK_SUCCESS <- Vk.vkEnumerateInstanceExtensionProperties nullPtr pn nullPtr
	n <- peek pn
	(pp, ps) <- Vk.mallocVkDataArray $ fromIntegral n
	Vk.VK_SUCCESS <- Vk.vkEnumerateInstanceExtensionProperties nullPtr pn pp
	print $ length ps
	putStrLn "available extensions:"
	for_ ps \p -> putStrLn . ('\t' :) =<< peekExtensionName p

peekExtensionName :: Vk.VkExtensionProperties -> IO String
peekExtensionName p = peekCStringLen (Vk.unsafePtr p `plusPtr` os, ln)
	<* Vk.touchVkData p
	where	os = Vk.fieldOffset @"extensionName" @Vk.VkExtensionProperties
		ln = Vk.fieldArrayLength @"extensionName" @Vk.VkExtensionProperties

setupDebugMessenger :: Vk.VkInstance -> IO (Ptr Vk.VkDebugUtilsMessengerEXT)
setupDebugMessenger i = if not enableValidationLayers then pure nullPtr else do
	pDebugMessenger :: Ptr Vk.VkDebugUtilsMessengerEXT <- malloc
	vkCreateDebugUtilsMessengerEXT <- createDebugUtilsMessengerEXT i
	createInfo <- populateDebugMessengerCreateInfo
	Vk.VK_SUCCESS <- vkCreateDebugUtilsMessengerEXT i (Vk.unsafePtr createInfo) nullPtr pDebugMessenger
	Vk.touchVkData createInfo
	pure pDebugMessenger

populateDebugMessengerCreateInfo :: IO Vk.VkDebugUtilsMessengerCreateInfoEXT
populateDebugMessengerCreateInfo = do
	Vk.newVkData \p -> do
		Vk.writeField @"sType" p
			Vk.VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT
		Vk.writeField @"pNext" p nullPtr
		Vk.writeField @"messageSeverity" p $
			Vk.VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT .|.
			Vk.VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT .|.
			Vk.VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
		Vk.writeField @"messageType" p $
			Vk.VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT .|.
			Vk.VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT .|.
			Vk.VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
		Vk.writeField @"pfnUserCallback" p
			=<< wrapDebugUtilsMessengerCallbackEXT debugCallback
		Vk.writeField @"pUserData" p nullPtr

checkValidationLayerSupport :: IO Bool
checkValidationLayerSupport = alloca \pn -> do
	Vk.VK_SUCCESS <- Vk.vkEnumerateInstanceLayerProperties pn nullPtr
	n <- peek pn
	(pp, ps) <- Vk.mallocVkDataArray $ fromIntegral n
	Vk.VK_SUCCESS <- Vk.vkEnumerateInstanceLayerProperties pn pp
	print $ length ps
	vls <- for ps \p -> do
		peekCStringLen (Vk.unsafePtr p `plusPtr` os, ln)
			<* Vk.touchVkData p
	let	vls' = takeWhile (/= '\NUL') <$> vls
	print vls'
	pure . null $ validationLayers \\ vls'
	where
	os = Vk.fieldOffset @"layerName" @Vk.VkLayerProperties
	ln = Vk.fieldArrayLength @"layerName" @Vk.VkLayerProperties

debugCallback :: Vk.HS_vkDebugUtilsMessengerCallbackEXT
debugCallback _messageSeverity _messageType pCallbackData _pUserData = do
	msg <- Vk.readField @"pMessage" pCallbackData
	putStrLn . ("validation layer: " ++) =<< peekCString msg
	pure Vk.VK_FALSE

mainLoop :: Glfw.Window -> IO ()
mainLoop w = do
	sc <- Glfw.windowShouldClose w
	Glfw.pollEvents
	bool (mainLoop w) (pure ()) sc

cleanup ::
	Glfw.Window -> Vk.VkInstance -> Ptr Vk.VkDebugUtilsMessengerEXT ->
	Vk.VkDevice -> Vk.VkSurfaceKHR -> IO ()
cleanup w i pdm d sfc = do
	Vk.vkDestroyDevice d nullPtr
	when enableValidationLayers do
		vkDestroyDebugUtilsMessengerEXT <-
			createDestroyDebugUtilsMessengerEXT i
		peek pdm >>= \dm -> vkDestroyDebugUtilsMessengerEXT i dm nullPtr
	Vk.vkDestroySurfaceKHR i sfc nullPtr
	Vk.vkDestroyInstance i nullPtr
	Glfw.destroyWindow w
	Glfw.terminate
