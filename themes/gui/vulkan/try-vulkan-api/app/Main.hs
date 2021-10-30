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
import Data.List
import Data.Bool

import qualified Graphics.Vulkan as Vk
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Ext.VK_EXT_debug_utils as Vk
import qualified Graphics.UI.GLFW as Glfw

import Lib
import ThEnv

validationLayers :: [String]
validationLayers = [
	"VK_LAYER_KHRONOS_validation"
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
	(i, dm) <- initVulkan
	mainLoop w
	cleanup w i dm

initWindow :: IO Glfw.Window
initWindow = do
	True <- Glfw.init
	Glfw.windowHint $ Glfw.WindowHint'ClientAPI Glfw.ClientAPI'NoAPI
	Glfw.windowHint $ Glfw.WindowHint'Resizable False
	Just w <- Glfw.createWindow 800 600 "Vulkan" Nothing Nothing
	pure w

initVulkan :: IO (Vk.VkInstance, Ptr Vk.VkDebugUtilsMessengerEXT)
initVulkan = do
	checkExtensionSupport
	i <- createInstance
	dm <- setupDebugMessenger i
	pure (i, dm)

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
	for_ ps \p -> do
		let	os = Vk.fieldOffset @"extensionName" @Vk.VkExtensionProperties
			ln = Vk.fieldArrayLength @"extensionName" @Vk.VkExtensionProperties
		putStrLn . ('\t' :) =<< peekCStringLen (Vk.unsafePtr p `plusPtr` os, ln)
		Vk.touchVkData p

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

cleanup :: Glfw.Window -> Vk.VkInstance -> Ptr Vk.VkDebugUtilsMessengerEXT -> IO ()
cleanup w i pdm = do
	when enableValidationLayers do
		vkDestroyDebugUtilsMessengerEXT <-
			createDestroyDebugUtilsMessengerEXT i
		peek pdm >>= \dm -> vkDestroyDebugUtilsMessengerEXT i dm nullPtr
	Vk.vkDestroyInstance i nullPtr
	Glfw.destroyWindow w
	Glfw.terminate
