{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Ptr
import Foreign.C.Types
import Control.Monad
import Data.Bits
import Data.List
import Data.Bool
import Vulkan.Zero
import Vulkan.Version

import qualified Data.Vector as Vc
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Vulkan as Vk

import Lib
import ThEnv
import VulkanTools

width, height :: CInt
width = 800; height = 600

validationLayers :: Vc.Vector BS.ByteString
validationLayers = Vc.fromList [
	"VK_LAYER_KHRONOS_validation"
	]

enableValidationLayers :: Bool
enableValidationLayers =
	maybe True (const False) $(lookupCompileEnvExp "NDEBUG")

main :: IO ()
main = do
	(Vk.SUCCESS, ps) <- Vk.enumerateInstanceExtensionProperties Nothing
	print $ length ps
	print `mapM_` ps
	print enableValidationLayers
	print =<< checkValidationLayerSupport

	w <- initWindow
	i <- initVulkan
	mainLoop w
	cleanup w i

initWindow :: IO GlfwWindow
initWindow = do
	glfwInit
	glfwWindowHint GlfwClientApi GlfwNoApi
	glfwWindowHint GlfwResizable GlfwFalse
	glfwCreateWindowSimple width height "Vulkan"

initVulkan :: IO Vk.Instance
initVulkan = do
	i <- createInstance
	setupDebugMessenger i
	pure i

createInstance :: IO Vk.Instance
createInstance = do
	ck <- checkValidationLayerSupport
	when (enableValidationLayers && not ck)
		$ error "validation layers requested, but not available!"
	let	appInfo = zero {
			Vk.applicationName = Just "Hello Triangle",
			Vk.applicationVersion = MAKE_API_VERSION 1 0 0,
			Vk.engineName = Just "No Engine",
			Vk.engineVersion = MAKE_API_VERSION 1 0 0,
			Vk.apiVersion = Vk.API_VERSION_1_0 }
	es <- getRequiredExtensions
	print es
	let	createInfo = zero {
		Vk.applicationInfo = Just appInfo,
		Vk.enabledLayerNames =
			bool Vc.empty validationLayers enableValidationLayers,
		Vk.enabledExtensionNames = es }
	Vk.createInstance createInfo Nothing

setupDebugMessenger :: Vk.Instance -> IO ()
setupDebugMessenger i = if not enableValidationLayers then pure () else do
	let	createInfo = zero {
			Vk.messageSeverity =
				Vk.DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT .|.
				Vk.DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT .|.
				Vk.DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT,
			Vk.messageType =
				Vk.DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT .|.
				Vk.DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT .|.
				Vk.DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT,
			Vk.userData = nullPtr }
	createInfo' <- setFnUserCallback createInfo debugCallback
	pure ()

getRequiredExtensions :: IO (Vc.Vector BS.ByteString)
getRequiredExtensions = do
	glfwExtensions <- glfwGetRequiredInstanceExtensions
	pure $ bool id
		(`Vc.snoc` "VK_EXT_debug_utils")
		enableValidationLayers glfwExtensions

mainLoop :: GlfwWindow -> IO ()
mainLoop w = do
	cls <- glfwWindowShouldClose w
	glfwPollEvents
	bool (mainLoop w) (pure ()) cls

cleanup :: GlfwWindow -> Vk.Instance -> IO ()
cleanup w i = do
	Vk.destroyInstance i Nothing
	glfwDestroyWindow w
	glfwTerminate

checkValidationLayerSupport :: IO Bool
checkValidationLayerSupport = do
	(Vk.SUCCESS, lps) <- Vk.enumerateInstanceLayerProperties
	print `mapM_` lps
	pure . null $ Vc.toList validationLayers \\ (Vk.layerName <$> Vc.toList lps)

debugCallback :: Vk.FN_vkDebugUtilsMessengerCallbackEXT
debugCallback _messageSeverity _messageType pCallbackData _pUserData =
	Vk.FALSE <$ (BSC.putStrLn . ("validation layer: " <>)
		. Vk.message =<< Vk.peekCStruct pCallbackData)
