{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.String
import Control.Monad
import Control.Monad.Fix
import Data.Bits
import Data.Bool
import Data.List

import qualified Graphics.UI.GLFW as Glfw
import qualified Vulkan as Vk
import qualified Vulkan.Ext as Vk.Ext
import qualified Vulkan.Ext.Internal as Vk.Ext.I

import ThEnv

width, height :: Int
width = 800; height = 600

validationLayers :: [String]
validationLayers = [
	"VK_LAYER_KHRONOS_validation"
	]

enableValidationLayers :: Bool
enableValidationLayers =
	maybe True (const False) $(lookupCompileEnvExp "NDEBUG")

main :: IO ()
main = run

run :: IO ()
run = do
	w <- initWindow
	(ist, dbgMssngr) <- initVulkan
	mainLoop w
	cleanup w ist dbgMssngr

initWindow :: IO Glfw.Window
initWindow = do
	True <- Glfw.init
	Glfw.windowHint $ Glfw.WindowHint'ClientAPI Glfw.ClientAPI'NoAPI
	Glfw.windowHint $ Glfw.WindowHint'Resizable False
	Just w <- Glfw.createWindow width height "Vulkan" Nothing Nothing
	pure w

initVulkan :: IO (Vk.Instance, Maybe Vk.Ext.I.DebugUtilsMessenger)
initVulkan = do
	ist <- createInstance
	dbgMssngr <- if enableValidationLayers
		then Just <$> setupDebugMessenger ist
		else pure Nothing
	pure (ist, dbgMssngr)

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
			Vk.instanceCreateInfoNext = Nothing :: Maybe Bool,
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
	glfwExtensions <- Glfw.getRequiredInstanceExtensions
	bool id (Vk.Ext.debugUtilsExtensionName :) enableValidationLayers
		<$> peekCString `mapM` glfwExtensions

setupDebugMessenger :: Vk.Instance -> IO Vk.Ext.I.DebugUtilsMessenger
setupDebugMessenger ist = do
	let	createInfo = Vk.Ext.DebugUtilsMessengerCreateInfo {
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
			Vk.Ext.debugUtilsMessengerCreateInfoFnUserCallback =
				debugCallback,
			Vk.Ext.debugUtilsMessengerCreateInfoUserData = Nothing }
	Vk.Ext.createDebugUtilsMessenger @() @() @() ist createInfo Nothing

debugCallback :: Vk.Ext.FnDebugUtilsMessengerCallback ()
debugCallback _messageSeverity _messageType callbackData _userData =
	putStrLn $ "validation layer: " ++
		Vk.Ext.debugUtilsMessengerCallbackDataMessage callbackData

mainLoop :: Glfw.Window -> IO ()
mainLoop w = do
	fix \loop -> bool (pure ()) loop =<< do
		Glfw.pollEvents
		not <$> Glfw.windowShouldClose w

cleanup :: Glfw.Window -> Vk.Instance ->
	Maybe Vk.Ext.I.DebugUtilsMessenger -> IO ()
cleanup w ist mdbgMssngr = do
--	maybe (pure ()) (Vk.Ext.destroyDebugUtilsMessenger ist dbgMssngr Nothing)
	Vk.destroyInstance ist (Nothing :: Maybe (Vk.AllocationCallbacks ()))
	Glfw.destroyWindow w
	Glfw.terminate

showExtensionProperties :: Vk.ExtensionProperties -> String
showExtensionProperties ep =
	Vk.extensionPropertiesExtensionName ep ++
	" (" ++ show (Vk.extensionPropertiesSpecVersion ep) ++ ")"
