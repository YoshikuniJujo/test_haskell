{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.String
import Control.Monad
import Control.Monad.Fix
import Data.Bool
import Data.List

import qualified Graphics.UI.GLFW as Glfw
import qualified Vulkan as Vk

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
	i <- initVulkan
	mainLoop w
	cleanup w i

initWindow :: IO Glfw.Window
initWindow = do
	True <- Glfw.init
	Glfw.windowHint $ Glfw.WindowHint'ClientAPI Glfw.ClientAPI'NoAPI
	Glfw.windowHint $ Glfw.WindowHint'Resizable False
	Just w <- Glfw.createWindow width height "Vulkan" Nothing Nothing
	pure w

initVulkan :: IO Vk.Instance
initVulkan = do
	ist <- createInstance
	when enableValidationLayers setupDebugMessenger
	pure ist

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
			Vk.instanceCreateInfoExtensions = [] }
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
	peekCString `mapM` glfwExtensions
	-- HERE check #const_str of hsc2hs

setupDebugMessenger :: IO ()
setupDebugMessenger = pure ()

mainLoop :: Glfw.Window -> IO ()
mainLoop w = do
	fix \loop -> bool (pure ()) loop =<< do
		Glfw.pollEvents
		not <$> Glfw.windowShouldClose w

cleanup :: Glfw.Window -> Vk.Instance ->IO ()
cleanup w i = do
	Vk.destroyInstance i (Nothing :: Maybe (Vk.AllocationCallbacks ()))
	Glfw.destroyWindow w
	Glfw.terminate

showExtensionProperties :: Vk.ExtensionProperties -> String
showExtensionProperties ep =
	Vk.extensionPropertiesExtensionName ep ++
	" (" ++ show (Vk.extensionPropertiesSpecVersion ep) ++ ")"
