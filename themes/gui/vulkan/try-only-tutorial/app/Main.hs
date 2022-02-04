{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.C.String
import Control.Monad.Fix
import Control.Monad.Cont
import Data.IORef
import Data.Bits
import Data.Bool
import Data.Word
import System.IO.Unsafe

import Tools
import Vulkan.Base

import qualified Data.ByteString.Char8 as BSC
import qualified Graphics.UI.GLFW as GlfwB

import qualified Vulkan as Vk
import qualified Vulkan.Instance as Vk.Instance
import qualified Vulkan.Enumerate as Vk.Enumerate
import qualified Vulkan.Ext.DebugUtils.Messenger as Vk.Ext.DU.Msngr

main :: IO ()
main = run

instance_ :: IORef Vk.Instance.Instance
instance_ = unsafePerformIO $ newIORef NullPtr

debugMessenger :: IORef Vk.Ext.DU.Msngr.Messenger
debugMessenger = unsafePerformIO $ newIORef NullPtr

run :: IO ()
run = do
	win <- initWindow
	initVulkan
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

initVulkan :: IO ()
initVulkan = do
	createInstance
	setupDebugMessenger

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

mainLoop :: GlfwB.Window -> IO ()
mainLoop win = do
	fix \loop -> bool (pure ()) loop =<< do
		GlfwB.pollEvents
		not <$> GlfwB.windowShouldClose win

cleanup :: GlfwB.Window -> IO ()
cleanup win = do
	ist <- readIORef instance_
	(\dm -> Vk.Ext.DU.Msngr.destroy ist dm NullPtr)
		=<< readIORef debugMessenger
	Vk.Instance.destroy ist NullPtr
	GlfwB.destroyWindow win
	GlfwB.terminate
