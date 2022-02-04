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
import Data.Bool
import Data.IORef
import System.IO.Unsafe

import Vulkan.Base

import qualified Data.ByteString.Char8 as BSC
import qualified Graphics.UI.GLFW as GlfwB

import qualified Vulkan as Vk
import qualified Vulkan.Instance as Vk.Instance
import qualified Vulkan.Enumerate as Vk.Enumerate

main :: IO ()
main = run

instance_ :: IORef Vk.Instance.Instance
instance_ = unsafePerformIO $ newIORef NullPtr

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

createInstance :: IO ()
createInstance = ($ pure) $ runContT do
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
	glfwExtensions <- lift $ GlfwB.getRequiredInstanceExtensions
	pGlfwExtensions <- cStringListToCStringArray glfwExtensions
	let	glfwExtensionCount = fromIntegral $ length glfwExtensions
		Vk.Instance.CreateInfo_ fCreateInfo = Vk.Instance.CreateInfo {
			Vk.Instance.createInfoSType = (),
			Vk.Instance.createInfoPNext = NullPtr,
			Vk.Instance.createInfoFlags = 0,
			Vk.Instance.createInfoPApplicationInfo = pAppInfo,
			Vk.Instance.createInfoEnabledLayerCount = 0,
			Vk.Instance.createInfoPpEnabledLayerNames = NullPtr,
			Vk.Instance.createInfoEnabledExtensionCount =
				glfwExtensionCount,
			Vk.Instance.createInfoPpEnabledExtensionNames =
				pGlfwExtensions }
	pCreateInfo <- ContT $ withForeignPtr fCreateInfo
	pInstance <- ContT alloca
	lift do	result <- Vk.Instance.create pCreateInfo NullPtr pInstance
		when (result /= success) $ error "failed to create instance!"
		writeIORef instance_ =<< peek pInstance

cStringListToCStringArray :: [CString] -> ContT r IO (Ptr CString)
cStringListToCStringArray cstrs = do
	pcstrs <- ContT $ allocaArray cstrc
	lift $ pokeArray pcstrs cstrs
	pure pcstrs
	where cstrc = length cstrs

stringsToCStrings :: [String] -> ContT r IO (Ptr CString)
stringsToCStrings strs = do
	pcstrs <- ContT $ allocaArray strc
	cstrs <- (ContT . withCString) `mapM` strs
	lift $ pokeArray pcstrs cstrs
	pure pcstrs
	where strc = length strs

mainLoop :: GlfwB.Window -> IO ()
mainLoop win = do
	fix \loop -> bool (pure ()) loop =<< do
		GlfwB.pollEvents
		not <$> GlfwB.windowShouldClose win

cleanup :: GlfwB.Window -> IO ()
cleanup win = do
	(`Vk.Instance.destroy` NullPtr) =<< readIORef instance_
	GlfwB.destroyWindow win
	GlfwB.terminate
