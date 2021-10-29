{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String
import Data.Foldable
import Data.Bool

import qualified Graphics.Vulkan as Vk
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.UI.GLFW as Glfw

main :: IO ()
main = do
	w <- initWindow
	i <- initVulkan
	mainLoop w
	cleanup w i

initWindow :: IO Glfw.Window
initWindow = do
	True <- Glfw.init
	Glfw.windowHint $ Glfw.WindowHint'ClientAPI Glfw.ClientAPI'NoAPI
	Glfw.windowHint $ Glfw.WindowHint'Resizable False
	Just w <- Glfw.createWindow 800 600 "Vulkan" Nothing Nothing
	pure w

initVulkan :: IO Vk.VkInstance
initVulkan = do
	checkExtensionSupport
	createInstance

createInstance :: IO Vk.VkInstance
createInstance = withCString "Hello Triangle" \cstr_ht ->
	withCString "No Engine" \cstr_ne -> do
		Glfw.getRequiredInstanceExtensions >>= \es -> withArrayLen es \n es' -> do
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
				Vk.writeField @"enabledLayerCount" p 0
			i <- malloc
			Vk.VK_SUCCESS <- Vk.vkCreateInstance (Vk.unsafePtr createInfo) nullPtr i
			Vk.touchVkData createInfo
			Vk.touchVkData appInfo
			peek i

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

mainLoop :: Glfw.Window -> IO ()
mainLoop w = do
	sc <- Glfw.windowShouldClose w
	Glfw.pollEvents
	bool (mainLoop w) (pure ()) sc

cleanup :: Glfw.Window -> Vk.VkInstance -> IO ()
cleanup w i = do
	Vk.vkDestroyInstance i nullPtr
	Glfw.destroyWindow w
	Glfw.terminate
