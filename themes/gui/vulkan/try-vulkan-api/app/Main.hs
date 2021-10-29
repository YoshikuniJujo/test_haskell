{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.String
import Data.Bool

import qualified Graphics.Vulkan as Vk
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.UI.GLFW as Glfw

main :: IO ()
main = do
	w <- initWindow
	initVulkan
	mainLoop w
	cleanup w

initWindow :: IO Glfw.Window
initWindow = do
	True <- Glfw.init
	Glfw.windowHint $ Glfw.WindowHint'ClientAPI Glfw.ClientAPI'NoAPI
	Glfw.windowHint $ Glfw.WindowHint'Resizable False
	Just w <- Glfw.createWindow 800 600 "Vulkan" Nothing Nothing
	pure w

initVulkan :: IO ()
initVulkan = do
	createInstance

createInstance :: IO ()
createInstance = withCString "Hello Triangle" \cstr_ht ->
	withCString "No Engine" \cstr_ne -> do
		appInfo :: Vk.VkApplicationInfo <- Vk.newVkData \p -> do
			Vk.writeField @"sType" p Vk.VK_STRUCTURE_TYPE_APPLICATION_INFO
			Vk.writeField @"pApplicationName" p cstr_ht
			Vk.writeField @"applicationVersion" p $ Vk._VK_MAKE_VERSION 1 0 0
			Vk.writeField @"pEngineName" p cstr_ne
			Vk.writeField @"engineVersion" p $ Vk._VK_MAKE_VERSION 1 0 0
			Vk.writeField @"apiVersion" p Vk.VK_API_VERSION_1_0
		pure ()

mainLoop :: Glfw.Window -> IO ()
mainLoop w = do
	sc <- Glfw.windowShouldClose w
	Glfw.pollEvents
	bool (mainLoop w) (pure ()) sc

cleanup :: Glfw.Window -> IO ()
cleanup w = do
	Glfw.destroyWindow w
	Glfw.terminate
