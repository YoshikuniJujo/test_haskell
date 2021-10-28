{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Bool
import Vulkan.Zero
import Vulkan.Version

import qualified Data.Vector as Vc
import qualified Vulkan as Vk

import Lib

main :: IO ()
main = do
	(Vk.SUCCESS, ps) <- Vk.enumerateInstanceExtensionProperties Nothing
	print $ length ps
	print `mapM_` ps
	w <- initWindow
	i <- initVulkan
	mainLoop w
	cleanup w i

initWindow :: IO GlfwWindow
initWindow = do
	glfwInit
	glfwWindowHint GlfwClientApi GlfwNoApi
	glfwWindowHint GlfwResizable GlfwFalse
	glfwCreateWindowSimple 800 600 "Vulkan"

initVulkan :: IO Vk.Instance
initVulkan = do
	createInstance

createInstance :: IO Vk.Instance
createInstance = do
	let	appInfo = zero {
			Vk.applicationName = Just "Hello Triangle",
			Vk.applicationVersion = MAKE_API_VERSION 1 0 0,
			Vk.engineName = Just "No Engine",
			Vk.engineVersion = MAKE_API_VERSION 1 0 0,
			Vk.apiVersion = Vk.API_VERSION_1_0 }
	es <- glfwGetRequiredInstanceExtensions
	print es
	let	createInfo = zero {
		Vk.applicationInfo = Just appInfo,
		Vk.enabledLayerNames = Vc.empty,
		Vk.enabledExtensionNames = es }
	Vk.createInstance createInfo Nothing

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
