{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import Data.List
import Data.Bool
import Vulkan.Zero
import Vulkan.Version

import qualified Data.Vector as Vc
import qualified Data.ByteString as BS
import qualified Vulkan as Vk

import Lib
import ThEnv

width, height :: CInt
width = 800; height = 600

validationLayers :: [BS.ByteString]
validationLayers = [
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

checkValidationLayerSupport :: IO Bool
checkValidationLayerSupport = do
	(Vk.SUCCESS, lps) <- Vk.enumerateInstanceLayerProperties
	print `mapM_` lps
	pure . null $ validationLayers \\ (Vk.layerName <$> Vc.toList lps)
