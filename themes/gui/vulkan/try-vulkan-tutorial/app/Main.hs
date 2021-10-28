{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Bool
import Vulkan.Zero
import Vulkan.Version

import qualified Vulkan as V

import Lib

main :: IO ()
main = do
	w <- initWindow
	initVulkan
	mainLoop w
	cleanup w

initWindow :: IO GlfwWindow
initWindow = do
	glfwInit
	glfwWindowHint GlfwClientApi GlfwNoApi
	glfwWindowHint GlfwResizable GlfwFalse
	glfwCreateWindowSimple 800 600 "Vulkan"

initVulkan :: IO ()
initVulkan = do
	createInstance

createInstance :: IO ()
createInstance = do
	let	appInfo = zero {
			V.applicationName = Just "Hello Triangle",
			V.applicationVersion = MAKE_API_VERSION 1 0 0,
			V.engineName = Just "No Engine",
			V.engineVersion = MAKE_API_VERSION 1 0 0,
--			V.apiVersion = V.API_VERSION_1_0
			}
	pure ()

mainLoop :: GlfwWindow -> IO ()
mainLoop w = do
	cls <- glfwWindowShouldClose w
	glfwPollEvents
	bool (mainLoop w) (pure ()) cls

cleanup :: GlfwWindow -> IO ()
cleanup w = do
	glfwDestroyWindow w
	glfwTerminate
