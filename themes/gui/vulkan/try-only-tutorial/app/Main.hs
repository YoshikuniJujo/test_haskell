{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.String
import Control.Monad.Fix
import Data.Bool

import Vulkan.Base

import qualified Graphics.UI.GLFW as GlfwB

import qualified Vulkan as Vk

main :: IO ()
main = run

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
createInstance = do
	hello <- newCString "Hello Triangle"
	noEngine <- newCString "No Engine"
	let	appInfo = Vk.ApplicationInfo {
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
	pure ()

mainLoop :: GlfwB.Window -> IO ()
mainLoop win = do
	fix \loop -> bool (pure ()) loop =<< do
		GlfwB.pollEvents
		not <$> GlfwB.windowShouldClose win

cleanup :: GlfwB.Window -> IO ()
cleanup win = do
	GlfwB.destroyWindow win
	GlfwB.terminate
