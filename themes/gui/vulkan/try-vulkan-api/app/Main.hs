{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Bool

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
initVulkan = pure ()

mainLoop :: Glfw.Window -> IO ()
mainLoop w = do
	sc <- Glfw.windowShouldClose w
	Glfw.pollEvents
	bool (mainLoop w) (pure ()) sc

cleanup :: Glfw.Window -> IO ()
cleanup w = do
	Glfw.destroyWindow w
	Glfw.terminate
