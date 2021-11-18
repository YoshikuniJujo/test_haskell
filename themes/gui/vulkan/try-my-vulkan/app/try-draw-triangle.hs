{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.Fix
import Data.Bool

import qualified Graphics.UI.GLFW as Glfw
import qualified Vulkan as Vk

width, height :: Int
width = 800; height = 600

main :: IO ()
main = run

run :: IO ()
run = do
	w <- initWindow
	initVulkan
	mainLoop w
	cleanup w

initWindow :: IO Glfw.Window
initWindow = do
	True <- Glfw.init
	Glfw.windowHint $ Glfw.WindowHint'ClientAPI Glfw.ClientAPI'NoAPI
	Glfw.windowHint $ Glfw.WindowHint'Resizable False
	Just w <- Glfw.createWindow width height "Vulkan" Nothing Nothing
	pure w

initVulkan :: IO ()
initVulkan = do
	createInstance

createInstance :: IO ()
createInstance = pure ()

mainLoop :: Glfw.Window -> IO ()
mainLoop w = do
	fix \loop -> bool (pure ()) loop =<< do
		Glfw.pollEvents
		not <$> Glfw.windowShouldClose w

cleanup :: Glfw.Window  ->IO ()
cleanup w = do
	Glfw.destroyWindow w
	Glfw.terminate
