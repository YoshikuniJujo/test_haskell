{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.Fix
import Data.Bool

import qualified Graphics.UI.GLFW as Glfw

import qualified Vulkan as Vk

main :: IO ()
main = do
	True <- Glfw.init
	Glfw.windowHint $ Glfw.WindowHint'ClientAPI Glfw.ClientAPI'NoAPI
	Just window <- Glfw.createWindow 800 600 "Vulkan window" Nothing Nothing

	extensions <- Vk.enumerateInstanceExtensionProperties Nothing
	print $ length extensions

	fix \loop -> bool (pure ()) loop =<< do
		Glfw.pollEvents
		not <$> Glfw.windowShouldClose window

	Glfw.destroyWindow window
	Glfw.terminate
