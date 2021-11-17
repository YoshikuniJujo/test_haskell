{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.Fix
import Data.Bool

import qualified Graphics.UI.GLFW as Glfw

main :: IO ()
main = do
	True <- Glfw.init
	Glfw.windowHint $ Glfw.WindowHint'ClientAPI Glfw.ClientAPI'NoAPI
	Just window <- Glfw.createWindow 800 600 "Vulkan window" Nothing Nothing

	while_ do
		Glfw.pollEvents
		sc <- Glfw.windowShouldClose window
		pure $ not sc

	Glfw.destroyWindow window
	Glfw.terminate

while_ :: Monad m => m Bool -> m ()
while_ act = fix \f -> bool (pure ()) f =<< act
