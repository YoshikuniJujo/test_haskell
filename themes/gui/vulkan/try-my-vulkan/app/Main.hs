{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import qualified Graphics.UI.GLFW as Glfw

main :: IO ()
main = do
	True <- Glfw.init
	Glfw.terminate
