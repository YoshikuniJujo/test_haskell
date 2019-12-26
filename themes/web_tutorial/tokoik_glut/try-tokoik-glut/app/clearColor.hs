{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

-- https://tokoik.github.io/opengl/libglut.html#4

import Control.Monad (void, join)
import System.Environment (getProgName, getArgs)

import Graphics.UI.GLUT

display :: IO ()
display = clear [ColorBuffer] >> flush

initializeWindow :: IO ()
initializeWindow = clearColor $= Color4 0 0 1 1

main :: IO ()
main = do
	void . join $ initialize <$> getProgName <*> getArgs
	initialDisplayMode $= [RGBAMode]
	void $ createWindow =<< getProgName
	displayCallback $= display
	initializeWindow
	mainLoop
