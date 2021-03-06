{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

-- https://tokoik.github.io/opengl/libglut.html#5

import Control.Monad (void, join)
import System.Environment (getProgName, getArgs)

import Graphics.UI.GLUT

display :: IO ()
display = do
	clear [ColorBuffer]
	color $ Color3 @GLdouble 1 0 0
	renderPrimitive LineLoop $ mapM_ (vertex . uncurry (Vertex2 @GLdouble))
		[(- 0.9, - 0.9), (0.9, - 0.9), (0.9, 0.9), (-0.9, 0.9)]
	flush

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
