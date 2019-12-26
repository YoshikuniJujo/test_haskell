{-# LANGUAGE TypeApplications, BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

-- https://tokoik.github.io/opengl/libglut.html#6

import Control.Monad (void, join)
import System.Environment (getProgName, getArgs)

import Graphics.UI.GLUT

display :: IO ()
display = do
	clear [ColorBuffer]
	renderPrimitive Polygon do
		color $ Color3 @GLdouble 1 0 0
		vertex $ Vertex2 @GLdouble (- 0.9) (- 0.9)
		color $ Color3 @GLdouble 0 1 0
		vertex $ Vertex2 @GLdouble 0.9 (- 0.9)
		color $ Color3 @GLdouble 0 0 1
		vertex $ Vertex2 @GLdouble 0.9 0.9
		color $ Color3 @GLdouble 1 1 0
		vertex $ Vertex2 @GLdouble (- 0.9) 0.9
	flush

resize :: Size -> IO ()
resize s@(Size w h) = do
	viewport $= (Position 0 0, s)
	loadIdentity
	ortho (- w') w' (- h') h' (- 1.0) 1.0
	where
	w' = realToFrac w / 200.0
	h' = realToFrac h / 200.0

initializeWindow :: IO ()
initializeWindow = clearColor $= Color4 1 1 1 1

main :: IO ()
main = do
	void . join $ initialize <$> getProgName <*> getArgs
	initialDisplayMode $= [RGBAMode]
	void $ createWindow =<< getProgName
	displayCallback $= display
	reshapeCallback $= Just resize
	initializeWindow
	mainLoop
