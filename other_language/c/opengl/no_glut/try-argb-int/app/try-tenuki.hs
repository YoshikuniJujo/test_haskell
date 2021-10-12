{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment
import Graphics.Rendering.OpenGL

import GetTextureImage
import PngArgb32

main :: IO ()
main = do
	w_ : h_ : _ <- getArgs
	let	w = read w_
		h = read h_
		w' = fromIntegral w
		h' = fromIntegral h
	writePngArgb32 "tenuki.png" =<< render (Size w h) do
		loadIdentity
		ortho (- w' / 200) (w' / 200) (- h' / 200) (h' / 200) (- 1.0) (1.0)
		clearColor $= Color4 1.1 1.0 1.0 1.0
		clear [ColorBuffer]
		renderPrimitive Polygon do
			color $ Color3 @GLdouble 1.0 0.0 0.0
			vertex @(Vertex2 GLdouble) $ Vertex2 (- 0.9) (- 0.9)
			color $ Color3 @GLdouble 0.0 1.0 0.0
			vertex @(Vertex2 GLdouble) $ Vertex2    0.9  (- 0.9)
			color $ Color3 @GLdouble 0.0 0.0 1.0
			vertex @(Vertex2 GLdouble) $ Vertex2    0.9     0.9
			color $ Color3 @GLdouble 1.0 1.0 0.0
			vertex @(Vertex2 GLdouble) $ Vertex2 (- 0.9)    0.9
