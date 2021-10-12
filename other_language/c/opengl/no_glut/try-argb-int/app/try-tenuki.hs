{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Rendering.OpenGL

import GetTextureImage
import PngArgb32

main :: IO ()
main = writePngArgb32 "tenuki.png" =<< render (Size 512 512) do
	clearColor $= Color4 0.0 0.0 1.0 1.0
	clear [ColorBuffer]
	color $ Color3 @GLdouble 1.0 0.0 0.0
	renderPrimitive LineLoop $ mapM_ vertex [
		Vertex2 (- 0.9) (- 0.9) :: Vertex2 GLdouble,
		Vertex2 0.9 (- 0.9), Vertex2 0.9 0.9, Vertex2 (- 0.9) 0.9 ]
