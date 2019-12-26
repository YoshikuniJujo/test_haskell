{-# LANGUAGE TypeApplications, BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

-- https://tokoik.github.io/opengl/libglut.html#5

import Control.Monad (void, join)
import System.Environment (getProgName, getArgs)

import Graphics.UI.GLUT

display :: IO ()
display = do
	clear [ColorBuffer]
	color (Color3 1 1 0 :: Color3 GLdouble)
	primitive Points 6 0 0
	primitive Lines 6 1 0
	primitive LineStrip 6 0 1
	primitive LineLoop 6 1 1
	primitive TriangleStrip 6 0 2
	primitive QuadStrip 6 1 2
	primitive Triangles 6 0 3
	prim Quads $ (points 1 3 !!) <$> [0, 1, 3, 2, 4, 5, 7, 6]
	prim TriangleFan $ points2 0 4
	prim Polygon $ points2 1 4
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

prim :: PrimitiveMode -> [(GLdouble, GLdouble)] -> IO ()
prim pm ps = renderPrimitive pm $ mapM_ (vertex . uncurry Vertex2) ps

primitive :: PrimitiveMode -> Int -> Int -> Int -> IO ()
primitive pm n x y = prim pm . take n $ points x y

points :: Int -> Int -> [(GLdouble, GLdouble)]
points x_ y_ = (<$> [0 :: Int ..]) \n ->
	(x0 + 0.1 * fromIntegral n, y0 - 0.15 * fromIntegral (n `mod` 2))
	where
	x0 = fromIntegral x_ * 0.8 - 0.8 :: GLdouble
	y0 = - (fromIntegral y_ * 0.35) + 0.8 :: GLdouble

points2 :: Int -> Int -> [(GLdouble, GLdouble)]
points2 x_ y_ = [
	(x0 + 0.3, y0), (x0, y0 - 0.15), (x0 + 0.12, y0 - 0.29),
	(x0 + 0.3, y0 - 0.35), (x0 + 0.48, y0 - 0.29), (x0 + 0.6, y0 - 0.15)
	]
	where
	x0 = fromIntegral x_ * 0.8 - 0.8 :: GLdouble
	y0 = - (fromIntegral y_ * 0.35) + 0.8 :: GLdouble
