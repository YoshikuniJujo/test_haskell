{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
-- import Control.Concurrent
import Data.Bool
import Graphics.UI.GLUT

main :: IO ()
main = do
	void $ initialize "foo" []
	win <- createWindow "foo"
	displayCallback $= display
	reshapeCallback $= Just resize
	keyboardCallback $= Just \c p -> print (c, p) >> display2 >> bool (return ()) leaveMainLoop (c == 'q')
	mainLoop

display :: DisplayCallback
display = do
	clear [ ColorBuffer ]
	color (Color3 1 0 0 :: Color3 GLdouble)
--	renderObject Solid $ Sphere' 1 20 20
	renderPrimitive Polygon $ mapM_ vertex [Vertex2 (- 0.9) (- 0.9) :: Vertex2 GLdouble, Vertex2 (- 0.9) 0.9, Vertex2 0.9 0.9]
	swapBuffers
--	flush

display2 :: DisplayCallback
display2 = do
	clear [ ColorBuffer ]
	color (Color3 0 1 0 :: Color3 GLdouble)
--	renderObject Solid $ Sphere' 1 20 20
	renderPrimitive Polygon $ mapM_ vertex [Vertex2 (- 0.9) (- 0.9) :: Vertex2 GLdouble, Vertex2 (- 0.9) 0.9, Vertex2 0.9 0.9]
	swapBuffers
--	flush

resize :: Size -> IO ()
resize s@(Size w h) = do
	viewport $= (Position 0 0, s)
	loadIdentity
	ortho (- w') (w') (- h') (h') (- 1.0) (1.0)
	where
	w' = realToFrac w / 200.0
	h' = realToFrac h / 200.0
