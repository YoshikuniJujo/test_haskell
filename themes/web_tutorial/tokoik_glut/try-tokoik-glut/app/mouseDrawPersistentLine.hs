{-# LANGUAGE TypeApplications, BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

-- https://tokoik.github.io/opengl/libglut.html#7

import Control.Arrow
import Control.Monad (void, join)
import Control.Concurrent.STM
import System.Environment (getProgName, getArgs)

import Graphics.UI.GLUT

type Line = (Vertex2 GLdouble, Vertex2 GLdouble)

display :: TVar [Line] -> IO ()
display vls = do
	clear [ColorBuffer]
	color $ Color3 @GLdouble 0 0 0
	renderPrimitive Lines . mapM_ (uncurry (>>) . (vertex *** vertex))
		=<< atomically (readTVar vls)
	flush

resize :: Size -> IO ()
resize s@(Size w_ h_) = do
	viewport $= (Position 0 0, s)
	loadIdentity
	ortho (- 0.5) (w - 0.5) (h - 0.5) (- 0.5) (- 1) 1
	where
	w = fromIntegral w_
	h = fromIntegral h_

mouse :: TVar [Line] ->
	TVar (Vertex2 GLdouble) -> MouseButton -> KeyState -> Position -> IO ()
mouse vls vp0 button state (Position x_ y_) = case button of
	LeftButton -> case state of
		Up -> do
			color $ Color3 @GLdouble 0 0 0
			renderPrimitive Lines . uncurry (>>)
				. (vertex *** vertex) =<< atomically do
					l <- (, p) <$> readTVar vp0
					l <$ modifyTVar vls (l :)
			flush
		Down -> atomically $ writeTVar vp0 p
		where p = Vertex2 (fromIntegral x_) (fromIntegral y_)
	MiddleButton -> return ()
	RightButton -> return ()
	WheelUp -> return ()
	WheelDown -> return ()
	_ -> return ()

initializeWindow :: IO ()
initializeWindow = clearColor $= Color4 1 1 1 1

main :: IO ()
main = do
	vp0 <- atomically . newTVar $ Vertex2 0 0
	vls <- atomically $ newTVar []
	initialWindowPosition $= Position 100 100
	initialWindowSize $= Size 640 480
	void . join $ initialize <$> getProgName <*> getArgs
	initialDisplayMode $= [RGBAMode]
	void $ createWindow =<< getProgName
	displayCallback $= display vls
	reshapeCallback $= Just resize
	mouseCallback $= Just (mouse vls vp0)
	initializeWindow
	mainLoop
