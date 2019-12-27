{-# LANGUAGE TypeApplications, BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

-- https://tokoik.github.io/opengl/libglut.html#7

import Control.Monad (void, join)
import Control.Concurrent.STM
import System.Environment (getProgName, getArgs)

import Graphics.UI.GLUT

display :: IO ()
display = do
	clear [ColorBuffer]
	flush

resize :: Size -> IO ()
resize s@(Size w_ h_) = do
	viewport $= (Position 0 0, s)
	loadIdentity
	ortho (- 0.5) (w - 0.5) (h - 0.5) (- 0.5) (- 1) 1
	where
	w = fromIntegral w_
	h = fromIntegral h_

mouse :: TVar (Vertex2 GLdouble) -> MouseButton -> KeyState -> Position -> IO ()
mouse vp0 button state (Position x_ y_) = case button of
	LeftButton -> case state of
		Up -> do
			p0 <- atomically $ readTVar vp0
			color $ Color3 @GLdouble 0 0 0
			renderPrimitive Lines do
				vertex p0
				vertex $ Vertex2 x y
			flush
		Down -> atomically . writeTVar vp0 $ Vertex2 x y
		where
		x = fromIntegral x_
		y = fromIntegral y_
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
	initialWindowPosition $= Position 100 100
	initialWindowSize $= Size 640 480
	void . join $ initialize <$> getProgName <*> getArgs
	initialDisplayMode $= [RGBAMode]
	void $ createWindow =<< getProgName
	displayCallback $= display
	reshapeCallback $= Just resize
	mouseCallback $= Just (mouse vp0)
	initializeWindow
	mainLoop
