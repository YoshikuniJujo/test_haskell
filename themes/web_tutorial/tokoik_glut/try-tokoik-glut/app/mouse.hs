{-# LANGUAGE TypeApplications, BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

-- https://tokoik.github.io/opengl/libglut.html#7

import Control.Monad (void, join)
import System.Environment (getProgName, getArgs)

import Graphics.UI.GLUT

display :: IO ()
display = do
	clear [ColorBuffer]
	flush

resize :: Size -> IO ()
resize s = do
	viewport $= (Position 0 0, s)
	loadIdentity

mouse :: MouseButton -> KeyState -> Position -> IO ()
mouse button state (Position x y) = do
	case button of
		LeftButton -> putStr "left"
		MiddleButton -> putStr "middle"
		RightButton -> putStr "right"
		WheelUp -> putStr "wheel up"
		WheelDown -> putStr "wheel down"
		_ -> return ()
	putStr " button is "
	case state of
		Up -> putStr "up"
		Down -> putStr "down"
	putStrLn $ " at (" ++ show x ++ ", " ++ show y ++ ")"

initializeWindow :: IO ()
initializeWindow = clearColor $= Color4 1 1 1 1

main :: IO ()
main = do
	initialWindowPosition $= Position 100 100
	initialWindowSize $= Size 640 480
	void . join $ initialize <$> getProgName <*> getArgs
	initialDisplayMode $= [RGBAMode]
	void $ createWindow =<< getProgName
	displayCallback $= display
	reshapeCallback $= Just resize
	mouseCallback $= Just mouse
	initializeWindow
	mainLoop
