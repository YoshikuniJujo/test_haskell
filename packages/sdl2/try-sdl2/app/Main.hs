{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import SDL

main :: IO ()
main = do
	initialize [
		InitTimer, InitAudio, InitVideo, InitJoystick,
		InitGameController, InitEvents ]
	window <- createWindow "My SDL Applicsation" defaultWindow
	renderer <- createRenderer window (- 1) defaultRenderer
	appLoop renderer
	destroyWindow window

appLoop :: Renderer -> IO ()
appLoop renderer = do
{-
	events <- pollEvents
	let	eventIsQPress event =
			case eventPayload event of
				KeyboardEvent keyboardEvent ->
					keyboardEventKeyMotion keyboardEvent == Pressed &&
						keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
				_ -> False
		qpressed = any eventIsQPress events
		-}
--	unless (null events) $ print events
	maybe (pure ()) print =<< pollEvent
	rendererDrawColor renderer $= V4 0 0 255 255
	clear renderer
	present renderer
--	unless qpressed (appLoop renderer)
	unless False (appLoop renderer)
