{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (init)

import Control.Monad
import Data.Bool
import Data.IORef
import Graphics.UI.GLUT
import System.Environment

import Fbo

fboWidth, fboHeight :: GLsizei
fboWidth = 512; fboHeight = 512

main :: IO ()
main = do
	(wdt, hgt) <- (,) <$> newIORef 0 <*> newIORef 0
	pn <- getProgName
	void $ initialize pn =<< getArgs
	void $ createWindow pn
	reshapeCallback $= Just \(Size w h) ->
		writeIORef wdt w >> writeIORef hgt h
	keyboardCallback $= Just \c p ->
		print (c, p) >> bool (return ()) leaveMainLoop (c == 'q')

	(fb, cb) <- initializeFbo fboWidth fboHeight
	displayCallback $= do
		drawFramebuffer fb fboWidth fboHeight
		drawTexture cb wdt hgt

	mainLoop
