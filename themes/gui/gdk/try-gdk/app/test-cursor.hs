{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.Char
import System.Environment

import Graphics.Gdk.General
import Graphics.Gdk.Windows
import Graphics.Gdk.Event
import Graphics.Gdk.Cursors
import Try.Tools

main :: IO ()
main = do
	print =<< join (gdkInit <$> getProgName <*> getArgs)
	w <- gdkWindowNew Nothing defaultGdkWindowAttr
	d <- gdkWindowGetDisplay w
	gdkWindowShow w
	gdkWindowSetCursor w =<< gdkCursorNewFromName d "crosshair"
	mainLoop \case
		GdkEventGdkDelete _d -> pure False
		GdkEventGdkKeyPress k -> do
			kv <- gdkEventKeyKeyval k
			pure $ kv /= fromIntegral (ord 'q')
		e -> True <$ print e
