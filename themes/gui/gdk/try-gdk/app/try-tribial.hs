{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.Char
import System.Environment

import Graphics.Gdk.General
import Graphics.Gdk.Cursors
import Graphics.Gdk.Windows
import Graphics.Gdk.EventStructures
import Graphics.Gdk.EventStructures.GdkKeySyms
import Try.Tools

main :: IO ()
main = do
	print =<< join (gdkInit <$> getProgName <*> getArgs)
	w <- gdkToplevelNew Nothing defaultGdkWindowAttr
	gdkWindowSetTitle w "あいうえお"
	gdkWindowShow w
	let	dpy = gdkWindowGetDisplay w
	c <- gdkCursorNewForDisplay dpy GdkMan
	gdkWindowSetCursor w c
	print c
	print =<< gdkWindowGetCursor w
	d <- gdkCursorNewForDisplay dpy GdkStar
	gdkWindowSetCursor w d
	print d
	print =<< gdkWindowGetCursor w
	mainLoop \case
		GdkEventGdkDelete _d -> pure False
		GdkEventGdkKeyPress k -> do
			kv <- gdkEventKeyKeyval <$> gdkEventKey k
			pure $ kv /= GdkKeySym (fromIntegral $ ord 'q')
		_ -> pure True
