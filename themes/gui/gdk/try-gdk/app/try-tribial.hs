{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.Char
import System.Environment

import Graphics.Gdk.General
import Graphics.Gdk.Cursors
import Graphics.Gdk.Windows
import Graphics.Gdk.Events
import Try.Tools

import Foreign.Marshal

main :: IO ()
main = do
	print =<< join (gdkInit <$> getProgName <*> getArgs)
	w <- gdkWindowNew Nothing defaultGdkWindowAttr
--	gdkWindowSetTitle w "あいうえお"
	p <- mallocBytes 20
	pokeArray p [104, 101, 108, 108, 111]
	c_gdk_window_set_title w p
	pokeArray p [111, 108, 108, 101, 104]
	free p
	gdkWindowShow w
	dpy <- gdkWindowGetDisplay w
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
			kv <- gdkEventKeyKeyval k
			pure $ kv /= fromIntegral (ord 'q')
		_ -> pure True
