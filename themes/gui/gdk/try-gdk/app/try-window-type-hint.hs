{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.Char
import Text.Read
import System.Environment

import Graphics.Gdk.General
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.EventStructures
import Graphics.Gdk.EventStructures.GdkKeySyms
import Graphics.Gdk.Values
import Try.Tools

main :: IO ()
main = do
	_ <- gdkInit "foo" []
	w <- gdkWindowNew Nothing $ minimalGdkWindowAttr
		GdkZeroEventsMask 100 100 gdkInputOutput GdkWindowToplevel
	h <- fromMaybe GdkWindowTypeHintNormal . readMaybe . head <$> getArgs
	print h
	gdkWindowSetTypeHint w h
	gdkWindowShow w
	mainLoopNew \case
		GdkEventGdkKeyPress k -> do
			let	kv = gdkEventKeyKeyval $ gdkEventKey k
			pure $ kv /= GdkKeySym (fromIntegral $ ord 'q')
		GdkEventGdkAny a -> True <$ print a
