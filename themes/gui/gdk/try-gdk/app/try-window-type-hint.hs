{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.Char
import Data.KeySym
import Text.Read
import System.Environment

import Graphics.Gdk.General
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.EventStructures
import Try.Tools

main :: IO ()
main = do
	_ <- gdkInit "foo" []
	w <- gdkToplevelNew Nothing $ minimalGdkWindowAttr
		GdkZeroEventsMask 100 100
	h <- fromMaybe GdkWindowTypeHintNormal . readMaybe . head <$> getArgs
	print h
	gdkWindowSetTypeHint w h
	gdkWindowShow w
	mainLoop \case
		GdkEventGdkKeyPress k -> do
			kv <- gdkEventKeyKeyval <$> gdkEventKey k
			pure $ kv /= KeySym (fromIntegral $ ord 'q')
		GdkEventGdkAny a -> True <$ print a
