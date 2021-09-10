{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.Char
import Data.KeySym
import System.Environment

import Graphics.Gdk.General
import Graphics.Gdk.Windows
import Graphics.Gdk.EventStructures
import Try.Tools

main :: IO ()
main = do
	print =<< join (gdkInit <$> getProgName <*> getArgs)
	w <- gdkToplevelNew Nothing defaultGdkWindowAttr
	gdkWindowSetTitle w "あいうえお"
	gdkWindowShow w
	mainLoop \case
		GdkEventGdkDelete _d -> pure False
		GdkEventGdkKeyPress k -> do
			kv <- gdkEventKeyKeyval <$> gdkEventKey k
			print k
			pure $ kv /= KeySym (fromIntegral $ ord 'q')
		e -> True <$ print e
