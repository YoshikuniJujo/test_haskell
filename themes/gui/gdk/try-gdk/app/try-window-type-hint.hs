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
import Graphics.Gdk.Events
import Graphics.Gdk.EventStructures
import Graphics.Gdk.Values
import Try.Tools

main :: IO ()
main = do
	gdkInit "foo" []
	w <- gdkWindowNew Nothing $ minimalGdkWindowAttr
		GdkZeroEventsMask 100 100 gdkInputOutput GdkWindowToplevel
	h <- fromMaybe GdkWindowTypeHintNormal . readMaybe . head <$> getArgs
	print h
	gdkWindowSetTypeHint w h
	gdkWindowShow w
	mainLoop \case
		GdkEventGdkKeyPress k -> do
			kv <- gdkEventKeyKeyval k
			pure $ kv /= fromIntegral (ord 'q')
		GdkEvent et p -> do
			putStrLn $ show et ++ " " ++ show p
			pure True
