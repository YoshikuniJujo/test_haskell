{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent
import Data.KeySym
import System.Environment

import Graphics.Gdk.General
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.Events
import Graphics.Gdk.EventStructures

import Try.Tools.DoWhile

main :: IO ()
main = do
	(_, _as) <- join $ gdkInit <$> getProgName <*> getArgs
	w <- gdkToplevelNew Nothing gdkWindowAttr
	gdkWindowShow w
	doWhile_ do
		threadDelay 100000
		doWhile $ gdkWithEvent \case
			Just (GdkEventGdkKeyPress e) -> do
				k <- gdkEventKey e
				print $ gdkEventKeyHardwareKeycode k
				print $ gdkEventKeyKeyval k
				pure case k of
					GdkEventKey { gdkEventKeyKeyval = Xk_q } -> Just False
					_ -> Nothing
			Just e -> Nothing <$ print e
			Nothing -> pure $ Just True

gdkWindowAttr :: GdkWindowAttr
gdkWindowAttr = minimalGdkWindowAttr
	(gdkEventMaskMultiBits [GdkKeyPressMask]) 400 400
