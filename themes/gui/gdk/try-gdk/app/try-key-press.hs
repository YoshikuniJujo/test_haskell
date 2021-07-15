{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent
import System.Environment

import Graphics.Gdk.General
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Events
import Graphics.Gdk.EventStructures
import Graphics.Gdk.EventStructures.GdkKeySyms
import Graphics.Gdk.Values

import Try.Tools.DoWhile

main :: IO ()
main = do
	(_, _as) <- join $ gdkInit <$> getProgName <*> getArgs
	w <- gdkWindowNew Nothing gdkWindowAttr
	gdkWindowShow w
	doWhile_ do
		threadDelay 100000
		doWhile $ gdkEventGet >>= \case
			Just (GdkEventGdkKeyPress (GdkEventKeyRaw { gdkEventKeyRawKeyval = GdkKey_q })) -> pure $ Just False
			Just (GdkEventGdkKeyPress k) -> Nothing <$ print k
			Just e -> Nothing <$ print e
			Nothing -> pure $ Just True

gdkWindowAttr :: GdkWindowAttr
gdkWindowAttr = minimalGdkWindowAttr
	(gdkEventMaskMultiBits [GdkButtonPressMask]) 400 400 gdkInputOutput
	GdkWindowToplevel
