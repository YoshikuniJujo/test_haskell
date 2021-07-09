{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent
import Data.Maybe
import Data.Char
import Text.Read
import System.Environment

import Graphics.Gdk.General
import Graphics.Gdk.Windows
import Graphics.Gdk.Events
import Graphics.Gdk.Values

import Try.Tools

main :: IO ()
main = do
	(_pn, as) <- join $ gdkInit <$> getProgName <*> getArgs
	print as
	let	ds = catMaybes $ readMaybe <$> as
		ds' = gdkWMDecorations ds
	w <- gdkWindowNew Nothing $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits [GdkKeyPressMask])
		100 100 gdkInputOutput GdkWindowToplevel
	print ds
	gdkWindowSetDecorations w ds'
	gdkWindowShow w
	doWhile_ do
		threadDelay 100000
		doWhile $ gdkEventGet >>= \case
			Just (GdkEventGdkKeyPress k) -> do
				kv <- gdkEventKeyKeyval k
				pure . Just $ kv /= fromIntegral (ord 'q')
			Just _ -> pure $ Just True
			Nothing -> pure Nothing
