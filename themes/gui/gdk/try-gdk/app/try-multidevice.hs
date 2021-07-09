{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent
import Data.Char
import System.Environment

import Graphics.Gdk.General
import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkSeat
import Graphics.Gdk.GdkDevice
import Graphics.Gdk.Windows
import Graphics.Gdk.Events
import Graphics.Gdk.Values

import Try.Tools

main :: IO ()
main = do
	(_pn, as) <- join $ gdkInit <$> getProgName <*> getArgs
	print as
	d <- gdkDisplayGetDefault
	st <- gdkDisplayGetDefaultSeat d
	dvp <- gdkSeatGetPointer st
	print d
	print st
	print =<< gdkDeviceGetName dvp
	print =<< gdkDeviceGetDeviceType dvp
	slv <- gdkDeviceListSlaveDevices dvp
	mapM_ (print <=< gdkDeviceGetName) slv
	w <- gdkWindowNew Nothing $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits [GdkKeyPressMask])
		100 100 gdkInputOutput GdkWindowToplevel
	gdkWindowShow w
	gdkWindowSetSupportMultidevice w True
	print =<< gdkWindowGetSupportMultidevice w
	doWhile_ do
		threadDelay 100000
		doWhile $ gdkEventGet >>= \case
			Just (GdkEventGdkKeyPress k) -> do
				kv <- gdkEventKeyKeyval k
				pure . Just $ kv /= fromIntegral (ord 'q')
			Just _ -> pure Nothing
			Nothing -> pure $ Just True
