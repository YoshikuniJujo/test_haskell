{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent
import Data.Maybe
import Data.Char
import System.Environment

import Graphics.Gdk.General
import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkSeat
import Graphics.Gdk.GdkDevice
import Graphics.Gdk.Cursors
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.Events
import Graphics.Gdk.EventStructures
import Graphics.Gdk.EventStructures.GdkKeySyms

import Try.Tools.DoWhile

main :: IO ()
main = do
	(_pn, as) <- join $ gdkInit <$> getProgName <*> getArgs
	print as
	d <- gdkDisplayGetDefault
	st <- gdkDisplayGetDefaultSeat d
	dvp <- gdkSeatGetPointer st
	print d
	print st
	print =<< gdkDeviceGetNameAndSource dvp
	print =<< gdkDeviceGetDeviceType dvp
	slv <- gdkDeviceListSlaveDevices dvp
	(print <=< gdkDeviceGetNameAndSource) `mapM_` fromJust slv
	w <- gdkWindowNew Nothing $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits [GdkKeyPressMask])
		100 100 GdkInputOutput GdkWindowToplevel
	gdkWindowShow w
	gdkWindowSetSupportMultidevice w True
--	gdkWindowSetCursor w =<< gdkCursorNewForDisplay d GdkHand2
	gdkWindowSetDeviceCursor w dvp =<< gdkCursorNewForDisplay d GdkMouse
	print =<< gdkWindowGetSupportMultidevice w
	doWhile_ do
		threadDelay 100000
		doWhile $ gdkWithEventGet \case
			Just (GdkEventGdkKeyPress k) -> do
				let	kv = gdkEventKeyKeyval $ gdkEventKey k
				pure . Just $ kv /= GdkKeySym (fromIntegral $ ord 'q')
			Just _ -> pure Nothing
			Nothing -> pure $ Just True

gdkDeviceGetNameAndSource :: IsGdkDevice d => d -> IO (String, GdkInputSource)
gdkDeviceGetNameAndSource d =
	(,) <$> gdkDeviceGetName d <*> gdkDeviceGetSource d
